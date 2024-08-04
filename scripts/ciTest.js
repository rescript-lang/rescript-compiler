//@ts-check
const cp = require("child_process");
const path = require("path");
const fs = require("fs");

const duneBinDir = require("./dune").duneBinDir;

const { exec } = require("../jscomp/build_tests/utils.js");

let ounitTest = false;
let mochaTest = false;
let bsbTest = false;
let formatTest = false;

if (process.argv.includes("-ounit")) {
  ounitTest = true;
}

if (process.argv.includes("-mocha")) {
  mochaTest = true;
}

if (process.argv.includes("-bsb")) {
  bsbTest = true;
}

if (process.argv.includes("-format")) {
  formatTest = true;
}

if (process.argv.includes("-all")) {
  ounitTest = true;
  mochaTest = true;
  bsbTest = true;
  formatTest = true;
}

async function runTests() {
  if (formatTest) {
    cp.execSync("npm run checkFormat", {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });

    cp.execSync("bash scripts/format_check.sh", {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });
  }

  if (ounitTest) {
    cp.execSync(path.join(duneBinDir, "ounit_tests"), {
      stdio: [0, 1, 2],
    });
  }

  // running generated js tests
  if (mochaTest) {
    cp.execSync(`npx mocha -t 10000 jscomp/test/**/*test.js`, {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });
  }

  if (bsbTest) {
    console.log("Doing build_tests");
    const buildTestDir = path.join(__dirname, "..", "jscomp", "build_tests");
    const files = fs.readdirSync(buildTestDir);

    let hasError = false;

    for (const file of files) {
      const testDir = path.join(buildTestDir, file);
      if (file === "node_modules" || !fs.lstatSync(testDir).isDirectory()) {
        break;
      }
      if (!fs.existsSync(path.join(testDir, "input.js"))) {
        console.warn(`input.js does not exist in ${testDir}`);
      } else {
        console.log(`testing ${file}`);

        // note existsSync test already ensure that it is a directory
        const out = await exec(`node`, ["input.js"], { cwd: testDir });
        console.log(out.stdout);

        if (out.status === 0) {
          console.log("✅ success in", file);
        } else {
          console.log(`❌ error in ${file} with stderr:\n`, out.stderr);
          hasError = true;
        }
      }
    }

    if (hasError) {
      process.exit(1);
    }
  }
}

runTests();
