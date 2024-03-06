//@ts-check
var cp = require("child_process");
var path = require("path");
var fs = require("fs");

var duneBinDir = require("./dune").duneBinDir;

var ounitTest = false;
var mochaTest = false;
var bsbTest = false;
var formatTest = false;
var all = false;

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
  all = true;
}
if (all) {
  ounitTest = true;
  mochaTest = true;
  bsbTest = true;
  formatTest = true;
}

async function runTests() {
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
    var buildTestDir = path.join(__dirname, "..", "jscomp", "build_tests");
    var files = fs.readdirSync(buildTestDir);
    var tasks = files.map(async function (file) {
      // @ts-ignore
      let resolve, reject;
      let promise = new Promise((res, rej) => {
        resolve = res;
        reject = rej;
      });
      var testDir = path.join(buildTestDir, file);
      if (file === "node_modules" || !fs.lstatSync(testDir).isDirectory()) {
        return;
      }
      if (!fs.existsSync(path.join(testDir, "input.js"))) {
        console.warn(`input.js does not exist in ${testDir}`);
      } else {
        console.log(`testing ${file}`);

        // note existsSync test already ensure that it is a directory
        let p = cp.spawn(`node`, ["input.js"], { cwd: testDir });

        p.stdout.setEncoding("utf8").on("data", line => {
          console.log(line);
        });

        let stderr = "";
        p.stderr.setEncoding("utf8").on("data", line => {
          stderr += line + "\n";
        });

        p.once("error", err => {
          console.log(`❌ error in ${file} with stderr:\n`, stderr);
          // @ts-ignore
          reject(err);
        });

        p.once("close", () => {
          if (!stderr) {
            console.log("✅ success in", file);
          }
          // @ts-ignore
          resolve();
        });
      }

      return promise;
    });

    await Promise.all(tasks);
  }

  if (formatTest) {
    cp.execSync("npm run checkFormat", {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });
  }
}

async function main() {
  try {
    await runTests();
  } catch (err) {
    console.error(err);
    process.exit(2);
  }
}
main();
