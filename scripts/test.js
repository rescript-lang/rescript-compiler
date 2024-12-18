//@ts-check
const cp = require("child_process");
const path = require("path");
const fs = require("fs");
const { rescript_exe } = require("#cli/bin_path");

const duneBinDir = require("./dune").duneBinDir;

const { exec } = require("../tests/build_tests/utils.js");

let ounitTest = false;
let mochaTest = false;
let bsbTest = false;
let formatTest = false;
let runtimeDocstrings = false;

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

if (process.argv.includes("-docstrings")) {
  runtimeDocstrings = true;
}

if (process.argv.includes("-all")) {
  ounitTest = true;
  mochaTest = true;
  bsbTest = true;
  formatTest = true;
  runtimeDocstrings = true;
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
    if (process.platform === "win32") {
      console.log("Skipping OUnit tests on Windows");
    } else {
      cp.execSync(path.join(duneBinDir, "ounit_tests"), {
        stdio: [0, 1, 2],
      });
    }
  }

  if (mochaTest) {
    cp.execSync(`${rescript_exe} clean`, {
      cwd: path.join(__dirname, "..", "tests/tests"),
      stdio: [0, 1, 2],
    });

    cp.execSync(`${rescript_exe} build`, {
      cwd: path.join(__dirname, "..", "tests/tests"),
      stdio: [0, 1, 2],
    });

    cp.execSync("npx mocha -t 10000 tests/tests/**/*_test.mjs", {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });

    cp.execSync("node tests/tests/src/core/Core_TestSuite.mjs", {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });

    cp.execSync("node tests/tests/src/core/Core_TempTests.mjs", {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });
  }

  if (bsbTest) {
    console.log("Doing build_tests");
    const buildTestDir = path.join(__dirname, "..", "tests", "build_tests");
    const files = fs.readdirSync(buildTestDir);

    let hasError = false;

    for (const file of files) {
      const testDir = path.join(buildTestDir, file);
      if (file === "node_modules" || !fs.lstatSync(testDir).isDirectory()) {
        continue;
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

  if (runtimeDocstrings) {
    if (process.platform === "win32") {
      console.log(`Skipping docstrings tests on ${process.platform}`);
    } else {
      console.log("Running runtime docstrings tests");
      cp.execSync(`${rescript_exe} build`, {
        cwd: path.join(__dirname, "..", "tests/docstrings_examples"),
        stdio: [0, 1, 2],
      });
      // Ignore some tests not supported by node v18
      cp.execSync(
        `node ${path.join("tests", "docstrings_examples", "DocTest.res.mjs")} --ignore-runtime-tests "Array.toReversed, Array.toSorted, Promise.withResolvers, Set.union, Set.isSupersetOf, Set.isSubsetOf, Set.isDisjointFrom, Set.intersection, Set.symmetricDifference, Set.difference"`,
        {
          cwd: path.join(__dirname, ".."),
          stdio: [0, 1, 2],
        },
      );
    }
  }
}

runTests();
