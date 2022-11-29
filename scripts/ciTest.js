//@ts-check
var cp = require("child_process");
var path = require("path");
var fs = require("fs");

var ounitTest = false;
var mochaTest = false;
var themeTest = false;
var bsbTest = false;
var formatTest = false;
var all = false;

if (process.argv.includes("-ounit")) {
  ounitTest = true;
}

if (process.argv.includes("-mocha")) {
  mochaTest = true;
}

if (process.argv.includes("-theme")) {
  themeTest = true;
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
  themeTest = true;
  bsbTest = true;
  formatTest = true;
}

function runTests() {
  if (ounitTest) {
    cp.execSync("ounit_tests", {
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
    var linkCmd = `npm link ../..`;
    console.log(linkCmd);
    cp.execSync(linkCmd, {
      cwd: buildTestDir,
      stdio: [0, 1, 2],
      encoding: "utf8",
    });
    var files = fs.readdirSync(buildTestDir);
    files.forEach(function (file) {
      var testDir = path.join(buildTestDir, file);
      if (file === "node_modules" || !fs.lstatSync(testDir).isDirectory()) {
        return;
      }
      if (!fs.existsSync(path.join(testDir, "input.js"))) {
        console.warn(`input.js does not exist in ${testDir}`);
      } else {
        console.log(`testing ${file}`);
        // note existsSync test already ensure that it is a directory
        cp.exec(
          `node input.js`,
          { cwd: testDir, encoding: "utf8" },
          function (error, stdout, stderr) {
            console.log(stdout);

            if (error !== null) {
              console.log(stderr);
              throw new Error(`❌ error in ${file}: \n${error} `);
            } else {
              console.log("✅ success in ", file);
            }
          }
        );
      }
    });
  }

  if (formatTest) {
    cp.execSync("npm run checkFormat", {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });
  }
}

function main() {
  try {
    runTests();
  } catch (err) {
    console.error(err);
    process.exit(2);
  }
}
main();
