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

function init() {
  if (!require("./buildocaml.js").checkEnvCompiler()) {
    // No compiler available on path.
    // Assume that we built the compiler from source beforehand and add it to the path.
    var vendorOCamlPath = path.join(
      __dirname,
      "..",
      "native",
      require("./buildocaml.js").getVersionPrefix(),
      "bin"
    );

    process.env["PATH"] =
      vendorOCamlPath + path.delimiter + process.env["PATH"];
  }

  var ninjaPath = path.join(__dirname, "..", process.platform, "ninja.exe");

  if (!fs.existsSync(ninjaPath)) {
    throw new Error("ninja could not be configured");
  }
}

function runTests() {
  // when binary was prebuilt, there can be no ocaml installation
  // var output =
  //     cp.execSync('which ocaml', { encoding: 'ascii' })
  // console.log('OCaml:', output)

  var binDir = path.join(__dirname, "..", "jscomp", "bin");
  if (ounitTest) {
    cp.execSync(
      `ocamlc.opt -w "-d" -warn-error -a -I . -c js_compiler.mli js_compiler.ml`,
      {
        cwd: path.join(__dirname, "..", "lib", "4.06.1", "unstable"),
        stdio: [0, 1, 2],
      }
    );
    // running tests for native code
    fs.copyFileSync(
      path.join(
        __dirname,
        "..",
        "lib",
        "4.06.1",
        "unstable",
        "all_ounit_tests.ml"
      ),
      path.join(binDir, "all_ounit_tests.ml")
    );
    cp.execSync(
      `ocamlopt.opt -g -w -40-30 -w "-d" ../stubs/ext_basic_hash_stubs.c  unix.cmxa str.cmxa all_ounit_tests.ml -o test.exe`,
      {
        cwd: binDir,
        stdio: [0, 1, 2],
      }
    );

    cp.execSync(path.join(binDir, "test.exe"), {
      cwd: binDir,
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
    init();
    runTests();
  } catch (err) {
    console.error(err);
    process.exit(2);
  }
}
main();
