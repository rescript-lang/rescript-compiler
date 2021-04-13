//@ts-check
var cp = require("child_process");

var path = require("path");

var installGlobal = false;
var ounitTest = false;
var mochaTest = false;
var themeTest = false;
var bsbTest = false;
var all = false;

if (process.argv.includes("-install-global")) {
  installGlobal = true;
}

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

if (process.argv.includes("-all")) {
  all = true;
}
if (all) {
  installGlobal = true;
  ounitTest = true;
  mochaTest = true;
  themeTest = true;
  bsbTest = true;
}

var fs = require("fs");

function init() {
  var vendorOCamlPath = path.join(
    __dirname,
    "..",
    "native",
    require("./buildocaml.js").getVersionPrefix(),
    "bin"
  );

  process.env["PATH"] = vendorOCamlPath + path.delimiter + process.env["PATH"];

  var ninjaPath = path.join(__dirname, "..", process.platform, "ninja.exe");

  if (!fs.existsSync(ninjaPath)) {
    throw new Error("ninja could not be configured");
  }
}

function main() {
  init();

  // when binary was prebuilt, there can be no ocaml installation
  // var output =
  //     cp.execSync('which ocaml', { encoding: 'ascii' })
  // console.log('OCaml:', output)
  var binDir = path.join(__dirname, "..", "jscomp", "bin");
  if (ounitTest) {
    cp.execSync(
      `ocamlc.opt -I . -c js_compiler.mli js_compiler.ml`,
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
      `ocamlopt.opt -g -w -40-30 ../stubs/ext_basic_hash_stubs.c -I +compiler-libs ocamlcommon.cmxa unix.cmxa str.cmxa all_ounit_tests.ml -o test.exe`,
      {
        cwd: binDir,
        stdio: [0, 1, 2],
      }
    );

    cp.execSync(`./test.exe`, { cwd: binDir, stdio: [0, 1, 2] });
  }

  // running generated js tests
  if (mochaTest) {
    cp.execSync(`mocha jscomp/test/**/*test.js`, {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });
  }

  // set up global directory properly using
  // npm config set prefix '~/.npm-global'
  if (installGlobal) {
    console.log("install bucklescript globally");
    cp.execSync("sudo npm i -g --unsafe-perm . && bsc -bs-internal-check", {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });
  }

  var bsbDir = cp
    .execSync(`bsb -where`, {
      cwd: path.join(__dirname, ".."),
      encoding: "utf8",
    })
    .trim();

  console.log("BSBDIR:", bsbDir);

  if (themeTest) {
    console.log("Doing theme tests");
    var themeOutput = cp.execSync(`bsb -themes`, { encoding: "ascii" });
    var themes = themeOutput
      .split("\n")
      .slice(1)
      .map((x) => x.trim())
      .filter((x) => x);
    var themesDir = path.join(__dirname, "..", "themes");

    if (fs.existsSync(themesDir)) {
      // fs.rmdirSync(themesDir,{recursive : true})
      cp.execSync(`rm -rf ${themesDir}`, { stdio: [0, 1, 2] });
      // we dont remove post-installation
      // since it is useful for debugging
    }
    fs.mkdirSync(themesDir);
    themes.forEach(function (theme) {
      cp.exec(
        `bsb -theme ${theme} -init ${theme}`,
        { cwd: themesDir, encoding: "utf8" },
        function (error, stdout, stderr) {
          console.log(stdout);
          console.log(stderr);
          if (error !== null) {
            throw new Error(`init theme ${theme} failed`);
          }
          let config = {
            cwd: path.join(themesDir, theme),
            encoding: "utf8",
          };
          var output;
          try {
            output = cp.execSync(`npm link rescript`, config);
            output = cp.execSync(`npm install`, config);
            output = cp.execSync(`npm run clean`, config);
            output = cp.execSync(`npm run build`, config);
          } catch (err) {
            console.error(`failed in theme ${theme}`);
            console.log(output + "");
            console.log(err + "");
          }
        }
      );
    });
  }

  if (bsbTest) {
    console.log("Doing build_tests");
    var buildTestDir = path.join(__dirname, "..", "jscomp", "build_tests");
    cp.execSync(`npm link rescript`, {
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
        // note existsSync test already ensure that it is a directory
        cp.exec(`node input.js`, { cwd: testDir, encoding: "utf8" }, function (
          error,
          stdout,
          stderr
        ) {
          console.log(stdout);

          if (error !== null) {
            console.log(stderr);
            throw new Error(`working in ${testDir} Error: \n${error} `);
          }
          console.log("success in ", file);
        });
      }
    });
  }
}

main();
