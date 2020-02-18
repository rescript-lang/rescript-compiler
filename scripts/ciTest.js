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
    var fn = fs.copyFileSync ? fs.copyFileSync : fs.renameSync;
    fn(
      path.join(
        __dirname,
        "..",
        "lib",
        require("./buildocaml.js").getVersionPrefix(),
        "unstable",
        "all_ounit_tests.ml"
      ),
      path.join(binDir, "all_ounit_tests.ml")
    );
    cp.execSync(
      `ocamlopt.opt -g -w -40-30 ../stubs/ext_basic_hash_stubs.c -I +compiler-libs ocamlcommon.cmxa unix.cmxa str.cmxa all_ounit_tests.ml -o test.exe`,
      {
        cwd: binDir,
        stdio: [0, 1, 2]
      }
    );
    cp.execSync(`./test.exe`, { cwd: binDir, stdio: [0, 1, 2] });
  }

  if (mochaTest) {
    cp.execSync(`mocha jscomp/test/**/*test.js`, {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2]
    });
  }

  if (installGlobal) {
    console.log("install bucklescript globally");
    cp.execSync("sudo npm i -g --unsafe-perm . && bsc -bs-internal-check", {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2]
    });
  }

  var bsbDir = cp
    .execSync(`bsb -where`, {
      cwd: path.join(__dirname, ".."),
      encoding: "utf8"
    })
    .trim();

  console.log("BSBDIR:", bsbDir);

  if (themeTest) {
    console.log("Doing theme tests");
    var themeOutput = cp.execSync(`bsb -themes`, { encoding: "ascii" });
    var themes = themeOutput
      .split("\n")
      .slice(1)
      .map(x => x.trim())
      .filter(x => x);
    var themesDir = path.join(__dirname, "..", "themes");
    fs.mkdirSync(themesDir);
    themes.forEach(function(theme) {
      cp.exec(
        `bsb -theme ${theme} -init ${theme}`,
        { cwd: themesDir, encoding: "utf8" },
        function(error, stdout, stderr) {
          console.log(stdout);
          console.log(stderr);
          if (error !== null) {
            throw new Error(`init theme ${theme} failed`);
          }
          cp.exec(
            `npm install && npm run clean && npm run build`,
            { cwd: path.join(themesDir, theme) },
            function(error, stdout, stderr) {
              console.log(stdout);
              console.log(stderr);
              if (error !== null) {
                throw new Error(
                  `clean && install & build theme ${theme} failed`
                );
              }
            }
          );
        }
      );
    });
  }

  if (bsbTest) {
    console.log("Doing build_tests");
    var buildTestDir = path.join(__dirname, "..", "jscomp", "build_tests");
    cp.execSync(`npm link bs-platform`, {
      cwd: buildTestDir,
      stdio: [0, 1, 2],
      encoding: "utf8"
    });
    var files = fs.readdirSync(buildTestDir);
    files.forEach(function(file) {
      var testDir = path.join(buildTestDir, file);
      if (!fs.existsSync(path.join(testDir, "input.js"))) {
        console.warn(`input.js does not exist in ${testDir}`);
      } else {
        // note existsSync test already ensure that it is a directory
        cp.exec(`node input.js`, { cwd: testDir, encoding: "utf8" }, function(
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
