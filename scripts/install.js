//@ts-check
// For Windows, we distribute a prebuilt bsc.exe
// To build on windows, we still need figure out constructing config.ml
// from existing  compiler

// For other OSes, we detect
// if there is other compiler installed and the version matches,
// we get the config.ml from existing OCaml compiler and build whole_compiler

// Otherwise, we build the compiler shipped with Buckle and use the
// old compiler.ml
// This will be run in npm postinstall, don't use too fancy features here

var cp = require("child_process");
var fs = require("fs");
var path = require("path");
var root_dir = path.join(__dirname, "..");
var lib_dir = path.join(root_dir, "lib");
var jscomp_dir = path.join(root_dir, "jscomp");

var supported_os = ["darwin", "freebsd", "linux", "openbsd", "win32"];
if (supported_os.indexOf(process.platform) < 0) {
  throw new Error("Not supported platform" + process.platform);
}
var is_windows = process.platform === "win32";

var ninja_bin_output = path.join(root_dir, process.platform, "ninja.exe");

/**
 * Make sure `ninja_bin_output` exists
 * The installation of `ninja.exe` is re-entrant, since we always pre-check if it is already installed
 * This is less problematic since `ninja.exe` is very stable
 */
function provideNinja() {
  var vendor_ninja_version = "1.9.0.git";
  var ninja_source_dir = path.join(root_dir, "vendor", "ninja");
  function build_ninja() {
    console.log(`building ninja`);
    ensureExists(ninja_source_dir);
    if (fs.existsSync(path.join(root_dir, "vendor", "ninja.tar.gz"))) {
      cp.execSync(`tar xzvf ../ninja.tar.gz`, {
        cwd: ninja_source_dir,
        stdio: [0, 1, 2],
      });
      console.log("No prebuilt Ninja, building Ninja now");
      var build_ninja_command = "./configure.py --bootstrap";
      cp.execSync(build_ninja_command, {
        cwd: ninja_source_dir,
        stdio: [0, 1, 2],
      });
      fs.copyFileSync(path.join(ninja_source_dir, "ninja"), ninja_bin_output);
    } else {
      console.log(`ninja.tar.gz not availble in CI mode`);
      require("../ninja/snapshot").build();
      fs.copyFileSync(path.join(root_dir, "ninja", "ninja"), ninja_bin_output);
    }

    console.log("ninja binary is ready: ", ninja_bin_output);
  }

  // sanity check to make sure the binary actually runs. Used for Linux. Too many variants
  /**
   *
   * @param {string} binary_path
   */
  function test_ninja_compatible(binary_path) {
    var version;
    try {
      version = cp
        .execSync(JSON.stringify(binary_path) + " --version", {
          encoding: "utf8",
          stdio: ["pipe", "pipe", "ignore"], // execSync outputs to stdout even if we catch the error. Silent it here
        })
        .trim();
    } catch (e) {
      console.log("ninja not compatible?", String(e));
      return false;
    }
    return version === vendor_ninja_version;
  }

  if (
    process.env.NINJA_FORCE_REBUILD === undefined &&
    fs.existsSync(ninja_bin_output) &&
    test_ninja_compatible(ninja_bin_output)
  ) {
    console.log(
      "ninja binary is already cached and installed: ",
      ninja_bin_output
    );
    return;
  }

  build_ninja();
}

/**
 *
 * @param {string} dir
 * TODO: `mkdirSync` may fail
 */
function ensureExists(dir) {
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir);
  }
}

/**
 * @returns {string|undefined}
 */
function checkPrebuiltBscCompiler() {
  if (process.env.BS_TRAVIS_CI) {
    return;
  }
  try {
    var version = String(
      cp.execFileSync(path.join(root_dir, process.platform, "bsc.exe"), ["-v"])
    );

    var myOCamlVersion = version.substr(
      version.indexOf(":") + 1,
      version.lastIndexOf(" ") - version.indexOf(":") - 1
    );
    console.log("checkoutput:", version, "ocaml version", myOCamlVersion);
    console.log("Prebuilt compiler works good");

    return myOCamlVersion;
  } catch (e) {
    console.log("No working prebuilt buckleScript compiler");
    if (is_windows) {
      throw new Error("no prebuilt bsc compiler on windows");
    }
    return;
  }
}
/**
 *
 * @param {string} stdlib
 */
function buildLibs(stdlib) {
  ensureExists(lib_dir);
  ensureExists(path.join(lib_dir, "js"));
  ensureExists(path.join(lib_dir, "es6"));
  process.env.NINJA_IGNORE_GENERATOR = "true";
  var releaseNinja = `
bsc = ../${process.platform}/bsc.exe
stdlib = ${stdlib}
subninja runtime/release.ninja
subninja others/release.ninja
subninja $stdlib/release.ninja
${process.env.BS_TRAVIS_CI ? "subninja test/build.ninja\n" : "\n"}
o all: phony runtime others $stdlib
`;
  var filePath = path.join(jscomp_dir, "release.ninja");
  fs.writeFileSync(filePath, releaseNinja, "ascii");
  var cleanArgs = ["-f", "release.ninja", "-t", "clean"];
  cp.execFileSync(ninja_bin_output, cleanArgs, {
    cwd: jscomp_dir,
    stdio: [0, 1, 2],
    shell: false,
  });
  var buildArgs = ["-f", "release.ninja", "--verbose", "-k", "1"];
  cp.execFileSync(ninja_bin_output, buildArgs, {
    cwd: jscomp_dir,
    stdio: [0, 1, 2],
    shell: false,
  });
  fs.unlinkSync(filePath);
  console.log("Build finished");
}

/**
 * @returns {string}
 */
function provideCompiler() {
  var myVersion = checkPrebuiltBscCompiler();
  if (myVersion !== undefined) {
    return myVersion;
  } else {
    myVersion = "4.06.1";
    var ocamlopt = path.join(
      __dirname,
      "..",
      "native",
      myVersion,
      "bin",
      "ocamlopt.opt"
    );
    if (!fs.existsSync(ocamlopt)) {
      require("./buildocaml.js").build(true);
    } else {
      console.log(ocamlopt, "is already there");
    }
    // Note this ninja file only works under *nix due to the suffix
    // under windows require '.exe'
    var releaseNinja = require("./ninjaFactory.js").libNinja({
      ocamlopt: ocamlopt,
      INCL: myVersion,
      isWin: is_windows,
    });

    var filePath = path.join(lib_dir, "release.ninja");
    fs.writeFileSync(filePath, releaseNinja, "ascii");
    cp.execFileSync(ninja_bin_output, ["-f", "release.ninja", "-t", "clean"], {
      cwd: lib_dir,
      stdio: [0, 1, 2],
    });
    cp.execFileSync(ninja_bin_output, ["-f", "release.ninja", "-v"], {
      cwd: lib_dir,
      stdio: [0, 1, 2],
    });
    fs.unlinkSync(filePath);
    return myVersion;
  }
}

provideNinja();

provideCompiler();

var stdlib = "stdlib-406";

if (process.env.BS_TRAVIS_CI) {
  buildLibs(stdlib);
  require("./installUtils.js").install();
}
