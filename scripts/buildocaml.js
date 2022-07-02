#!/usr/bin/env node
//@ts-check
var cp = require("child_process");
var path = require("path");
var fs = require("fs");

var ocamlSrcDir = path.join(__dirname, "..", "native");
var ocamlVersionFilePath = path.join(ocamlSrcDir, "VERSION");

/**
 * Checks whether an OCaml compiler is available.
 * If it is, we do not need to build one from the vendored sources.
 */
function checkEnvCompiler() {
  try {
    var o = cp.execSync(`ocamlopt.opt -version`, {
      encoding: "utf-8",
    });
    console.log("checking env compiler");
    console.log(o);
    return true;
  } catch (e) {
    console.log(e);
    return false;
  }
}
exports.checkEnvCompiler = checkEnvCompiler;

/**
 * Ensures the ocaml folder exists at the root of the project, either from the submodule,
 * or by extracting the vendor/ocaml.tar.gz there
 */
function ensureOCamlExistsSync() {
  if (!fs.existsSync(ocamlVersionFilePath)) {
    if (!fs.existsSync(ocamlSrcDir)) {
      fs.mkdirSync(ocamlSrcDir);
    }

    console.log("Extracting OCaml sources...");
    cp.execSync(`tar xzf ../vendor/ocaml.tar.gz`, {
      cwd: ocamlSrcDir,
      stdio: [0, 1, 2],
    });
  }
}

/**
 * @type {string}
 */
var cached = undefined;
// FIXME: this works in CI, but for release build, submodule
// is carried, so it needs to be fixed
/**
 * @returns{string}
 */
function getVersionPrefix() {
  if (cached !== undefined) {
    return cached;
  }

  ensureOCamlExistsSync();

  console.log(`${ocamlVersionFilePath} is used in version detection`);
  var version = fs.readFileSync(ocamlVersionFilePath, "ascii");
  cached = version.substr(0, version.indexOf("+"));
  return cached;
}
exports.getVersionPrefix = getVersionPrefix;

/**
 *
 * @param {boolean} config
 * @returns {string}
 */
function build(config) {
  ensureOCamlExistsSync();

  var prefix = path.normalize(
    path.join(__dirname, "..", "native", getVersionPrefix())
  );

  if (config) {
    var { make } = require("./config.js");
    cp.execSync(
      "bash ./configure --disable-naked-pointers --enable-flambda -prefix " +
        prefix +
        ` && ${make} clean`,
      { cwd: ocamlSrcDir, stdio: [0, 1, 2] }
    );
  }

  cp.execSync(`${make} -j9 world.opt && ${make} install `, {
    cwd: ocamlSrcDir,
    stdio: [0, 1, 2],
  });
  return prefix;
}

exports.build = build;
if (require.main === module) {
  build(!process.argv.includes("-noconfig"));
}
