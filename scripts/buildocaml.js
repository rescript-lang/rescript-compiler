#!/usr/bin/env node
//@ts-check
var cp = require("child_process");
var path = require("path");
var fs = require("fs");

var ocamlSrcDir = path.join(__dirname, "..", "ocaml");
var ocamlVersionFilePath = path.join(ocamlSrcDir, "VERSION");

/**
 * Ensures the ocaml folder exists at the root of the project, either from the submodule,
 * or by extracting the vendor/ocaml.tar.gz there
 */
function ensureOCamlExistsSync() {
  if (!fs.existsSync(ocamlSrcDir)) {
    fs.mkdirSync(ocamlSrcDir);
  }
  if (!fs.existsSync(ocamlVersionFilePath)) {
    cp.execSync(`tar xzvf ../vendor/ocaml.tar.gz`, {
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
 */
function build(config) {
  ensureOCamlExistsSync();

  var prefix = path.normalize(
    path.join(__dirname, "..", "native", getVersionPrefix())
  );

  if (config) {
    var { make } = require("./config.js");
    cp.execSync(
      './configure -cc "gcc -Wno-implicit-function-declaration -fcommon" -flambda -prefix ' +
        prefix +
        ` -no-ocamlbuild  -no-curses -no-graph -no-pthread -no-debugger && ${make} clean`,
      { cwd: ocamlSrcDir, stdio: [0, 1, 2] }
    );
  }

  cp.execSync(`${make} -j9 world.opt && ${make} install `, {
    cwd: ocamlSrcDir,
    stdio: [0, 1, 2],
  });
}

exports.build = build;
if (require.main === module) {
  build(!process.argv.includes("-noconfig"));
}
