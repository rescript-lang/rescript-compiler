#!/usr/bin/env node
//@ts-check
var cp = require("child_process");
var path = require("path");
var fs = require("fs");

var ocamlSrcDir = path.join(__dirname, "..", "native");
var ocamlVersionFilePath = path.join(ocamlSrcDir, "VERSION");

/**
 * Ensures the ocaml folder exists at the root of the project, either from the submodule,
 * or by extracting the vendor/ocaml.tar.gz there
 */
function ensureOCamlExistsSync() {
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
      './configure  --disable-naked-pointers --enable-flambda -prefix ' +
        prefix +
        ` && ${make} clean`,
      { cwd: ocamlSrcDir, stdio: [0, 1, 2] }
    );
  }

  cp.execSync(`${make} -j9 world.opt && ${make} install `, {
    cwd: ocamlSrcDir,
    stdio: [0, 1, 2],
  });
  return prefix
}

exports.build = build;
if (require.main === module) {
  build(!process.argv.includes("-noconfig"));
}
