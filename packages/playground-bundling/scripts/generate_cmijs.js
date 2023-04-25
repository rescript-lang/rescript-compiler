#!/usr/bin/env node

/*
 * Requires the version matching `rescript` binary to be `npm link`ed in this
 * project. Or in other words: You need to build cmij files with the same
 * rescript version as the compiler bundle.
 *
 * This script extracts all cmi / cmj files of all dependencies listed in the
 * project root's bsconfig.json, creates cmij.js files for each library and
 * puts them in the compiler playground directory.
 *
 * The cmij files are representing the marshaled dependencies that can be used with the ReScript
 * playground bundle.
 *
 * This script is intended to be called by the `repl.js` script, but it can be independently run
 * by either running it within the compiler repo, or by providing a proper `PLAYGROUND_DIR` environment
 * flag pointing to the target folder the artifacts will be created.
 */

const child_process = require("child_process");
const fs = require("fs");
const path = require("path");

const bsconfig = require("../bsconfig.json");

const PLAYGROUND_DIR =
  process.env.PLAYGROUND ||
  path.join(__dirname, "..", "..", "..", "playground");

const PROJECT_ROOT_DIR = path.join(__dirname, "..");
const PACKAGES_DIR = path.join(PLAYGROUND_DIR, "packages");

const config = {
  cwd: PROJECT_ROOT_DIR,
  encoding: "utf8",
  stdio: [0, 1, 2],
  shell: true,
};

function e(cmd) {
  console.log(`>>>>>> running command: ${cmd}`);
  child_process.execSync(cmd, config);
  console.log(`<<<<<<`);
}

const packages = bsconfig["bs-dependencies"];

// We need to build the compiler's builtin modules as a separate cmij.
// Otherwise we can't use them for compilation within the playground.
function buildCompilerCmij() {
  const rescriptLibOcamlFolder = path.join(
    PROJECT_ROOT_DIR,
    "node_modules",
    "rescript",
    "lib",
    "ocaml"
  );

  const rescriptCompilerCmijFile = path.join(PACKAGES_DIR, "compilerCmij.js");

  e(
    `find ${rescriptLibOcamlFolder} -name "*.cmi" -or -name "*.cmj" | xargs -n1 basename | xargs js_of_ocaml build-fs -o ${rescriptCompilerCmijFile} -I ${rescriptLibOcamlFolder}`
  );
}

packages.forEach(function installLib(package) {
  const libOcamlFolder = path.join(
    PROJECT_ROOT_DIR,
    "node_modules",
    package,
    "lib",
    "ocaml"
  );
  const libEs6Folder = path.join(
    PROJECT_ROOT_DIR,
    "node_modules",
    package,
    "lib",
    "es6"
  );
  const outputFolder = path.join(PACKAGES_DIR, package);

  const cmijFile = path.join(outputFolder, `cmij.js`);

  if (!fs.existsSync(PLAYGROUND_DIR)) {
    console.error(`PLAYGROUND_DIR "${PLAYGROUND_DIR}" does not exist`);
    process.exit(1);
  }

  if (!fs.existsSync(outputFolder)) {
    fs.mkdirSync(outputFolder, { recursive: true });
  }
  e(`find ${libEs6Folder} -name '*.js' -exec cp {} ${outputFolder} \\;`);
  e(
    `find ${libOcamlFolder} -name "*.cmi" -or -name "*.cmj" | xargs -n1 basename | xargs js_of_ocaml build-fs -o ${cmijFile} -I ${libOcamlFolder}`
  );
});
