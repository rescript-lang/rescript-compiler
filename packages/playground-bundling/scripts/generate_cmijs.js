#!/usr/bin/env node

/*
 * Requires the version matching `rescript` binary to be `npm link`ed in this
 * project. Or in other words: You need to build cmij files with the same
 * rescript version as the compiler bundle.
 *
 * This script extracts all cmi / cmj files of the rescript/lib/ocaml and all
 * dependencies listed in the project root's bsconfig.json, creates cmij.js
 * files for each library and puts them in the compiler playground directory.
 *
 * The cmij files are representing the marshaled dependencies that can be used with the ReScript
 * playground bundle.
 */

const child_process = require("child_process");
const fs = require("fs");
const path = require("path");

const bsconfig = require("../bsconfig.json");

const RESCRIPT_COMPILER_ROOT_DIR = path.join(__dirname, "..", "..", "..");
const PLAYGROUND_DIR = path.join(RESCRIPT_COMPILER_ROOT_DIR, "playground");

// The playground-bundling root dir
const PROJECT_ROOT_DIR = path.join(__dirname, "..");

// Final target output directory where all the cmijs will be stored
const PACKAGES_DIR = path.join(PLAYGROUND_DIR, "packages");

// Making sure this directory exists, since it's not checked in to git
if (!fs.existsSync(PACKAGES_DIR)) {
  fs.mkdirSync(PACKAGES_DIR, { recursive: true });
}

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

e(`npm install`);
e(`npm link ${RESCRIPT_COMPILER_ROOT_DIR}`);

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

function buildThirdPartyCmijs() {
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
}

buildCompilerCmij();
buildThirdPartyCmijs();
