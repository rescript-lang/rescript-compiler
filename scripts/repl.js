#!/usr/bin/env node


/*
 * This script is used to compile a bundled compiler file into a standalone JS bundle.
 * The compiled output (including the compiled stdlib files), will be put in the defined
 * `PLAYGROUND` directory.
 *
 * Example:
 *
 * ```
 * mkdir playground && mkdir playground/stdlib
 * PLAYGROUND=../playground node scripts/repl.js js_compiler
 * ```
 *
 */

//@ts-check
var child_process = require("child_process");
var fs = require("fs");
var path = require("path");

var ocamlVersion = "4.06.1";
var jscompDir = path.join(__dirname, "..", "jscomp");
var sourceDir = path.join(
  __dirname,
  "..",
  "lib",
  ocamlVersion,
  "unstable"
);

var config = {
  cwd: jscompDir,
  encoding: "utf8",
  stdio: [0, 1, 2],
  shell: true,
};
/**
 *
 * @param {string} cmd
 */
function e(cmd) {
  console.log(`>>>>>> running command: ${cmd}`);
  child_process.execSync(cmd, config);
  console.log(`<<<<<<`);
}

if (!process.env.PLAYGROUND) {
  var defaultPlayground = `../../playground`;
  console.warn(
    `PLAYGROUND env var unset, defaulting to ${defaultPlayground}`
  );
  process.env.PLAYGROUND = defaultPlayground;
}

var playground = process.env.PLAYGROUND;
var OCAMLC = `ocamlc.opt`

var JSOO = `js_of_ocaml`;

function prepare(isDev, targetCompilerFile) {
  var [env, ocamlFlag, jsooFlag] = isDev
    ? ["development", "-g ", "--pretty "]
    : ["production", "", ""];
  console.log(`building byte code version of the compiler [${env}]`);
  e(
    `${OCAMLC} ${ocamlFlag}-w -30-40 -no-check-prims -I ${sourceDir} ${sourceDir}/${targetCompilerFile}.mli ${sourceDir}/${targetCompilerFile}.ml -o jsc.byte `
  );
  console.log("building js version");
  e(`${JSOO} compile jsc.byte ${jsooFlag}-o exports.js`);
  console.log("copy js artifacts");
  e(`cp ../lib/js/*.js ${playground}/stdlib`);
  e(`mv ./exports.js ${playground}`);
}

function prepublish() {
  var mainPackageJson = JSON.parse(
    fs.readFileSync(path.join(__dirname, "..", "package.json"))
  );
  var packageJson = JSON.stringify(
    {
      name: "rescript-compiler",
      version: mainPackageJson.version,
      license: mainPackageJson.license,
      description: mainPackageJson.description,
      repository: mainPackageJson.repository,
      author: mainPackageJson.author,
      maintainers: mainPackageJson.maintainers,
      bugs: mainPackageJson.bugs,
      homepage: mainPackageJson.homepage,
      main: "exports.js",
    },
    null,
    2
  );

  fs.writeFileSync(jscompDir + `/${playground}/package.json`, packageJson, {
    encoding: "utf8",
  });
}

// Relevant target compiler files can be found in jscomp/snapshot.ninja
let targetCompilerFile = "js_compiler"

// Let's derive the target file to compile from the last argument list.
if(process.argv.length > 2) {
  const lastArg = process.argv[process.argv.length - 1]

  if(!lastArg.startsWith("-")) {
    targetCompilerFile = lastArg
  }
}

prepare(process.argv.includes("-development"), targetCompilerFile);
if (process.argv.includes("-prepublish")) {
  prepublish();
}
