#!/usr/bin/env node

var p = require("child_process");

var path = require("path");


var ocamlVersion = "4.06.1";
var jscompDir = path.join(__dirname, "..", "jscomp");
var jsRefmtCompDir = path.join(__dirname, "..", "lib", ocamlVersion, "unstable");

var config = {
  cwd: jscompDir,
  encoding: "utf8",
  stdio: [0, 1, 2],
  shell: true
};
function e(cmd) {
  console.log(`>>>>>> running command: ${cmd}`);
  p.execSync(cmd, config);
  console.log(`<<<<<<`);
}

if (!process.env.BS_PLAYGROUND) {
  var defaultPlayground = `../../bucklescript-playground`;
  console.warn(
    `BS_PLAYGROUND env var unset, defaulting to ${defaultPlayground}`
  );
  process.env.BS_PLAYGROUND = defaultPlayground;
}

var playground = process.env.BS_PLAYGROUND;

function prepare() {
  e(`hash hash js_of_ocaml 2>/dev/null || { echo >&2 "js_of_ocaml not found on path. Please install version 3.5.1 (with opam switch ${ocamlVersion}), and put it on your path."; exit 1; }
`);

  e(
    `ocamlc.opt -w -30-40 -no-check-prims -I ${jsRefmtCompDir} ${jsRefmtCompDir}/js_compiler.mli ${jsRefmtCompDir}/js_compiler.ml -o jsc.byte && js_of_ocaml jsc.byte -o exports.js`
  );

  e(`cp ../lib/js/*.js ${playground}/stdlib`);
  e(`mv ./exports.js ${playground}`)
}


prepare();

