#!/usr/bin/env node

var p = require("child_process");
var fs = require("fs");
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
  var defaultPlayground = __dirname;
  console.warn(
    `BS_PLAYGROUND env var unset, defaulting to ${defaultPlayground}`
  );
  process.env.BS_PLAYGROUND = defaultPlayground;
}

var playground = process.env.BS_PLAYGROUND;

function prepare() {
  p.execSync(
    `js_of_ocaml 2>/dev/null || { echo >&2 "js_of_ocaml not found on path. Please run \"esy && esy build-playground\"."; exit 1; }`,
    config
  );
  e(
    `ocamlc.opt -w -30-40 -no-check-prims -I ${jsRefmtCompDir} ${jsRefmtCompDir}/js_refmt_compiler.mli ${jsRefmtCompDir}/js_refmt_compiler.ml -o jsc.byte && js_of_ocaml jsc.byte -o exports.js`
  );

  e(`mkdir -p ${playground}/stdlib`);
  e(`cp ../lib/js/*.js ${playground}/stdlib`);
  e(`mv ./exports.js ${playground}`)
}

function prepublish() {
  var mainPackageJson = JSON.parse(fs.readFileSync(path.join(__dirname, '..', 'package.json')));
  var packageJson = JSON.stringify(
    {
      name: "reason-js-compiler",
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

  fs.writeFileSync(
    path.join(__dirname, `package.json`),
    packageJson,
    {
      encoding: "utf8",
    }
  );

}

prepare();
if (process.argv.includes("-prepublish")) {
  prepublish();
}
