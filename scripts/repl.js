#!/usr/bin/env node

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
function prepare(isDev) {
  var [env, ocamlFlag, jsooFlag] = isDev
    ? ["development", "-g ", "--pretty "]
    : ["production", "", ""];
  console.log(`building byte code version of the compiler [${env}]`);
  e(
    `${OCAMLC} ${ocamlFlag}-w -30-40 -no-check-prims -I ${sourceDir} ${sourceDir}/js_compiler.mli ${sourceDir}/js_compiler.ml -o jsc.byte `
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

prepare(process.argv.includes("-development"));
if (process.argv.includes("-prepublish")) {
  prepublish();
}
