#!/usr/bin/env node
//@ts-check
var p = require("child_process");
var fs = require("fs");
var path = require("path");

var ocamlVersion = "4.06.1";
var jscompDir = path.join(__dirname, "..", "jscomp");
var jsRefmtCompDir = path.join(
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

var nativePath = path.join(__dirname, "..", "native", "4.06.1", "bin");
var OCAMLC = path.join(nativePath, "ocamlc.opt");
var OCAMLRUN = path.join(nativePath, "ocamlrun");
var JSOO = path.join(__dirname, "..", "vendor", "js_of_ocaml.bc");
function prepare(isDev) {
  var [env, ocamlFlag, jsooFlag] = isDev
    ? ["development", "-g ", "--pretty "]
    : ["production", "", ""];
  console.log(`building byte code version of the compiler [${env}]`);
  e(
    `${OCAMLC} ${ocamlFlag}-w -30-40 -no-check-prims -I ${jsRefmtCompDir} ${jsRefmtCompDir}/js_refmt_compiler.mli ${jsRefmtCompDir}/js_refmt_compiler.ml -o jsc.byte `
  );
  console.log("building js version");
  e(`${OCAMLRUN} ${JSOO} compile jsc.byte ${jsooFlag}-o exports.js`);
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

  fs.writeFileSync(jscompDir + `/${playground}/package.json`, packageJson, {
    encoding: "utf8",
  });
}

prepare(process.argv.includes("-development"));
if (process.argv.includes("-prepublish")) {
  prepublish();
}
