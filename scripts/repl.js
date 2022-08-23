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
 * PLAYGROUND=../playground node scripts/repl.js
 * ```
 *
 * You may also pass an alternative JSOO entrypoint (we have multiple defined
 * in `jscomp/snapshot.ninja`) as a first parameter:
 *
 * ```
 * # Builds the "simpler" JS bundle
 * mkdir playground && mkdir playground/stdlib
 * PLAYGROUND=../playground node scripts/repl.js js_compiler
 * ```
 */

//@ts-check
var child_process = require("child_process");
var fs = require("fs");
var path = require("path");

var ocamlVersion = "4.06.1";
var jscompDir = path.join(__dirname, "..", "jscomp");
var sourceDir = path.join(__dirname, "..", "lib", ocamlVersion, "unstable");

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
  var defaultPlayground = path.join(__dirname, "..", "playground");
  console.warn(`PLAYGROUND env var unset, defaulting to ${defaultPlayground}`);
  process.env.PLAYGROUND = defaultPlayground;
}

const BUILD_THIRD_PARTY = process.env.BUILD_THIRD_PARTY || false;
var playground = process.env.PLAYGROUND;
var OCAMLC = `ocamlc.opt`;

// This mini project is needed to build cmij files for third party deps like @rescript/react
const PLAYGROUND_BUNDLING = path.join(
  __dirname,
  "..",
  "packages",
  "playground-bundling"
);

var JSOO = `js_of_ocaml`;

function prepare(isDev, targetCompilerFile) {
  var [env, ocamlFlag, jsooFlag] = isDev
    ? ["development", "-g ", "--pretty "]
    : ["production", "", ""];
  console.log(
    `building byte code version of target compiler file '${targetCompilerFile}' [${env}]`
  );

  if (!fs.existsSync(playground)) {
    console.log(`Creating output folder "${playground}"`);

    // Create both, the `playground` and `playground/stdlib` dir
    const stdlibDir = path.join(playground, "stdlib");
    fs.mkdirSync(stdlibDir, { recursive: true });
  }

  const mliFile = path.join(sourceDir, targetCompilerFile + ".mli");
  const mlFile = path.join(sourceDir, targetCompilerFile + ".ml");

  // There may be a situation where the .mli file doesn't exist (mostly when
  // the snapshot hasn't been checked into `lib/4.06.1/unstable`
  e(`touch ${mliFile}`);
  e(
    `${OCAMLC} ${ocamlFlag}-w -30-40 -no-check-prims -I ${sourceDir} ${mliFile} ${mlFile} -o jsc.byte `
  );
  console.log(
    `building js version for compiler target '${targetCompilerFile}'`
  );
  e(`${JSOO} compile jsc.byte ${jsooFlag}-o compiler.js`);
  console.log("copy js artifacts");
  e(`cp ../lib/js/*.js ${playground}/stdlib`);
  e(`mv ./compiler.js ${playground}`);

  // BUILDING THIRD PARTY CMIJ FILES
  if (BUILD_THIRD_PARTY) {
    console.log("Building third party packages...");
    e(`cd ${PLAYGROUND_BUNDLING} && npm run build`);
  }
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
      main: "compiler.js",
    },
    null,
    2
  );

  fs.writeFileSync(jscompDir + `/${playground}/package.json`, packageJson, {
    encoding: "utf8",
  });
}

// Relevant target compiler files can be found in jscomp/snapshot.ninja
let targetCompilerFile = "js_playground_compiler";

// Let's derive the target file to compile from the last argument list.
if (process.argv.length > 2) {
  const lastArg = process.argv[process.argv.length - 1];

  if (!lastArg.startsWith("-")) {
    targetCompilerFile = lastArg;
  }
}

prepare(process.argv.includes("-development"), targetCompilerFile);
if (process.argv.includes("-prepublish")) {
  prepublish();
}
