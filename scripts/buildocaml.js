//@ts-check
var cp = require("child_process");
var path = require("path");
var fs = require("fs");

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
  var file = path.join(__dirname, "..", "ocaml", "VERSION");
  if (fs.existsSync(file)) {
    var version = fs.readFileSync(file, "ascii");
    cached = version.substr(0, version.indexOf("+"));
    return cached;
  }
  console.warn(`cannot find '${file}'`);

  file = path.join(__dirname, "..", "OCAML_VERSION");
  if (fs.existsSync(file)) {
    var version = fs.readFileSync(file, "ascii");
    cached = version.substr(0, version.indexOf("+"));
    return cached;
  }
  console.warn(`cannot find '${file}'`);

  console.warn("You should create OCAML_VERSION or ocaml/VERSION file to specify OCaml version like '4.02.3+buckle-master'");
  console.warn(`for example,
bucklescript>cat ocaml/VERSION 
4.02.3+BS

# The version string is the first line of this file.
# It must be in the format described in stdlib/sys.mli
`);

  throw new Error("version file not found");
}
exports.getVersionPrefix = getVersionPrefix;

/**
 *
 * @param {boolean} config
 */
function build(config) {
  var ocamlSrcDir = path.join(__dirname, "..", "ocaml");
  if (!fs.existsSync(ocamlSrcDir)) {
    fs.mkdirSync(ocamlSrcDir);
  }
  if (!fs.existsSync(path.join(ocamlSrcDir, "VERSION"))) {
    cp.execSync(`tar xzvf ../ocaml.tar.gz`, {
      cwd: ocamlSrcDir,
      stdio: [0, 1, 2]
    });
  }

  var prefix = path.normalize(
    path.join(__dirname, "..", "native", getVersionPrefix())
  );
  if (config) {
    cp.execSync(
      "./configure -prefix " +
        prefix +
        " -no-ocamlbuild  -no-curses -no-graph -no-pthread -no-debugger && make clean",
      { cwd: path.join(__dirname, "..", "ocaml"), stdio: [0, 1, 2] }
    );
  }
  cp.execSync("make -j9 world.opt && make install ", {
    cwd: path.join(__dirname, "..", "ocaml"),
    stdio: [0, 1, 2]
  });
}

exports.build = build;
if (require.main === module) {
  build(!process.argv.includes("-noconfig"));
}
