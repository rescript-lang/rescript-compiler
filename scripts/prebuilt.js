#!/usr/bin/env node
//@ts-check
var cp = require("child_process");
var path = require("path");
var { is_windows } = require("./config.js");

var root = path.join(__dirname, "..");
var root_config = { cwd: root, stdio: [0, 1, 2], encoding: "utf8" };


var ocamlVersion = require("./buildocaml.js").getVersionPrefix();
var fs = require("fs");
var hostPlatform = "darwin";

function rebuild() {
  cp.execSync(`node ${path.join(__dirname, "ninja.js")} cleanbuild`, {
    cwd: __dirname,
    stdio: [0, 1, 2]
  });
}
var assert = require('assert')
/**
 *
 * @param {string} src
 * @param {(file:string)=>boolean} filter
 * @param {string} dest
 */
function installDirBy(src, dest, filter) {
  fs.readdir(src, function(err, files) {
    if (err === null) {
      files.forEach(function(file) {
        if (filter(file)) {
          var x = path.join(src, file);
          var y = path.join(dest, file);
          // console.log(x, '----->', y )
          fs.copyFile(x, y, (err) => assert.equal(err,null));
        }
      });
    } else {
      throw err;
    }
  });
}

function install() {
  var root_dir = path.join(__dirname, "..");
  var lib_dir = path.join(root_dir, "lib");
  var jscomp_dir = path.join(root_dir, "jscomp");
  var runtime_dir = path.join(jscomp_dir, "runtime");
  var others_dir = path.join(jscomp_dir, "others");
  var ocaml_dir = path.join(lib_dir, "ocaml");
  var stdlib_dir = path.join(jscomp_dir, "stdlib-406");

  installDirBy(runtime_dir, ocaml_dir, function(file) {
    var y = path.parse(file);
    return y.name === "js" || y.ext.includes("cm");
    // install js.cmi, js.mli
  });
  installDirBy(others_dir, ocaml_dir, function(file) {
    var y = path.parse(file);
    return y.ext === ".ml" || y.ext === ".mli" || y.ext.includes("cm");
  });
  installDirBy(stdlib_dir, ocaml_dir, function(file) {
    var y = path.parse(file);
    return y.ext === ".ml" || y.ext === ".mli" || y.ext.includes("cm");
  });
}

function buildCompiler() {
  // for 4.02.3 it relies on OCAMLLIB to find stdlib path
  // for 4.06.1 OCAMLLIB is another PATH
  // delete process.env.OCAMLLIB
  var prebuilt = "prebuilt.ninja";
  var content = require("./ninjaFactory.js").libNinja({
    ocamlopt: is_windows
      ? `ocamlopt.opt.exe`
      : `../native/${ocamlVersion}/bin/ocamlopt.opt`,
    INCL: ocamlVersion,
    isWin: is_windows
  });

  fs.writeFileSync(path.join(root, "lib", prebuilt), content, "ascii");
  process.env.PATH = `${path.join(__dirname, "..", process.platform)}${
    path.delimiter
  }${process.env.PATH}`;
  let ninjaPath = `ninja.exe`;
  cp.execSync(
    `${ninjaPath} -C lib -f ${prebuilt} -v -t clean && ${ninjaPath} -v -C lib -f ${prebuilt}`,
    root_config
  );
}
if (!is_windows) {
  if (!process.argv.includes("-noclean")) {
    rebuild();
  }

  require("./ninja.js").updateRelease();
}

function createOCamlTar() {
  if (process.platform === hostPlatform) {
    install();
    cp.execSync(`git -C ocaml status -uno`, { cwd: root, stdio: [0, 1, 2] });
    cp.execSync(
      `git  -C ocaml archive --format=tar.gz HEAD -o ../vendor/ocaml.tar.gz`,
      { cwd: root, stdio: [0, 1, 2] }
    );
  }
}

createOCamlTar();
buildCompiler();
