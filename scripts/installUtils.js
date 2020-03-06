#!/usr/bin/env node
//@ts-check
var fs = require("fs");
var path = require("path");
var assert = require("assert");
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
          fs.copyFile(x, y, err => assert.equal(err, null));
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
  if (!fs.existsSync(ocaml_dir)) {
    fs.mkdirSync(ocaml_dir);
  }
  // sync up with cmij_main.ml
  installDirBy(runtime_dir, ocaml_dir, function(file) {
    var y = path.parse(file);
    return y.name === "js" && y.ext !== ".cmj";
    // install js.cmi, js.mli
  });

  // for merlin or other IDE
  var installed_suffixes = [".ml", ".mli", ".cmi", ".cmt", ".cmti"];
  installDirBy(others_dir, ocaml_dir, function(file) {
    var y = path.parse(file);
    if (y.ext === ".cmi") {
      return !y.base.match(/Belt_internal/i);
    }
    return installed_suffixes.includes(y.ext);
  });
  installDirBy(stdlib_dir, ocaml_dir, function(file) {
    var y = path.parse(file);
    return installed_suffixes.includes(y.ext);
  });
}
exports.install = install;
