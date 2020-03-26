'use strict';

var Fs = require("fs");
var Path = require("path");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var package_json = "package.json";

function find_package_json(_dir) {
  while(true) {
    var dir = _dir;
    if (Fs.existsSync(Path.join(dir, package_json))) {
      return dir;
    }
    var new_dir = Path.dirname(dir);
    if (new_dir === dir) {
      throw Caml_builtin_exceptions.not_found;
    }
    _dir = new_dir;
    continue ;
  };
}

var match = typeof __dirname === "undefined" ? void 0 : __dirname;

if (match !== void 0) {
  console.log(find_package_json(match));
}

exports.package_json = package_json;
exports.find_package_json = find_package_json;
/* match Not a pure module */
