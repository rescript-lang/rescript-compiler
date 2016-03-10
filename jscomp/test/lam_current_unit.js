// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Filename = require("../stdlib/filename");
var $$String = require("../stdlib/string");

var file = [""];

var debug_file = [""];

function set_file(f) {
  file[0] = f;
  return /* () */0;
}

function get_file() {
  return file[0];
}

function get_module_name() {
  return Filename.chop_extension($$String.uncapitalize(file[0]));
}

function iset_debug_file() {
  return /* () */0;
}

function set_debug_file(f) {
  debug_file[0] = f;
  return /* () */0;
}

function get_debug_file() {
  return debug_file[0];
}

function is_same_file() {
  if (debug_file[0] !== "") {
    return +(debug_file[0] === file[0]);
  }
  else {
    return /* false */0;
  }
}

exports.file            = file;
exports.debug_file      = debug_file;
exports.set_file        = set_file;
exports.get_file        = get_file;
exports.get_module_name = get_module_name;
exports.iset_debug_file = iset_debug_file;
exports.set_debug_file  = set_debug_file;
exports.get_debug_file  = get_debug_file;
exports.is_same_file    = is_same_file;
/* Filename Not a pure module */
