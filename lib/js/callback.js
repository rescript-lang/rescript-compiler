'use strict';

var Obj = require("./obj.js");

function register(name, v) {
  return /* () */0;
}

function register_exception(name, exn) {
  /* XXX */exn.tag === Obj.object_tag ? exn : exn[0];
  return /* () */0;
}

exports.register = register;
exports.register_exception = register_exception;
/* No side effect */
