// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var Obj = require("./obj");

function register(_, _$1) {
  return /* () */0;
}

function register_exception(_, exn) {
  Caml_obj_runtime.caml_obj_tag(exn) === Obj.object_tag ? exn : exn[0];
  return /* () */0;
}

exports.register = register;
exports.register_exception = register_exception;
/* No side effect */
