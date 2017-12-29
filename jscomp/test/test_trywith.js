'use strict';

var Curry = require("../../lib/js/curry.js");
var Js_exn = require("../../lib/js/js_exn.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function ff(g, x) {
  try {
    Curry._1(g, x);
  }
  catch (exn){
    if (exn !== Caml_builtin_exceptions.not_found) {
      throw exn;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (exn$1){
    if (exn$1 !== Caml_builtin_exceptions.out_of_memory) {
      throw exn$1;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn){
    var exn$2 = Js_exn.internalToOCamlException(raw_exn);
    if (exn$2[0] !== Caml_builtin_exceptions.sys_error) {
      throw exn$2;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$1){
    var exn$3 = Js_exn.internalToOCamlException(raw_exn$1);
    if (exn$3[0] !== Caml_builtin_exceptions.invalid_argument) {
      throw exn$3;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (exn$4){
    if (exn$4 !== Caml_builtin_exceptions.end_of_file) {
      throw exn$4;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$2){
    var exn$5 = Js_exn.internalToOCamlException(raw_exn$2);
    if (exn$5[0] !== Caml_builtin_exceptions.match_failure) {
      throw exn$5;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (exn$6){
    if (exn$6 !== Caml_builtin_exceptions.stack_overflow) {
      throw exn$6;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (exn$7){
    if (exn$7 !== Caml_builtin_exceptions.sys_blocked_io) {
      throw exn$7;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$3){
    var exn$8 = Js_exn.internalToOCamlException(raw_exn$3);
    if (exn$8[0] !== Caml_builtin_exceptions.assert_failure) {
      throw exn$8;
    }
    
  }
  try {
    return Curry._1(g, x);
  }
  catch (raw_exn$4){
    var exn$9 = Js_exn.internalToOCamlException(raw_exn$4);
    if (exn$9[0] === Caml_builtin_exceptions.undefined_recursive_module) {
      return /* () */0;
    } else {
      throw exn$9;
    }
  }
}

function u() {
  throw Caml_builtin_exceptions.not_found;
}

function f(x) {
  if (typeof x === "number") {
    return 2;
  } else if (x.tag) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_trywith.ml",
            51,
            9
          ]
        ];
  } else {
    return 1;
  }
}

var u1 = "bad character decimal encoding \\";

var v = "bad character decimal encoding \\%c%c%c";

exports.ff = ff;
exports.u = u;
exports.u1 = u1;
exports.v = v;
exports.f = f;
/* No side effect */
