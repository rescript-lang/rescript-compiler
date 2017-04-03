'use strict';

var Curry                   = require("../../lib/js/curry.js");
var Js_exn                  = require("../../lib/js/js_exn.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function ff(g, x) {
  try {
    Curry._1(g, x);
  }
  catch (exn){
    var exn$1 = Js_exn.internalToOCamlException(exn);
    if (exn$1 !== Caml_builtin_exceptions.not_found) {
      throw exn$1;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (exn$2){
    var exn$3 = Js_exn.internalToOCamlException(exn$2);
    if (exn$3 !== Caml_builtin_exceptions.out_of_memory) {
      throw exn$3;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (exn$4){
    var exn$5 = Js_exn.internalToOCamlException(exn$4);
    if (exn$5[0] !== Caml_builtin_exceptions.sys_error) {
      throw exn$5;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (exn$6){
    var exn$7 = Js_exn.internalToOCamlException(exn$6);
    if (exn$7[0] !== Caml_builtin_exceptions.invalid_argument) {
      throw exn$7;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (exn$8){
    var exn$9 = Js_exn.internalToOCamlException(exn$8);
    if (exn$9 !== Caml_builtin_exceptions.end_of_file) {
      throw exn$9;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (exn$10){
    var exn$11 = Js_exn.internalToOCamlException(exn$10);
    if (exn$11[0] !== Caml_builtin_exceptions.match_failure) {
      throw exn$11;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (exn$12){
    var exn$13 = Js_exn.internalToOCamlException(exn$12);
    if (exn$13 !== Caml_builtin_exceptions.stack_overflow) {
      throw exn$13;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (exn$14){
    var exn$15 = Js_exn.internalToOCamlException(exn$14);
    if (exn$15 !== Caml_builtin_exceptions.sys_blocked_io) {
      throw exn$15;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (exn$16){
    var exn$17 = Js_exn.internalToOCamlException(exn$16);
    if (exn$17[0] !== Caml_builtin_exceptions.assert_failure) {
      throw exn$17;
    }
    
  }
  try {
    return Curry._1(g, x);
  }
  catch (exn$18){
    var exn$19 = Js_exn.internalToOCamlException(exn$18);
    if (exn$19[0] === Caml_builtin_exceptions.undefined_recursive_module) {
      return /* () */0;
    } else {
      throw exn$19;
    }
  }
}

function u() {
  throw Caml_builtin_exceptions.not_found;
}

function f(x) {
  if (typeof x === "number") {
    return 2;
  } else {
    switch (x.tag | 0) {
      case 0 : 
          return 1;
      case 1 : 
          throw [
                Caml_builtin_exceptions.assert_failure,
                [
                  "test_trywith.ml",
                  51,
                  9
                ]
              ];
      default:
        return 2;
    }
  }
}

var u1 = "bad character decimal encoding \\";

var v = "bad character decimal encoding \\%c%c%c";

exports.ff = ff;
exports.u  = u;
exports.u1 = u1;
exports.v  = v;
exports.f  = f;
/* No side effect */
