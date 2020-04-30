'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function ff(g, x) {
  try {
    Curry._1(g, x);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.ExceptionID !== "Not_found") {
      throw exn;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$1){
    var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
    if (exn$1.ExceptionID !== "Out_of_memory") {
      throw exn$1;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$2){
    var exn$2 = Caml_js_exceptions.internalToOCamlException(raw_exn$2);
    if (exn$2.ExceptionID !== "Sys_error") {
      throw exn$2;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$3){
    var exn$3 = Caml_js_exceptions.internalToOCamlException(raw_exn$3);
    if (exn$3.ExceptionID !== "Invalid_argument") {
      throw exn$3;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$4){
    var exn$4 = Caml_js_exceptions.internalToOCamlException(raw_exn$4);
    if (exn$4.ExceptionID !== "End_of_file") {
      throw exn$4;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$5){
    var exn$5 = Caml_js_exceptions.internalToOCamlException(raw_exn$5);
    if (exn$5.ExceptionID !== "Match_failure") {
      throw exn$5;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$6){
    var exn$6 = Caml_js_exceptions.internalToOCamlException(raw_exn$6);
    if (exn$6.ExceptionID !== "Stack_overflow") {
      throw exn$6;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$7){
    var exn$7 = Caml_js_exceptions.internalToOCamlException(raw_exn$7);
    if (exn$7.ExceptionID !== "Sys_blocked_io") {
      throw exn$7;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$8){
    var exn$8 = Caml_js_exceptions.internalToOCamlException(raw_exn$8);
    if (exn$8.ExceptionID !== "Assert_failure") {
      throw exn$8;
    }
    
  }
  try {
    return Curry._1(g, x);
  }
  catch (raw_exn$9){
    var exn$9 = Caml_js_exceptions.internalToOCamlException(raw_exn$9);
    if (exn$9.ExceptionID === "Undefined_recursive_module") {
      return ;
    }
    throw exn$9;
  }
}

function u(param) {
  throw {
        ExceptionID: "Not_found"
      };
}

function f(x) {
  if (typeof x === "number") {
    return 2;
  }
  if (!x.tag) {
    return 1;
  }
  throw {
        ExceptionID: "Assert_failure",
        _1: /* tuple */[
          "test_trywith.ml",
          51,
          9
        ]
      };
}

var u1 = "bad character decimal encoding \\";

var v = "bad character decimal encoding \\%c%c%c";

exports.ff = ff;
exports.u = u;
exports.u1 = u1;
exports.v = v;
exports.f = f;
/* No side effect */
