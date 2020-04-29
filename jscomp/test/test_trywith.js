'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function ff(g, x) {
  try {
    Curry._1(g, x);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.ExceptionID !== /* Not_found */-6) {
      throw exn;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$1){
    var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
    if (exn$1.ExceptionID !== /* Out_of_memory */0) {
      throw exn$1;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$2){
    var exn$2 = Caml_js_exceptions.internalToOCamlException(raw_exn$2);
    if (exn$2.ExceptionID !== /* Sys_error */-1) {
      throw exn$2;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$3){
    var exn$3 = Caml_js_exceptions.internalToOCamlException(raw_exn$3);
    if (exn$3.ExceptionID !== /* Invalid_argument */-3) {
      throw exn$3;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$4){
    var exn$4 = Caml_js_exceptions.internalToOCamlException(raw_exn$4);
    if (exn$4.ExceptionID !== /* End_of_file */-4) {
      throw exn$4;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$5){
    var exn$5 = Caml_js_exceptions.internalToOCamlException(raw_exn$5);
    if (exn$5.ExceptionID !== /* Match_failure */-7) {
      throw exn$5;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$6){
    var exn$6 = Caml_js_exceptions.internalToOCamlException(raw_exn$6);
    if (exn$6.ExceptionID !== /* Stack_overflow */-8) {
      throw exn$6;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$7){
    var exn$7 = Caml_js_exceptions.internalToOCamlException(raw_exn$7);
    if (exn$7.ExceptionID !== /* Sys_blocked_io */-10) {
      throw exn$7;
    }
    
  }
  try {
    Curry._1(g, x);
  }
  catch (raw_exn$8){
    var exn$8 = Caml_js_exceptions.internalToOCamlException(raw_exn$8);
    if (exn$8.ExceptionID !== /* Assert_failure */-9) {
      throw exn$8;
    }
    
  }
  try {
    return Curry._1(g, x);
  }
  catch (raw_exn$9){
    var exn$9 = Caml_js_exceptions.internalToOCamlException(raw_exn$9);
    if (exn$9.ExceptionID === /* Undefined_recursive_module */-11) {
      return ;
    }
    throw exn$9;
  }
}

function u(param) {
  throw {
        ExceptionID: -6,
        Debug: "Not_found"
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
        ExceptionID: -9,
        _1: /* tuple */[
          "test_trywith.ml",
          51,
          9
        ],
        Debug: "Assert_failure"
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
