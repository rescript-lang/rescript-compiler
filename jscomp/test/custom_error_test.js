'use strict';

var Caml_option = require("../../lib/js/caml_option.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var asJsExn = Caml_js_exceptions.caml_as_js_exn;

function test_js_error(param) {
  var exit = 0;
  var e;
  try {
    e = JSON.parse(" {\"x\" : }");
    exit = 1;
  }
  catch (raw_err){
    var err = Caml_js_exceptions.internalToOCamlException(raw_err);
    var match = Caml_js_exceptions.caml_as_js_exn(err);
    if (match !== undefined) {
      console.log(Caml_option.valFromOption(match).stack);
      return undefined;
    } else {
      throw err;
    }
  }
  if (exit === 1) {
    return Caml_option.some(e);
  }
  
}

function test_js_error2(param) {
  try {
    return JSON.parse(" {\"x\" : }");
  }
  catch (raw_e){
    var e = Caml_js_exceptions.internalToOCamlException(raw_e);
    var match = Caml_js_exceptions.caml_as_js_exn(e);
    if (match !== undefined) {
      console.log(Caml_option.valFromOption(match).stack);
      throw e;
    } else {
      throw e;
    }
  }
}

function example1(param) {
  var exit = 0;
  var v;
  try {
    v = JSON.parse(" {\"x\"  }");
    exit = 1;
  }
  catch (raw_err){
    var err = Caml_js_exceptions.internalToOCamlException(raw_err);
    var match = Caml_js_exceptions.caml_as_js_exn(err);
    if (match !== undefined) {
      console.log(Caml_option.valFromOption(match).stack);
      return undefined;
    } else {
      throw err;
    }
  }
  if (exit === 1) {
    return Caml_option.some(v);
  }
  
}

function example2(param) {
  try {
    return Caml_option.some(JSON.parse(" {\"x\"}"));
  }
  catch (raw_e){
    var e = Caml_js_exceptions.internalToOCamlException(raw_e);
    if (Caml_js_exceptions.caml_as_js_exn(e) !== undefined) {
      return undefined;
    } else {
      throw e;
    }
  }
}

exports.asJsExn = asJsExn;
exports.test_js_error = test_js_error;
exports.test_js_error2 = test_js_error2;
exports.example1 = example1;
exports.example2 = example2;
/* No side effect */
