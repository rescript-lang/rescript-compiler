'use strict';

var Js_exn = require("../../lib/js/js_exn.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function test_js_error(param) {
  var e;
  try {
    e = JSON.parse(" {\"x\" : }");
  }
  catch (raw_err){
    var err = Caml_js_exceptions.internalToOCamlException(raw_err);
    if (err.ExceptionID === Js_exn.$$Error) {
      console.log(err._1.stack);
      return ;
    }
    throw err;
  }
  return Caml_option.some(e);
}

function test_js_error2(param) {
  try {
    return JSON.parse(" {\"x\" : }");
  }
  catch (raw_e){
    var e = Caml_js_exceptions.internalToOCamlException(raw_e);
    if (e.ExceptionID === Js_exn.$$Error) {
      console.log(e._1.stack);
      throw e;
    }
    throw e;
  }
}

function example1(param) {
  var v;
  try {
    v = JSON.parse(" {\"x\"  }");
  }
  catch (raw_err){
    var err = Caml_js_exceptions.internalToOCamlException(raw_err);
    if (err.ExceptionID === Js_exn.$$Error) {
      console.log(err._1.stack);
      return ;
    }
    throw err;
  }
  return Caml_option.some(v);
}

function example2(param) {
  try {
    return Caml_option.some(JSON.parse(" {\"x\"}"));
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.ExceptionID === Js_exn.$$Error) {
      return ;
    }
    throw exn;
  }
}

exports.test_js_error = test_js_error;
exports.test_js_error2 = test_js_error2;
exports.example1 = example1;
exports.example2 = example2;
/* No side effect */
