'use strict';

var Js_exn = require("../../lib/js/js_exn.js");
var Js_primitive = require("../../lib/js/js_primitive.js");

function test_js_error() {
  var exit = 0;
  var e;
  try {
    e = JSON.parse(" {\"x\" : }");
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Js_exn.$$Error) {
      console.log(Js_primitive.undefined_to_opt(exn[1].stack));
      return /* None */0;
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Some */[e];
  }
  
}

function test_js_error2() {
  try {
    return JSON.parse(" {\"x\" : }");
  }
  catch (raw_e){
    var e = Js_exn.internalToOCamlException(raw_e);
    if (e[0] === Js_exn.$$Error) {
      console.log(Js_primitive.undefined_to_opt(e[1].stack));
      throw e;
    } else {
      throw e;
    }
  }
}

function example1() {
  var exit = 0;
  var v;
  try {
    v = JSON.parse(" {\"x\"  }");
    exit = 1;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Js_exn.$$Error) {
      console.log(Js_primitive.undefined_to_opt(exn[1].stack));
      return /* None */0;
    } else {
      throw exn;
    }
  }
  if (exit === 1) {
    return /* Some */[v];
  }
  
}

function example2() {
  try {
    return /* Some */[JSON.parse(" {\"x\"}")];
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Js_exn.$$Error) {
      return /* None */0;
    } else {
      throw exn;
    }
  }
}

exports.test_js_error = test_js_error;
exports.test_js_error2 = test_js_error2;
exports.example1 = example1;
exports.example2 = example2;
/* No side effect */
