'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var x = {
  contents: 1
};

var y = {
  contents: 2
};

function f(param) {
  var a = {
    contents: param[0]
  };
  var b = {
    contents: param[1]
  };
  console.log(a, b);
  
}

function g(param) {
  return 3;
}

function a0(f) {
  var u = Curry._1(f, undefined);
  if (u !== null) {
    console.log(u);
    console.log(u);
    return 1;
  } else {
    return 0;
  }
}

function a1(f) {
  var E = Caml_exceptions.create("E");
  try {
    return Curry._1(f, undefined);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.CamlExt === E) {
      return 1;
    }
    throw exn;
  }
}

var a = 1;

var b = 2;

exports.a = a;
exports.b = b;
exports.x = x;
exports.y = y;
exports.f = f;
exports.g = g;
exports.a0 = a0;
exports.a1 = a1;
/* No side effect */
