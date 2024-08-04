'use strict';

let Js_exn = require("./js_exn.js");
let Caml_option = require("./caml_option.js");

function test(x) {
  return x === undefined;
}

function testAny(x) {
  return x === undefined;
}

function getExn(f) {
  if (f !== undefined) {
    return f;
  }
  throw new Error("Failure", {
        cause: {
          RE_EXN_ID: "Failure",
          _1: "Js.Undefined.getExn"
        }
      });
}

function bind(x, f) {
  if (x !== undefined) {
    return f(x);
  }
  
}

function iter(x, f) {
  if (x !== undefined) {
    return f(x);
  }
  
}

function fromOption(x) {
  if (x !== undefined) {
    return Caml_option.valFromOption(x);
  }
  
}

let from_opt = fromOption;

exports.test = test;
exports.testAny = testAny;
exports.getExn = getExn;
exports.bind = bind;
exports.iter = iter;
exports.fromOption = fromOption;
exports.from_opt = from_opt;
/* Js_exn Not a pure module */
