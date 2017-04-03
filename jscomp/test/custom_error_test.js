'use strict';

var Js_exn       = require("../../lib/js/js_exn.js");
var Js_primitive = require("../../lib/js/js_primitive.js");

function test_js_error() {
  var exit = 0;
  var e;
  try {
    e = JSON.parse(" {\"x\" : }");
    exit = 1;
  }
  catch (exn){
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
  catch (e){
    if (e[0] === Js_exn.$$Error) {
      console.log(Js_primitive.undefined_to_opt(e[1].stack));
      throw e;
    } else {
      throw e;
    }
  }
}

exports.test_js_error  = test_js_error;
exports.test_js_error2 = test_js_error2;
/* No side effect */
