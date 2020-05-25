'use strict';

var Mt = require("./mt.js");
var Js_exn = require("../../lib/js/js_exn.js");
var Js_option = require("../../lib/js/js_option.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */{
    _0: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    _1: suites.contents
  };
  
}

function handler(e) {
  if (e.RE_EXN_ID === Js_exn.$$Error) {
    console.log("js error");
    return Promise.resolve(0);
  }
  if (e.RE_EXN_ID === "Not_found") {
    console.log("hi");
    return Promise.resolve(0);
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "promise_catch_test.ml",
          22,
          9
        ],
        Error: new Error()
      };
}

function myHandler(match) {
  if (Caml_exceptions.caml_is_extension(match)) {
    if (match.RE_EXN_ID === "Not_found") {
      return 1;
    } else if (match.RE_EXN_ID === Js_exn.$$Error) {
      return 2;
    } else {
      return ;
    }
  }
  
}

function f(x) {
  return x.catch(handler);
}

var exit = 0;

var val;

try {
  val = JSON.parse(" 1. +  ");
  exit = 1;
}
catch (raw_e){
  var e = Caml_js_exceptions.internalToOCamlException(raw_e);
  eq("File \"promise_catch_test.ml\", line 36, characters 7-14", true, Js_option.isSomeValue((function (xxx, y) {
              return xxx === y;
            }), 2, myHandler(e)));
}

if (exit === 1) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "promise_catch_test.ml",
          39,
          9
        ],
        Error: new Error()
      };
}

Mt.from_pair_suites("Promise_catch_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.handler = handler;
exports.myHandler = myHandler;
exports.f = f;
/*  Not a pure module */
