'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_exn = require("../../lib/js/js_exn.js");
var Js_option = require("../../lib/js/js_option.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

function handler(e) {
  if (e[0] === Js_exn.$$Error) {
    console.log("js error");
    return Promise.resolve(0);
  } else if (e === Caml_builtin_exceptions.not_found) {
    console.log("hi");
    return Promise.resolve(0);
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "promise_catch_test.ml",
            22,
            9
          ]
        ];
  }
}

function myHandler(match) {
  if (Caml_exceptions.isCamlExceptionOrOpenVariant(match)) {
    if (match === Caml_builtin_exceptions.not_found) {
      return /* Some */[1];
    } else if (match[0] === Js_exn.$$Error) {
      return /* Some */[2];
    } else {
      return /* None */0;
    }
  } else {
    return /* None */0;
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
  var e = Js_exn.internalToOCamlException(raw_e);
  eq("File \"promise_catch_test.ml\", line 36, characters 7-14", /* true */1, Js_option.isSomeValue((function (xxx, y) {
              return +(xxx === y);
            }), 2, myHandler(e)));
}

if (exit === 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "promise_catch_test.ml",
          39,
          9
        ]
      ];
}

Mt.from_pair_suites("promise_catch_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.handler = handler;
exports.myHandler = myHandler;
exports.f = f;
/*  Not a pure module */
