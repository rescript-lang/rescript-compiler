'use strict';

var Mt                      = require("./mt.js");
var Block                   = require("../../lib/js/block.js");
var Js_exn                  = require("../../lib/js/js_exn.js");
var Caml_exceptions         = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      function () {
        return /* Eq */Block.__(0, [
                  x,
                  y
                ]);
      }
    ],
    suites[0]
  ];
  return /* () */0;
}

var A = Caml_exceptions.create("Promise_catch_test.A");

function handler(e) {
  var e$1 = Js_exn.internalToOCamlException(e);
  if (e$1[0] === Js_exn.$$Error) {
    return /* Error */106380200;
  } else if (e$1 === Caml_builtin_exceptions.not_found) {
    return /* Not_found */-358247754;
  } else if (e$1[0] === Caml_builtin_exceptions.invalid_argument) {
    return /* Invalid_argument */-50278363;
  } else {
    return /* Any */3257036;
  }
}

var B = Caml_exceptions.create("Promise_catch_test.B");

eq("File \"promise_catch_test.ml\", line 26, characters 5-12", handler(1), /* Error */106380200);

eq("File \"promise_catch_test.ml\", line 27, characters 5-12", handler(Caml_builtin_exceptions.not_found), /* Not_found */-358247754);

eq("File \"promise_catch_test.ml\", line 28, characters 5-12", handler([
          Caml_builtin_exceptions.invalid_argument,
          ""
        ]), /* Invalid_argument */-50278363);

eq("File \"promise_catch_test.ml\", line 29, characters 5-12", handler(Caml_builtin_exceptions.stack_overflow), /* Any */3257036);

eq("File \"promise_catch_test.ml\", line 30, characters 5-12", handler([
          A,
          3
        ]), /* Any */3257036);

Mt.from_pair_suites("promise_catch_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
exports.A       = A;
exports.handler = handler;
exports.B       = B;
/*  Not a pure module */
