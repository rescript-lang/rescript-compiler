'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Js_exn = require("../../lib/js/js_exn.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = [/* [] */0];

var counter = [0];

function add_test(loc, test) {
  counter[0] = counter[0] + 1 | 0;
  var id = loc + (" id " + counter[0]);
  suites[0] = /* :: */[
    /* tuple */[
      id,
      test
    ],
    suites[0]
  ];
  return /* () */0;
}

function eq(loc, x, y) {
  return add_test(loc, (function () {
                return /* Eq */Block.__(0, [
                          x,
                          y
                        ]);
              }));
}

function false_(loc) {
  return add_test(loc, (function () {
                return /* Ok */Block.__(4, [/* false */0]);
              }));
}

function true_(loc) {
  return add_test(loc, (function () {
                return /* Ok */Block.__(4, [/* true */1]);
              }));
}

var exit = 0;

var e;

try {
  e = JSON.parse(" {\"x\"}");
  exit = 1;
}
catch (raw_exn){
  var exn = Js_exn.internalToOCamlException(raw_exn);
  if (exn[0] === Js_exn.$$Error) {
    add_test("File \"js_exception_catch_test.ml\", line 21, characters 10-17", (function () {
            return /* Ok */Block.__(4, [/* true */1]);
          }));
  } else {
    throw exn;
  }
}

if (exit === 1) {
  add_test("File \"js_exception_catch_test.ml\", line 22, characters 16-23", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
}

var A = Caml_exceptions.create("Js_exception_catch_test.A");

var B = Caml_exceptions.create("Js_exception_catch_test.B");

var C = Caml_exceptions.create("Js_exception_catch_test.C");

function test(f) {
  try {
    Curry._1(f, /* () */0);
    return /* No_error */-465676758;
  }
  catch (raw_e){
    var e = Js_exn.internalToOCamlException(raw_e);
    if (e === Caml_builtin_exceptions.not_found) {
      return /* Not_found */-358247754;
    } else if (e[0] === Caml_builtin_exceptions.invalid_argument) {
      if (e[1] === "x") {
        return /* Invalid_argument */-50278363;
      } else {
        return /* Invalid_any */545126980;
      }
    } else if (e[0] === A) {
      if (e[1] !== 2) {
        return /* A_any */740357294;
      } else {
        return /* A2 */14545;
      }
    } else if (e === B) {
      return /* B */66;
    } else if (e[0] === C) {
      if (e[1] !== 1 || e[2] !== 2) {
        return /* C_any */-756146768;
      } else {
        return /* C */67;
      }
    } else if (e[0] === Js_exn.$$Error) {
      return /* Js_error */634022066;
    } else {
      return /* Any */3257036;
    }
  }
}

eq("File \"js_exception_catch_test.ml\", line 43, characters 5-12", test((function () {
            return /* () */0;
          })), /* No_error */-465676758);

eq("File \"js_exception_catch_test.ml\", line 44, characters 5-12", test((function () {
            throw Caml_builtin_exceptions.not_found;
          })), /* Not_found */-358247754);

eq("File \"js_exception_catch_test.ml\", line 45, characters 5-12", test((function () {
            throw [
                  Caml_builtin_exceptions.invalid_argument,
                  "x"
                ];
          })), /* Invalid_argument */-50278363);

eq("File \"js_exception_catch_test.ml\", line 46, characters 5-12", test((function () {
            throw [
                  Caml_builtin_exceptions.invalid_argument,
                  ""
                ];
          })), /* Invalid_any */545126980);

eq("File \"js_exception_catch_test.ml\", line 47, characters 5-12", test((function () {
            throw [
                  A,
                  2
                ];
          })), /* A2 */14545);

eq("File \"js_exception_catch_test.ml\", line 48, characters 5-12", test((function () {
            throw [
                  A,
                  3
                ];
          })), /* A_any */740357294);

eq("File \"js_exception_catch_test.ml\", line 49, characters 5-12", test((function () {
            throw B;
          })), /* B */66);

eq("File \"js_exception_catch_test.ml\", line 50, characters 5-12", test((function () {
            throw [
                  C,
                  1,
                  2
                ];
          })), /* C */67);

eq("File \"js_exception_catch_test.ml\", line 51, characters 5-12", test((function () {
            throw [
                  C,
                  0,
                  2
                ];
          })), /* C_any */-756146768);

eq("File \"js_exception_catch_test.ml\", line 52, characters 5-12", test((function () {
            throw new Error("x");
          })), /* Js_error */634022066);

eq("File \"js_exception_catch_test.ml\", line 53, characters 5-12", test((function () {
            throw [
                  Caml_builtin_exceptions.failure,
                  "x"
                ];
          })), /* Any */3257036);

Mt.from_pair_suites("js_exception_catch_test.ml", suites[0]);

exports.suites = suites;
exports.add_test = add_test;
exports.eq = eq;
exports.false_ = false_;
exports.true_ = true_;
exports.A = A;
exports.B = B;
exports.C = C;
exports.test = test;
/*  Not a pure module */
