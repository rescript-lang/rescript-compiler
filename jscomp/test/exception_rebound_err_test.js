'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
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
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  
}

var A = Caml_exceptions.create("Exception_rebound_err_test.A");

var B = Caml_exceptions.create("Exception_rebound_err_test.B");

var C = Caml_exceptions.create("Exception_rebound_err_test.C");

function test_js_error4(param) {
  try {
    JSON.parse(" {\"x\"}");
    return 1;
  }
  catch (raw_e){
    var e = Caml_js_exceptions.internalToOCamlException(raw_e);
    if (e.ExceptionID === /* Not_found */-6) {
      return 2;
    }
    if (e.ExceptionID === /* Invalid_argument */-3 && e._1 === "x") {
      return 3;
    }
    if (e.ExceptionID === A.ExceptionID) {
      if (e._1 !== 2) {
        return 7;
      } else {
        return 4;
      }
    } else if (e.ExceptionID === B.ExceptionID) {
      return 5;
    } else if (e.ExceptionID === C.ExceptionID && !(e._1 !== 1 || e._2 !== 2)) {
      return 6;
    } else {
      return 7;
    }
  }
}

function f(g) {
  try {
    return Curry._1(g, undefined);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.ExceptionID === /* Not_found */-6) {
      return 1;
    }
    throw exn;
  }
}

eq("File \"exception_rebound_err_test.ml\", line 24, characters 6-13", test_js_error4(undefined), 7);

Mt.from_pair_suites("Exception_rebound_err_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.A = A;
exports.B = B;
exports.C = C;
exports.test_js_error4 = test_js_error4;
exports.f = f;
/*  Not a pure module */
