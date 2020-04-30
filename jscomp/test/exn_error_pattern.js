'use strict';

var Mt = require("./mt.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function f(match) {
  if (Caml_exceptions.caml_is_extension(match)) {
    if (match.ExceptionID === "Not_found") {
      return 0;
    } else if (match.ExceptionID === "Invalid_argument" || match.ExceptionID === "Stack_overflow") {
      return 1;
    } else if (match.ExceptionID === "Sys_error") {
      return 2;
    } else {
      return ;
    }
  }
  
}

var A = Caml_exceptions.create("Exn_error_pattern.A");

var B = Caml_exceptions.create("Exn_error_pattern.B");

function g(match) {
  if (Caml_exceptions.caml_is_extension(match)) {
    if (match.ExceptionID === "Not_found" || match.ExceptionID === "Invalid_argument") {
      return 0;
    } else if (match.ExceptionID === "Sys_error") {
      return 2;
    } else if (match.ExceptionID === A || match.ExceptionID === B) {
      return match._1;
    } else {
      return ;
    }
  }
  
}

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

eq("File \"exn_error_pattern.ml\", line 34, characters 5-12", f({
          ExceptionID: "Not_found"
        }), 0);

eq("File \"exn_error_pattern.ml\", line 35, characters 5-12", f({
          ExceptionID: "Invalid_argument",
          _1: ""
        }), 1);

eq("File \"exn_error_pattern.ml\", line 36, characters 5-12", f({
          ExceptionID: "Stack_overflow"
        }), 1);

eq("File \"exn_error_pattern.ml\", line 37, characters 5-12", f({
          ExceptionID: "Sys_error",
          _1: ""
        }), 2);

var tmp;

try {
  throw new Error("x");
}
catch (raw_e){
  tmp = Caml_js_exceptions.internalToOCamlException(raw_e);
}

eq("File \"exn_error_pattern.ml\", line 38, characters 5-12", f(tmp), undefined);

Mt.from_pair_suites("Exn_error_pattern", suites.contents);

exports.f = f;
exports.A = A;
exports.B = B;
exports.g = g;
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
