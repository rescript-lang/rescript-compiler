'use strict';

var Mt = require("./mt.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function f(match) {
  if (Caml_exceptions.caml_is_extension(match)) {
    if (match === Caml_builtin_exceptions.not_found) {
      return 0;
    } else if (match[0] === Caml_builtin_exceptions.invalid_argument || match === Caml_builtin_exceptions.stack_overflow) {
      return 1;
    } else if (match[0] === Caml_builtin_exceptions.sys_error) {
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
    if (match === Caml_builtin_exceptions.not_found || match[0] === Caml_builtin_exceptions.invalid_argument) {
      return 0;
    } else if (match[0] === Caml_builtin_exceptions.sys_error) {
      return 2;
    } else if (match[0] === A || match[0] === B) {
      return match[1];
    } else {
      return ;
    }
  }
  
}

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

eq("File \"exn_error_pattern.ml\", line 34, characters 5-12", f(Caml_builtin_exceptions.not_found), 0);

eq("File \"exn_error_pattern.ml\", line 35, characters 5-12", f([
          Caml_builtin_exceptions.invalid_argument,
          ""
        ]), 1);

eq("File \"exn_error_pattern.ml\", line 36, characters 5-12", f(Caml_builtin_exceptions.stack_overflow), 1);

eq("File \"exn_error_pattern.ml\", line 37, characters 5-12", f([
          Caml_builtin_exceptions.sys_error,
          ""
        ]), 2);

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
