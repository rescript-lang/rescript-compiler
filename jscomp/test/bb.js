'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function f(x) {
  if (x !== 98) {
    if (x >= 99) {
      return "c";
    } else {
      return "a";
    }
  } else {
    return "b";
  }
}

function ff(x) {
  switch (x) {
    case "a" : 
        return /* a */97;
    case "b" : 
        return /* b */98;
    case "c" : 
        return /* c */99;
    default:
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bb.ml",
              17,
              9
            ]
          ];
  }
}

function test(x) {
  var match;
  switch (x) {
    case "a" : 
        match = /* a */97;
        break;
    case "b" : 
        match = /* b */98;
        break;
    case "c" : 
        match = /* c */99;
        break;
    default:
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bb.ml",
              26,
              13
            ]
          ];
  }
  if (match !== 98) {
    if (match >= 99) {
      return "c";
    } else {
      return "a";
    }
  } else {
    return "b";
  }
}

var test_poly = "a";

var c = f(/* a */97);

var d = f(/* b */98);

var e = f(/* c */99);

exports.f         = f;
exports.ff        = ff;
exports.test      = test;
exports.test_poly = test_poly;
exports.c         = c;
exports.d         = d;
exports.e         = e;
/* c Not a pure module */
