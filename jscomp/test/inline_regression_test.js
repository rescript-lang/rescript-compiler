'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var $$String = require("../../lib/js/string.js");
var Filename = require("../../lib/js/filename.js");
var Caml_string = require("../../lib/js/caml_string.js");

function generic_basename(is_dir_sep, current_dir_name, name) {
  if (name === "") {
    return current_dir_name;
  } else {
    var _n = name.length - 1 | 0;
    while(true) {
      var n = _n;
      if (n < 0) {
        return $$String.sub(name, 0, 1);
      }
      if (!Curry._2(is_dir_sep, name, n)) {
        var _n$1 = n;
        var p = n + 1 | 0;
        while(true) {
          var n$1 = _n$1;
          if (n$1 < 0) {
            return $$String.sub(name, 0, p);
          }
          if (Curry._2(is_dir_sep, name, n$1)) {
            return $$String.sub(name, n$1 + 1 | 0, (p - n$1 | 0) - 1 | 0);
          }
          _n$1 = n$1 - 1 | 0;
          continue ;
        };
      }
      _n = n - 1 | 0;
      continue ;
    };
  }
}

function basename(param) {
  return generic_basename((function (s, i) {
                return Caml_string.get(s, i) === /* '/' */47;
              }), Filename.current_dir_name, param);
}

var suites_0 = [
  "basename",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: basename("b/c/a.b"),
              _1: "a.b"
            };
    })
];

var suites = {
  hd: suites_0,
  tl: /* [] */0
};

Mt.from_pair_suites("Inline_regression_test", suites);

exports.generic_basename = generic_basename;
exports.basename = basename;
exports.suites = suites;
/*  Not a pure module */
