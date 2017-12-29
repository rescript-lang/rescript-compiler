'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
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
      } else if (Curry._2(is_dir_sep, name, n)) {
        _n = n - 1 | 0;
        continue ;
        
      } else {
        var _n$1 = n;
        var p = n + 1 | 0;
        while(true) {
          var n$1 = _n$1;
          if (n$1 < 0) {
            return $$String.sub(name, 0, p);
          } else if (Curry._2(is_dir_sep, name, n$1)) {
            return $$String.sub(name, n$1 + 1 | 0, (p - n$1 | 0) - 1 | 0);
          } else {
            _n$1 = n$1 - 1 | 0;
            continue ;
            
          }
        };
      }
    };
  }
}

function basename(param) {
  return generic_basename((function (s, i) {
                return +(Caml_string.get(s, i) === /* "/" */47);
              }), Filename.current_dir_name, param);
}

var suites_000 = /* tuple */[
  "basename",
  (function () {
      return /* Eq */Block.__(0, [
                basename("b/c/a.b"),
                "a.b"
              ]);
    })
];

var suites = /* :: */[
  suites_000,
  /* [] */0
];

Mt.from_pair_suites("inline_regression_test.ml", suites);

exports.generic_basename = generic_basename;
exports.basename = basename;
exports.suites = suites;
/*  Not a pure module */
