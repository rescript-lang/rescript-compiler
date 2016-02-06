// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Filename   = require("../stdlib/filename");
var Mt         = require("./mt");
var $$String   = require("../stdlib/string");
var Caml_curry = require("../runtime/caml_curry");

function generic_basename(is_dir_sep, current_dir_name, name) {
  if (name === "") {
    return current_dir_name;
  }
  else {
    var _n = name.length - 1;
    while(true) {
      var n = _n;
      if (n < 0) {
        return $$String.sub(name, 0, 1);
      }
      else if (Caml_curry.app2(is_dir_sep, name, n)) {
        _n = n - 1;
      }
      else {
        var _n$1 = n;
        var p = n + 1;
        while(true) {
          var n$1 = _n$1;
          if (n$1 < 0) {
            return $$String.sub(name, 0, p);
          }
          else if (Caml_curry.app2(is_dir_sep, name, n$1)) {
            return $$String.sub(name, n$1 + 1, p - n$1 - 1);
          }
          else {
            _n$1 = n$1 - 1;
          }
        };
      }
    };
  }
}

function basename(param) {
  return generic_basename(function (s, i) {
              return +(s[i] === "/");
            }, Filename.current_dir_name, param);
}

var suites_001 = [
  /* tuple */0,
  "basename",
  function () {
    return [
            /* Eq */0,
            basename("b/c/a.b"),
            "a.b"
          ];
  }
];

var suites = [
  /* :: */0,
  suites_001,
  /* [] */0
];

Mt.from_pair_suites("inline_regression_test.ml", suites);

exports.generic_basename = generic_basename;
exports.basename         = basename;
exports.suites           = suites;
/*  Not a pure module */
