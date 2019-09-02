'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function f(x) {
  if (x > 3 || x < 1) {
    throw [
          Caml_builtin_exceptions.match_failure,
          /* tuple */[
            "test_incomplete.ml",
            3,
            2
          ]
        ];
  } else {
    return /* "a" */97;
  }
}

function f2(x) {
  if (x !== undefined) {
    return 0;
  } else {
    return 1;
  }
}

function f3(x) {
  switch (/* XXX */x.tag) {
    case "A" :
    case "C" :
        return x.Arg0 + 1 | 0;
    case "B" :
    case "D" :
        return x.Arg0 + 2 | 0;
    
  }
}

exports.f = f;
exports.f2 = f2;
exports.f3 = f3;
/* No side effect */
