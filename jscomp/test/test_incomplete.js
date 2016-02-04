// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_exceptions = require("../runtime/caml_exceptions");

function f(x) {
  if (x > 3 || x < 1) {
    throw [
          0,
          Caml_exceptions.Match_failure,
          [
            0,
            "test_incomplete.ml",
            3,
            2
          ]
        ];
  }
  else {
    return /* "a" */97;
  }
}

function f2(x) {
  if (x) {
    return 0;
  }
  else {
    return 1;
  }
}

function f3(x) {
  switch (x[0]) {
    case 0 : 
    case 2 : 
        return x[1] + 1;
    case 1 : 
    case 3 : 
        return x[1] + 2;
    
  }
}

exports.f  = f;
exports.f2 = f2;
exports.f3 = f3;
/* No side effect */
