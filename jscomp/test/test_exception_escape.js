// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");

var A = {
  0: "Test_exception_escape.N.A",
  1: ++ Caml_builtin_exceptions.caml_oo_last_id,
  length: 2,
  tag: 248
};

var f;

try {
  throw [
        A,
        3
      ];
}
catch (exn){
  f = 3;
}

var N = [f];

exports.N = N;
/* f Not a pure module */
