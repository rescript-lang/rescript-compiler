// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Caml_exceptions = require("../runtime/caml_exceptions");

var A = [
  248,
  "Test_exception_escape.N.A",
  ++ Caml_exceptions.caml_oo_last_id
];

var f;

try {
  throw [
        0,
        A,
        3
      ];
}
catch (exn){
  f = 3;
}

var N = [
  0,
  f
];

exports.N = N;
/* f Not a pure module */
