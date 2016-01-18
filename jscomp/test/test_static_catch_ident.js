// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_exceptions = require("../runtime/caml_exceptions");

var Scan_failure = [
  248,
  "Test_static_catch_ident.Scan_failure",
  ++ Caml_exceptions.caml_oo_last_id
];

function scanf_bad_input(_, x) {
  var exit = 0;
  var s;
  if (x[1] === Scan_failure) {
    s = x[2];
    exit = 1;
  }
  else {
    if (x[1] === Caml_exceptions.Failure) {
      s = x[2];
      exit = 1;
    }
    else {
      throw x;
    }
  }
  if (exit === 1) {
    for(var i = 0; i<= 100; ++i){
      console.log(s);
      console.log("don't inlinie");
    }
    return /* () */0;
  }
  
}

exports.Scan_failure = Scan_failure;
exports.scanf_bad_input = scanf_bad_input;
/* No side effect */
