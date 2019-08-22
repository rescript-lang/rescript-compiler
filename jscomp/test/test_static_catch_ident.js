'use strict';

var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var Scan_failure = Caml_exceptions.create("Test_static_catch_ident.Scan_failure");

function scanf_bad_input(ib, x) {
  if (x[0] !== Scan_failure) {
    if (x[0] !== Caml_builtin_exceptions.failure) {
      throw x;
    }
    
  }
  for(var i = 0; i <= 100; ++i){
    console.log(s);
    console.log("don't inlinie");
  }
  return /* () */0;
}

exports.Scan_failure = Scan_failure;
exports.scanf_bad_input = scanf_bad_input;
/* No side effect */
