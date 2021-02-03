'use strict';

var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var Scan_failure = /* @__PURE__ */Caml_exceptions.create("Test_static_catch_ident.Scan_failure");

function scanf_bad_input(ib, x) {
  var s;
  if (x.RE_EXN_ID === Scan_failure || x.RE_EXN_ID === "Failure") {
    s = x._1;
  } else {
    throw x;
  }
  for(var i = 0; i <= 100; ++i){
    console.log(s);
    console.log("don't inlinie");
  }
  
}

exports.Scan_failure = Scan_failure;
exports.scanf_bad_input = scanf_bad_input;
/* No side effect */
