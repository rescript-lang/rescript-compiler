'use strict';

var Caml_format = require("../../lib/js/caml_format.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var Inline_record = Caml_exceptions.create("Record_extension_test.Inline_record");

function f(x) {
  if (x[0] === Inline_record) {
    return x[/* x */1] + Caml_format.caml_int_of_string(x[/* y */2]) | 0;
  }
  
}

function f2(x) {
  if (x) {
    return x[/* x */0];
  } else {
    return 0;
  }
}

exports.Inline_record = Inline_record;
exports.f = f;
exports.f2 = f2;
/* No side effect */
