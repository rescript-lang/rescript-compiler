'use strict';

var Block = require("../../lib/js/block.js");
var Caml_format = require("../../lib/js/caml_format.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var Inline_record = Caml_exceptions.create("Record_extension_test.Inline_record");

function f(x) {
  if (x[0] === Inline_record) {
    return x[/* x */1] + Caml_format.caml_int_of_string(x[/* y */2]) | 0;
  }
  
}

var v0 = [
  Inline_record,
  /* x */3,
  /* y */"4"
];

function f2(x) {
  if (typeof x === "number" || x.tag) {
    return 0;
  } else {
    return x[/* x */0];
  }
}

function f2_with(x) {
  if (typeof x === "number" || x.tag) {
    return x;
  } else {
    return /* C */Block.__(0, [
              /* x */0,
              /* y */x[/* y */1]
            ]);
  }
}

exports.Inline_record = Inline_record;
exports.f = f;
exports.v0 = v0;
exports.f2 = f2;
exports.f2_with = f2_with;
/* No side effect */
