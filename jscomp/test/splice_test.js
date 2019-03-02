'use strict';

var Caml_obj = require("../../lib/js/caml_obj.js");

Math.max(1);

function f00(a, b) {
  return a.send(b);
}

function f1(c) {
  return Caml_obj.splice(Math.max, 1, c);
}

Math.max(1, 2, 3);

exports.f00 = f00;
exports.f1 = f1;
/*  Not a pure module */
