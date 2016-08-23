'use strict';

var Xx$slashfoo_class = require("xx/foo_class");
var Mk                = require("xx/foo_class");

function f() {
  return new Mk(3);
}

function v() {
  return Xx$slashfoo_class.ff(3);
}

exports.f = f;
exports.v = v;
/* xx/foo_class Not a pure module */
