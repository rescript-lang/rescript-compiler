'use strict';

var Mk = require("xx/foo_class");

function f() {
  return new Mk(3);
}

function v() {
  return Mk.ff(3);
}

exports.f = f;
exports.v = v;
/* xx/foo_class Not a pure module */
