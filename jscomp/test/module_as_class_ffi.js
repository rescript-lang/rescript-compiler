'use strict';

var Foo_class = require("xx/foo_class");

function f() {
  return new Foo_class(3);
}

function v() {
  return Foo_class.ff(3);
}

exports.f = f;
exports.v = v;
/* xx/foo_class Not a pure module */
