'use strict';

var Mk = require("xx/foo_class");

function f() {
  return new Mk(3);
}

exports.f = f;
/* xx/foo_class Not a pure module */
