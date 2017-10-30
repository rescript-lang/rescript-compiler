'use strict';

var Moduleid = require("#moduleid");

function f() {
  return Moduleid.name;
}

exports.f = f;
/* #moduleid Not a pure module */
