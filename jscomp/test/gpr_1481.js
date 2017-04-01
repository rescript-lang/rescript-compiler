'use strict';

var ModuleId = require("#moduleid");

function f() {
  return ModuleId.name;
}

exports.f = f;
/* #moduleid Not a pure module */
