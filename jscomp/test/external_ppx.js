'use strict';

var External_ppxGen = require("./external_ppx.gen");

function f(prim) {
  return External_ppxGen.f(prim);
}

exports.f = f;
/* ./external_ppx.gen Not a pure module */
