// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var External_ppxGen = require("./external_ppx.gen");

function renamed(param) {
  var tmp = {
    type: "123",
    normal: 12
  };
  if (param !== undefined) {
    tmp.WIDTH = param;
  }
  return tmp;
}

var u = {
  hi: 2,
  lo: 3,
  lo2: {hi:-3 },
  lo3: -1,
  lo4: -3
};

function f(prim) {
  return External_ppxGen.f(prim);
}

exports.renamed = renamed;
exports.u = u;
exports.f = f;
/* ./external_ppx.gen Not a pure module */
