// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var CamlinternalLazy = require("../stdlib/camlinternalLazy");

var u = [
  0,
  3
];

var v = [
  246,
  function () {
    u[1] = 32;
    return /* () */0;
  }
];

function lazy_test() {
  var h = u[1];
  var tag = Caml_obj_runtime.caml_obj_tag(v);
  if (tag !== 250) {
    if (tag === 246) {
      CamlinternalLazy.force_lazy_block(v);
    }
    else {
      ;
    }
  }
  var g = u[1];
  return [
          /* tuple */0,
          h,
          g
        ];
}

exports.u = u;
exports.v = v;
exports.lazy_test = lazy_test;
/* No side effect */
