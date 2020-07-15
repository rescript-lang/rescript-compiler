'use strict';


function M(U) {
  var v = U.f(100, "x");
  return {
          v
        };
}

function f() {
  return 3;
}

f();

function $plus$great(a, h) {
  return h(a);
}

function u(h) {
  return $plus$great(3, h);
}

exports.M = M;
exports.f = f;
exports.$plus$great = $plus$great;
exports.u = u;
/*  Not a pure module */
