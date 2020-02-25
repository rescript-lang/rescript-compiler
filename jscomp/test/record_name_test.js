'use strict';


function f(x) {
  return {
          THIS_IS_NOT_EXPRESSIBLE_IN_BUCKLE: x
        };
}

function set(x) {
  x.THIS_IS_NOT_EXPRESSIBLE_IN_BUCKLE = 3;
  return (x.THIS_IS_NOT_EXPRESSIBLE_IN_BUCKLE << 1);
}

function f1(u) {
  return u.x.x.x.y;
}

function f2(x) {
  x["x'"] = x["x'"] + 3 | 0;
  return {
          "x'": x["x'"] + 3 | 0
        };
}

function f3(x) {
  x.in = x.in + 3 | 0;
  return {
          in: x.in + 3 | 0
        };
}

function f4(param) {
  return (((param.EXACT_MAPPING_TO_JS_LABEL + param.EXACT_2 | 0) + param.z.hello | 0) << 1);
}

function u(param) {
  return {
          x: 22,
          h: 3
        };
}

exports.f = f;
exports.set = set;
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.u = u;
/* No side effect */
