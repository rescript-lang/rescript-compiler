// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


let a = {
  x: 3,
  y: [1]
};

let b = {
  x: 3,
  y: [1],
  z: 3,
  u: ((x, y) => {
    return x + y | 0;
  })
};

function f(obj) {
  return obj.x + obj.y.length | 0;
}

let u = f(a);

let v = f(b);

exports.a = a;
exports.b = b;
exports.f = f;
exports.u = u;
exports.v = v;
/* u Not a pure module */
