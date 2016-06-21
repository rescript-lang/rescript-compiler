// GENERATED CODE BY BUCKLESCRIPT VERSION 0.5.5 , PLEASE EDIT WITH CARE
'use strict';


function f(h) {
  return h.x.y.z;
}

function f2(h) {
  return h.x.y.z;
}

function f3(h, x, y) {
  var tmp = h.paint(x, y);
  return tmp.draw(x, y);
}

function f4(h, x, y) {
  var tmp = h.paint = /* tuple */[
    x,
    y
  ];
  return tmp.draw = /* tuple */[
          x,
          y
        ];
}

exports.f  = f;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
/* No side effect */
