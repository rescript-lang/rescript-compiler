// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_curry = require("../runtime/caml_curry");

function f1(x) {
  if (x === null) {
    return 3;
  }
  else {
    return x + 1;
  }
}

function f2(x) {
  if (x === null) {
    return 3;
  }
  else {
    return x + 1;
  }
}

function f5(h, _) {
  var u = Caml_curry.app1(h, 32);
  if (u === null) {
    return 3;
  }
  else {
    return u + 1;
  }
}

function f4(h, x) {
  var u = Caml_curry.app1(h, 32);
  var v = 32 + x;
  if (u === null) {
    return 1 + v;
  }
  else {
    return u + 1;
  }
}

function f6(x, y) {
  return +(x === y);
}

function f7(x) {
  return x;
}

function f8(x) {
  if (x === null) {
    return 2;
  }
  else if (x === null) {
    return 1;
  }
  else {
    return 0;
  }
}

var u = f8(/* None */0);

exports.f1 = f1;
exports.f2 = f2;
exports.f5 = f5;
exports.f4 = f4;
exports.f6 = f6;
exports.f7 = f7;
exports.f8 = f8;
exports.u  = u;
/* u Not a pure module */
