// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_curry = require("../runtime/caml_curry");

function f1(x) {
  if (x === null) {
    return 3;
  }
  else {
    return x + 1 | 0;
  }
}

function f2(x) {
  if (x === null) {
    return 3;
  }
  else {
    return x + 1 | 0;
  }
}

function f5(h, _) {
  var u = Caml_curry.app1(h, 32);
  if (u === null) {
    return 3;
  }
  else {
    return u + 1 | 0;
  }
}

function f4(h, x) {
  var u = Caml_curry.app1(h, 32);
  var v = 32 + x | 0;
  if (u === null) {
    return 1 + v | 0;
  }
  else {
    return u + 1 | 0;
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

function f9(x) {
  if (x === null) {
    return /* None */0;
  }
  else {
    return [x];
  }
}

function f10(x) {
  return +(x === null);
}

var f11 = /* false */0;

function f1$1(x) {
  if (x === undefined) {
    return 3;
  }
  else {
    return x + 1 | 0;
  }
}

function f2$1(x) {
  if (x === undefined) {
    return 3;
  }
  else {
    return x + 1 | 0;
  }
}

function f5$1(h, _) {
  var u = Caml_curry.app1(h, 32);
  if (u === undefined) {
    return 3;
  }
  else {
    return u + 1 | 0;
  }
}

function f4$1(h, x) {
  var u = Caml_curry.app1(h, 32);
  var v = 32 + x | 0;
  if (u === undefined) {
    return 1 + v | 0;
  }
  else {
    return u + 1 | 0;
  }
}

function f6$1(x, y) {
  return +(x === y);
}

function f7$1(x) {
  return x;
}

function f8$1(x) {
  if (x === undefined) {
    return 2;
  }
  else if (x === undefined) {
    return 1;
  }
  else {
    return 0;
  }
}

var u$1 = f8$1(/* None */0);

function f9$1(x) {
  if (x === undefined) {
    return /* None */0;
  }
  else {
    return [x];
  }
}

function f10$1(x) {
  return +(x === undefined);
}

var f11$1 = /* false */0;

var Undef = /* module */[
  f1$1,
  f2$1,
  f5$1,
  f4$1,
  f6$1,
  f7$1,
  f8$1,
  u$1,
  f9$1,
  f10$1,
  f11$1
];

exports.f1    = f1;
exports.f2    = f2;
exports.f5    = f5;
exports.f4    = f4;
exports.f6    = f6;
exports.f7    = f7;
exports.f8    = f8;
exports.u     = u;
exports.f9    = f9;
exports.f10   = f10;
exports.f11   = f11;
exports.Undef = Undef;
/* u Not a pure module */
