// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Curry        = require("../runtime/curry");
var js_primitive = require("../runtime/js_primitive");

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
  var u = Curry._1(h, 32);
  if (u === null) {
    return 3;
  }
  else {
    return u + 1 | 0;
  }
}

function f4(h, x) {
  var u = Curry._1(h, 32);
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

var Test_null = /* module */[
  f1,
  f2,
  f5,
  f4,
  f6,
  f7,
  f8,
  u,
  f9,
  f10,
  f11
];

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
  var u = Curry._1(h, 32);
  if (u === undefined) {
    return 3;
  }
  else {
    return u + 1 | 0;
  }
}

function f4$1(h, x) {
  var u = Curry._1(h, 32);
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

var Test_def = /* module */[
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

function f1$2(x) {
  if (x === null) {
    return 3;
  }
  else {
    return x + 1 | 0;
  }
}

function f2$2(x) {
  if (x === null) {
    return 3;
  }
  else {
    return x + 1 | 0;
  }
}

function f5$2(h, _) {
  var u = Curry._1(h, 32);
  if (u === null) {
    return 3;
  }
  else {
    return u + 1 | 0;
  }
}

function f4$2(h, x) {
  var u = Curry._1(h, 32);
  var v = 32 + x | 0;
  if (u === null) {
    return 1 + v | 0;
  }
  else {
    return u + 1 | 0;
  }
}

function f6$2(x, y) {
  return +(x === y);
}

function f7$2(x) {
  return x;
}

function f8$2(x) {
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

var u$2 = f8$2(/* None */0);

var f9$2 = js_primitive.js_from_nullable_def

var f10$2 = js_primitive.js_is_nil_undef

var f11$2 = js_primitive.js_is_nil_undef(3);

var Test_null_def = /* module */[
  f1$2,
  f2$2,
  f5$2,
  f4$2,
  f6$2,
  f7$2,
  f8$2,
  u$2,
  f9$2,
  f10$2,
  f11$2
];

exports.Test_null     = Test_null;
exports.Test_def      = Test_def;
exports.Test_null_def = Test_null_def;
/* u Not a pure module */
