'use strict';


function f1(x) {
  return x(/* () */0);
}

function f2(x, y) {
  return x(y, /* () */0);
}

function f3(fn, a, b) {
  return fn(a, b);
}

function id(prim) {
  return prim;
}

function f4(fn, a, b) {
  return id(fn, a, b);
}

function f5(a, b) {
  return f(a, b);
}

function f6(a, b) {
  return id((function (prim, prim$1) {
                return f(prim, prim$1);
              }), a, b);
}

function f7(a, b) {
  return f(a, b);
}

function f8(f, a, b) {
  return f(a, b);
}

function f9(f, a, b) {
  return f(a, b);
}

function poly_f(a, b, c) {
  debugger;
  return /* tuple */[
          a,
          b,
          c
        ];
}

function rec_f(_a, _b, c) {
  while(true) {
    var b = _b;
    var a = _a;
    if (a === 0) {
      return rec_f(a, b, c + 1 | 0) + 3 | 0;
    } else {
      _b = b - 2 | 0;
      _a = a - 1 | 0;
      continue ;
    }
  };
}

exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.id = id;
exports.f4 = f4;
exports.f5 = f5;
exports.f6 = f6;
exports.f7 = f7;
exports.f8 = f8;
exports.f9 = f9;
exports.poly_f = poly_f;
exports.rec_f = rec_f;
/* No side effect */
