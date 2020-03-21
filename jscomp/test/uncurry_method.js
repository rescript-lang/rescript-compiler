'use strict';


var obj = {
  hi: (function (a, b) {
      return a + b | 0;
    }),
  say: (function (a, b) {
      return a - b | 0;
    }),
  xx: (function (a, b) {
      return a - b | 0;
    })
};

function f(x, a, b) {
  return x.hi(a, b);
}

var h = obj.hi;

console.log(f(obj, 3, 4));

function x(h) {
  return h.raw(0, 0);
}

function f1(u) {
  return u.exit(2);
}

var obj3 = {
  hi: (function (name, age) {
      console.log(name);
      return /* () */0;
    }),
  hh: (function () {
      var self = this ;
      return self.hi("x", 20);
    })
};

var obj2 = {
  hi: (function (a, b) {
      return a + b | 0;
    }),
  say: (function (a, b) {
      var self = this ;
      return self.hi(a, b) - 1 | 0;
    }),
  xx: (function (a, b) {
      return a - b | 0;
    })
};

exports.obj = obj;
exports.f = f;
exports.h = h;
exports.x = x;
exports.f1 = f1;
exports.obj3 = obj3;
exports.obj2 = obj2;
/* obj Not a pure module */
