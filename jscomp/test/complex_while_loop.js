'use strict';

var Pervasives = require("../../lib/js/pervasives.js");

function f(param) {
  var n = /* record */{
    contents: 0
  };
  while((function () {
          var fib = function (n) {
            if (n === 0 || n === 1) {
              return 1;
            } else {
              return fib(n - 1 | 0) + fib(n - 2 | 0) | 0;
            }
          };
          return fib(n.contents) > 10;
        })()) {
    console.log(String(n.contents));
    Pervasives.incr(n);
  };
  return /* () */0;
}

function ff(param) {
  while((function () {
          var b = 9;
          return (3 + b | 0) > 10;
        })()) {
    
  };
  return /* () */0;
}

exports.f = f;
exports.ff = ff;
/* No side effect */
