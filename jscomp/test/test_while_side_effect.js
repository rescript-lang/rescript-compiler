'use strict';

var Pervasives = require("../../lib/js/pervasives.js");

var v = /* record */{
  contents: 0
};

while(console.log(String(v.contents)), Pervasives.incr(v), v.contents < 10) {
  
};

function fib(n) {
  if (n === 0 || n === 1) {
    return 1;
  } else {
    return fib(n - 1 | 0) + fib(n - 2 | 0) | 0;
  }
}

var x = /* record */{
  contents: 3
};

while((function () {
        var y = /* record */{
          contents: 3
        };
        console.log(String(x.contents));
        Pervasives.incr(y);
        Pervasives.incr(x);
        return (fib(x.contents) + fib(x.contents) | 0) < 20;
      })()) {
  console.log(String(3));
};

exports.v = v;
exports.fib = fib;
exports.x = x;
/*  Not a pure module */
