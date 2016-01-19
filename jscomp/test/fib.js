// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

function fib(n) {
  if (1 < (n >>> 0)) {
    return fib(n - 1) + fib(n - 2);
  }
  else {
    return 1;
  }
}

exports.fib = fib;
/* No side effect */
