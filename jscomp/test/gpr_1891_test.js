'use strict';

var Curry = require("../../lib/js/curry.js");

function foo(x) {
  if (typeof x === "number" || x[0] !== 3505894 || x[1] !== 3) {
    console.log("2");
    return ;
  } else {
    console.log("1");
    return ;
  }
}

function foo2(x) {
  if (typeof x === "number" || x[0] !== 3505894 || x[1] !== 3) {
    return "xxx";
  } else {
    return "xxxx";
  }
}

function foo3(x) {
  if (typeof x === "number" || x[0] !== 3505894 || x[1] !== 3) {
    return 2;
  } else {
    return 1;
  }
}

function foo4(x, h) {
  if (typeof x === "number" || x[0] !== 3505894 || x[1] !== 3) {
    return ;
  } else {
    return Curry._1(h, void 0);
  }
}

function foo5(x) {
  if (typeof x === "number" || x[0] !== 3505894 || x[1] !== 3) {
    console.log("x");
    return ;
  } else {
    console.log("hi");
    return ;
  }
}

exports.foo = foo;
exports.foo2 = foo2;
exports.foo3 = foo3;
exports.foo4 = foo4;
exports.foo5 = foo5;
/* No side effect */
