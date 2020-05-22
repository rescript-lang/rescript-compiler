'use strict';

var Curry = require("../../lib/js/curry.js");

function foo(x) {
  if (typeof x === "number" || x.HASH !== 3505894 || x.value !== 3) {
    console.log("2");
  } else {
    console.log("1");
  }
  
}

function foo2(x) {
  if (typeof x === "number" || x.HASH !== 3505894 || x.value !== 3) {
    return "xxx";
  } else {
    return "xxxx";
  }
}

function foo3(x) {
  if (typeof x === "number" || x.HASH !== 3505894 || x.value !== 3) {
    return 2;
  } else {
    return 1;
  }
}

function foo4(x, h) {
  if (typeof x === "number" || x.HASH !== 3505894 || x.value !== 3) {
    return ;
  } else {
    return Curry._1(h, undefined);
  }
}

function foo5(x) {
  if (typeof x === "number" || x.HASH !== 3505894 || x.value !== 3) {
    console.log("x");
  } else {
    console.log("hi");
  }
  
}

exports.foo = foo;
exports.foo2 = foo2;
exports.foo3 = foo3;
exports.foo4 = foo4;
exports.foo5 = foo5;
/* No side effect */
