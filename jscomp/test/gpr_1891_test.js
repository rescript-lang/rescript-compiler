'use strict';

var Curry = require("../../lib/js/curry.js");

function foo(x) {
  if (typeof x === "object" && x.NAME === "Foo" && x.VAL === 3) {
    console.log("1");
  } else {
    console.log("2");
  }
  
}

function foo2(x) {
  if (typeof x === "object" && x.NAME === "Foo" && x.VAL === 3) {
    return "xxxx";
  } else {
    return "xxx";
  }
}

function foo3(x) {
  if (typeof x === "object" && x.NAME === "Foo" && x.VAL === 3) {
    return 1;
  } else {
    return 2;
  }
}

function foo4(x, h) {
  if (typeof x === "object" && x.NAME === "Foo" && x.VAL === 3) {
    return Curry._1(h, undefined);
  }
  
}

function foo5(x) {
  if (typeof x === "object" && x.NAME === "Foo" && x.VAL === 3) {
    console.log("hi");
  } else {
    console.log("x");
  }
  
}

exports.foo = foo;
exports.foo2 = foo2;
exports.foo3 = foo3;
exports.foo4 = foo4;
exports.foo5 = foo5;
/* No side effect */
