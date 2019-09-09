'use strict';

var Block = require("../../lib/js/block.js");

var b = /* B */Block.__(0, [34]);

var c = /* C */Block.__(1, [
    4,
    2
  ]);

var d = /* D */Block.__(2, [/* tuple */[
      4,
      2
    ]]);

console.log("a1", /* A1 */0);

console.log("a2", /* A2 */1);

console.log("b", b);

console.log("c", c);

console.log("d", d);

function foo(param) {
  if (typeof param === "number") {
    if (param === /* A1 */0) {
      return 1;
    } else {
      return 2;
    }
  } else {
    switch (param.tag | 0) {
      case /* B */0 :
          return param[0];
      case /* C */1 :
          return param[0] + param[1] | 0;
      case /* D */2 :
          var match = param[0];
          return match[0] + match[1] | 0;
      
    }
  }
}

function fooA1(param) {
  if (typeof param === "number" && param === 0) {
    return 1;
  } else {
    return 42;
  }
}

function fooC(param) {
  if (typeof param === "number" || param.tag !== /* C */1) {
    return 42;
  } else {
    return param[0] + param[1] | 0;
  }
}

function switchNum(param) {
  switch (param) {
    case /* Unknown */0 :
        return "0";
    case /* Unknown */1 :
        return "1";
    case /* Unknown */2 :
        return "2";
    default:
      return "_";
  }
}

var a1 = /* A1 */0;

var a2 = /* A2 */1;

exports.a1 = a1;
exports.a2 = a2;
exports.b = b;
exports.c = c;
exports.d = d;
exports.foo = foo;
exports.fooA1 = fooA1;
exports.fooC = fooC;
exports.switchNum = switchNum;
/*  Not a pure module */
