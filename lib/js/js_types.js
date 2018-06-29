'use strict';

var Block = require("./block.js");

function classify(x) {
  var ty = typeof x;
  if (ty === "undefined") {
    return /* JSUndefined */3;
  } else if (x === null) {
    return /* JSNull */2;
  } else if (ty === "number") {
    return /* JSNumber */Block.__(0, [x]);
  } else if (ty === "string") {
    return /* JSString */Block.__(1, [x]);
  } else if (ty === "boolean") {
    if (x === true) {
      return /* JSTrue */1;
    } else {
      return /* JSFalse */0;
    }
  } else if (ty === "function") {
    return /* JSFunction */Block.__(2, [x]);
  } else if (ty === "object") {
    return /* JSObject */Block.__(3, [x]);
  } else {
    return /* JSSymbol */Block.__(4, [x]);
  }
}

function test(x, v) {
  switch (v) {
    case 0 : 
        return typeof x === "undefined";
    case 1 : 
        return x === null;
    case 2 : 
        return typeof x === "boolean";
    case 3 : 
        return typeof x === "number";
    case 4 : 
        return typeof x === "string";
    case 5 : 
        return typeof x === "function";
    case 6 : 
        return typeof x === "object";
    case 7 : 
        return typeof x === "symbol";
    
  }
}

exports.test = test;
exports.classify = classify;
/* No side effect */
