'use strict';

var $$Array = require("./array");

function reify_type(x) {
  return /* tuple */[
          typeof x === "string" ? /* String */0 : (
              typeof x === "number" ? /* Number */1 : (
                  typeof x === "boolean" ? /* Boolean */4 : (
                      x === null ? /* Null */5 : (
                          Array.isArray(x) ? /* Array */3 : /* Object */2
                        )
                    )
                )
            ),
          x
        ];
}

function test(x, v) {
  switch (v) {
    case 0 : 
        return +(typeof x === "string");
    case 1 : 
        return +(typeof x === "number");
    case 2 : 
        if (x !== null && typeof x === "object") {
          return 1 - +Array.isArray(x);
        }
        else {
          return /* false */0;
        }
    case 3 : 
        return +Array.isArray(x);
    case 4 : 
        return +(typeof x === "boolean");
    case 5 : 
        return +(x === null);
    
  }
}

var $$null$1 = null;

function string(s) {
  return s;
}

function number(f) {
  return f;
}

function numberOfInt(i) {
  return i;
}

function $$boolean(b) {
  if (b) {
    return true;
  }
  else {
    return false;
  }
}

function object_(o) {
  return o;
}

function array_(a) {
  return a;
}

function stringArray(a) {
  return a;
}

function numberArray(a) {
  return a;
}

function intArray(a) {
  return a;
}

function booleanArray(a) {
  return $$Array.map($$boolean, a);
}

function objectArray(a) {
  return a;
}

var reifyType = reify_type;

exports.reify_type   = reify_type;
exports.reifyType    = reifyType;
exports.test         = test;
exports.$$null       = $$null$1;
exports.string       = string;
exports.number       = number;
exports.numberOfInt  = numberOfInt;
exports.$$boolean    = $$boolean;
exports.object_      = object_;
exports.array_       = array_;
exports.stringArray  = stringArray;
exports.numberArray  = numberArray;
exports.intArray     = intArray;
exports.booleanArray = booleanArray;
exports.objectArray  = objectArray;
/* null Not a pure module */
