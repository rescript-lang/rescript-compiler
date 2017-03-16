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

function number_of_int(i) {
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

function string_array(a) {
  return a;
}

function number_array(a) {
  return a;
}

function int_array(a) {
  return a;
}

function boolean_array(a) {
  return $$Array.map($$boolean, a);
}

exports.reify_type    = reify_type;
exports.test          = test;
exports.$$null        = $$null$1;
exports.string        = string;
exports.number        = number;
exports.number_of_int = number_of_int;
exports.$$boolean     = $$boolean;
exports.object_       = object_;
exports.array_        = array_;
exports.string_array  = string_array;
exports.number_array  = number_array;
exports.int_array     = int_array;
exports.boolean_array = boolean_array;
/* null Not a pure module */
