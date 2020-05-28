'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_option = require("../../lib/js/caml_option.js");

console.log([
      "hello world",
      1
    ]);

console.log(1337);

console.log("hello world");

var arg_string = {
  HASH: /* String */-976970511,
  VAL: "hi runtime"
};

console.log(arg_string.VAL);

var arg_pair = {
  HASH: /* Pair */892012602,
  VAL: [
    "hi",
    1
  ]
};

console.log(arg_pair.VAL);

console.log(undefined);

console.log(1, undefined);

console.log(2, "hi");

console.log(3, "hi");

console.log(4, undefined);

var some_arg = {
  HASH: /* Bool */737456202,
  VAL: true
};

console.log(5, Caml_option.option_unwrap(some_arg));

console.log(6, undefined);

console.log(7, Caml_option.option_unwrap((console.log("trace"), undefined)));

function dyn_log3(prim, prim$1, prim$2) {
  console.log(prim.VAL, Caml_option.option_unwrap(prim$1));
  
}

dyn_log3({
      HASH: /* Int */3654863,
      VAL: 8
    }, {
      HASH: /* Bool */737456202,
      VAL: true
    }, undefined);

console.log("foo");

console.log({
      foo: 1
    });

function dyn_log4(prim) {
  console.log(prim.VAL);
  
}

console.log({
      foo: 2
    });

function f(x) {
  console.log(x.VAL);
  
}

function ff0(x, p) {
  console.log(Caml_option.option_unwrap(x), p);
  
}

function ff1(x, p) {
  console.log(Caml_option.option_unwrap(Curry._1(x, undefined)), p);
  
}

function test00(param) {
  return {
          a: 1,
          b: 2,
          x: undefined
        };
}

var none_arg;

exports.arg_string = arg_string;
exports.arg_pair = arg_pair;
exports.some_arg = some_arg;
exports.none_arg = none_arg;
exports.dyn_log3 = dyn_log3;
exports.dyn_log4 = dyn_log4;
exports.f = f;
exports.ff0 = ff0;
exports.ff1 = ff1;
exports.test00 = test00;
/*  Not a pure module */
