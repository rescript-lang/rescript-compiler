'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_option = require("../../lib/js/caml_option.js");

console.log(/* tuple */[
      "hello world",
      1
    ]);

console.log(1337);

console.log("hello world");

var arg_string = /* `String */[
  -976970511,
  "hi runtime"
];

console.log(arg_string[1]);

var arg_pair = /* `Pair */[
  892012602,
  /* tuple */[
    "hi",
    1
  ]
];

console.log(arg_pair[1]);

console.log(void 0);

console.log(1, void 0);

console.log(2, "hi");

console.log(3, "hi");

console.log(4, void 0);

var some_arg = /* `Bool */[
  737456202,
  true
];

console.log(5, some_arg !== void 0 ? Caml_option.valFromOption(some_arg)[1] : void 0);

console.log(6, void 0);

console.log(7, Caml_option.option_get_unwrap((console.log("trace"), void 0)));

function dyn_log3(prim, prim$1, prim$2) {
  console.log(prim[1], prim$1 !== void 0 ? Caml_option.valFromOption(prim$1)[1] : void 0);
  
}

dyn_log3(/* `Int */[
      3654863,
      8
    ], /* `Bool */[
      737456202,
      true
    ], void 0);

console.log("foo");

console.log({
      foo: 1
    });

function dyn_log4(prim) {
  console.log(prim[1]);
  
}

console.log({
      foo: 2
    });

function f(x) {
  console.log(x[1]);
  
}

function ff0(x, p) {
  console.log(x !== void 0 ? Caml_option.valFromOption(x)[1] : void 0, p);
  
}

function ff1(x, p) {
  console.log(Caml_option.option_get_unwrap(Curry._1(x, void 0)), p);
  
}

function test00(param) {
  return {
          a: 1,
          b: 2,
          x: void 0
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
