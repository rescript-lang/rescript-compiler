'use strict';

var Js_primitive = require("../../lib/js/js_primitive.js");

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

console.log(/* () */0);

console.log(1, undefined);

console.log(2, "hi");

console.log(3, "hi");

console.log(4, undefined);

var some_arg = /* Some */[/* `Bool */[
    737456202,
    true
  ]];

console.log(5, some_arg ? some_arg[0][1] : undefined);

console.log(6, undefined);

console.log(7, Js_primitive.option_get_unwrap((console.log("trace"), /* None */0)));

function dyn_log3(prim, prim$1, _) {
  console.log(prim[1], prim$1 ? prim$1[0][1] : undefined);
  return /* () */0;
}

dyn_log3(/* `Int */[
      3654863,
      8
    ], /* Some */[/* `Bool */[
        737456202,
        true
      ]], /* () */0);

console.log("foo");

console.log({
      foo: 1
    });

function dyn_log4(prim) {
  console.log(prim[1]);
  return /* () */0;
}

console.log({
      foo: 2
    });

var none_arg = /* None */0;

exports.arg_string = arg_string;
exports.arg_pair = arg_pair;
exports.some_arg = some_arg;
exports.none_arg = none_arg;
exports.dyn_log3 = dyn_log3;
exports.dyn_log4 = dyn_log4;
/*  Not a pure module */
