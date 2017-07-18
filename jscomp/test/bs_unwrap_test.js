'use strict';


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

console.log(1, "hi");

console.log("foo");

console.log({
      foo: 1
    });

exports.arg_string = arg_string;
exports.arg_pair   = arg_pair;
/*  Not a pure module */
