'use strict';

var Curry = require("../../lib/js/curry.js");

function map_pair(r, param) {
  return /* tuple */[
          Curry._1(r, param[0]),
          Curry._1(r, param[1])
        ];
}

function u(x) {
  return x;
}

map_pair(u, /* tuple */[
      3,
      true
    ]);

var hi = /* array */[
  3,
  2,
  "x"
];

console.log(3);

console.log("x");

exports.hi = hi;
/*  Not a pure module */
