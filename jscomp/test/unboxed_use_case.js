'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");

function map_pair(r, param) {
  return [
          Curry._1(r, param[0]),
          Curry._1(r, param[1])
        ];
}

function u(x) {
  return x;
}

map_pair(u, [
      3,
      true
    ]);

var hi = [
  3,
  2,
  "x"
];

console.log(3);

console.log("x");

var v0 = {};

Caml_obj.update_dummy(v0, {
      NAME: "A",
      VAL: v0
    });

var v1 = {
  NAME: "A",
  VAL: "B"
};

exports.hi = hi;
exports.v0 = v0;
exports.v1 = v1;
/*  Not a pure module */
