'use strict';

var Curry       = require("../../lib/js/curry.js");
var Js_mapperRt = require("../../lib/js/js_mapperRt.js");

var constantArray = /* array */[
  /* tuple */[
    68,
    "D"
  ],
  /* tuple */[
    67,
    "C"
  ],
  /* tuple */[
    102,
    "x"
  ]
];

function uToJs(polyvar) {
  return Js_mapperRt.search(polyvar, constantArray);
}

function uFromJs(str) {
  return Js_mapperRt.revSearch(3, constantArray, str);
}

function $neg$tilde(f, v) {
  if (v) {
    return Curry._1(f, v[0]);
  } else {
    return "None";
  }
}

var v_000 = $neg$tilde(uToJs, uFromJs("x"));

var v_001 = $neg$tilde(uToJs, uFromJs("D"));

var v_002 = $neg$tilde(uToJs, uFromJs("C"));

var v_003 = $neg$tilde(uToJs, uFromJs("N"));

var v = /* tuple */[
  v_000,
  v_001,
  v_002,
  v_003
];

console.log(uToJs(/* f */102));

console.log(v);

exports.uToJs      = uToJs;
exports.uFromJs    = uFromJs;
exports.$neg$tilde = $neg$tilde;
exports.v          = v;
/* v Not a pure module */
