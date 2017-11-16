'use strict';

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

console.log(uToJs(/* f */102));

exports.uToJs = uToJs;
/*  Not a pure module */
