'use strict';

var Js_mapperRt = require("../../lib/js/js_mapperRt.js");

var jsMapperConstantArray = /* array */[
  /* tuple */[
    97,
    "x"
  ],
  /* tuple */[
    98,
    "b"
  ],
  /* tuple */[
    99,
    "c"
  ],
  /* tuple */[
    117,
    "hi"
  ]
];

function tToJs(param) {
  return Js_mapperRt.binarySearch(4, param, jsMapperConstantArray);
}

function tFromJs(param) {
  return Js_mapperRt.revSearch(4, jsMapperConstantArray, param);
}

var v = tToJs;

var u = tFromJs;

exports.tToJs = tToJs;
exports.tFromJs = tFromJs;
exports.v = v;
exports.u = u;
/* No side effect */
