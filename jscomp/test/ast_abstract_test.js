'use strict';

var Js_mapperRt = require("../../lib/js/js_mapperRt.js");

function tToJs(param) {
  return {
          x: param[/* x */0],
          y: param[/* y */1],
          z: param[/* z */2]
        };
}

function tFromJs(param) {
  return /* record */[
          /* x */param.x,
          /* y */param.y,
          /* z */param.z
        ];
}

var v0 = {
  x: /* x */3,
  y: /* y : false */0,
  z: /* z : false */0
};

var v1 = {
  x: /* x */3,
  y: /* y : false */0,
  z: /* z */""
};

var jsMapperConstantArray = /* array */[
  /* tuple */[
    97,
    "a"
  ],
  /* tuple */[
    98,
    "b"
  ],
  /* tuple */[
    99,
    "c"
  ]
];

function xToJs(param) {
  return Js_mapperRt.binSearch(3, param, jsMapperConstantArray);
}

function xFromJs(param) {
  return Js_mapperRt.revSearchAssert(jsMapperConstantArray, param);
}

var x0 = xToJs(/* a */97);

var x1 = xToJs(/* b */98);

var jsMapperConstantArray$1 = /* int array */[
  0,
  3,
  4
];

function aToJs(param) {
  return jsMapperConstantArray$1[param];
}

function aFromJs(param) {
  return Js_mapperRt.fromIntAssert(jsMapperConstantArray$1, param);
}

var a0 = aToJs(/* A */0);

var a1 = aToJs(/* B */1);

function bToJs(param) {
  return param + 0 | 0;
}

function bFromJs(param) {
  return param - 0 | 0;
}

var b0 = 0;

var b1 = 1;

function cToJs(param) {
  return param + 3 | 0;
}

function cFromJs(param) {
  return param - 3 | 0;
}

var c0 = 3;

exports.tToJs   = tToJs;
exports.tFromJs = tFromJs;
exports.v0      = v0;
exports.v1      = v1;
exports.xToJs   = xToJs;
exports.xFromJs = xFromJs;
exports.x0      = x0;
exports.x1      = x1;
exports.aToJs   = aToJs;
exports.aFromJs = aFromJs;
exports.a0      = a0;
exports.a1      = a1;
exports.bToJs   = bToJs;
exports.bFromJs = bFromJs;
exports.b0      = b0;
exports.b1      = b1;
exports.cToJs   = cToJs;
exports.cFromJs = cFromJs;
exports.c0      = c0;
/* x0 Not a pure module */
