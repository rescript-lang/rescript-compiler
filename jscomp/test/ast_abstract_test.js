'use strict';

var Mt          = require("./mt.js");
var Block       = require("../../lib/js/block.js");
var Js_mapperRt = require("../../lib/js/js_mapperRt.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

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
  return Js_mapperRt.revSearchAssert(3, jsMapperConstantArray, param);
}

function idx(v) {
  return eq("File \"ast_abstract_test.ml\", line 32, characters 17-24", xFromJs(xToJs(v)), v);
}

var x0 = xToJs(/* a */97);

var x1 = xToJs(/* b */98);

idx(/* a */97);

idx(/* b */98);

idx(/* c */99);

var jsMapperConstantArray$1 = /* int array */[
  0,
  3,
  4
];

function aToJs(param) {
  return jsMapperConstantArray$1[param];
}

function aFromJs(param) {
  return Js_mapperRt.fromIntAssert(3, jsMapperConstantArray$1, param);
}

function id(x) {
  return eq("File \"ast_abstract_test.ml\", line 49, characters 8-15", aFromJs(aToJs(x)), x);
}

var a0 = aToJs(/* A */0);

var a1 = aToJs(/* B */1);

id(/* A */0);

id(/* B */1);

id(/* C */2);

function bToJs(param) {
  return param + 0 | 0;
}

function bFromJs(param) {
  if (!(param <= 3 && 0 <= param)) {
    throw new Error("ASSERT FAILURE");
  }
  return param - 0 | 0;
}

var b0 = 0;

var b1 = 1;

function idb(v) {
  return eq("File \"ast_abstract_test.ml\", line 71, characters 5-12", bFromJs(v + 0 | 0), v);
}

idb(/* D0 */0);

idb(/* D1 */1);

idb(/* D2 */2);

idb(/* D3 */3);

function cToJs(param) {
  return param + 3 | 0;
}

function cFromJs(param) {
  if (!(param <= 6 && 3 <= param)) {
    throw new Error("ASSERT FAILURE");
  }
  return param - 3 | 0;
}

var c0 = 3;

function idc(v) {
  return eq("File \"ast_abstract_test.ml\", line 83, characters 15-22", cFromJs(v + 3 | 0), v);
}

idc(/* D0 */0);

idc(/* D1 */1);

idc(/* D2 */2);

idc(/* D3 */3);

function hToJs(param) {
  return param + 0 | 0;
}

function hFromJs(param) {
  if (!(param <= 1 && 0 <= param)) {
    throw new Error("ASSERT FAILURE");
  }
  return param - 0 | 0;
}

function zToJs(param) {
  return param + 0 | 0;
}

function zFromJs(param) {
  if (param <= 2 && 0 <= param) {
    return /* Some */[param - 0 | 0];
  } else {
    return /* None */0;
  }
}

Mt.from_pair_suites("ast_abstract_test.ml", suites[0]);

var jsMapperEraseType = /* JsMapperEraseType */0;

var b = /* B */1;

var zXx = /* ZXx */2;

exports.suites            = suites;
exports.test_id           = test_id;
exports.eq                = eq;
exports.tToJs             = tToJs;
exports.tFromJs           = tFromJs;
exports.v0                = v0;
exports.v1                = v1;
exports.xToJs             = xToJs;
exports.xFromJs           = xFromJs;
exports.idx               = idx;
exports.x0                = x0;
exports.x1                = x1;
exports.aToJs             = aToJs;
exports.aFromJs           = aFromJs;
exports.id                = id;
exports.a0                = a0;
exports.a1                = a1;
exports.bToJs             = bToJs;
exports.bFromJs           = bFromJs;
exports.b0                = b0;
exports.b1                = b1;
exports.idb               = idb;
exports.cToJs             = cToJs;
exports.cFromJs           = cFromJs;
exports.c0                = c0;
exports.idc               = idc;
exports.jsMapperEraseType = jsMapperEraseType;
exports.b                 = b;
exports.hToJs             = hToJs;
exports.hFromJs           = hFromJs;
exports.zXx               = zXx;
exports.zToJs             = zToJs;
exports.zFromJs           = zFromJs;
/* x0 Not a pure module */
