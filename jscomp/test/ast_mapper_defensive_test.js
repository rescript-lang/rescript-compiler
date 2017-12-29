'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_mapperRt = require("../../lib/js/js_mapperRt.js");

var suites = [/* [] */0];

var test_id = [0];

function $$throw(loc, x) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* ThrowAny */Block.__(7, [x]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

function aToJs(param) {
  return param + 0 | 0;
}

function aFromJs(param) {
  if (!(param <= 2 && 0 <= param)) {
    throw new Error("ASSERT FAILURE");
  }
  return param - 0 | 0;
}

var jsMapperConstantArray = /* int array */[
  0,
  3,
  4
];

function bToJs(param) {
  return jsMapperConstantArray[param];
}

function bFromJs(param) {
  return Js_mapperRt.fromIntAssert(3, jsMapperConstantArray, param);
}

var jsMapperConstantArray$1 = /* array */[
  /* tuple */[
    22125,
    "c0"
  ],
  /* tuple */[
    22126,
    "c1"
  ],
  /* tuple */[
    22127,
    "c2"
  ]
];

function cToJs(param) {
  return Js_mapperRt.binSearch(3, param, jsMapperConstantArray$1);
}

function cFromJs(param) {
  return Js_mapperRt.revSearchAssert(3, jsMapperConstantArray$1, param);
}

$$throw("File \"ast_mapper_defensive_test.ml\", line 28, characters 16-23", (function () {
        aFromJs(3);
        return /* () */0;
      }));

$$throw("File \"ast_mapper_defensive_test.ml\", line 29, characters 15-22", (function () {
        bFromJs(2);
        return /* () */0;
      }));

$$throw("File \"ast_mapper_defensive_test.ml\", line 30, characters 15-22", (function () {
        cFromJs(33);
        return /* () */0;
      }));

Mt.from_pair_suites("ast_mapper_defensive_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.$$throw = $$throw;
exports.aToJs = aToJs;
exports.aFromJs = aFromJs;
exports.bToJs = bToJs;
exports.bFromJs = bFromJs;
exports.cToJs = cToJs;
exports.cFromJs = cFromJs;
/*  Not a pure module */
