'use strict';

var Mt = require("./mt.js");
var Js_mapperRt = require("../../lib/js/js_mapperRt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function $$throw(loc, x) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* ThrowAny */7,
                  _0: x
                };
        })
    ],
    tl: suites.contents
  };
  
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

var jsMapperConstantArray = [
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

var jsMapperConstantArray$1 = [
  [
    22125,
    "c0"
  ],
  [
    22126,
    "c1"
  ],
  [
    22127,
    "c2"
  ]
];

function cToJs(param) {
  return Js_mapperRt.binarySearch(3, param, jsMapperConstantArray$1);
}

function cFromJs(param) {
  return Js_mapperRt.revSearchAssert(3, jsMapperConstantArray$1, param);
}

$$throw("File \"ast_mapper_defensive_test.ml\", line 28, characters 16-23", (function (param) {
        aFromJs(3);
        
      }));

$$throw("File \"ast_mapper_defensive_test.ml\", line 29, characters 15-22", (function (param) {
        bFromJs(2);
        
      }));

$$throw("File \"ast_mapper_defensive_test.ml\", line 30, characters 15-22", (function (param) {
        cFromJs(33);
        
      }));

Mt.from_pair_suites("Ast_mapper_defensive_test", suites.contents);

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
