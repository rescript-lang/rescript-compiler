'use strict';

var $$Array = require("../../lib/js/array.js");
var Js_mapperRt = require("../../lib/js/js_mapperRt.js");

var suites = /* record */[/* contents */"[]"];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: x,
                  Arg1: y
                };
        })
    ],
    Arg1: suites[0]
  };
  return /* () */0;
}

var jsMapperConstantArray = /* array */[
  /* tuple */[
    67,
    "C"
  ],
  /* tuple */[
    68,
    "D"
  ],
  /* tuple */[
    102,
    "x"
  ]
];

function uToJs(param) {
  return Js_mapperRt.binarySearch(3, param, jsMapperConstantArray);
}

function uFromJs(param) {
  return Js_mapperRt.revSearch(3, jsMapperConstantArray, param);
}

function eqU(x, y) {
  return x === y;
}

function eqUOpt(x, y) {
  if (x !== undefined) {
    if (y !== undefined) {
      return x === y;
    } else {
      return false;
    }
  } else {
    return y === undefined;
  }
}

var jsMapperConstantArray$1 = /* array */[
  0,
  3,
  4,
  5
];

function vToJs(param) {
  return jsMapperConstantArray$1[param];
}

function vFromJs(param) {
  return Js_mapperRt.fromInt(4, jsMapperConstantArray$1, param);
}

function eqV(x, y) {
  return x === y;
}

function eqVOpt(x, y) {
  if (x !== undefined) {
    if (y !== undefined) {
      return x === y;
    } else {
      return false;
    }
  } else {
    return y === undefined;
  }
}

function s(param) {
  switch (param) {
    case "A0" :
        return "A0";
    case "A1" :
        return "A1";
    case "A2" :
        return "A2";
    case "A3" :
        return "A3";
    
  }
}

eq("File \"ast_js_mapper_poly_test.ml\", line 45, characters 5-12", $$Array.map(vToJs, /* array */[
          "A0",
          "A1",
          "A2",
          "A3"
        ]), /* array */[
      0,
      3,
      4,
      5
    ]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.uToJs = uToJs;
exports.uFromJs = uFromJs;
exports.eqU = eqU;
exports.eqUOpt = eqUOpt;
exports.vToJs = vToJs;
exports.vFromJs = vFromJs;
exports.eqV = eqV;
exports.eqVOpt = eqVOpt;
exports.s = s;
/*  Not a pure module */
