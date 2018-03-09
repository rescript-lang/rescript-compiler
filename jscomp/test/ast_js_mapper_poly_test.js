'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Js_mapperRt = require("../../lib/js/js_mapperRt.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
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
  return +(x === y);
}

function eqUOpt(x, y) {
  if (x) {
    if (y) {
      return +(x[0] === y[0]);
    } else {
      return /* false */0;
    }
  } else if (y) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

eq("File \"ast_js_mapper_poly_test.ml\", line 25, characters 5-12", eqUOpt(uFromJs("x"), /* Some */[/* f */102]), /* true */1);

eq("File \"ast_js_mapper_poly_test.ml\", line 26, characters 5-12", eqUOpt(uFromJs("D"), /* Some */[/* D */68]), /* true */1);

eq("File \"ast_js_mapper_poly_test.ml\", line 27, characters 5-12", eqUOpt(uFromJs("C"), /* Some */[/* C */67]), /* true */1);

eq("File \"ast_js_mapper_poly_test.ml\", line 28, characters 5-12", eqUOpt(uFromJs("f"), /* None */0), /* true */1);

eq("File \"ast_js_mapper_poly_test.ml\", line 29, characters 5-12", $$Array.map(uToJs, /* array */[
          /* D */68,
          /* C */67,
          /* f */102
        ]), /* array */[
      "D",
      "C",
      "x"
    ]);

var jsMapperConstantArray$1 = /* int array */[
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
  return +(x === y);
}

function eqVOpt(x, y) {
  if (x) {
    if (y) {
      return +(x[0] === y[0]);
    } else {
      return /* false */0;
    }
  } else if (y) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

function s(param) {
  switch (param) {
    case 0 : 
        return "A0";
    case 1 : 
        return "A1";
    case 2 : 
        return "A2";
    case 3 : 
        return "A3";
    
  }
}

eq("File \"ast_js_mapper_poly_test.ml\", line 54, characters 5-12", $$Array.map(vToJs, /* int array */[
          /* A0 */0,
          /* A1 */1,
          /* A2 */2,
          /* A3 */3
        ]), /* int array */[
      0,
      3,
      4,
      5
    ]);

eq("File \"ast_js_mapper_poly_test.ml\", line 55, characters 5-12", $$Array.map(vFromJs, /* array */[
          0,
          1,
          2,
          3,
          4,
          5,
          6
        ]), /* array */[
      /* Some */[/* A0 */0],
      /* None */0,
      /* None */0,
      /* Some */[/* A1 */1],
      /* Some */[/* A2 */2],
      /* Some */[/* A3 */3],
      /* None */0
    ]);

function v1ToJs(param) {
  return param + 0 | 0;
}

function v1FromJs(param) {
  if (param <= 5 && 0 <= param) {
    return /* Some */[param - 0 | 0];
  } else {
    return /* None */0;
  }
}

eq("File \"ast_js_mapper_poly_test.ml\", line 68, characters 5-12", $$Array.map(v1ToJs, /* array */[
          /* B0 */0,
          /* B1 */1,
          /* B2 */2,
          /* B3 */3,
          /* B4 */4,
          /* B5 */5
        ]), /* array */[
      0,
      1,
      2,
      3,
      4,
      5
    ]);

eq("File \"ast_js_mapper_poly_test.ml\", line 69, characters 5-12", $$Array.map(v1FromJs, /* array */[
          -1,
          0,
          1,
          2,
          3,
          4,
          5,
          6
        ]), /* array */[
      /* None */0,
      /* Some */[/* B0 */0],
      /* Some */[/* B1 */1],
      /* Some */[/* B2 */2],
      /* Some */[/* B3 */3],
      /* Some */[/* B4 */4],
      /* Some */[/* B5 */5],
      /* None */0
    ]);

function v2ToJs(param) {
  return param + 2 | 0;
}

function v2FromJs(param) {
  if (param <= 7 && 2 <= param) {
    return /* Some */[param - 2 | 0];
  } else {
    return /* None */0;
  }
}

eq("File \"ast_js_mapper_poly_test.ml\", line 86, characters 5-12", $$Array.map(v2ToJs, /* array */[
          /* C0 */0,
          /* C1 */1,
          /* C2 */2,
          /* C3 */3,
          /* C4 */4,
          /* C5 */5
        ]), /* array */[
      2,
      3,
      4,
      5,
      6,
      7
    ]);

eq("File \"ast_js_mapper_poly_test.ml\", line 89, characters 5-12", $$Array.map(v2FromJs, /* array */[
          0,
          1,
          2,
          3,
          4,
          5,
          6,
          7,
          8
        ]), $$Array.append($$Array.append(/* array */[
              /* None */0,
              /* None */0
            ], $$Array.map((function (x) {
                    return /* Some */[x];
                  }), /* array */[
                  /* C0 */0,
                  /* C1 */1,
                  /* C2 */2,
                  /* C3 */3,
                  /* C4 */4,
                  /* C5 */5
                ])), /* array */[/* None */0]));

Mt.from_pair_suites("ast_js_mapper_poly_test.ml", suites[0]);

var $plus$great = $$Array.append;

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
exports.v1ToJs = v1ToJs;
exports.v1FromJs = v1FromJs;
exports.v2ToJs = v2ToJs;
exports.v2FromJs = v2FromJs;
exports.$plus$great = $plus$great;
/*  Not a pure module */
