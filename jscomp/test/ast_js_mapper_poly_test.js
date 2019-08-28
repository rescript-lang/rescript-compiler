'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Js_mapperRt = require("../../lib/js/js_mapperRt.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  Pervasives.incr(test_id);
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
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

eq("File \"ast_js_mapper_poly_test.ml\", line 25, characters 5-12", eqUOpt(uFromJs("x"), /* f */102), true);

eq("File \"ast_js_mapper_poly_test.ml\", line 26, characters 5-12", eqUOpt(uFromJs("D"), /* D */68), true);

eq("File \"ast_js_mapper_poly_test.ml\", line 27, characters 5-12", eqUOpt(uFromJs("C"), /* C */67), true);

eq("File \"ast_js_mapper_poly_test.ml\", line 28, characters 5-12", eqUOpt(uFromJs("f"), undefined), true);

eq("File \"ast_js_mapper_poly_test.ml\", line 29, characters 5-12", $$Array.map(uToJs, /* array */[
          /* D */68,
          /* C */67,
          /* f */102
        ]), /* array */[
      "D",
      "C",
      "x"
    ]);

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
    case /* A0 */0 :
        return "A0";
    case /* A1 */1 :
        return "A1";
    case /* A2 */2 :
        return "A2";
    case /* A3 */3 :
        return "A3";
    
  }
}

eq("File \"ast_js_mapper_poly_test.ml\", line 54, characters 5-12", $$Array.map(vToJs, /* array */[
          /* A0 */0,
          /* A1 */1,
          /* A2 */2,
          /* A3 */3
        ]), /* array */[
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
      /* A0 */0,
      undefined,
      undefined,
      /* A1 */1,
      /* A2 */2,
      /* A3 */3,
      undefined
    ]);

function v1ToJs(param) {
  return param + 0 | 0;
}

function v1FromJs(param) {
  if (param <= 5 && 0 <= param) {
    return param - 0 | 0;
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
      undefined,
      /* B0 */0,
      /* B1 */1,
      /* B2 */2,
      /* B3 */3,
      /* B4 */4,
      /* B5 */5,
      undefined
    ]);

function v2ToJs(param) {
  return param + 2 | 0;
}

function v2FromJs(param) {
  if (param <= 7 && 2 <= param) {
    return param - 2 | 0;
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
              undefined,
              undefined
            ], $$Array.map((function (x) {
                    return x;
                  }), /* array */[
                  /* C0 */0,
                  /* C1 */1,
                  /* C2 */2,
                  /* C3 */3,
                  /* C4 */4,
                  /* C5 */5
                ])), /* array */[undefined]));

Mt.from_pair_suites("Ast_js_mapper_poly_test", suites.contents);

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
