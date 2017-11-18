'use strict';

var $$Array     = require("../../lib/js/array.js");
var Curry       = require("../../lib/js/curry.js");
var Js_mapperRt = require("../../lib/js/js_mapperRt.js");

var jsMapperConstantArray = /* array */[
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

function uToJs(param) {
  return Js_mapperRt.search(param, jsMapperConstantArray);
}

function uFromJs(str) {
  return Js_mapperRt.revSearch(3, jsMapperConstantArray, str);
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

var jsMapperConstantArray$1 = /* int array */[
  0,
  3,
  4,
  5
];

function vToJs(variant) {
  return jsMapperConstantArray$1[variant];
}

function vFromJs($$int) {
  return Js_mapperRt.fromInt(4, jsMapperConstantArray$1, $$int);
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

console.log(vToJs(/* A3 */3));

console.log($$Array.map((function (x) {
            var match = vFromJs(x);
            if (match) {
              return s(match[0]);
            } else {
              return "None";
            }
          }), /* array */[
          0,
          1,
          2,
          3,
          4,
          5
        ]));

function v1ToJs(variant) {
  return variant + 0 | 0;
}

function v1FromJs($$int) {
  if ($$int <= 5 && 0 <= $$int) {
    return /* Some */[$$int];
  } else {
    return /* None */0;
  }
}

function v2ToJs(variant) {
  return variant + 2 | 0;
}

function v2FromJs($$int) {
  if ($$int <= 7 && 2 <= $$int) {
    return /* Some */[$$int];
  } else {
    return /* None */0;
  }
}

console.log($$Array.map(v2ToJs, /* array */[
          /* C0 */0,
          /* C1 */1,
          /* C2 */2,
          /* C3 */3,
          /* C4 */4,
          /* C5 */5
        ]));

var xs = $$Array.map(v2ToJs, /* array */[
      /* C0 */0,
      /* C1 */1,
      /* C2 */2,
      /* C3 */3,
      /* C4 */4,
      /* C5 */5
    ]);

console.log($$Array.map(v2FromJs, $$Array.map((function (prim) {
                return prim + 1 | 0;
              }), xs)));

exports.uToJs      = uToJs;
exports.uFromJs    = uFromJs;
exports.$neg$tilde = $neg$tilde;
exports.v          = v;
exports.vToJs      = vToJs;
exports.vFromJs    = vFromJs;
exports.s          = s;
exports.v1ToJs     = v1ToJs;
exports.v1FromJs   = v1FromJs;
exports.v2ToJs     = v2ToJs;
exports.v2FromJs   = v2FromJs;
exports.xs         = xs;
/* v Not a pure module */
