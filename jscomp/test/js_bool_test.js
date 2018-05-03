'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

function f(x) {
  if (x) {
    return true;
  } else {
    return false;
  }
}

function f2(x) {
  if (x) {
    return true;
  } else {
    return false;
  }
}

function f4(x) {
  if (x) {
    return true;
  } else {
    return false;
  }
}

var u = ( 1);

var v = ( true);

var suites_000 = /* tuple */[
  "caml_bool_eq_caml_bool",
  (function () {
      return /* Eq */Block.__(0, [
                u,
                true
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "js_bool_eq_js_bool",
    (function () {
        return /* Eq */Block.__(0, [
                  v,
                  true
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "js_bool_neq_acml_bool",
      (function () {
          return /* Eq */Block.__(0, [
                    true,
                    true === (true)
                  ]);
        })
    ],
    /* [] */0
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

function ff(u) {
  if (u === true) {
    return 1;
  } else {
    return 2;
  }
}

function fi(x, y) {
  return x === y;
}

function fb(x, y) {
  return x === y;
}

function fadd(x, y) {
  return x + y | 0;
}

function ffadd(x, y) {
  return x + y;
}

function ss(x) {
  return "xx" > x;
}

function bb(x) {
  return /* tuple */[
          true > x,
          false,
          true,
          true <= x,
          false,
          false < x,
          false >= x,
          true
        ];
}

var bool_array = /* array */[
  true,
  false
];

Mt.from_pair_suites("js_bool_test.ml", suites);

var f3 = true;

exports.f = f;
exports.f2 = f2;
exports.f4 = f4;
exports.f3 = f3;
exports.u = u;
exports.v = v;
exports.suites = suites;
exports.ff = ff;
exports.fi = fi;
exports.fb = fb;
exports.fadd = fadd;
exports.ffadd = ffadd;
exports.ss = ss;
exports.bb = bb;
exports.bool_array = bool_array;
/* u Not a pure module */
