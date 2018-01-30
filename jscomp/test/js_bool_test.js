'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

function f(x) {
  if (x) {
    return /* true */1;
  } else {
    return /* false */0;
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

var f3 = /* true */1;

var u = ( 1);

var v = ( true);

var suites_000 = /* tuple */[
  "caml_bool_eq_caml_bool",
  (function () {
      return /* Eq */Block.__(0, [
                u,
                /* true */1
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
                    /* false */0,
                    +(/* true */1 === (true))
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

Mt.from_pair_suites("js_bool_test.ml", suites);

exports.f = f;
exports.f2 = f2;
exports.f4 = f4;
exports.f3 = f3;
exports.u = u;
exports.v = v;
exports.suites = suites;
/* u Not a pure module */
