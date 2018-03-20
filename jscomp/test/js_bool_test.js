'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");

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

function boolNot(x) {
  return !x;
}

function boolAnd(f, g, x) {
  if (Curry._1(f, x)) {
    return Curry._1(g, x);
  } else {
    return false;
  }
}

function boolOr(f, g, x) {
  if (Curry._1(f, x)) {
    return true;
  } else {
    return Curry._1(g, x);
  }
}

function polyEq(x, y) {
  return Caml_obj.caml_equal(y, x);
}

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
      "js_bool_eq_ocaml_bool",
      (function () {
          return /* Eq */Block.__(0, [
                    true,
                    true === (true)
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "js_bool_is_ocaml_bool",
        (function () {
            return /* Eq */Block.__(0, [
                      true,
                      true === true
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "testBoolNot",
          (function () {
              return /* Eq */Block.__(0, [
                        true,
                        !true === (false)
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "testBoolAnd",
            (function () {
                return /* Eq */Block.__(0, [
                          true,
                          boolAnd((function () {
                                  return false;
                                }), (function () {
                                  return false;
                                }), /* () */0) === (false)
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "testBoolOr",
              (function () {
                  return /* Eq */Block.__(0, [
                            true,
                            boolOr((function () {
                                    return true;
                                  }), (function () {
                                    return true;
                                  }), /* () */0) === (true)
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "testPolyEq",
                (function () {
                    return /* Eq */Block.__(0, [
                              true,
                              Caml_obj.caml_equal(3, 3) === (true)
                            ]);
                  })
              ],
              /* [] */0
            ]
          ]
        ]
      ]
    ]
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

Mt.from_pair_suites("js_bool_test.ml", suites);

var f3 = true;

exports.f = f;
exports.f2 = f2;
exports.f4 = f4;
exports.f3 = f3;
exports.u = u;
exports.v = v;
exports.boolNot = boolNot;
exports.boolAnd = boolAnd;
exports.boolOr = boolOr;
exports.polyEq = polyEq;
exports.suites = suites;
exports.ff = ff;
/* u Not a pure module */
