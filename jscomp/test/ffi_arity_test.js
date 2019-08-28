'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Pervasives = require("../../lib/js/pervasives.js");

function f(v) {
  if (v % 2 === 0) {
    return (function (v) {
        return Caml_int32.imul(v, v);
      });
  } else {
    return (function (v) {
        return v + v | 0;
      });
  }
}

var v = /* array */[
    1,
    2,
    3
  ].map((function (param, param$1) {
        return f(param)(param$1);
      }));

var vv = /* array */[
    1,
    2,
    3
  ].map((function (prim, prim$1) {
        return prim + prim$1 | 0;
      }));

var hh = /* array */[
    "1",
    "2",
    "3"
  ].map((function (prim) {
        return parseInt(prim);
      }));

function u() {
  return 3;
}

var vvv = /* record */{
  contents: 0
};

function fff(param) {
  console.log("x");
  console.log("x");
  return Pervasives.incr(vvv);
}

function g() {
  return fff(/* () */0);
}

function abc(x, y, z) {
  console.log("xx");
  console.log("yy");
  return (x + y | 0) + z | 0;
}

var abc_u = abc;

g();

Mt.from_pair_suites("Ffi_arity_test", /* :: */[
      /* tuple */[
        "File \"ffi_arity_test.ml\", line 45, characters 4-11",
        (function (param) {
            return /* Eq */Block.__(0, [
                      v,
                      /* array */[
                        0,
                        1,
                        4
                      ]
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "File \"ffi_arity_test.ml\", line 46, characters 4-11",
          (function (param) {
              return /* Eq */Block.__(0, [
                        vv,
                        /* array */[
                          1,
                          3,
                          5
                        ]
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "File \"ffi_arity_test.ml\", line 47, characters 4-11",
            (function (param) {
                return /* Eq */Block.__(0, [
                          hh,
                          /* array */[
                            1,
                            2,
                            3
                          ]
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "File \"ffi_arity_test.ml\", line 48, characters 4-11",
              (function (param) {
                  return /* Eq */Block.__(0, [
                            /* array */[
                                  1,
                                  2,
                                  3
                                ].map((function (x) {
                                      return (function (y) {
                                          return x + y | 0;
                                        });
                                    })).map((function (y) {
                                    return Caml_int32.imul(Curry._1(y, 0), Curry._1(y, 1));
                                  })),
                            /* array */[
                              2,
                              6,
                              12
                            ]
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "File \"ffi_arity_test.ml\", line 53, characters 4-11",
                (function (param) {
                    return /* Eq */Block.__(0, [
                              /* array */[
                                  1,
                                  2,
                                  3
                                ].map((function (x, param) {
                                      var y = Caml_int32.imul(x, x);
                                      return (function (i) {
                                                  return y + i | 0;
                                                })(param);
                                    })),
                              /* array */[
                                1,
                                5,
                                11
                              ]
                            ]);
                  })
              ],
              /* [] */0
            ]
          ]
        ]
      ]
    ]);

function bar(fn) {
  return Curry._1(fn, /* () */0);
}

(Curry._1(function (){console.log("forgiving arity")}, /* () */0));

exports.f = f;
exports.v = v;
exports.vv = vv;
exports.hh = hh;
exports.u = u;
exports.vvv = vvv;
exports.fff = fff;
exports.g = g;
exports.abc = abc;
exports.abc_u = abc_u;
exports.bar = bar;
/* v Not a pure module */
