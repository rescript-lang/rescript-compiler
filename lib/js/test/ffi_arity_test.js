// GENERATED CODE BY BUCKLESCRIPT VERSION 0.5.5 , PLEASE EDIT WITH CARE
'use strict';

var Mt         = require("./mt");
var Caml_int32 = require("../caml_int32");
var Block      = require("../block");
var Curry      = require("../curry");

function f(v) {
  if (v % 2) {
    return function (v) {
      return v + v | 0;
    };
  }
  else {
    return function (v) {
      return Caml_int32.imul(v, v);
    };
  }
}

var v = /* int array */[
    1,
    2,
    3
  ].map(function (param, param$1) {
      return Curry._2(f, param, param$1);
    });

var vv = /* int array */[
    1,
    2,
    3
  ].map(function (prim, prim$1) {
      return prim + prim$1 | 0;
    });

var hh = /* array */[
    "1",
    "2",
    "3"
  ].map(function (prim) {
      return parseInt(prim);
    });

function u() {
  return 3;
}

Mt.from_pair_suites("ffi_arity_test.ml", /* :: */[
      /* tuple */[
        'File "ffi_arity_test.ml", line 23, characters 4-11',
        function () {
          return /* Eq */Block.__(0, [
                    v,
                    /* int array */[
                      0,
                      1,
                      4
                    ]
                  ]);
        }
      ],
      /* :: */[
        /* tuple */[
          'File "ffi_arity_test.ml", line 24, characters 4-11',
          function () {
            return /* Eq */Block.__(0, [
                      vv,
                      /* int array */[
                        1,
                        3,
                        5
                      ]
                    ]);
          }
        ],
        /* :: */[
          /* tuple */[
            'File "ffi_arity_test.ml", line 25, characters 4-11',
            function () {
              return /* Eq */Block.__(0, [
                        hh,
                        /* int array */[
                          1,
                          2,
                          3
                        ]
                      ]);
            }
          ],
          /* :: */[
            /* tuple */[
              'File "ffi_arity_test.ml", line 26, characters 4-11',
              function () {
                return /* Eq */Block.__(0, [
                          /* int array */[
                                1,
                                2,
                                3
                              ].map(function (x) {
                                  return function (y) {
                                    return x + y | 0;
                                  };
                                }).map(function (y) {
                                return Caml_int32.imul(Curry._1(y, 0), Curry._1(y, 1));
                              }),
                          /* int array */[
                            2,
                            6,
                            12
                          ]
                        ]);
              }
            ],
            /* :: */[
              /* tuple */[
                'File "ffi_arity_test.ml", line 31, characters 4-11',
                function () {
                  return /* Eq */Block.__(0, [
                            /* int array */[
                                1,
                                2,
                                3
                              ].map(function (param, param$1) {
                                  return Curry._2(function (x) {
                                              var y = Caml_int32.imul(x, x);
                                              return function (i) {
                                                return y + i | 0;
                                              };
                                            }, param, param$1);
                                }),
                            /* int array */[
                              1,
                              5,
                              11
                            ]
                          ]);
                }
              ],
              /* [] */0
            ]
          ]
        ]
      ]
    ]);

exports.f  = f;
exports.v  = v;
exports.vv = vv;
exports.hh = hh;
exports.u  = u;
/* v Not a pure module */
