// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt         = require("./mt");
var Mt_global  = require("./mt_global");
var Curry      = require("../runtime/curry");
var Printf     = require("../stdlib/printf");
var Int64      = require("../stdlib/int64");
var Caml_array = require("../runtime/caml_array");
var Random     = require("../stdlib/random");

var id = [0];

var suites = [/* [] */0];

function eq(f) {
  return function (param, param$1) {
    return Mt_global.collect_eq(id, suites, f, param, param$1);
  };
}

function neq(f) {
  return function (param, param$1) {
    return Mt_global.collect_neq(id, suites, f, param, param$1);
  };
}

function approx(f) {
  return function (param, param$1) {
    return Mt_global.collect_approx(id, suites, f, param, param$1);
  };
}

Random.self_init(/* () */0);

var param = Random.$$int(1000);

Random.self_init(/* () */0);

var param$1 = Random.$$int(10000);

Mt_global.collect_neq(id, suites, 'File "random_test.ml", line 12, characters 6-13', param$1, param);

Random.init(0);

var v = Caml_array.caml_make_vect(10, /* false */0);

for(var i = 0; i <= 9; ++i){
  v[i] = Random.bool(/* () */0);
}

var param$2 = /* array */[
  /* true */1,
  /* true */1,
  /* true */1,
  /* true */1,
  /* true */1,
  /* false */0,
  /* true */1,
  /* true */1,
  /* true */1,
  /* false */0
];

Mt_global.collect_eq(id, suites, 'File "random_test.ml", line 26, characters 5-12', v, param$2);

var f = Random.int64(Int64.max_int);

var h = Random.int64(/* int64 */[
      0,
      3
    ]);

var vv = Random.bits(/* () */0);

var xx = Random.$$float(3.0);

var xxx = Random.int32(103);

Curry._5(Printf.printf(/* Format */[
          /* Int64 */{
            0: /* Int_d */0,
            1: /* No_padding */0,
            2: /* No_precision */0,
            3: /* Char_literal */{
              0: /* " " */32,
              1: /* Int64 */{
                0: /* Int_d */0,
                1: /* No_padding */0,
                2: /* No_precision */0,
                3: /* Char_literal */{
                  0: /* " " */32,
                  1: /* Int */{
                    0: /* Int_d */0,
                    1: /* No_padding */0,
                    2: /* No_precision */0,
                    3: /* Char_literal */{
                      0: /* " " */32,
                      1: /* Float */{
                        0: /* Float_f */0,
                        1: /* No_padding */0,
                        2: /* No_precision */0,
                        3: /* Char_literal */{
                          0: /* " " */32,
                          1: /* Int32 */{
                            0: /* Int_d */0,
                            1: /* No_padding */0,
                            2: /* No_precision */0,
                            3: /* String_literal */{
                              0: " \n",
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 11
                            },
                            length: 4,
                            tag: 5
                          },
                          length: 2,
                          tag: 12
                        },
                        length: 4,
                        tag: 8
                      },
                      length: 2,
                      tag: 12
                    },
                    length: 4,
                    tag: 4
                  },
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 7
              },
              length: 2,
              tag: 12
            },
            length: 4,
            tag: 7
          },
          "%Ld %Ld %d %f %ld \n"
        ]), f, h, vv, xx, xxx);

Mt.from_pair_suites("random_test.ml", suites[0]);

exports.id     = id;
exports.suites = suites;
exports.eq     = eq;
exports.neq    = neq;
exports.approx = approx;
exports.v      = v;
exports.f      = f;
exports.h      = h;
exports.vv     = vv;
exports.xx     = xx;
exports.xxx    = xxx;
/*  Not a pure module */
