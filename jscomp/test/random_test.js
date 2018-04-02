'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Int64 = require("../../lib/js/int64.js");
var Printf = require("../../lib/js/printf.js");
var Random = require("../../lib/js/random.js");
var Mt_global = require("./mt_global.js");
var Caml_array = require("../../lib/js/caml_array.js");

var id = [0];

var suites = [/* [] */0];

function eq(f) {
  return (function (param, param$1) {
      return Mt_global.collect_eq(id, suites, f, param, param$1);
    });
}

function neq(f) {
  return (function (param, param$1) {
      return Mt_global.collect_neq(id, suites, f, param, param$1);
    });
}

function approx(f) {
  return (function (param, param$1) {
      return Mt_global.collect_approx(id, suites, f, param, param$1);
    });
}

Random.self_init(/* () */0);

var param = Random.$$int(1000);

Random.self_init(/* () */0);

var param$1 = Random.$$int(10000);

Mt_global.collect_neq(id, suites, "File \"random_test.ml\", line 12, characters 6-13", param$1, param);

Random.init(0);

var v = Caml_array.caml_make_vect(10, false);

for(var i = 0; i <= 9; ++i){
  Caml_array.caml_array_set(v, i, Random.bool(/* () */0));
}

var param$2 = /* array */[
  true,
  true,
  true,
  true,
  true,
  false,
  true,
  true,
  true,
  false
];

Mt_global.collect_eq(id, suites, "File \"random_test.ml\", line 26, characters 5-12", v, param$2);

var f = Random.int64(Int64.max_int);

var h = Random.int64(/* int64 */[
      /* hi */0,
      /* lo */3
    ]);

var vv = Random.bits(/* () */0);

var xx = Random.$$float(3.0);

var xxx = Random.int32(103);

Curry._5(Printf.printf(/* Format */[
          /* Int64 */Block.__(7, [
              /* Int_d */0,
              /* No_padding */0,
              /* No_precision */0,
              /* Char_literal */Block.__(12, [
                  /* " " */32,
                  /* Int64 */Block.__(7, [
                      /* Int_d */0,
                      /* No_padding */0,
                      /* No_precision */0,
                      /* Char_literal */Block.__(12, [
                          /* " " */32,
                          /* Int */Block.__(4, [
                              /* Int_d */0,
                              /* No_padding */0,
                              /* No_precision */0,
                              /* Char_literal */Block.__(12, [
                                  /* " " */32,
                                  /* Float */Block.__(8, [
                                      /* Float_f */0,
                                      /* No_padding */0,
                                      /* No_precision */0,
                                      /* Char_literal */Block.__(12, [
                                          /* " " */32,
                                          /* Int32 */Block.__(5, [
                                              /* Int_d */0,
                                              /* No_padding */0,
                                              /* No_precision */0,
                                              /* String_literal */Block.__(11, [
                                                  " \n",
                                                  /* End_of_format */0
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ])
                ])
            ]),
          "%Ld %Ld %d %f %ld \n"
        ]), f, h, vv, xx, xxx);

Mt.from_pair_suites("random_test.ml", suites[0]);

exports.id = id;
exports.suites = suites;
exports.eq = eq;
exports.neq = neq;
exports.approx = approx;
exports.v = v;
exports.f = f;
exports.h = h;
exports.vv = vv;
exports.xx = xx;
exports.xxx = xxx;
/*  Not a pure module */
