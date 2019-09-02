'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Int64 = require("../../lib/js/int64.js");
var Printf = require("../../lib/js/printf.js");
var Random = require("../../lib/js/random.js");
var Mt_global = require("./mt_global.js");
var Caml_array = require("../../lib/js/caml_array.js");

var id = /* record */[/* contents */0];

var suites = /* record */[/* contents */"[]"];

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

Curry._5(Printf.printf(/* constructor */{
          tag: "Format",
          Arg0: /* constructor */{
            tag: "Int64",
            Arg0: "Int_d",
            Arg1: "No_padding",
            Arg2: "No_precision",
            Arg3: /* constructor */{
              tag: "Char_literal",
              Arg0: /* " " */32,
              Arg1: /* constructor */{
                tag: "Int64",
                Arg0: "Int_d",
                Arg1: "No_padding",
                Arg2: "No_precision",
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* " " */32,
                  Arg1: /* constructor */{
                    tag: "Int",
                    Arg0: "Int_d",
                    Arg1: "No_padding",
                    Arg2: "No_precision",
                    Arg3: /* constructor */{
                      tag: "Char_literal",
                      Arg0: /* " " */32,
                      Arg1: /* constructor */{
                        tag: "Float",
                        Arg0: "Float_f",
                        Arg1: "No_padding",
                        Arg2: "No_precision",
                        Arg3: /* constructor */{
                          tag: "Char_literal",
                          Arg0: /* " " */32,
                          Arg1: /* constructor */{
                            tag: "Int32",
                            Arg0: "Int_d",
                            Arg1: "No_padding",
                            Arg2: "No_precision",
                            Arg3: /* constructor */{
                              tag: "String_literal",
                              Arg0: " \n",
                              Arg1: "End_of_format"
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          },
          Arg1: "%Ld %Ld %d %f %ld \n"
        }), f, h, vv, xx, xxx);

Mt.from_pair_suites("Random_test", suites[0]);

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
