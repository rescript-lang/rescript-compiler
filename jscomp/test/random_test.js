'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Int64 = require("../../lib/js/int64.js");
var Printf = require("../../lib/js/printf.js");
var Random = require("../../lib/js/random.js");
var Mt_global = require("./mt_global.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");

var id = {
  contents: 0
};

var suites = {
  contents: /* [] */0
};

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

Mt_global.collect_neq(id, suites, "File \"random_test.ml\", line 12, characters 6-13", (Random.self_init(undefined), Random.$$int(10000)), (Random.self_init(undefined), Random.$$int(1000)));

Random.init(0);

var v = Caml_array.caml_make_vect(10, false);

for(var i = 0; i <= 9; ++i){
  Caml_array.set(v, i, Random.bool(undefined));
}

Mt_global.collect_eq(id, suites, "File \"random_test.ml\", line 26, characters 5-12", v, [
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
    ]);

var f = Random.int64(Int64.max_int);

var h = Random.int64(/* @__PURE__ */Caml_int64.mk(3, 0));

var vv = Random.bits(undefined);

var xx = Random.$$float(3.0);

var xxx = Random.int32(103);

Curry._5(Printf.printf(/* Format */{
          _0: {
            TAG: /* Int64 */7,
            _0: /* Int_d */0,
            _1: /* No_padding */0,
            _2: /* No_precision */0,
            _3: {
              TAG: /* Char_literal */12,
              _0: /* ' ' */32,
              _1: {
                TAG: /* Int64 */7,
                _0: /* Int_d */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* ' ' */32,
                  _1: {
                    TAG: /* Int */4,
                    _0: /* Int_d */0,
                    _1: /* No_padding */0,
                    _2: /* No_precision */0,
                    _3: {
                      TAG: /* Char_literal */12,
                      _0: /* ' ' */32,
                      _1: {
                        TAG: /* Float */8,
                        _0: /* Float_f */0,
                        _1: /* No_padding */0,
                        _2: /* No_precision */0,
                        _3: {
                          TAG: /* Char_literal */12,
                          _0: /* ' ' */32,
                          _1: {
                            TAG: /* Int32 */5,
                            _0: /* Int_d */0,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: {
                              TAG: /* String_literal */11,
                              _0: " \n",
                              _1: /* End_of_format */0
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
          _1: "%Ld %Ld %d %f %ld \n"
        }), f, h, vv, xx, xxx);

Mt.from_pair_suites("Random_test", suites.contents);

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
