'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");
var Caml_float = require("../../lib/js/caml_float.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Pervasives = require("../../lib/js/pervasives.js");

var one_float = Caml_int64.mk(0, 1072693248);

var int32_pairs = [
  [
    32,
    4.48415508583941463e-44
  ],
  [
    3,
    4.20389539297445121e-45
  ]
];

function from_pairs(pair) {
  return List.concat($$Array.to_list($$Array.mapi((function (i, param) {
                        var f = param[1];
                        var i32 = param[0];
                        return /* :: */{
                                _0: [
                                  Curry._1(Printf.sprintf(/* Format */{
                                            _0: {
                                              tag: /* String_literal */11,
                                              _0: "int32_float_of_bits ",
                                              _1: {
                                                tag: /* Int */4,
                                                _0: /* Int_d */0,
                                                _1: /* No_padding */0,
                                                _2: /* No_precision */0,
                                                _3: /* End_of_format */0
                                              }
                                            },
                                            _1: "int32_float_of_bits %d"
                                          }), i),
                                  (function (param) {
                                      return {
                                              tag: /* Eq */0,
                                              _0: Caml_float.caml_int32_float_of_bits(i32),
                                              _1: f
                                            };
                                    })
                                ],
                                _1: /* :: */{
                                  _0: [
                                    Curry._1(Printf.sprintf(/* Format */{
                                              _0: {
                                                tag: /* String_literal */11,
                                                _0: "int32_bits_of_float ",
                                                _1: {
                                                  tag: /* Int */4,
                                                  _0: /* Int_d */0,
                                                  _1: /* No_padding */0,
                                                  _2: /* No_precision */0,
                                                  _3: /* End_of_format */0
                                                }
                                              },
                                              _1: "int32_bits_of_float %d"
                                            }), i),
                                    (function (param) {
                                        return {
                                                tag: /* Eq */0,
                                                _0: Caml_float.caml_int32_bits_of_float(f),
                                                _1: i32
                                              };
                                      })
                                  ],
                                  _1: /* [] */0
                                }
                              };
                      }), int32_pairs)));
}

var suites = Pervasives.$at(/* :: */{
      _0: [
        "one",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: Caml_int64.bits_of_float(1.0),
                    _1: one_float
                  };
          })
      ],
      _1: /* :: */{
        _0: [
          "two",
          (function (param) {
              return {
                      tag: /* Eq */0,
                      _0: Caml_int64.float_of_bits(one_float),
                      _1: 1.0
                    };
            })
        ],
        _1: /* [] */0
      }
    }, from_pairs(int32_pairs));

Mt.from_pair_suites("Float_of_bits_test", suites);

exports.one_float = one_float;
exports.int32_pairs = int32_pairs;
exports.from_pairs = from_pairs;
exports.suites = suites;
/* suites Not a pure module */
