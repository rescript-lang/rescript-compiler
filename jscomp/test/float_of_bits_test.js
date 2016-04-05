// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_int64 = require("../runtime/caml_int64");
var Caml_float = require("../runtime/caml_float");
var Pervasives = require("../stdlib/pervasives");
var Mt         = require("./mt");
var Printf     = require("../stdlib/printf");
var $$Array    = require("../stdlib/array");
var Caml_curry = require("../runtime/caml_curry");
var List       = require("../stdlib/list");

var one_float_001 = (0 >>> 0);

var one_float = /* int64 */[
  1072693248,
  one_float_001
];

var int32_pairs = /* array */[
  /* tuple */[
    32,
    4.48415508583941463e-44
  ],
  /* tuple */[
    3,
    4.20389539297445121e-45
  ]
];

function from_pairs() {
  return List.concat($$Array.to_list($$Array.mapi(function (i, param) {
                      var f = param[1];
                      var i32 = param[0];
                      return /* :: */[
                              /* tuple */[
                                Caml_curry.app1(Printf.sprintf(/* Format */{
                                          0: /* String_literal */{
                                            0: "int32_float_of_bits ",
                                            1: /* Int */{
                                              0: /* Int_d */0,
                                              1: /* No_padding */0,
                                              2: /* No_precision */0,
                                              3: /* End_of_format */0,
                                              length: 4,
                                              tag: 4
                                            },
                                            length: 2,
                                            tag: 11
                                          },
                                          1: "int32_float_of_bits %d",
                                          length: 2,
                                          tag: 0
                                        }), i),
                                function () {
                                  return /* Eq */{
                                          0: Caml_float.caml_int32_float_of_bits(i32),
                                          1: f,
                                          length: 2,
                                          tag: 0
                                        };
                                }
                              ],
                              /* :: */[
                                /* tuple */[
                                  Caml_curry.app1(Printf.sprintf(/* Format */{
                                            0: /* String_literal */{
                                              0: "int32_bits_of_float ",
                                              1: /* Int */{
                                                0: /* Int_d */0,
                                                1: /* No_padding */0,
                                                2: /* No_precision */0,
                                                3: /* End_of_format */0,
                                                length: 4,
                                                tag: 4
                                              },
                                              length: 2,
                                              tag: 11
                                            },
                                            1: "int32_bits_of_float %d",
                                            length: 2,
                                            tag: 0
                                          }), i),
                                  function () {
                                    return /* Eq */{
                                            0: Caml_float.caml_int32_bits_of_float(f),
                                            1: i32,
                                            length: 2,
                                            tag: 0
                                          };
                                  }
                                ],
                                /* [] */0
                              ]
                            ];
                    }, int32_pairs)));
}

var suites = Pervasives.$at(/* :: */[
      /* tuple */[
        "one",
        function () {
          return /* Eq */{
                  0: Caml_int64.bits_of_float(1.0),
                  1: one_float,
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* :: */[
        /* tuple */[
          "two",
          function () {
            return /* Eq */{
                    0: Caml_int64.float_of_bits(one_float),
                    1: 1.0,
                    length: 2,
                    tag: 0
                  };
          }
        ],
        /* [] */0
      ]
    ], from_pairs(int32_pairs));

Mt.from_pair_suites("float_of_bits_test.ml", suites);

exports.one_float   = one_float;
exports.int32_pairs = int32_pairs;
exports.from_pairs  = from_pairs;
exports.suites      = suites;
/* suites Not a pure module */
