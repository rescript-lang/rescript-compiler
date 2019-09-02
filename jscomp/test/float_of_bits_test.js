'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");
var Caml_float = require("../../lib/js/caml_float.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Pervasives = require("../../lib/js/pervasives.js");

var one_float = /* int64 */[
  /* hi */1072693248,
  /* lo */0
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

function from_pairs(pair) {
  return List.concat($$Array.to_list($$Array.mapi((function (i, param) {
                        var f = param[1];
                        var i32 = param[0];
                        return /* constructor */{
                                tag: "::",
                                Arg0: /* tuple */[
                                  Curry._1(Printf.sprintf(/* constructor */{
                                            tag: "Format",
                                            Arg0: /* constructor */{
                                              tag: "String_literal",
                                              Arg0: "int32_float_of_bits ",
                                              Arg1: /* constructor */{
                                                tag: "Int",
                                                Arg0: "Int_d",
                                                Arg1: "No_padding",
                                                Arg2: "No_precision",
                                                Arg3: "End_of_format"
                                              }
                                            },
                                            Arg1: "int32_float_of_bits %d"
                                          }), i),
                                  (function (param) {
                                      return /* constructor */{
                                              tag: "Eq",
                                              Arg0: Caml_float.caml_int32_float_of_bits(i32),
                                              Arg1: f
                                            };
                                    })
                                ],
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: /* tuple */[
                                    Curry._1(Printf.sprintf(/* constructor */{
                                              tag: "Format",
                                              Arg0: /* constructor */{
                                                tag: "String_literal",
                                                Arg0: "int32_bits_of_float ",
                                                Arg1: /* constructor */{
                                                  tag: "Int",
                                                  Arg0: "Int_d",
                                                  Arg1: "No_padding",
                                                  Arg2: "No_precision",
                                                  Arg3: "End_of_format"
                                                }
                                              },
                                              Arg1: "int32_bits_of_float %d"
                                            }), i),
                                    (function (param) {
                                        return /* constructor */{
                                                tag: "Eq",
                                                Arg0: Caml_float.caml_int32_bits_of_float(f),
                                                Arg1: i32
                                              };
                                      })
                                  ],
                                  Arg1: "[]"
                                }
                              };
                      }), int32_pairs)));
}

var suites = Pervasives.$at(/* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "one",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: Caml_int64.bits_of_float(1.0),
                    Arg1: one_float
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "two",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: Caml_int64.float_of_bits(one_float),
                      Arg1: 1.0
                    };
            })
        ],
        Arg1: "[]"
      }
    }, from_pairs(int32_pairs));

Mt.from_pair_suites("Float_of_bits_test", suites);

exports.one_float = one_float;
exports.int32_pairs = int32_pairs;
exports.from_pairs = from_pairs;
exports.suites = suites;
/* suites Not a pure module */
