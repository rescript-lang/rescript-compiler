// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj    = require("../runtime/caml_obj");
var Caml_float  = require("../runtime/caml_float");
var Pervasives  = require("../stdlib/pervasives");
var Caml_format = require("../runtime/caml_format");
var Mt          = require("./mt");
var Printf      = require("../stdlib/printf");
var $$Array     = require("../stdlib/array");
var Caml_curry  = require("../runtime/caml_curry");

var of_string = Caml_obj.caml_obj_dup(/* array */[
      /* tuple */[
        0,
        "0"
      ],
      /* tuple */[
        3,
        "03"
      ],
      /* tuple */[
        -3,
        "-03"
      ],
      /* tuple */[
        -63,
        "-0x3f"
      ],
      /* tuple */[
        -31,
        "-0x1f"
      ],
      /* tuple */[
        47,
        "0X2f"
      ],
      /* tuple */[
        11,
        "0O13"
      ],
      /* tuple */[
        8,
        "0o10"
      ],
      /* tuple */[
        3,
        "0b11"
      ],
      /* tuple */[
        1,
        "0b01"
      ],
      /* tuple */[
        0,
        "0b00"
      ],
      /* tuple */[
        -3,
        "-0b11"
      ],
      /* tuple */[
        -5,
        "-0B101"
      ],
      /* tuple */[
        332,
        "0332"
      ],
      /* tuple */[
        -32,
        "-32"
      ],
      /* tuple */[
        1,
        "-0xffff_ffff"
      ],
      /* tuple */[
        -1,
        "0xffff_ffff"
      ]
    ]);

function from_float_of_string(xs) {
  return $$Array.mapi(function (_, _$1) {
              return Pervasives.string_of_float;
            }, xs);
}

function from_of_string() {
  return $$Array.to_list($$Array.mapi(function (i, param) {
                  var b = param[1];
                  var a = param[0];
                  return /* tuple */[
                          Caml_curry.app1(Printf.sprintf(/* Format */{
                                    0: /* String_literal */{
                                      0: "of_string ",
                                      1: /* Scan_get_counter */{
                                        0: /* Token_counter */2,
                                        1: /* End_of_format */0,
                                        length: 2,
                                        tag: 21
                                      },
                                      length: 2,
                                      tag: 11
                                    },
                                    1: "of_string %L",
                                    length: 2,
                                    tag: 0
                                  }), i),
                          function () {
                            return /* Eq */{
                                    0: Caml_format.caml_int_of_string(b),
                                    1: a,
                                    length: 2,
                                    tag: 0
                                  };
                          }
                        ];
                }, of_string));
}

function u(v) {
  return Caml_curry.app1(Printf.sprintf(/* Format */{
                  0: /* Int */{
                    0: /* Int_d */0,
                    1: /* Lit_padding */{
                      0: /* Right */1,
                      1: 33,
                      length: 2,
                      tag: 0
                    },
                    2: /* No_precision */0,
                    3: /* End_of_format */0,
                    length: 4,
                    tag: 4
                  },
                  1: "%33d",
                  length: 2,
                  tag: 0
                }), v);
}

function to_str(s) {
  return Caml_format.caml_int_of_string(s);
}

var v = Caml_curry.app1(Printf.sprintf(/* Format */{
          0: /* Int */{
            0: /* Int_d */0,
            1: /* Lit_padding */{
              0: /* Right */1,
              1: 3,
              length: 2,
              tag: 0
            },
            2: /* No_precision */0,
            3: /* End_of_format */0,
            length: 4,
            tag: 4
          },
          1: "%3d",
          length: 2,
          tag: 0
        }), 3333);

var pairs = Caml_obj.caml_obj_dup(/* array */[
      /* tuple */[
        /* FP_infinite */3,
        "infinity"
      ],
      /* tuple */[
        /* FP_infinite */3,
        "+infinity"
      ],
      /* tuple */[
        /* FP_infinite */3,
        "-infinity"
      ],
      /* tuple */[
        /* FP_zero */2,
        "0"
      ],
      /* tuple */[
        /* FP_zero */2,
        "0."
      ]
    ]);

Mt.from_pair_suites("caml_format_test.ml", Pervasives.$at(from_of_string(of_string), Pervasives.$at(/* :: */[
              /* tuple */[
                "isnan_of_string",
                function () {
                  return /* Eq */{
                          0: /* true */1,
                          1: +(Caml_float.caml_classify_float(Caml_format.caml_float_of_string("nan")) === /* FP_nan */4),
                          length: 2,
                          tag: 0
                        };
                }
              ],
              /* [] */0
            ], Pervasives.$at($$Array.to_list($$Array.mapi(function (i, param) {
                          var b = param[1];
                          var a = param[0];
                          return /* tuple */[
                                  Caml_curry.app1(Printf.sprintf(/* Format */{
                                            0: /* String_literal */{
                                              0: "infinity_of_string ",
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
                                            1: "infinity_of_string %d",
                                            length: 2,
                                            tag: 0
                                          }), i),
                                  function () {
                                    return /* Eq */{
                                            0: a,
                                            1: Caml_float.caml_classify_float(Caml_format.caml_float_of_string(b)),
                                            length: 2,
                                            tag: 0
                                          };
                                  }
                                ];
                        }, pairs)), /* :: */[
                  /* tuple */[
                    "throw",
                    function () {
                      return /* ThrowAny */{
                              0: function () {
                                return Caml_format.caml_float_of_string("");
                              },
                              length: 1,
                              tag: 3
                            };
                    }
                  ],
                  /* [] */0
                ]))));

exports.of_string            = of_string;
exports.from_float_of_string = from_float_of_string;
exports.from_of_string       = from_of_string;
exports.u                    = u;
exports.to_str               = to_str;
exports.v                    = v;
/* v Not a pure module */
