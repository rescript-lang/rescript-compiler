// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_int64  = require("../runtime/caml_int64");
var Caml_obj    = require("../runtime/caml_obj");
var Caml_float  = require("../runtime/caml_float");
var Pervasives  = require("../stdlib/pervasives");
var Caml_format = require("../runtime/caml_format");
var Mt          = require("./mt");
var Printf      = require("../stdlib/printf");
var Int64       = require("../stdlib/int64");
var $$Array     = require("../stdlib/array");
var Caml_curry  = require("../runtime/caml_curry");
var Format      = require("../stdlib/format");

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

var pairs$1 = /* array */[
  /* tuple */[
    3232,
    "32_32.0"
  ],
  /* tuple */[
    1.000,
    "1.000"
  ],
  /* tuple */[
    12.000,
    "12.000"
  ]
];

var suites = Pervasives.$at(from_of_string(of_string), Pervasives.$at(/* :: */[
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
                    }, pairs)), Pervasives.$at(/* :: */[
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
                  /* :: */[
                    /* tuple */[
                      "format_int",
                      function () {
                        return /* Eq */{
                                0: "                              33",
                                1: Caml_format.caml_format_int("%32d", 33),
                                length: 2,
                                tag: 0
                              };
                      }
                    ],
                    /* [] */0
                  ]
                ], $$Array.to_list($$Array.mapi(function (i, param) {
                          var b = param[1];
                          var a = param[0];
                          return /* tuple */[
                                  Caml_curry.app1(Printf.sprintf(/* Format */{
                                            0: /* String_literal */{
                                              0: "normal_float_of_string ",
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
                                            1: "normal_float_of_string %d",
                                            length: 2,
                                            tag: 0
                                          }), i),
                                  function () {
                                    return /* Eq */{
                                            0: a,
                                            1: Caml_format.caml_float_of_string(b),
                                            length: 2,
                                            tag: 0
                                          };
                                  }
                                ];
                        }, pairs$1))))));

function ff(param) {
  return Caml_format.caml_format_int("%32d", param);
}

var a = Format.asprintf;

var formatter_suites_000 = /* tuple */[
  "fmt_concat",
  function () {
    return /* Eq */{
            0: Caml_curry.app6(Format.asprintf(Pervasives.$caret$caret(/* Format */{
                          0: /* String */{
                            0: /* No_padding */0,
                            1: /* Char_literal */{
                              0: /* " " */32,
                              1: /* Int */{
                                0: /* Int_d */0,
                                1: /* Lit_padding */{
                                  0: /* Zeros */2,
                                  1: 3,
                                  length: 2,
                                  tag: 0
                                },
                                2: /* No_precision */0,
                                3: /* Char_literal */{
                                  0: /* " " */32,
                                  1: /* Scan_get_counter */{
                                    0: /* Token_counter */2,
                                    1: /* End_of_format */0,
                                    length: 2,
                                    tag: 21
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
                            length: 2,
                            tag: 2
                          },
                          1: "%s %03d %L",
                          length: 2,
                          tag: 0
                        }, /* Format */{
                          0: /* Caml_string */{
                            0: /* No_padding */0,
                            1: /* Char_literal */{
                              0: /* " " */32,
                              1: /* Int */{
                                0: /* Int_d */0,
                                1: /* Lit_padding */{
                                  0: /* Zeros */2,
                                  1: 3,
                                  length: 2,
                                  tag: 0
                                },
                                2: /* No_precision */0,
                                3: /* Char_literal */{
                                  0: /* " " */32,
                                  1: /* Scan_get_counter */{
                                    0: /* Token_counter */2,
                                    1: /* End_of_format */0,
                                    length: 2,
                                    tag: 21
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
                            length: 2,
                            tag: 3
                          },
                          1: "%S %03d %L",
                          length: 2,
                          tag: 0
                        })), "32", 33, 33, "a", 33, 3),
            1: '32 033 33"a" 033 3',
            length: 2,
            tag: 0
          };
  }
];

var formatter_suites_001 = /* :: */[
  /* tuple */[
    "fmt_gen",
    function () {
      return /* Eq */{
              0: Caml_curry.app8(Format.asprintf(Pervasives.$caret$caret(/* Format */{
                            0: /* String */{
                              0: /* No_padding */0,
                              1: /* Char_literal */{
                                0: /* " " */32,
                                1: /* Int */{
                                  0: /* Int_d */0,
                                  1: /* Lit_padding */{
                                    0: /* Zeros */2,
                                    1: 3,
                                    length: 2,
                                    tag: 0
                                  },
                                  2: /* No_precision */0,
                                  3: /* Char_literal */{
                                    0: /* " " */32,
                                    1: /* Scan_get_counter */{
                                      0: /* Token_counter */2,
                                      1: /* End_of_format */0,
                                      length: 2,
                                      tag: 21
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
                              length: 2,
                              tag: 2
                            },
                            1: "%s %03d %L",
                            length: 2,
                            tag: 0
                          }, /* Format */{
                            0: /* Caml_string */{
                              0: /* No_padding */0,
                              1: /* Char_literal */{
                                0: /* " " */32,
                                1: /* Int */{
                                  0: /* Int_d */0,
                                  1: /* Lit_padding */{
                                    0: /* Zeros */2,
                                    1: 3,
                                    length: 2,
                                    tag: 0
                                  },
                                  2: /* No_precision */0,
                                  3: /* Char_literal */{
                                    0: /* " " */32,
                                    1: /* Scan_get_counter */{
                                      0: /* Token_counter */2,
                                      1: /* Char_literal */{
                                        0: /* " " */32,
                                        1: /* Alpha */{
                                          0: /* End_of_format */0,
                                          length: 1,
                                          tag: 15
                                        },
                                        length: 2,
                                        tag: 12
                                      },
                                      length: 2,
                                      tag: 21
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
                              length: 2,
                              tag: 3
                            },
                            1: "%S %03d %L %a",
                            length: 2,
                            tag: 0
                          })), "32", 33, 33, "a", 33, 3, function (param, param$1) {
                    return Format.pp_print_list(/* None */0, Format.pp_print_int, param, param$1);
                  }, /* :: */[
                    1,
                    /* :: */[
                      2,
                      /* :: */[
                        3,
                        /* [] */0
                      ]
                    ]
                  ]),
              1: '32 033 33"a" 033 3 12\n3',
              length: 2,
              tag: 0
            };
    }
  ],
  /* :: */[
    /* tuple */[
      "long_fmt",
      function () {
        return /* Eq */{
                0: Caml_curry.app(Format.asprintf(/* Format */{
                          0: /* Int */{
                            0: /* Int_d */0,
                            1: /* No_padding */0,
                            2: /* No_precision */0,
                            3: /* Char_literal */{
                              0: /* " " */32,
                              1: /* Int */{
                                0: /* Int_i */3,
                                1: /* No_padding */0,
                                2: /* No_precision */0,
                                3: /* Char_literal */{
                                  0: /* " " */32,
                                  1: /* Int */{
                                    0: /* Int_u */12,
                                    1: /* No_padding */0,
                                    2: /* No_precision */0,
                                    3: /* Char_literal */{
                                      0: /* " " */32,
                                      1: /* Scan_get_counter */{
                                        0: /* Char_counter */1,
                                        1: /* Char_literal */{
                                          0: /* " " */32,
                                          1: /* Scan_get_counter */{
                                            0: /* Line_counter */0,
                                            1: /* Char_literal */{
                                              0: /* " " */32,
                                              1: /* Scan_get_counter */{
                                                0: /* Token_counter */2,
                                                1: /* Char_literal */{
                                                  0: /* " " */32,
                                                  1: /* Scan_get_counter */{
                                                    0: /* Token_counter */2,
                                                    1: /* Char_literal */{
                                                      0: /* " " */32,
                                                      1: /* Int */{
                                                        0: /* Int_x */6,
                                                        1: /* No_padding */0,
                                                        2: /* No_precision */0,
                                                        3: /* Char_literal */{
                                                          0: /* " " */32,
                                                          1: /* Int */{
                                                            0: /* Int_X */8,
                                                            1: /* No_padding */0,
                                                            2: /* No_precision */0,
                                                            3: /* Char_literal */{
                                                              0: /* " " */32,
                                                              1: /* Int */{
                                                                0: /* Int_o */10,
                                                                1: /* No_padding */0,
                                                                2: /* No_precision */0,
                                                                3: /* Char_literal */{
                                                                  0: /* " " */32,
                                                                  1: /* String */{
                                                                    0: /* No_padding */0,
                                                                    1: /* Char_literal */{
                                                                      0: /* " " */32,
                                                                      1: /* Caml_string */{
                                                                        0: /* No_padding */0,
                                                                        1: /* Char_literal */{
                                                                          0: /* " " */32,
                                                                          1: /* Char */{
                                                                            0: /* Char_literal */{
                                                                              0: /* " " */32,
                                                                              1: /* Caml_char */{
                                                                                0: /* Char_literal */{
                                                                                  0: /* " " */32,
                                                                                  1: /* Float */{
                                                                                    0: /* Float_f */0,
                                                                                    1: /* No_padding */0,
                                                                                    2: /* No_precision */0,
                                                                                    3: /* Char_literal */{
                                                                                      0: /* " " */32,
                                                                                      1: /* Float */{
                                                                                        0: /* Float_F */15,
                                                                                        1: /* No_padding */0,
                                                                                        2: /* No_precision */0,
                                                                                        3: /* Char_literal */{
                                                                                          0: /* " " */32,
                                                                                          1: /* Float */{
                                                                                            0: /* Float_e */3,
                                                                                            1: /* No_padding */0,
                                                                                            2: /* No_precision */0,
                                                                                            3: /* Char_literal */{
                                                                                              0: /* " " */32,
                                                                                              1: /* Float */{
                                                                                                0: /* Float_E */6,
                                                                                                1: /* No_padding */0,
                                                                                                2: /* No_precision */0,
                                                                                                3: /* Char_literal */{
                                                                                                  0: /* " " */32,
                                                                                                  1: /* Float */{
                                                                                                    0: /* Float_g */9,
                                                                                                    1: /* No_padding */0,
                                                                                                    2: /* No_precision */0,
                                                                                                    3: /* Char_literal */{
                                                                                                      0: /* " " */32,
                                                                                                      1: /* Float */{
                                                                                                        0: /* Float_G */12,
                                                                                                        1: /* No_padding */0,
                                                                                                        2: /* No_precision */0,
                                                                                                        3: /* Char_literal */{
                                                                                                          0: /* " " */32,
                                                                                                          1: /* Bool */{
                                                                                                            0: /* Char_literal */{
                                                                                                              0: /* " " */32,
                                                                                                              1: /* Bool */{
                                                                                                                0: /* Char_literal */{
                                                                                                                  0: /* " " */32,
                                                                                                                  1: /* Int32 */{
                                                                                                                    0: /* Int_d */0,
                                                                                                                    1: /* No_padding */0,
                                                                                                                    2: /* No_precision */0,
                                                                                                                    3: /* Char_literal */{
                                                                                                                      0: /* " " */32,
                                                                                                                      1: /* Int32 */{
                                                                                                                        0: /* Int_i */3,
                                                                                                                        1: /* No_padding */0,
                                                                                                                        2: /* No_precision */0,
                                                                                                                        3: /* Char_literal */{
                                                                                                                          0: /* " " */32,
                                                                                                                          1: /* Int32 */{
                                                                                                                            0: /* Int_u */12,
                                                                                                                            1: /* No_padding */0,
                                                                                                                            2: /* No_precision */0,
                                                                                                                            3: /* Char_literal */{
                                                                                                                              0: /* " " */32,
                                                                                                                              1: /* Int32 */{
                                                                                                                                0: /* Int_x */6,
                                                                                                                                1: /* No_padding */0,
                                                                                                                                2: /* No_precision */0,
                                                                                                                                3: /* Char_literal */{
                                                                                                                                  0: /* " " */32,
                                                                                                                                  1: /* Int32 */{
                                                                                                                                    0: /* Int_X */8,
                                                                                                                                    1: /* No_padding */0,
                                                                                                                                    2: /* No_precision */0,
                                                                                                                                    3: /* Char_literal */{
                                                                                                                                      0: /* " " */32,
                                                                                                                                      1: /* Int32 */{
                                                                                                                                        0: /* Int_o */10,
                                                                                                                                        1: /* No_padding */0,
                                                                                                                                        2: /* No_precision */0,
                                                                                                                                        3: /* Char_literal */{
                                                                                                                                          0: /* " " */32,
                                                                                                                                          1: /* Nativeint */{
                                                                                                                                            0: /* Int_d */0,
                                                                                                                                            1: /* No_padding */0,
                                                                                                                                            2: /* No_precision */0,
                                                                                                                                            3: /* Char_literal */{
                                                                                                                                              0: /* " " */32,
                                                                                                                                              1: /* Nativeint */{
                                                                                                                                                0: /* Int_i */3,
                                                                                                                                                1: /* No_padding */0,
                                                                                                                                                2: /* No_precision */0,
                                                                                                                                                3: /* Char_literal */{
                                                                                                                                                  0: /* " " */32,
                                                                                                                                                  1: /* Nativeint */{
                                                                                                                                                    0: /* Int_u */12,
                                                                                                                                                    1: /* No_padding */0,
                                                                                                                                                    2: /* No_precision */0,
                                                                                                                                                    3: /* Char_literal */{
                                                                                                                                                      0: /* " " */32,
                                                                                                                                                      1: /* Nativeint */{
                                                                                                                                                        0: /* Int_x */6,
                                                                                                                                                        1: /* No_padding */0,
                                                                                                                                                        2: /* No_precision */0,
                                                                                                                                                        3: /* Char_literal */{
                                                                                                                                                          0: /* " " */32,
                                                                                                                                                          1: /* Nativeint */{
                                                                                                                                                            0: /* Int_x */6,
                                                                                                                                                            1: /* No_padding */0,
                                                                                                                                                            2: /* No_precision */0,
                                                                                                                                                            3: /* Char_literal */{
                                                                                                                                                              0: /* " " */32,
                                                                                                                                                              1: /* Nativeint */{
                                                                                                                                                                0: /* Int_o */10,
                                                                                                                                                                1: /* No_padding */0,
                                                                                                                                                                2: /* No_precision */0,
                                                                                                                                                                3: /* String_literal */{
                                                                                                                                                                  0: "  ",
                                                                                                                                                                  1: /* End_of_format */0,
                                                                                                                                                                  length: 2,
                                                                                                                                                                  tag: 11
                                                                                                                                                                },
                                                                                                                                                                length: 4,
                                                                                                                                                                tag: 6
                                                                                                                                                              },
                                                                                                                                                              length: 2,
                                                                                                                                                              tag: 12
                                                                                                                                                            },
                                                                                                                                                            length: 4,
                                                                                                                                                            tag: 6
                                                                                                                                                          },
                                                                                                                                                          length: 2,
                                                                                                                                                          tag: 12
                                                                                                                                                        },
                                                                                                                                                        length: 4,
                                                                                                                                                        tag: 6
                                                                                                                                                      },
                                                                                                                                                      length: 2,
                                                                                                                                                      tag: 12
                                                                                                                                                    },
                                                                                                                                                    length: 4,
                                                                                                                                                    tag: 6
                                                                                                                                                  },
                                                                                                                                                  length: 2,
                                                                                                                                                  tag: 12
                                                                                                                                                },
                                                                                                                                                length: 4,
                                                                                                                                                tag: 6
                                                                                                                                              },
                                                                                                                                              length: 2,
                                                                                                                                              tag: 12
                                                                                                                                            },
                                                                                                                                            length: 4,
                                                                                                                                            tag: 6
                                                                                                                                          },
                                                                                                                                          length: 2,
                                                                                                                                          tag: 12
                                                                                                                                        },
                                                                                                                                        length: 4,
                                                                                                                                        tag: 5
                                                                                                                                      },
                                                                                                                                      length: 2,
                                                                                                                                      tag: 12
                                                                                                                                    },
                                                                                                                                    length: 4,
                                                                                                                                    tag: 5
                                                                                                                                  },
                                                                                                                                  length: 2,
                                                                                                                                  tag: 12
                                                                                                                                },
                                                                                                                                length: 4,
                                                                                                                                tag: 5
                                                                                                                              },
                                                                                                                              length: 2,
                                                                                                                              tag: 12
                                                                                                                            },
                                                                                                                            length: 4,
                                                                                                                            tag: 5
                                                                                                                          },
                                                                                                                          length: 2,
                                                                                                                          tag: 12
                                                                                                                        },
                                                                                                                        length: 4,
                                                                                                                        tag: 5
                                                                                                                      },
                                                                                                                      length: 2,
                                                                                                                      tag: 12
                                                                                                                    },
                                                                                                                    length: 4,
                                                                                                                    tag: 5
                                                                                                                  },
                                                                                                                  length: 2,
                                                                                                                  tag: 12
                                                                                                                },
                                                                                                                length: 1,
                                                                                                                tag: 9
                                                                                                              },
                                                                                                              length: 2,
                                                                                                              tag: 12
                                                                                                            },
                                                                                                            length: 1,
                                                                                                            tag: 9
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
                                                                                                    tag: 8
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
                                                                                            tag: 8
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
                                                                                    tag: 8
                                                                                  },
                                                                                  length: 2,
                                                                                  tag: 12
                                                                                },
                                                                                length: 1,
                                                                                tag: 1
                                                                              },
                                                                              length: 2,
                                                                              tag: 12
                                                                            },
                                                                            length: 1,
                                                                            tag: 0
                                                                          },
                                                                          length: 2,
                                                                          tag: 12
                                                                        },
                                                                        length: 2,
                                                                        tag: 3
                                                                      },
                                                                      length: 2,
                                                                      tag: 12
                                                                    },
                                                                    length: 2,
                                                                    tag: 2
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
                                                            tag: 4
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
                                                    length: 2,
                                                    tag: 21
                                                  },
                                                  length: 2,
                                                  tag: 12
                                                },
                                                length: 2,
                                                tag: 21
                                              },
                                              length: 2,
                                              tag: 12
                                            },
                                            length: 2,
                                            tag: 21
                                          },
                                          length: 2,
                                          tag: 12
                                        },
                                        length: 2,
                                        tag: 21
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
                                tag: 4
                              },
                              length: 2,
                              tag: 12
                            },
                            length: 4,
                            tag: 4
                          },
                          1: "%d %i %u %n %l %L %N %x %X %o %s %S %c %C %f %F %e %E %g %G %B %b %ld %li %lu %lx %lX %lo %nd %ni %nu %nx %nx %no  ",
                          length: 2,
                          tag: 0
                        }), [
                      1,
                      2,
                      3,
                      4,
                      5,
                      6,
                      7,
                      8,
                      9,
                      10,
                      "a",
                      "b",
                      /* "c" */99,
                      /* "d" */100,
                      1,
                      2,
                      3,
                      4,
                      5,
                      6,
                      /* true */1,
                      /* false */0,
                      0,
                      1,
                      2,
                      3,
                      4,
                      5,
                      6,
                      7,
                      8,
                      9,
                      10,
                      11
                    ]),
                1: "1 2 3 4 5 6 7 8 9 12 a \"b\" c 'd' 1.000000 2. 3.000000e+00 4.000000E+00 5 6 true false 0 1 2 3 4 5 6 7 8 9 a 13  ",
                length: 2,
                tag: 0
              };
      }
    ],
    /* :: */[
      /* tuple */[
        "long_fmt_2",
        function () {
          return /* Eq */{
                  0: Caml_curry.app(Format.asprintf(/* Format */{
                            0: /* Formatting_gen */{
                              0: /* Open_box */{
                                0: /* Format */{
                                  0: /* End_of_format */0,
                                  1: "",
                                  length: 2,
                                  tag: 0
                                },
                                length: 1,
                                tag: 1
                              },
                              1: /* Int */{
                                0: /* Int_d */0,
                                1: /* Lit_padding */{
                                  0: /* Right */1,
                                  1: 23,
                                  length: 2,
                                  tag: 0
                                },
                                2: /* No_precision */0,
                                3: /* Char_literal */{
                                  0: /* " " */32,
                                  1: /* Int */{
                                    0: /* Int_i */3,
                                    1: /* Lit_padding */{
                                      0: /* Right */1,
                                      1: 2,
                                      length: 2,
                                      tag: 0
                                    },
                                    2: /* No_precision */0,
                                    3: /* Char_literal */{
                                      0: /* " " */32,
                                      1: /* Int */{
                                        0: /* Int_u */12,
                                        1: /* Lit_padding */{
                                          0: /* Right */1,
                                          1: 3,
                                          length: 2,
                                          tag: 0
                                        },
                                        2: /* No_precision */0,
                                        3: /* Char_literal */{
                                          0: /* " " */32,
                                          1: /* Scan_get_counter */{
                                            0: /* Char_counter */1,
                                            1: /* Char_literal */{
                                              0: /* " " */32,
                                              1: /* Int */{
                                                0: /* Int_x */6,
                                                1: /* Lit_padding */{
                                                  0: /* Right */1,
                                                  1: 0,
                                                  length: 2,
                                                  tag: 0
                                                },
                                                2: /* No_precision */0,
                                                3: /* String_literal */{
                                                  0: "l ",
                                                  1: /* Int */{
                                                    0: /* Int_x */6,
                                                    1: /* Lit_padding */{
                                                      0: /* Right */1,
                                                      1: 0,
                                                      length: 2,
                                                      tag: 0
                                                    },
                                                    2: /* No_precision */0,
                                                    3: /* String_literal */{
                                                      0: "L ",
                                                      1: /* Scan_get_counter */{
                                                        0: /* Token_counter */2,
                                                        1: /* Char_literal */{
                                                          0: /* " " */32,
                                                          1: /* Int */{
                                                            0: /* Int_x */6,
                                                            1: /* Lit_padding */{
                                                              0: /* Zeros */2,
                                                              1: 3,
                                                              length: 2,
                                                              tag: 0
                                                            },
                                                            2: /* No_precision */0,
                                                            3: /* Char_literal */{
                                                              0: /* " " */32,
                                                              1: /* Int */{
                                                                0: /* Int_X */8,
                                                                1: /* No_padding */0,
                                                                2: /* No_precision */0,
                                                                3: /* Char_literal */{
                                                                  0: /* " " */32,
                                                                  1: /* Int */{
                                                                    0: /* Int_o */10,
                                                                    1: /* No_padding */0,
                                                                    2: /* No_precision */0,
                                                                    3: /* Char_literal */{
                                                                      0: /* " " */32,
                                                                      1: /* String */{
                                                                        0: /* No_padding */0,
                                                                        1: /* Char_literal */{
                                                                          0: /* " " */32,
                                                                          1: /* Caml_string */{
                                                                            0: /* No_padding */0,
                                                                            1: /* Char_literal */{
                                                                              0: /* " " */32,
                                                                              1: /* Char */{
                                                                                0: /* Char_literal */{
                                                                                  0: /* " " */32,
                                                                                  1: /* Caml_char */{
                                                                                    0: /* Char_literal */{
                                                                                      0: /* " " */32,
                                                                                      1: /* Float */{
                                                                                        0: /* Float_f */0,
                                                                                        1: /* Lit_padding */{
                                                                                          0: /* Right */1,
                                                                                          1: 3,
                                                                                          length: 2,
                                                                                          tag: 0
                                                                                        },
                                                                                        2: /* No_precision */0,
                                                                                        3: /* Char_literal */{
                                                                                          0: /* " " */32,
                                                                                          1: /* Float */{
                                                                                            0: /* Float_F */15,
                                                                                            1: /* Lit_padding */{
                                                                                              0: /* Right */1,
                                                                                              1: 2,
                                                                                              length: 2,
                                                                                              tag: 0
                                                                                            },
                                                                                            2: /* No_precision */0,
                                                                                            3: /* Char_literal */{
                                                                                              0: /* " " */32,
                                                                                              1: /* Float */{
                                                                                                0: /* Float_e */3,
                                                                                                1: /* Lit_padding */{
                                                                                                  0: /* Right */1,
                                                                                                  1: 2,
                                                                                                  length: 2,
                                                                                                  tag: 0
                                                                                                },
                                                                                                2: /* No_precision */0,
                                                                                                3: /* Char_literal */{
                                                                                                  0: /* " " */32,
                                                                                                  1: /* Float */{
                                                                                                    0: /* Float_E */6,
                                                                                                    1: /* No_padding */0,
                                                                                                    2: /* No_precision */0,
                                                                                                    3: /* Char_literal */{
                                                                                                      0: /* " " */32,
                                                                                                      1: /* Float */{
                                                                                                        0: /* Float_g */9,
                                                                                                        1: /* No_padding */0,
                                                                                                        2: /* No_precision */0,
                                                                                                        3: /* Char_literal */{
                                                                                                          0: /* " " */32,
                                                                                                          1: /* Float */{
                                                                                                            0: /* Float_G */12,
                                                                                                            1: /* No_padding */0,
                                                                                                            2: /* No_precision */0,
                                                                                                            3: /* Char_literal */{
                                                                                                              0: /* " " */32,
                                                                                                              1: /* Bool */{
                                                                                                                0: /* Char_literal */{
                                                                                                                  0: /* " " */32,
                                                                                                                  1: /* Bool */{
                                                                                                                    0: /* Char_literal */{
                                                                                                                      0: /* " " */32,
                                                                                                                      1: /* Int32 */{
                                                                                                                        0: /* Int_d */0,
                                                                                                                        1: /* No_padding */0,
                                                                                                                        2: /* No_precision */0,
                                                                                                                        3: /* Char_literal */{
                                                                                                                          0: /* " " */32,
                                                                                                                          1: /* Int32 */{
                                                                                                                            0: /* Int_i */3,
                                                                                                                            1: /* No_padding */0,
                                                                                                                            2: /* No_precision */0,
                                                                                                                            3: /* Char_literal */{
                                                                                                                              0: /* " " */32,
                                                                                                                              1: /* Int32 */{
                                                                                                                                0: /* Int_u */12,
                                                                                                                                1: /* No_padding */0,
                                                                                                                                2: /* No_precision */0,
                                                                                                                                3: /* Char_literal */{
                                                                                                                                  0: /* " " */32,
                                                                                                                                  1: /* Int32 */{
                                                                                                                                    0: /* Int_x */6,
                                                                                                                                    1: /* No_padding */0,
                                                                                                                                    2: /* No_precision */0,
                                                                                                                                    3: /* Char_literal */{
                                                                                                                                      0: /* " " */32,
                                                                                                                                      1: /* Int32 */{
                                                                                                                                        0: /* Int_X */8,
                                                                                                                                        1: /* No_padding */0,
                                                                                                                                        2: /* No_precision */0,
                                                                                                                                        3: /* Char_literal */{
                                                                                                                                          0: /* " " */32,
                                                                                                                                          1: /* Int32 */{
                                                                                                                                            0: /* Int_o */10,
                                                                                                                                            1: /* No_padding */0,
                                                                                                                                            2: /* No_precision */0,
                                                                                                                                            3: /* Char_literal */{
                                                                                                                                              0: /* " " */32,
                                                                                                                                              1: /* Nativeint */{
                                                                                                                                                0: /* Int_d */0,
                                                                                                                                                1: /* No_padding */0,
                                                                                                                                                2: /* No_precision */0,
                                                                                                                                                3: /* Char_literal */{
                                                                                                                                                  0: /* " " */32,
                                                                                                                                                  1: /* Nativeint */{
                                                                                                                                                    0: /* Int_i */3,
                                                                                                                                                    1: /* No_padding */0,
                                                                                                                                                    2: /* No_precision */0,
                                                                                                                                                    3: /* Char_literal */{
                                                                                                                                                      0: /* " " */32,
                                                                                                                                                      1: /* Nativeint */{
                                                                                                                                                        0: /* Int_u */12,
                                                                                                                                                        1: /* No_padding */0,
                                                                                                                                                        2: /* No_precision */0,
                                                                                                                                                        3: /* Char_literal */{
                                                                                                                                                          0: /* " " */32,
                                                                                                                                                          1: /* Nativeint */{
                                                                                                                                                            0: /* Int_x */6,
                                                                                                                                                            1: /* No_padding */0,
                                                                                                                                                            2: /* No_precision */0,
                                                                                                                                                            3: /* Char_literal */{
                                                                                                                                                              0: /* " " */32,
                                                                                                                                                              1: /* Nativeint */{
                                                                                                                                                                0: /* Int_x */6,
                                                                                                                                                                1: /* No_padding */0,
                                                                                                                                                                2: /* No_precision */0,
                                                                                                                                                                3: /* Char_literal */{
                                                                                                                                                                  0: /* " " */32,
                                                                                                                                                                  1: /* Nativeint */{
                                                                                                                                                                    0: /* Int_o */10,
                                                                                                                                                                    1: /* No_padding */0,
                                                                                                                                                                    2: /* No_precision */0,
                                                                                                                                                                    3: /* String_literal */{
                                                                                                                                                                      0: "  ",
                                                                                                                                                                      1: /* Formatting_lit */{
                                                                                                                                                                        0: /* Close_box */0,
                                                                                                                                                                        1: /* End_of_format */0,
                                                                                                                                                                        length: 2,
                                                                                                                                                                        tag: 17
                                                                                                                                                                      },
                                                                                                                                                                      length: 2,
                                                                                                                                                                      tag: 11
                                                                                                                                                                    },
                                                                                                                                                                    length: 4,
                                                                                                                                                                    tag: 6
                                                                                                                                                                  },
                                                                                                                                                                  length: 2,
                                                                                                                                                                  tag: 12
                                                                                                                                                                },
                                                                                                                                                                length: 4,
                                                                                                                                                                tag: 6
                                                                                                                                                              },
                                                                                                                                                              length: 2,
                                                                                                                                                              tag: 12
                                                                                                                                                            },
                                                                                                                                                            length: 4,
                                                                                                                                                            tag: 6
                                                                                                                                                          },
                                                                                                                                                          length: 2,
                                                                                                                                                          tag: 12
                                                                                                                                                        },
                                                                                                                                                        length: 4,
                                                                                                                                                        tag: 6
                                                                                                                                                      },
                                                                                                                                                      length: 2,
                                                                                                                                                      tag: 12
                                                                                                                                                    },
                                                                                                                                                    length: 4,
                                                                                                                                                    tag: 6
                                                                                                                                                  },
                                                                                                                                                  length: 2,
                                                                                                                                                  tag: 12
                                                                                                                                                },
                                                                                                                                                length: 4,
                                                                                                                                                tag: 6
                                                                                                                                              },
                                                                                                                                              length: 2,
                                                                                                                                              tag: 12
                                                                                                                                            },
                                                                                                                                            length: 4,
                                                                                                                                            tag: 5
                                                                                                                                          },
                                                                                                                                          length: 2,
                                                                                                                                          tag: 12
                                                                                                                                        },
                                                                                                                                        length: 4,
                                                                                                                                        tag: 5
                                                                                                                                      },
                                                                                                                                      length: 2,
                                                                                                                                      tag: 12
                                                                                                                                    },
                                                                                                                                    length: 4,
                                                                                                                                    tag: 5
                                                                                                                                  },
                                                                                                                                  length: 2,
                                                                                                                                  tag: 12
                                                                                                                                },
                                                                                                                                length: 4,
                                                                                                                                tag: 5
                                                                                                                              },
                                                                                                                              length: 2,
                                                                                                                              tag: 12
                                                                                                                            },
                                                                                                                            length: 4,
                                                                                                                            tag: 5
                                                                                                                          },
                                                                                                                          length: 2,
                                                                                                                          tag: 12
                                                                                                                        },
                                                                                                                        length: 4,
                                                                                                                        tag: 5
                                                                                                                      },
                                                                                                                      length: 2,
                                                                                                                      tag: 12
                                                                                                                    },
                                                                                                                    length: 1,
                                                                                                                    tag: 9
                                                                                                                  },
                                                                                                                  length: 2,
                                                                                                                  tag: 12
                                                                                                                },
                                                                                                                length: 1,
                                                                                                                tag: 9
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
                                                                                                        tag: 8
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
                                                                                                tag: 8
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
                                                                                        tag: 8
                                                                                      },
                                                                                      length: 2,
                                                                                      tag: 12
                                                                                    },
                                                                                    length: 1,
                                                                                    tag: 1
                                                                                  },
                                                                                  length: 2,
                                                                                  tag: 12
                                                                                },
                                                                                length: 1,
                                                                                tag: 0
                                                                              },
                                                                              length: 2,
                                                                              tag: 12
                                                                            },
                                                                            length: 2,
                                                                            tag: 3
                                                                          },
                                                                          length: 2,
                                                                          tag: 12
                                                                        },
                                                                        length: 2,
                                                                        tag: 2
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
                                                                tag: 4
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
                                                        length: 2,
                                                        tag: 21
                                                      },
                                                      length: 2,
                                                      tag: 11
                                                    },
                                                    length: 4,
                                                    tag: 4
                                                  },
                                                  length: 2,
                                                  tag: 11
                                                },
                                                length: 4,
                                                tag: 4
                                              },
                                              length: 2,
                                              tag: 12
                                            },
                                            length: 2,
                                            tag: 21
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
                                    tag: 4
                                  },
                                  length: 2,
                                  tag: 12
                                },
                                length: 4,
                                tag: 4
                              },
                              length: 2,
                              tag: 18
                            },
                            1: "@[%23d %2i %3u %4n %0xl %0xL %N %03x %X %o %s %S %c %C %3f %2F %2e %E %g %G %B %b %ld %li %lu %lx %lX %lo %nd %ni %nu %nx %nx %no  @]",
                            length: 2,
                            tag: 0
                          }), [
                        1,
                        2,
                        3,
                        4,
                        5,
                        6,
                        7,
                        8,
                        9,
                        10,
                        "a",
                        "b",
                        /* "c" */99,
                        /* "d" */100,
                        1,
                        2,
                        3,
                        4,
                        5,
                        6,
                        /* true */1,
                        /* false */0,
                        0,
                        1,
                        2,
                        3,
                        4,
                        5,
                        6,
                        7,
                        8,
                        9,
                        10,
                        11
                      ]),
                  1: "                      1  2   3 4 5l 6L 7 008 9 12 a \"b\" c 'd' 1.000000 2. 3.000000e+00 4.000000E+00 5 6 true false 0 1 2 3 4 5 6 7 8 9 a 13  ",
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* :: */[
        /* tuple */[
          "width_1",
          function () {
            return /* Eq */{
                    0: Caml_curry.app1(Format.asprintf(/* Format */{
                              0: /* Int */{
                                0: /* Int_d */0,
                                1: /* Lit_padding */{
                                  0: /* Zeros */2,
                                  1: 14,
                                  length: 2,
                                  tag: 0
                                },
                                2: /* No_precision */0,
                                3: /* End_of_format */0,
                                length: 4,
                                tag: 4
                              },
                              1: "%014d",
                              length: 2,
                              tag: 0
                            }), 32),
                    1: "00000000000032",
                    length: 2,
                    tag: 0
                  };
          }
        ],
        /* :: */[
          /* tuple */[
            "width_2",
            function () {
              return /* Eq */{
                      0: Caml_curry.app1(Format.asprintf(/* Format */{
                                0: /* Float */{
                                  0: /* Float_f */0,
                                  1: /* Lit_padding */{
                                    0: /* Right */1,
                                    1: 10,
                                    length: 2,
                                    tag: 0
                                  },
                                  2: /* Lit_precision */{
                                    0: 3,
                                    length: 1,
                                    tag: 0
                                  },
                                  3: /* End_of_format */0,
                                  length: 4,
                                  tag: 8
                                },
                                1: "%10.3f",
                                length: 2,
                                tag: 0
                              }), 32333.02),
                      1: " 32333.020",
                      length: 2,
                      tag: 0
                    };
            }
          ],
          /* :: */[
            /* tuple */[
              "alternate_1",
              function () {
                return /* Eq */{
                        0: Caml_curry.app1(Format.asprintf(/* Format */{
                                  0: /* Int */{
                                    0: /* Int_x */6,
                                    1: /* Lit_padding */{
                                      0: /* Right */1,
                                      1: 0,
                                      length: 2,
                                      tag: 0
                                    },
                                    2: /* No_precision */0,
                                    3: /* End_of_format */0,
                                    length: 4,
                                    tag: 4
                                  },
                                  1: "%0x",
                                  length: 2,
                                  tag: 0
                                }), 32333),
                        1: "7e4d",
                        length: 2,
                        tag: 0
                      };
              }
            ],
            /* :: */[
              /* tuple */[
                "alternate_2",
                function () {
                  return /* Eq */{
                          0: Caml_curry.app1(Format.asprintf(/* Format */{
                                    0: /* Int */{
                                      0: /* Int_Cx */7,
                                      1: /* Lit_padding */{
                                        0: /* Right */1,
                                        1: 0,
                                        length: 2,
                                        tag: 0
                                      },
                                      2: /* No_precision */0,
                                      3: /* End_of_format */0,
                                      length: 4,
                                      tag: 4
                                    },
                                    1: "%#0x",
                                    length: 2,
                                    tag: 0
                                  }), 32333),
                          1: "0x7e4d",
                          length: 2,
                          tag: 0
                        };
                }
              ],
              /* :: */[
                /* tuple */[
                  "alternate_3",
                  function () {
                    return /* Eq */{
                            0: /* tuple */[
                              Caml_curry.app1(a(/* Format */{
                                        0: /* Int */{
                                          0: /* Int_Co */11,
                                          1: /* No_padding */0,
                                          2: /* No_precision */0,
                                          3: /* End_of_format */0,
                                          length: 4,
                                          tag: 4
                                        },
                                        1: "%#o",
                                        length: 2,
                                        tag: 0
                                      }), 32),
                              Caml_curry.app1(a(/* Format */{
                                        0: /* Int */{
                                          0: /* Int_o */10,
                                          1: /* No_padding */0,
                                          2: /* No_precision */0,
                                          3: /* End_of_format */0,
                                          length: 4,
                                          tag: 4
                                        },
                                        1: "%o",
                                        length: 2,
                                        tag: 0
                                      }), 32)
                            ],
                            1: /* tuple */[
                              "040",
                              "40"
                            ],
                            length: 2,
                            tag: 0
                          };
                  }
                ],
                /* :: */[
                  /* tuple */[
                    "justify_0",
                    function () {
                      return /* Eq */{
                              0: Caml_format.caml_format_int("%-8d", 32),
                              1: "32      ",
                              length: 2,
                              tag: 0
                            };
                    }
                  ],
                  /* :: */[
                    /* tuple */[
                      "sign_p",
                      function () {
                        return /* Eq */{
                                0: Caml_curry.app1(Format.asprintf(/* Format */{
                                          0: /* Int */{
                                            0: /* Int_pd */1,
                                            1: /* Lit_padding */{
                                              0: /* Right */1,
                                              1: 4,
                                              length: 2,
                                              tag: 0
                                            },
                                            2: /* No_precision */0,
                                            3: /* End_of_format */0,
                                            length: 4,
                                            tag: 4
                                          },
                                          1: "%+4d",
                                          length: 2,
                                          tag: 0
                                        }), 32),
                                1: " +32",
                                length: 2,
                                tag: 0
                              };
                      }
                    ],
                    /* :: */[
                      /* tuple */[
                        "sign_2p",
                        function () {
                          return /* Eq */{
                                  0: Caml_curry.app1(Format.asprintf(/* Format */{
                                            0: /* Int */{
                                              0: /* Int_sd */2,
                                              1: /* Lit_padding */{
                                                0: /* Right */1,
                                                1: 4,
                                                length: 2,
                                                tag: 0
                                              },
                                              2: /* No_precision */0,
                                              3: /* End_of_format */0,
                                              length: 4,
                                              tag: 4
                                            },
                                            1: "% 4d",
                                            length: 2,
                                            tag: 0
                                          }), 32),
                                  1: "  32",
                                  length: 2,
                                  tag: 0
                                };
                        }
                      ],
                      /* :: */[
                        /* tuple */[
                          "sign_3p",
                          function () {
                            return /* Eq */{
                                    0: Caml_curry.app1(a(/* Format */{
                                              0: /* Int32 */{
                                                0: /* Int_u */12,
                                                1: /* No_padding */0,
                                                2: /* No_precision */0,
                                                3: /* End_of_format */0,
                                                length: 4,
                                                tag: 5
                                              },
                                              1: "%lu",
                                              length: 2,
                                              tag: 0
                                            }), -1),
                                    1: "4294967295",
                                    length: 2,
                                    tag: 0
                                  };
                          }
                        ],
                        /* :: */[
                          /* tuple */[
                            "sign_4p",
                            function () {
                              return /* Eq */{
                                      0: Caml_curry.app1(a(/* Format */{
                                                0: /* Int32 */{
                                                  0: /* Int_d */0,
                                                  1: /* No_padding */0,
                                                  2: /* No_precision */0,
                                                  3: /* End_of_format */0,
                                                  length: 4,
                                                  tag: 5
                                                },
                                                1: "%ld",
                                                length: 2,
                                                tag: 0
                                              }), -1),
                                      1: "-1",
                                      length: 2,
                                      tag: 0
                                    };
                            }
                          ],
                          /* :: */[
                            /* tuple */[
                              "width_3",
                              function () {
                                return /* Eq */{
                                        0: Caml_format.caml_format_int("%032d", 32),
                                        1: "00000000000000000000000000000032",
                                        length: 2,
                                        tag: 0
                                      };
                              }
                            ],
                            /* :: */[
                              /* tuple */[
                                "prec_1",
                                function () {
                                  return /* Eq */{
                                          0: Caml_curry.app1(a(/* Format */{
                                                    0: /* Int */{
                                                      0: /* Int_d */0,
                                                      1: /* No_padding */0,
                                                      2: /* Lit_precision */{
                                                        0: 10,
                                                        length: 1,
                                                        tag: 0
                                                      },
                                                      3: /* End_of_format */0,
                                                      length: 4,
                                                      tag: 4
                                                    },
                                                    1: "%.10d",
                                                    length: 2,
                                                    tag: 0
                                                  }), 32),
                                          1: "0000000032",
                                          length: 2,
                                          tag: 0
                                        };
                                }
                              ],
                              /* :: */[
                                /* tuple */[
                                  "prec_2",
                                  function () {
                                    return /* Eq */{
                                            0: Caml_format.caml_format_int("%.10d", 32),
                                            1: "0000000032",
                                            length: 2,
                                            tag: 0
                                          };
                                  }
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "prec_3",
                                    function () {
                                      return /* Eq */{
                                              0: Caml_format.caml_format_int("%.d", 32),
                                              1: "32",
                                              length: 2,
                                              tag: 0
                                            };
                                    }
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "prec_4",
                                      function () {
                                        return /* Eq */{
                                                0: Caml_format.caml_format_int("%.d", 32),
                                                1: "32",
                                                length: 2,
                                                tag: 0
                                              };
                                      }
                                    ],
                                    /* [] */0
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
];

var formatter_suites = /* :: */[
  formatter_suites_000,
  formatter_suites_001
];

var float_data = /* array */[
  /* tuple */[
    "%f",
    32,
    "32.000000"
  ],
  /* tuple */[
    "%f",
    Pervasives.nan,
    "nan"
  ],
  /* tuple */[
    "%f",
    Pervasives.infinity,
    "inf"
  ],
  /* tuple */[
    "%f",
    Pervasives.neg_infinity,
    "-inf"
  ],
  /* tuple */[
    "%1.e",
    13000,
    "1e+04"
  ],
  /* tuple */[
    "%1.3e",
    2.3e-05,
    "2.300e-05"
  ],
  /* tuple */[
    "%3.10e",
    3e+56,
    "3.0000000000e+56"
  ],
  /* tuple */[
    "%3.10f",
    20000000000,
    "20000000000.0000000000"
  ],
  /* tuple */[
    "%3.3f",
    -3300,
    "-3300.000"
  ],
  /* tuple */[
    "%1.g",
    13000,
    "1e+04"
  ],
  /* tuple */[
    "%1.3g",
    2.3e-05,
    "2.3e-05"
  ],
  /* tuple */[
    "%3.10g",
    3e+56,
    "3e+56"
  ],
  /* tuple */[
    "%3.10g",
    20000000000,
    "2e+10"
  ],
  /* tuple */[
    "%3.3g",
    -3300,
    "-3.3e+03"
  ],
  /* tuple */[
    "%3.3g",
    -0.0033,
    "-0.0033"
  ],
  /* tuple */[
    "%3.10g",
    30000000000,
    "3e+10"
  ],
  /* tuple */[
    "%3.0g",
    30000000000,
    "3e+10"
  ],
  /* tuple */[
    "%3.g",
    30000000000,
    "3e+10"
  ],
  /* tuple */[
    "%3.g",
    3,
    "  3"
  ],
  /* tuple */[
    "%1.1g",
    2.1,
    "2"
  ],
  /* tuple */[
    "%1.2g",
    2.1,
    "2.1"
  ]
];

function ident(ppf, s) {
  return Caml_curry.app1(Format.fprintf(ppf, /* Format */{
                  0: /* String */{
                    0: /* No_padding */0,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 2
                  },
                  1: "%s",
                  length: 2,
                  tag: 0
                }), s);
}

function kwd(ppf, s) {
  return Caml_curry.app1(Format.fprintf(ppf, /* Format */{
                  0: /* String */{
                    0: /* No_padding */0,
                    1: /* End_of_format */0,
                    length: 2,
                    tag: 2
                  },
                  1: "%s",
                  length: 2,
                  tag: 0
                }), s);
}

function pr_exp0(ppf, lam) {
  var exit = 0;
  switch (lam.tag | 0) {
    case 1 : 
        return Caml_curry.app2(Format.fprintf(ppf, /* Format */{
                        0: /* Alpha */{
                          0: /* End_of_format */0,
                          length: 1,
                          tag: 15
                        },
                        1: "%a",
                        length: 2,
                        tag: 0
                      }), ident, lam[0]);
    case 0 : 
    case 2 : 
        exit = 1;
        break;
    
  }
  if (exit === 1) {
    return Caml_curry.app2(Format.fprintf(ppf, /* Format */{
                    0: /* Formatting_gen */{
                      0: /* Open_box */{
                        0: /* Format */{
                          0: /* String_literal */{
                            0: "<1>",
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 11
                          },
                          1: "<1>",
                          length: 2,
                          tag: 0
                        },
                        length: 1,
                        tag: 1
                      },
                      1: /* Char_literal */{
                        0: /* "(" */40,
                        1: /* Alpha */{
                          0: /* Char_literal */{
                            0: /* ")" */41,
                            1: /* Formatting_lit */{
                              0: /* Close_box */0,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 17
                            },
                            length: 2,
                            tag: 12
                          },
                          length: 1,
                          tag: 15
                        },
                        length: 2,
                        tag: 12
                      },
                      length: 2,
                      tag: 18
                    },
                    1: "@[<1>(%a)@]",
                    length: 2,
                    tag: 0
                  }), pr_lambda, lam);
  }
  
}

function pr_app(ppf, e) {
  return Caml_curry.app2(Format.fprintf(ppf, /* Format */{
                  0: /* Formatting_gen */{
                    0: /* Open_box */{
                      0: /* Format */{
                        0: /* String_literal */{
                          0: "<2>",
                          1: /* End_of_format */0,
                          length: 2,
                          tag: 11
                        },
                        1: "<2>",
                        length: 2,
                        tag: 0
                      },
                      length: 1,
                      tag: 1
                    },
                    1: /* Alpha */{
                      0: /* Formatting_lit */{
                        0: /* Close_box */0,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 17
                      },
                      length: 1,
                      tag: 15
                    },
                    length: 2,
                    tag: 18
                  },
                  1: "@[<2>%a@]",
                  length: 2,
                  tag: 0
                }), pr_other_applications, e);
}

function pr_other_applications(ppf, f) {
  switch (f.tag | 0) {
    case 0 : 
    case 1 : 
        return pr_exp0(ppf, f);
    case 2 : 
        return Caml_curry.app4(Format.fprintf(ppf, /* Format */{
                        0: /* Alpha */{
                          0: /* Formatting_lit */{
                            0: /* Break */{
                              0: "@ ",
                              1: 1,
                              2: 0,
                              length: 3,
                              tag: 0
                            },
                            1: /* Alpha */{
                              0: /* End_of_format */0,
                              length: 1,
                              tag: 15
                            },
                            length: 2,
                            tag: 17
                          },
                          length: 1,
                          tag: 15
                        },
                        1: "%a@ %a",
                        length: 2,
                        tag: 0
                      }), pr_app, f[0], pr_exp0, f[1]);
    
  }
}

function pr_lambda(ppf, e) {
  switch (e.tag | 0) {
    case 0 : 
        return Caml_curry.app8(Format.fprintf(ppf, /* Format */{
                        0: /* Formatting_gen */{
                          0: /* Open_box */{
                            0: /* Format */{
                              0: /* String_literal */{
                                0: "<1>",
                                1: /* End_of_format */0,
                                length: 2,
                                tag: 11
                              },
                              1: "<1>",
                              length: 2,
                              tag: 0
                            },
                            length: 1,
                            tag: 1
                          },
                          1: /* Alpha */{
                            0: /* Alpha */{
                              0: /* Alpha */{
                                0: /* Formatting_lit */{
                                  0: /* Break */{
                                    0: "@ ",
                                    1: 1,
                                    2: 0,
                                    length: 3,
                                    tag: 0
                                  },
                                  1: /* Alpha */{
                                    0: /* Formatting_lit */{
                                      0: /* Close_box */0,
                                      1: /* End_of_format */0,
                                      length: 2,
                                      tag: 17
                                    },
                                    length: 1,
                                    tag: 15
                                  },
                                  length: 2,
                                  tag: 17
                                },
                                length: 1,
                                tag: 15
                              },
                              length: 1,
                              tag: 15
                            },
                            length: 1,
                            tag: 15
                          },
                          length: 2,
                          tag: 18
                        },
                        1: "@[<1>%a%a%a@ %a@]",
                        length: 2,
                        tag: 0
                      }), kwd, "\\", ident, e[0], kwd, ".", pr_lambda, e[1]);
    case 1 : 
    case 2 : 
        return pr_app(ppf, e);
    
  }
}

var string_of_lambda = Caml_curry.app1(Format.asprintf(/* Format */{
          0: /* Alpha */{
            0: /* End_of_format */0,
            length: 1,
            tag: 15
          },
          1: "%a",
          length: 2,
          tag: 0
        }), pr_lambda);

var Lambda_suites = /* module */[
  ident,
  kwd,
  pr_exp0,
  pr_app,
  pr_other_applications,
  pr_lambda,
  string_of_lambda
];

var lambda_suites = /* array */[
  /* tuple */[
    /* Var */{
      0: "x",
      length: 1,
      tag: 1
    },
    "x"
  ],
  /* tuple */[
    /* Apply */{
      0: /* Var */{
        0: "x",
        length: 1,
        tag: 1
      },
      1: /* Var */{
        0: "y",
        length: 1,
        tag: 1
      },
      length: 2,
      tag: 2
    },
    "x y"
  ],
  /* tuple */[
    /* Lambda */{
      0: "z",
      1: /* Apply */{
        0: /* Var */{
          0: "x",
          length: 1,
          tag: 1
        },
        1: /* Var */{
          0: "y",
          length: 1,
          tag: 1
        },
        length: 2,
        tag: 2
      },
      length: 2,
      tag: 0
    },
    "\\z. x y"
  ],
  /* tuple */[
    /* Lambda */{
      0: "z",
      1: /* Lambda */{
        0: "z",
        1: /* Apply */{
          0: /* Var */{
            0: "x",
            length: 1,
            tag: 1
          },
          1: /* Var */{
            0: "y",
            length: 1,
            tag: 1
          },
          length: 2,
          tag: 2
        },
        length: 2,
        tag: 0
      },
      length: 2,
      tag: 0
    },
    "\\z. \\z. x y"
  ]
];

function from_lambda_pairs() {
  return $$Array.to_list($$Array.mapi(function (i, param) {
                  var b = param[1];
                  var a = param[0];
                  return /* tuple */[
                          Caml_curry.app1(Printf.sprintf(/* Format */{
                                    0: /* String_literal */{
                                      0: "lambda_print ",
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
                                    1: "lambda_print %d",
                                    length: 2,
                                    tag: 0
                                  }), i),
                          function () {
                            return /* Eq */{
                                    0: Caml_curry.app1(string_of_lambda, a),
                                    1: b,
                                    length: 2,
                                    tag: 0
                                  };
                          }
                        ];
                }, lambda_suites));
}

var ksprintf_suites_000 = /* tuple */[
  "ksprintf",
  function () {
    var f = function (fmt) {
      return Format.ksprintf(function (x) {
                  return x + x;
                }, fmt);
    };
    return /* Eq */{
            0: Caml_curry.app2(f(/* Format */{
                      0: /* String */{
                        0: /* No_padding */0,
                        1: /* Char_literal */{
                          0: /* " " */32,
                          1: /* String */{
                            0: /* No_padding */0,
                            1: /* String_literal */{
                              0: " a ",
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 11
                            },
                            length: 2,
                            tag: 2
                          },
                          length: 2,
                          tag: 12
                        },
                        length: 2,
                        tag: 2
                      },
                      1: "%s %s a ",
                      length: 2,
                      tag: 0
                    }), "x", "xx"),
            1: "x xx a x xx a ",
            length: 2,
            tag: 0
          };
  }
];

var ksprintf_suites_001 = /* :: */[
  /* tuple */[
    "sprintf",
    function () {
      return /* Eq */{
              0: Caml_curry.app2(Format.sprintf(/* Format */{
                        0: /* String */{
                          0: /* No_padding */0,
                          1: /* Char_literal */{
                            0: /* " " */32,
                            1: /* Caml_string */{
                              0: /* No_padding */0,
                              1: /* End_of_format */0,
                              length: 2,
                              tag: 3
                            },
                            length: 2,
                            tag: 12
                          },
                          length: 2,
                          tag: 2
                        },
                        1: "%s %S",
                        length: 2,
                        tag: 0
                      }), "x", "X"),
              1: 'x "X"',
              length: 2,
              tag: 0
            };
    }
  ],
  /* [] */0
];

var ksprintf_suites = /* :: */[
  ksprintf_suites_000,
  ksprintf_suites_001
];

var a$1 = Format.asprintf;

var int64_suites_000 = /* tuple */[
  "i32_simple",
  function () {
    return /* Eq */{
            0: Caml_curry.app1(Format.asprintf(/* Format */{
                      0: /* Nativeint */{
                        0: /* Int_x */6,
                        1: /* No_padding */0,
                        2: /* No_precision */0,
                        3: /* End_of_format */0,
                        length: 4,
                        tag: 6
                      },
                      1: "%nx",
                      length: 2,
                      tag: 0
                    }), 4294967295),
            1: "ffffffff",
            length: 2,
            tag: 0
          };
  }
];

var int64_suites_001 = /* :: */[
  /* tuple */[
    "i32_simple1",
    function () {
      return /* Eq */{
              0: Caml_curry.app1(Format.asprintf(/* Format */{
                        0: /* Nativeint */{
                          0: /* Int_o */10,
                          1: /* No_padding */0,
                          2: /* No_precision */0,
                          3: /* End_of_format */0,
                          length: 4,
                          tag: 6
                        },
                        1: "%no",
                        length: 2,
                        tag: 0
                      }), 4294967295),
              1: "37777777777",
              length: 2,
              tag: 0
            };
    }
  ],
  /* :: */[
    /* tuple */[
      "i64_simple",
      function () {
        return /* Eq */{
                0: Caml_curry.app1(Format.asprintf(/* Format */{
                          0: /* Int64 */{
                            0: /* Int_d */0,
                            1: /* No_padding */0,
                            2: /* No_precision */0,
                            3: /* End_of_format */0,
                            length: 4,
                            tag: 7
                          },
                          1: "%Ld",
                          length: 2,
                          tag: 0
                        }), /* int64 */[
                      3,
                      0
                    ]),
                1: "3",
                length: 2,
                tag: 0
              };
      }
    ],
    /* :: */[
      /* tuple */[
        "i64_simple2",
        function () {
          return /* Eq */{
                  0: Caml_curry.app1(Format.asprintf(/* Format */{
                            0: /* Int64 */{
                              0: /* Int_x */6,
                              1: /* No_padding */0,
                              2: /* No_precision */0,
                              3: /* End_of_format */0,
                              length: 4,
                              tag: 7
                            },
                            1: "%Lx",
                            length: 2,
                            tag: 0
                          }), /* int64 */[
                        33,
                        0
                      ]),
                  1: "21",
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* :: */[
        /* tuple */[
          "i64_simple3",
          function () {
            return /* Eq */{
                    0: Caml_curry.app1(Format.asprintf(/* Format */{
                              0: /* Int64 */{
                                0: /* Int_i */3,
                                1: /* No_padding */0,
                                2: /* No_precision */0,
                                3: /* End_of_format */0,
                                length: 4,
                                tag: 7
                              },
                              1: "%Li",
                              length: 2,
                              tag: 0
                            }), /* int64 */[
                          33,
                          0
                        ]),
                    1: "33",
                    length: 2,
                    tag: 0
                  };
          }
        ],
        /* :: */[
          /* tuple */[
            "i64_simple4",
            function () {
              return /* Eq */{
                      0: Caml_curry.app2(a$1, /* Format */{
                            0: /* Int64 */{
                              0: /* Int_X */8,
                              1: /* No_padding */0,
                              2: /* No_precision */0,
                              3: /* End_of_format */0,
                              length: 4,
                              tag: 7
                            },
                            1: "%LX",
                            length: 2,
                            tag: 0
                          }, /* int64 */[
                            44,
                            0
                          ]),
                      1: "2C",
                      length: 2,
                      tag: 0
                    };
            }
          ],
          /* :: */[
            /* tuple */[
              "i64_simple5",
              function () {
                return /* Eq */{
                        0: Caml_curry.app2(a$1, /* Format */{
                              0: /* Int64 */{
                                0: /* Int_x */6,
                                1: /* No_padding */0,
                                2: /* No_precision */0,
                                3: /* End_of_format */0,
                                length: 4,
                                tag: 7
                              },
                              1: "%Lx",
                              length: 2,
                              tag: 0
                            }, /* int64 */[
                              44,
                              0
                            ]),
                        1: "2c",
                        length: 2,
                        tag: 0
                      };
              }
            ],
            /* :: */[
              /* tuple */[
                "i64_simple6",
                function () {
                  return /* Eq */{
                          0: Caml_curry.app3(a$1, /* Format */{
                                0: /* Int64 */{
                                  0: /* Int_x */6,
                                  1: /* Arg_padding */{
                                    0: /* Right */1,
                                    length: 1,
                                    tag: 1
                                  },
                                  2: /* No_precision */0,
                                  3: /* End_of_format */0,
                                  length: 4,
                                  tag: 7
                                },
                                1: "%*Lx",
                                length: 2,
                                tag: 0
                              }, 5, /* int64 */[
                                44,
                                0
                              ]),
                          1: "   2c",
                          length: 2,
                          tag: 0
                        };
                }
              ],
              /* :: */[
                /* tuple */[
                  "i64_simple7",
                  function () {
                    return /* Eq */{
                            0: Caml_format.caml_int64_format("%d", /* int64 */[
                                  3333,
                                  0
                                ]),
                            1: "3333",
                            length: 2,
                            tag: 0
                          };
                  }
                ],
                /* :: */[
                  /* tuple */[
                    "i64_simple8",
                    function () {
                      return /* Eq */{
                              0: Caml_curry.app3(a$1, /* Format */{
                                    0: /* Int64 */{
                                      0: /* Int_d */0,
                                      1: /* No_padding */0,
                                      2: /* No_precision */0,
                                      3: /* Int64 */{
                                        0: /* Int_d */0,
                                        1: /* Lit_padding */{
                                          0: /* Zeros */2,
                                          1: 18,
                                          length: 2,
                                          tag: 0
                                        },
                                        2: /* No_precision */0,
                                        3: /* End_of_format */0,
                                        length: 4,
                                        tag: 7
                                      },
                                      length: 4,
                                      tag: 7
                                    },
                                    1: "%Ld%018Ld",
                                    length: 2,
                                    tag: 0
                                  }, /* int64 */[
                                    3,
                                    0
                                  ], /* int64 */[
                                    3,
                                    0
                                  ]),
                              1: "3000000000000000003",
                              length: 2,
                              tag: 0
                            };
                    }
                  ],
                  /* :: */[
                    /* tuple */[
                      "i64_simple9",
                      function () {
                        return /* Eq */{
                                0: Caml_curry.app3(a$1, /* Format */{
                                      0: /* Int64 */{
                                        0: /* Int_d */0,
                                        1: /* No_padding */0,
                                        2: /* No_precision */0,
                                        3: /* Int64 */{
                                          0: /* Int_d */0,
                                          1: /* Lit_padding */{
                                            0: /* Zeros */2,
                                            1: 18,
                                            length: 2,
                                            tag: 0
                                          },
                                          2: /* No_precision */0,
                                          3: /* End_of_format */0,
                                          length: 4,
                                          tag: 7
                                        },
                                        length: 4,
                                        tag: 7
                                      },
                                      1: "%Ld%018Ld",
                                      length: 2,
                                      tag: 0
                                    }, /* int64 */[
                                      1548746752,
                                      107288
                                    ], /* int64 */[
                                      0,
                                      0
                                    ]),
                                1: "460800000000000000000000000000000",
                                length: 2,
                                tag: 0
                              };
                      }
                    ],
                    /* :: */[
                      /* tuple */[
                        "i64_simple10",
                        function () {
                          return /* Eq */{
                                  0: Caml_curry.app2(a$1, /* Format */{
                                        0: /* Int64 */{
                                          0: /* Int_x */6,
                                          1: /* No_padding */0,
                                          2: /* No_precision */0,
                                          3: /* End_of_format */0,
                                          length: 4,
                                          tag: 7
                                        },
                                        1: "%Lx",
                                        length: 2,
                                        tag: 0
                                      }, Int64.max_int),
                                  1: "7fffffffffffffff",
                                  length: 2,
                                  tag: 0
                                };
                        }
                      ],
                      /* :: */[
                        /* tuple */[
                          "i64_simple15",
                          function () {
                            return /* Eq */{
                                    0: Caml_curry.app2(a$1, /* Format */{
                                          0: /* Int64 */{
                                            0: /* Int_d */0,
                                            1: /* No_padding */0,
                                            2: /* No_precision */0,
                                            3: /* End_of_format */0,
                                            length: 4,
                                            tag: 7
                                          },
                                          1: "%Ld",
                                          length: 2,
                                          tag: 0
                                        }, /* int64 */[
                                          -1,
                                          -1
                                        ]),
                                    1: "-1",
                                    length: 2,
                                    tag: 0
                                  };
                          }
                        ],
                        /* :: */[
                          /* tuple */[
                            "i64_simple16",
                            function () {
                              return /* Eq */{
                                      0: Caml_curry.app2(a$1, /* Format */{
                                            0: /* Int64 */{
                                              0: /* Int_d */0,
                                              1: /* No_padding */0,
                                              2: /* No_precision */0,
                                              3: /* End_of_format */0,
                                              length: 4,
                                              tag: 7
                                            },
                                            1: "%Ld",
                                            length: 2,
                                            tag: 0
                                          }, /* int64 */[
                                            -11111,
                                            -1
                                          ]),
                                      1: "-11111",
                                      length: 2,
                                      tag: 0
                                    };
                            }
                          ],
                          /* :: */[
                            /* tuple */[
                              "i64_simple14",
                              function () {
                                return /* Eq */{
                                        0: Caml_curry.app2(a$1, /* Format */{
                                              0: /* Int64 */{
                                                0: /* Int_X */8,
                                                1: /* No_padding */0,
                                                2: /* No_precision */0,
                                                3: /* End_of_format */0,
                                                length: 4,
                                                tag: 7
                                              },
                                              1: "%LX",
                                              length: 2,
                                              tag: 0
                                            }, /* int64 */[
                                              -1,
                                              -1
                                            ]),
                                        1: "FFFFFFFFFFFFFFFF",
                                        length: 2,
                                        tag: 0
                                      };
                              }
                            ],
                            /* :: */[
                              /* tuple */[
                                "i64_simple17",
                                function () {
                                  return /* Eq */{
                                          0: Caml_curry.app2(a$1, /* Format */{
                                                0: /* Int64 */{
                                                  0: /* Int_x */6,
                                                  1: /* No_padding */0,
                                                  2: /* No_precision */0,
                                                  3: /* End_of_format */0,
                                                  length: 4,
                                                  tag: 7
                                                },
                                                1: "%Lx",
                                                length: 2,
                                                tag: 0
                                              }, /* int64 */[
                                                -1,
                                                -1
                                              ]),
                                          1: "ffffffffffffffff",
                                          length: 2,
                                          tag: 0
                                        };
                                }
                              ],
                              /* :: */[
                                /* tuple */[
                                  "i64_simple11",
                                  function () {
                                    return /* Eq */{
                                            0: Caml_curry.app2(a$1, /* Format */{
                                                  0: /* Int64 */{
                                                    0: /* Int_X */8,
                                                    1: /* No_padding */0,
                                                    2: /* No_precision */0,
                                                    3: /* End_of_format */0,
                                                    length: 4,
                                                    tag: 7
                                                  },
                                                  1: "%LX",
                                                  length: 2,
                                                  tag: 0
                                                }, Int64.max_int),
                                            1: "7FFFFFFFFFFFFFFF",
                                            length: 2,
                                            tag: 0
                                          };
                                  }
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "i64_simple12",
                                    function () {
                                      return /* Eq */{
                                              0: Caml_curry.app2(a$1, /* Format */{
                                                    0: /* Int64 */{
                                                      0: /* Int_X */8,
                                                      1: /* No_padding */0,
                                                      2: /* No_precision */0,
                                                      3: /* End_of_format */0,
                                                      length: 4,
                                                      tag: 7
                                                    },
                                                    1: "%LX",
                                                    length: 2,
                                                    tag: 0
                                                  }, Int64.min_int),
                                              1: "8000000000000000",
                                              length: 2,
                                              tag: 0
                                            };
                                    }
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "i64_simple17",
                                      function () {
                                        return /* Eq */{
                                                0: Caml_curry.app2(a$1, /* Format */{
                                                      0: /* Int64 */{
                                                        0: /* Int_u */12,
                                                        1: /* No_padding */0,
                                                        2: /* No_precision */0,
                                                        3: /* End_of_format */0,
                                                        length: 4,
                                                        tag: 7
                                                      },
                                                      1: "%Lu",
                                                      length: 2,
                                                      tag: 0
                                                    }, /* int64 */[
                                                      -1,
                                                      -1
                                                    ]),
                                                1: "18446744073709551615",
                                                length: 2,
                                                tag: 0
                                              };
                                      }
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "i64_simple21",
                                        function () {
                                          return /* Eq */{
                                                  0: Caml_curry.app2(a$1, /* Format */{
                                                        0: /* Int64 */{
                                                          0: /* Int_u */12,
                                                          1: /* No_padding */0,
                                                          2: /* No_precision */0,
                                                          3: /* End_of_format */0,
                                                          length: 4,
                                                          tag: 7
                                                        },
                                                        1: "%Lu",
                                                        length: 2,
                                                        tag: 0
                                                      }, /* int64 */[
                                                        -10000,
                                                        -1
                                                      ]),
                                                  1: "18446744073709541616",
                                                  length: 2,
                                                  tag: 0
                                                };
                                        }
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "i64_simple19",
                                          function () {
                                            return /* Eq */{
                                                    0: Caml_curry.app2(a$1, /* Format */{
                                                          0: /* Int64 */{
                                                            0: /* Int_o */10,
                                                            1: /* No_padding */0,
                                                            2: /* No_precision */0,
                                                            3: /* End_of_format */0,
                                                            length: 4,
                                                            tag: 7
                                                          },
                                                          1: "%Lo",
                                                          length: 2,
                                                          tag: 0
                                                        }, Int64.min_int),
                                                    1: "1000000000000000000000",
                                                    length: 2,
                                                    tag: 0
                                                  };
                                          }
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "i64_simple13",
                                            function () {
                                              return /* Eq */{
                                                      0: Caml_curry.app2(a$1, /* Format */{
                                                            0: /* Int64 */{
                                                              0: /* Int_X */8,
                                                              1: /* No_padding */0,
                                                              2: /* No_precision */0,
                                                              3: /* End_of_format */0,
                                                              length: 4,
                                                              tag: 7
                                                            },
                                                            1: "%LX",
                                                            length: 2,
                                                            tag: 0
                                                          }, Caml_int64.add(Int64.min_int, /* int64 */[
                                                                1,
                                                                0
                                                              ])),
                                                      1: "8000000000000001",
                                                      length: 2,
                                                      tag: 0
                                                    };
                                            }
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "i64_simple20",
                                              function () {
                                                return /* Eq */{
                                                        0: Caml_curry.app2(a$1, /* Format */{
                                                              0: /* Int64 */{
                                                                0: /* Int_x */6,
                                                                1: /* Lit_padding */{
                                                                  0: /* Right */1,
                                                                  1: 12,
                                                                  length: 2,
                                                                  tag: 0
                                                                },
                                                                2: /* No_precision */0,
                                                                3: /* End_of_format */0,
                                                                length: 4,
                                                                tag: 7
                                                              },
                                                              1: "%12Lx",
                                                              length: 2,
                                                              tag: 0
                                                            }, /* int64 */[
                                                              3,
                                                              0
                                                            ]),
                                                        1: "           3",
                                                        length: 2,
                                                        tag: 0
                                                      };
                                              }
                                            ],
                                            /* [] */0
                                          ]
                                        ]
                                      ]
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
];

var int64_suites = /* :: */[
  int64_suites_000,
  int64_suites_001
];

Mt.from_pair_suites("caml_format_test.ml", Pervasives.$at(suites, Pervasives.$at(formatter_suites, Pervasives.$at(from_lambda_pairs(lambda_suites), Pervasives.$at(ksprintf_suites, Pervasives.$at($$Array.to_list($$Array.mapi(function (i, param) {
                                  var str_result = param[2];
                                  var f = param[1];
                                  var fmt = param[0];
                                  return /* tuple */[
                                          Caml_curry.app1(Printf.sprintf(/* Format */{
                                                    0: /* String_literal */{
                                                      0: "float_format ",
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
                                                    1: "float_format %d",
                                                    length: 2,
                                                    tag: 0
                                                  }), i),
                                          function () {
                                            return /* Eq */{
                                                    0: Caml_format.caml_format_float(fmt, f),
                                                    1: str_result,
                                                    length: 2,
                                                    tag: 0
                                                  };
                                          }
                                        ];
                                }, float_data)), int64_suites))))));

var float_suites = /* :: */[
  "float_nan",
  /* [] */0
];

var hh = /* int64 */[
  -858993460,
  214748364
];

var hhh = /* int64 */[
  0,
  268435456
];

exports.of_string            = of_string;
exports.from_float_of_string = from_float_of_string;
exports.from_of_string       = from_of_string;
exports.u                    = u;
exports.to_str               = to_str;
exports.v                    = v;
exports.suites               = suites;
exports.ff                   = ff;
exports.a                    = a;
exports.formatter_suites     = formatter_suites;
exports.float_data           = float_data;
exports.float_suites         = float_suites;
exports.Lambda_suites        = Lambda_suites;
exports.lambda_suites        = lambda_suites;
exports.from_lambda_pairs    = from_lambda_pairs;
exports.ksprintf_suites      = ksprintf_suites;
exports.int64_suites         = int64_suites;
exports.hh                   = hh;
exports.hhh                  = hhh;
/* v Not a pure module */
