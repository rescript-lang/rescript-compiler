'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Int64 = require("../../lib/js/int64.js");
var $$Buffer = require("../../lib/js/buffer.js");
var Format = require("../../lib/js/format.js");
var Printf = require("../../lib/js/printf.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_format = require("../../lib/js/caml_format.js");

var of_string = /* array */[
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
];

function from_float_of_string(xs) {
  return $$Array.mapi((function (i, param) {
                return Pervasives.string_of_float;
              }), xs);
}

function from_of_string(xs) {
  return $$Array.to_list($$Array.mapi((function (i, param) {
                    var b = param[1];
                    var a = param[0];
                    return /* tuple */[
                            Curry._1(Printf.sprintf(/* Format */[
                                      /* String_literal */Block.__(11, [
                                          "of_string ",
                                          /* Scan_get_counter */Block.__(21, [
                                              /* Token_counter */2,
                                              /* End_of_format */0
                                            ])
                                        ]),
                                      "of_string %L"
                                    ]), i),
                            (function (param) {
                                return /* Eq */Block.__(0, [
                                          Caml_format.caml_int_of_string(b),
                                          a
                                        ]);
                              })
                          ];
                  }), of_string));
}

function u(v) {
  return Curry._1(Printf.sprintf(/* Format */[
                  /* Int */Block.__(4, [
                      /* Int_d */0,
                      /* Lit_padding */Block.__(0, [
                          /* Right */1,
                          33
                        ]),
                      /* No_precision */0,
                      /* End_of_format */0
                    ]),
                  "%33d"
                ]), v);
}

var to_str = Caml_format.caml_int_of_string;

var v = Curry._1(Printf.sprintf(/* Format */[
          /* Int */Block.__(4, [
              /* Int_d */0,
              /* Lit_padding */Block.__(0, [
                  /* Right */1,
                  3
                ]),
              /* No_precision */0,
              /* End_of_format */0
            ]),
          "%3d"
        ]), 3333);

var pairs = /* array */[
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
];

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
            (function (param) {
                return /* Eq */Block.__(0, [
                          true,
                          Pervasives.classify_float(Caml_format.caml_float_of_string("nan")) === /* FP_nan */4
                        ]);
              })
          ],
          /* [] */0
        ], Pervasives.$at($$Array.to_list($$Array.mapi((function (i, param) {
                        var b = param[1];
                        var a = param[0];
                        return /* tuple */[
                                Curry._1(Printf.sprintf(/* Format */[
                                          /* String_literal */Block.__(11, [
                                              "infinity_of_string ",
                                              /* Int */Block.__(4, [
                                                  /* Int_d */0,
                                                  /* No_padding */0,
                                                  /* No_precision */0,
                                                  /* End_of_format */0
                                                ])
                                            ]),
                                          "infinity_of_string %d"
                                        ]), i),
                                (function (param) {
                                    return /* Eq */Block.__(0, [
                                              a,
                                              Pervasives.classify_float(Caml_format.caml_float_of_string(b))
                                            ]);
                                  })
                              ];
                      }), pairs)), Pervasives.$at(/* :: */[
                  /* tuple */[
                    "throw",
                    (function (param) {
                        return /* ThrowAny */Block.__(7, [(function (param) {
                                      Caml_format.caml_float_of_string("");
                                      return /* () */0;
                                    })]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "format_int",
                      (function (param) {
                          return /* Eq */Block.__(0, [
                                    "                              33",
                                    Caml_format.caml_format_int("%32d", 33)
                                  ]);
                        })
                    ],
                    /* [] */0
                  ]
                ], $$Array.to_list($$Array.mapi((function (i, param) {
                            var b = param[1];
                            var a = param[0];
                            return /* tuple */[
                                    Curry._1(Printf.sprintf(/* Format */[
                                              /* String_literal */Block.__(11, [
                                                  "normal_float_of_string ",
                                                  /* Int */Block.__(4, [
                                                      /* Int_d */0,
                                                      /* No_padding */0,
                                                      /* No_precision */0,
                                                      /* End_of_format */0
                                                    ])
                                                ]),
                                              "normal_float_of_string %d"
                                            ]), i),
                                    (function (param) {
                                        return /* Eq */Block.__(0, [
                                                  a,
                                                  Caml_format.caml_float_of_string(b)
                                                ]);
                                      })
                                  ];
                          }), pairs$1))))));

function ff(param) {
  return Caml_format.caml_format_int("%32d", param);
}

var formatter_suites_000 = /* tuple */[
  "fmt_concat",
  (function (param) {
      return /* Eq */Block.__(0, [
                Curry._6(Format.asprintf(Pervasives.$caret$caret(/* Format */[
                              /* String */Block.__(2, [
                                  /* No_padding */0,
                                  /* Char_literal */Block.__(12, [
                                      /* " " */32,
                                      /* Int */Block.__(4, [
                                          /* Int_d */0,
                                          /* Lit_padding */Block.__(0, [
                                              /* Zeros */2,
                                              3
                                            ]),
                                          /* No_precision */0,
                                          /* Char_literal */Block.__(12, [
                                              /* " " */32,
                                              /* Scan_get_counter */Block.__(21, [
                                                  /* Token_counter */2,
                                                  /* End_of_format */0
                                                ])
                                            ])
                                        ])
                                    ])
                                ]),
                              "%s %03d %L"
                            ], /* Format */[
                              /* Caml_string */Block.__(3, [
                                  /* No_padding */0,
                                  /* Char_literal */Block.__(12, [
                                      /* " " */32,
                                      /* Int */Block.__(4, [
                                          /* Int_d */0,
                                          /* Lit_padding */Block.__(0, [
                                              /* Zeros */2,
                                              3
                                            ]),
                                          /* No_precision */0,
                                          /* Char_literal */Block.__(12, [
                                              /* " " */32,
                                              /* Scan_get_counter */Block.__(21, [
                                                  /* Token_counter */2,
                                                  /* End_of_format */0
                                                ])
                                            ])
                                        ])
                                    ])
                                ]),
                              "%S %03d %L"
                            ])), "32", 33, 33, "a", 33, 3),
                "32 033 33\"a\" 033 3"
              ]);
    })
];

var formatter_suites_001 = /* :: */[
  /* tuple */[
    "fmt_gen",
    (function (param) {
        return /* Eq */Block.__(0, [
                  Curry._8(Format.asprintf(Pervasives.$caret$caret(/* Format */[
                                /* String */Block.__(2, [
                                    /* No_padding */0,
                                    /* Char_literal */Block.__(12, [
                                        /* " " */32,
                                        /* Int */Block.__(4, [
                                            /* Int_d */0,
                                            /* Lit_padding */Block.__(0, [
                                                /* Zeros */2,
                                                3
                                              ]),
                                            /* No_precision */0,
                                            /* Char_literal */Block.__(12, [
                                                /* " " */32,
                                                /* Scan_get_counter */Block.__(21, [
                                                    /* Token_counter */2,
                                                    /* End_of_format */0
                                                  ])
                                              ])
                                          ])
                                      ])
                                  ]),
                                "%s %03d %L"
                              ], /* Format */[
                                /* Caml_string */Block.__(3, [
                                    /* No_padding */0,
                                    /* Char_literal */Block.__(12, [
                                        /* " " */32,
                                        /* Int */Block.__(4, [
                                            /* Int_d */0,
                                            /* Lit_padding */Block.__(0, [
                                                /* Zeros */2,
                                                3
                                              ]),
                                            /* No_precision */0,
                                            /* Char_literal */Block.__(12, [
                                                /* " " */32,
                                                /* Scan_get_counter */Block.__(21, [
                                                    /* Token_counter */2,
                                                    /* Char_literal */Block.__(12, [
                                                        /* " " */32,
                                                        /* Alpha */Block.__(15, [/* End_of_format */0])
                                                      ])
                                                  ])
                                              ])
                                          ])
                                      ])
                                  ]),
                                "%S %03d %L %a"
                              ])), "32", 33, 33, "a", 33, 3, (function (param, param$1) {
                          return Format.pp_print_list(undefined, Format.pp_print_int, param, param$1);
                        }), /* :: */[
                        1,
                        /* :: */[
                          2,
                          /* :: */[
                            3,
                            /* [] */0
                          ]
                        ]
                      ]),
                  "32 033 33\"a\" 033 3 12\n3"
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "long_fmt",
      (function (param) {
          return /* Eq */Block.__(0, [
                    Curry.app(Format.asprintf(/* Format */[
                              /* Int */Block.__(4, [
                                  /* Int_d */0,
                                  /* No_padding */0,
                                  /* No_precision */0,
                                  /* Char_literal */Block.__(12, [
                                      /* " " */32,
                                      /* Int */Block.__(4, [
                                          /* Int_i */3,
                                          /* No_padding */0,
                                          /* No_precision */0,
                                          /* Char_literal */Block.__(12, [
                                              /* " " */32,
                                              /* Int */Block.__(4, [
                                                  /* Int_u */12,
                                                  /* No_padding */0,
                                                  /* No_precision */0,
                                                  /* Char_literal */Block.__(12, [
                                                      /* " " */32,
                                                      /* Scan_get_counter */Block.__(21, [
                                                          /* Char_counter */1,
                                                          /* Char_literal */Block.__(12, [
                                                              /* " " */32,
                                                              /* Scan_get_counter */Block.__(21, [
                                                                  /* Line_counter */0,
                                                                  /* Char_literal */Block.__(12, [
                                                                      /* " " */32,
                                                                      /* Scan_get_counter */Block.__(21, [
                                                                          /* Token_counter */2,
                                                                          /* Char_literal */Block.__(12, [
                                                                              /* " " */32,
                                                                              /* Scan_get_counter */Block.__(21, [
                                                                                  /* Token_counter */2,
                                                                                  /* Char_literal */Block.__(12, [
                                                                                      /* " " */32,
                                                                                      /* Int */Block.__(4, [
                                                                                          /* Int_x */6,
                                                                                          /* No_padding */0,
                                                                                          /* No_precision */0,
                                                                                          /* Char_literal */Block.__(12, [
                                                                                              /* " " */32,
                                                                                              /* Int */Block.__(4, [
                                                                                                  /* Int_X */8,
                                                                                                  /* No_padding */0,
                                                                                                  /* No_precision */0,
                                                                                                  /* Char_literal */Block.__(12, [
                                                                                                      /* " " */32,
                                                                                                      /* Int */Block.__(4, [
                                                                                                          /* Int_o */10,
                                                                                                          /* No_padding */0,
                                                                                                          /* No_precision */0,
                                                                                                          /* Char_literal */Block.__(12, [
                                                                                                              /* " " */32,
                                                                                                              /* String */Block.__(2, [
                                                                                                                  /* No_padding */0,
                                                                                                                  /* Char_literal */Block.__(12, [
                                                                                                                      /* " " */32,
                                                                                                                      /* Caml_string */Block.__(3, [
                                                                                                                          /* No_padding */0,
                                                                                                                          /* Char_literal */Block.__(12, [
                                                                                                                              /* " " */32,
                                                                                                                              /* Char */Block.__(0, [/* Char_literal */Block.__(12, [
                                                                                                                                      /* " " */32,
                                                                                                                                      /* Caml_char */Block.__(1, [/* Char_literal */Block.__(12, [
                                                                                                                                              /* " " */32,
                                                                                                                                              /* Float */Block.__(8, [
                                                                                                                                                  /* Float_f */0,
                                                                                                                                                  /* No_padding */0,
                                                                                                                                                  /* No_precision */0,
                                                                                                                                                  /* Char_literal */Block.__(12, [
                                                                                                                                                      /* " " */32,
                                                                                                                                                      /* Float */Block.__(8, [
                                                                                                                                                          /* Float_F */15,
                                                                                                                                                          /* No_padding */0,
                                                                                                                                                          /* No_precision */0,
                                                                                                                                                          /* Char_literal */Block.__(12, [
                                                                                                                                                              /* " " */32,
                                                                                                                                                              /* Float */Block.__(8, [
                                                                                                                                                                  /* Float_e */3,
                                                                                                                                                                  /* No_padding */0,
                                                                                                                                                                  /* No_precision */0,
                                                                                                                                                                  /* Char_literal */Block.__(12, [
                                                                                                                                                                      /* " " */32,
                                                                                                                                                                      /* Float */Block.__(8, [
                                                                                                                                                                          /* Float_E */6,
                                                                                                                                                                          /* No_padding */0,
                                                                                                                                                                          /* No_precision */0,
                                                                                                                                                                          /* Char_literal */Block.__(12, [
                                                                                                                                                                              /* " " */32,
                                                                                                                                                                              /* Float */Block.__(8, [
                                                                                                                                                                                  /* Float_g */9,
                                                                                                                                                                                  /* No_padding */0,
                                                                                                                                                                                  /* No_precision */0,
                                                                                                                                                                                  /* Char_literal */Block.__(12, [
                                                                                                                                                                                      /* " " */32,
                                                                                                                                                                                      /* Float */Block.__(8, [
                                                                                                                                                                                          /* Float_G */12,
                                                                                                                                                                                          /* No_padding */0,
                                                                                                                                                                                          /* No_precision */0,
                                                                                                                                                                                          /* Char_literal */Block.__(12, [
                                                                                                                                                                                              /* " " */32,
                                                                                                                                                                                              /* Bool */Block.__(9, [
                                                                                                                                                                                                  /* No_padding */0,
                                                                                                                                                                                                  /* Char_literal */Block.__(12, [
                                                                                                                                                                                                      /* " " */32,
                                                                                                                                                                                                      /* Bool */Block.__(9, [
                                                                                                                                                                                                          /* No_padding */0,
                                                                                                                                                                                                          /* Char_literal */Block.__(12, [
                                                                                                                                                                                                              /* " " */32,
                                                                                                                                                                                                              /* Int32 */Block.__(5, [
                                                                                                                                                                                                                  /* Int_d */0,
                                                                                                                                                                                                                  /* No_padding */0,
                                                                                                                                                                                                                  /* No_precision */0,
                                                                                                                                                                                                                  /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                      /* " " */32,
                                                                                                                                                                                                                      /* Int32 */Block.__(5, [
                                                                                                                                                                                                                          /* Int_i */3,
                                                                                                                                                                                                                          /* No_padding */0,
                                                                                                                                                                                                                          /* No_precision */0,
                                                                                                                                                                                                                          /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                              /* " " */32,
                                                                                                                                                                                                                              /* Int32 */Block.__(5, [
                                                                                                                                                                                                                                  /* Int_u */12,
                                                                                                                                                                                                                                  /* No_padding */0,
                                                                                                                                                                                                                                  /* No_precision */0,
                                                                                                                                                                                                                                  /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                      /* " " */32,
                                                                                                                                                                                                                                      /* Int32 */Block.__(5, [
                                                                                                                                                                                                                                          /* Int_x */6,
                                                                                                                                                                                                                                          /* No_padding */0,
                                                                                                                                                                                                                                          /* No_precision */0,
                                                                                                                                                                                                                                          /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                              /* " " */32,
                                                                                                                                                                                                                                              /* Int32 */Block.__(5, [
                                                                                                                                                                                                                                                  /* Int_X */8,
                                                                                                                                                                                                                                                  /* No_padding */0,
                                                                                                                                                                                                                                                  /* No_precision */0,
                                                                                                                                                                                                                                                  /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                                      /* " " */32,
                                                                                                                                                                                                                                                      /* Int32 */Block.__(5, [
                                                                                                                                                                                                                                                          /* Int_o */10,
                                                                                                                                                                                                                                                          /* No_padding */0,
                                                                                                                                                                                                                                                          /* No_precision */0,
                                                                                                                                                                                                                                                          /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                                              /* " " */32,
                                                                                                                                                                                                                                                              /* Nativeint */Block.__(6, [
                                                                                                                                                                                                                                                                  /* Int_d */0,
                                                                                                                                                                                                                                                                  /* No_padding */0,
                                                                                                                                                                                                                                                                  /* No_precision */0,
                                                                                                                                                                                                                                                                  /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                                                      /* " " */32,
                                                                                                                                                                                                                                                                      /* Nativeint */Block.__(6, [
                                                                                                                                                                                                                                                                          /* Int_i */3,
                                                                                                                                                                                                                                                                          /* No_padding */0,
                                                                                                                                                                                                                                                                          /* No_precision */0,
                                                                                                                                                                                                                                                                          /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                                                              /* " " */32,
                                                                                                                                                                                                                                                                              /* Nativeint */Block.__(6, [
                                                                                                                                                                                                                                                                                  /* Int_u */12,
                                                                                                                                                                                                                                                                                  /* No_padding */0,
                                                                                                                                                                                                                                                                                  /* No_precision */0,
                                                                                                                                                                                                                                                                                  /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                                                                      /* " " */32,
                                                                                                                                                                                                                                                                                      /* Nativeint */Block.__(6, [
                                                                                                                                                                                                                                                                                          /* Int_x */6,
                                                                                                                                                                                                                                                                                          /* No_padding */0,
                                                                                                                                                                                                                                                                                          /* No_precision */0,
                                                                                                                                                                                                                                                                                          /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                                                                              /* " " */32,
                                                                                                                                                                                                                                                                                              /* Nativeint */Block.__(6, [
                                                                                                                                                                                                                                                                                                  /* Int_x */6,
                                                                                                                                                                                                                                                                                                  /* No_padding */0,
                                                                                                                                                                                                                                                                                                  /* No_precision */0,
                                                                                                                                                                                                                                                                                                  /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                                                                                      /* " " */32,
                                                                                                                                                                                                                                                                                                      /* Nativeint */Block.__(6, [
                                                                                                                                                                                                                                                                                                          /* Int_o */10,
                                                                                                                                                                                                                                                                                                          /* No_padding */0,
                                                                                                                                                                                                                                                                                                          /* No_precision */0,
                                                                                                                                                                                                                                                                                                          /* String_literal */Block.__(11, [
                                                                                                                                                                                                                                                                                                              "  ",
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
                                                                                                                                                                                                                                                                        ])
                                                                                                                                                                                                                                                                    ])
                                                                                                                                                                                                                                                                ])
                                                                                                                                                                                                                                                            ])
                                                                                                                                                                                                                                                        ])
                                                                                                                                                                                                                                                    ])
                                                                                                                                                                                                                                                ])
                                                                                                                                                                                                                                            ])
                                                                                                                                                                                                                                        ])
                                                                                                                                                                                                                                    ])
                                                                                                                                                                                                                                ])
                                                                                                                                                                                                                            ])
                                                                                                                                                                                                                        ])
                                                                                                                                                                                                                    ])
                                                                                                                                                                                                                ])
                                                                                                                                                                                                            ])
                                                                                                                                                                                                        ])
                                                                                                                                                                                                    ])
                                                                                                                                                                                                ])
                                                                                                                                                                                            ])
                                                                                                                                                                                        ])
                                                                                                                                                                                    ])
                                                                                                                                                                                ])
                                                                                                                                                                            ])
                                                                                                                                                                        ])
                                                                                                                                                                    ])
                                                                                                                                                                ])
                                                                                                                                                            ])
                                                                                                                                                        ])
                                                                                                                                                    ])
                                                                                                                                                ])
                                                                                                                                            ])])
                                                                                                                                    ])])
                                                                                                                            ])
                                                                                                                        ])
                                                                                                                    ])
                                                                                                                ])
                                                                                                            ])
                                                                                                        ])
                                                                                                    ])
                                                                                                ])
                                                                                            ])
                                                                                        ])
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
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
                              "%d %i %u %n %l %L %N %x %X %o %s %S %c %C %f %F %e %E %g %G %B %b %ld %li %lu %lx %lX %lo %nd %ni %nu %nx %nx %no  "
                            ]), [
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
                          true,
                          false,
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
                    "1 2 3 4 5 6 7 8 9 12 a \"b\" c 'd' 1.000000 2. 3.000000e+00 4.000000E+00 5 6 true false 0 1 2 3 4 5 6 7 8 9 a 13  "
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "long_fmt_2",
        (function (param) {
            return /* Eq */Block.__(0, [
                      Curry.app(Format.asprintf(/* Format */[
                                /* Formatting_gen */Block.__(18, [
                                    /* Open_box */Block.__(1, [/* Format */[
                                          /* End_of_format */0,
                                          ""
                                        ]]),
                                    /* Int */Block.__(4, [
                                        /* Int_d */0,
                                        /* Lit_padding */Block.__(0, [
                                            /* Right */1,
                                            23
                                          ]),
                                        /* No_precision */0,
                                        /* Char_literal */Block.__(12, [
                                            /* " " */32,
                                            /* Int */Block.__(4, [
                                                /* Int_i */3,
                                                /* Lit_padding */Block.__(0, [
                                                    /* Right */1,
                                                    2
                                                  ]),
                                                /* No_precision */0,
                                                /* Char_literal */Block.__(12, [
                                                    /* " " */32,
                                                    /* Int */Block.__(4, [
                                                        /* Int_u */12,
                                                        /* Lit_padding */Block.__(0, [
                                                            /* Right */1,
                                                            3
                                                          ]),
                                                        /* No_precision */0,
                                                        /* Char_literal */Block.__(12, [
                                                            /* " " */32,
                                                            /* Scan_get_counter */Block.__(21, [
                                                                /* Char_counter */1,
                                                                /* Char_literal */Block.__(12, [
                                                                    /* " " */32,
                                                                    /* Int */Block.__(4, [
                                                                        /* Int_x */6,
                                                                        /* Lit_padding */Block.__(0, [
                                                                            /* Right */1,
                                                                            0
                                                                          ]),
                                                                        /* No_precision */0,
                                                                        /* String_literal */Block.__(11, [
                                                                            "l ",
                                                                            /* Int */Block.__(4, [
                                                                                /* Int_x */6,
                                                                                /* Lit_padding */Block.__(0, [
                                                                                    /* Right */1,
                                                                                    0
                                                                                  ]),
                                                                                /* No_precision */0,
                                                                                /* String_literal */Block.__(11, [
                                                                                    "L ",
                                                                                    /* Scan_get_counter */Block.__(21, [
                                                                                        /* Token_counter */2,
                                                                                        /* Char_literal */Block.__(12, [
                                                                                            /* " " */32,
                                                                                            /* Int */Block.__(4, [
                                                                                                /* Int_x */6,
                                                                                                /* Lit_padding */Block.__(0, [
                                                                                                    /* Zeros */2,
                                                                                                    3
                                                                                                  ]),
                                                                                                /* No_precision */0,
                                                                                                /* Char_literal */Block.__(12, [
                                                                                                    /* " " */32,
                                                                                                    /* Int */Block.__(4, [
                                                                                                        /* Int_X */8,
                                                                                                        /* No_padding */0,
                                                                                                        /* No_precision */0,
                                                                                                        /* Char_literal */Block.__(12, [
                                                                                                            /* " " */32,
                                                                                                            /* Int */Block.__(4, [
                                                                                                                /* Int_o */10,
                                                                                                                /* No_padding */0,
                                                                                                                /* No_precision */0,
                                                                                                                /* Char_literal */Block.__(12, [
                                                                                                                    /* " " */32,
                                                                                                                    /* String */Block.__(2, [
                                                                                                                        /* No_padding */0,
                                                                                                                        /* Char_literal */Block.__(12, [
                                                                                                                            /* " " */32,
                                                                                                                            /* Caml_string */Block.__(3, [
                                                                                                                                /* No_padding */0,
                                                                                                                                /* Char_literal */Block.__(12, [
                                                                                                                                    /* " " */32,
                                                                                                                                    /* Char */Block.__(0, [/* Char_literal */Block.__(12, [
                                                                                                                                            /* " " */32,
                                                                                                                                            /* Caml_char */Block.__(1, [/* Char_literal */Block.__(12, [
                                                                                                                                                    /* " " */32,
                                                                                                                                                    /* Float */Block.__(8, [
                                                                                                                                                        /* Float_f */0,
                                                                                                                                                        /* Lit_padding */Block.__(0, [
                                                                                                                                                            /* Right */1,
                                                                                                                                                            3
                                                                                                                                                          ]),
                                                                                                                                                        /* No_precision */0,
                                                                                                                                                        /* Char_literal */Block.__(12, [
                                                                                                                                                            /* " " */32,
                                                                                                                                                            /* Float */Block.__(8, [
                                                                                                                                                                /* Float_F */15,
                                                                                                                                                                /* Lit_padding */Block.__(0, [
                                                                                                                                                                    /* Right */1,
                                                                                                                                                                    2
                                                                                                                                                                  ]),
                                                                                                                                                                /* No_precision */0,
                                                                                                                                                                /* Char_literal */Block.__(12, [
                                                                                                                                                                    /* " " */32,
                                                                                                                                                                    /* Float */Block.__(8, [
                                                                                                                                                                        /* Float_e */3,
                                                                                                                                                                        /* Lit_padding */Block.__(0, [
                                                                                                                                                                            /* Right */1,
                                                                                                                                                                            2
                                                                                                                                                                          ]),
                                                                                                                                                                        /* No_precision */0,
                                                                                                                                                                        /* Char_literal */Block.__(12, [
                                                                                                                                                                            /* " " */32,
                                                                                                                                                                            /* Float */Block.__(8, [
                                                                                                                                                                                /* Float_E */6,
                                                                                                                                                                                /* No_padding */0,
                                                                                                                                                                                /* No_precision */0,
                                                                                                                                                                                /* Char_literal */Block.__(12, [
                                                                                                                                                                                    /* " " */32,
                                                                                                                                                                                    /* Float */Block.__(8, [
                                                                                                                                                                                        /* Float_g */9,
                                                                                                                                                                                        /* No_padding */0,
                                                                                                                                                                                        /* No_precision */0,
                                                                                                                                                                                        /* Char_literal */Block.__(12, [
                                                                                                                                                                                            /* " " */32,
                                                                                                                                                                                            /* Float */Block.__(8, [
                                                                                                                                                                                                /* Float_G */12,
                                                                                                                                                                                                /* No_padding */0,
                                                                                                                                                                                                /* No_precision */0,
                                                                                                                                                                                                /* Char_literal */Block.__(12, [
                                                                                                                                                                                                    /* " " */32,
                                                                                                                                                                                                    /* Bool */Block.__(9, [
                                                                                                                                                                                                        /* No_padding */0,
                                                                                                                                                                                                        /* Char_literal */Block.__(12, [
                                                                                                                                                                                                            /* " " */32,
                                                                                                                                                                                                            /* Bool */Block.__(9, [
                                                                                                                                                                                                                /* No_padding */0,
                                                                                                                                                                                                                /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                    /* " " */32,
                                                                                                                                                                                                                    /* Int32 */Block.__(5, [
                                                                                                                                                                                                                        /* Int_d */0,
                                                                                                                                                                                                                        /* No_padding */0,
                                                                                                                                                                                                                        /* No_precision */0,
                                                                                                                                                                                                                        /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                            /* " " */32,
                                                                                                                                                                                                                            /* Int32 */Block.__(5, [
                                                                                                                                                                                                                                /* Int_i */3,
                                                                                                                                                                                                                                /* No_padding */0,
                                                                                                                                                                                                                                /* No_precision */0,
                                                                                                                                                                                                                                /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                    /* " " */32,
                                                                                                                                                                                                                                    /* Int32 */Block.__(5, [
                                                                                                                                                                                                                                        /* Int_u */12,
                                                                                                                                                                                                                                        /* No_padding */0,
                                                                                                                                                                                                                                        /* No_precision */0,
                                                                                                                                                                                                                                        /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                            /* " " */32,
                                                                                                                                                                                                                                            /* Int32 */Block.__(5, [
                                                                                                                                                                                                                                                /* Int_x */6,
                                                                                                                                                                                                                                                /* No_padding */0,
                                                                                                                                                                                                                                                /* No_precision */0,
                                                                                                                                                                                                                                                /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                                    /* " " */32,
                                                                                                                                                                                                                                                    /* Int32 */Block.__(5, [
                                                                                                                                                                                                                                                        /* Int_X */8,
                                                                                                                                                                                                                                                        /* No_padding */0,
                                                                                                                                                                                                                                                        /* No_precision */0,
                                                                                                                                                                                                                                                        /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                                            /* " " */32,
                                                                                                                                                                                                                                                            /* Int32 */Block.__(5, [
                                                                                                                                                                                                                                                                /* Int_o */10,
                                                                                                                                                                                                                                                                /* No_padding */0,
                                                                                                                                                                                                                                                                /* No_precision */0,
                                                                                                                                                                                                                                                                /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                                                    /* " " */32,
                                                                                                                                                                                                                                                                    /* Nativeint */Block.__(6, [
                                                                                                                                                                                                                                                                        /* Int_d */0,
                                                                                                                                                                                                                                                                        /* No_padding */0,
                                                                                                                                                                                                                                                                        /* No_precision */0,
                                                                                                                                                                                                                                                                        /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                                                            /* " " */32,
                                                                                                                                                                                                                                                                            /* Nativeint */Block.__(6, [
                                                                                                                                                                                                                                                                                /* Int_i */3,
                                                                                                                                                                                                                                                                                /* No_padding */0,
                                                                                                                                                                                                                                                                                /* No_precision */0,
                                                                                                                                                                                                                                                                                /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                                                                    /* " " */32,
                                                                                                                                                                                                                                                                                    /* Nativeint */Block.__(6, [
                                                                                                                                                                                                                                                                                        /* Int_u */12,
                                                                                                                                                                                                                                                                                        /* No_padding */0,
                                                                                                                                                                                                                                                                                        /* No_precision */0,
                                                                                                                                                                                                                                                                                        /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                                                                            /* " " */32,
                                                                                                                                                                                                                                                                                            /* Nativeint */Block.__(6, [
                                                                                                                                                                                                                                                                                                /* Int_x */6,
                                                                                                                                                                                                                                                                                                /* No_padding */0,
                                                                                                                                                                                                                                                                                                /* No_precision */0,
                                                                                                                                                                                                                                                                                                /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                                                                                    /* " " */32,
                                                                                                                                                                                                                                                                                                    /* Nativeint */Block.__(6, [
                                                                                                                                                                                                                                                                                                        /* Int_x */6,
                                                                                                                                                                                                                                                                                                        /* No_padding */0,
                                                                                                                                                                                                                                                                                                        /* No_precision */0,
                                                                                                                                                                                                                                                                                                        /* Char_literal */Block.__(12, [
                                                                                                                                                                                                                                                                                                            /* " " */32,
                                                                                                                                                                                                                                                                                                            /* Nativeint */Block.__(6, [
                                                                                                                                                                                                                                                                                                                /* Int_o */10,
                                                                                                                                                                                                                                                                                                                /* No_padding */0,
                                                                                                                                                                                                                                                                                                                /* No_precision */0,
                                                                                                                                                                                                                                                                                                                /* String_literal */Block.__(11, [
                                                                                                                                                                                                                                                                                                                    "  ",
                                                                                                                                                                                                                                                                                                                    /* Formatting_lit */Block.__(17, [
                                                                                                                                                                                                                                                                                                                        /* Close_box */0,
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
                                                                                                                                                                                                                                                                                  ])
                                                                                                                                                                                                                                                                              ])
                                                                                                                                                                                                                                                                          ])
                                                                                                                                                                                                                                                                      ])
                                                                                                                                                                                                                                                                  ])
                                                                                                                                                                                                                                                              ])
                                                                                                                                                                                                                                                          ])
                                                                                                                                                                                                                                                      ])
                                                                                                                                                                                                                                                  ])
                                                                                                                                                                                                                                              ])
                                                                                                                                                                                                                                          ])
                                                                                                                                                                                                                                      ])
                                                                                                                                                                                                                                  ])
                                                                                                                                                                                                                              ])
                                                                                                                                                                                                                          ])
                                                                                                                                                                                                                      ])
                                                                                                                                                                                                                  ])
                                                                                                                                                                                                              ])
                                                                                                                                                                                                          ])
                                                                                                                                                                                                      ])
                                                                                                                                                                                                  ])
                                                                                                                                                                                              ])
                                                                                                                                                                                          ])
                                                                                                                                                                                      ])
                                                                                                                                                                                  ])
                                                                                                                                                                              ])
                                                                                                                                                                          ])
                                                                                                                                                                      ])
                                                                                                                                                                  ])
                                                                                                                                                              ])
                                                                                                                                                          ])
                                                                                                                                                      ])
                                                                                                                                                  ])])
                                                                                                                                          ])])
                                                                                                                                  ])
                                                                                                                              ])
                                                                                                                          ])
                                                                                                                      ])
                                                                                                                  ])
                                                                                                              ])
                                                                                                          ])
                                                                                                      ])
                                                                                                  ])
                                                                                              ])
                                                                                          ])
                                                                                      ])
                                                                                  ])
                                                                              ])
                                                                          ])
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
                                "@[%23d %2i %3u %4n %0xl %0xL %N %03x %X %o %s %S %c %C %3f %2F %2e %E %g %G %B %b %ld %li %lu %lx %lX %lo %nd %ni %nu %nx %nx %no  @]"
                              ]), [
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
                            true,
                            false,
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
                      "                      1  2   3 4 5l 6L 7 008 9 12 a \"b\" c 'd' 1.000000 2. 3.000000e+00 4.000000E+00 5 6 true false 0 1 2 3 4 5 6 7 8 9 a 13  "
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "width_1",
          (function (param) {
              return /* Eq */Block.__(0, [
                        Curry._1(Format.asprintf(/* Format */[
                                  /* Int */Block.__(4, [
                                      /* Int_d */0,
                                      /* Lit_padding */Block.__(0, [
                                          /* Zeros */2,
                                          14
                                        ]),
                                      /* No_precision */0,
                                      /* End_of_format */0
                                    ]),
                                  "%014d"
                                ]), 32),
                        "00000000000032"
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "width_2",
            (function (param) {
                return /* Eq */Block.__(0, [
                          Curry._1(Format.asprintf(/* Format */[
                                    /* Float */Block.__(8, [
                                        /* Float_f */0,
                                        /* Lit_padding */Block.__(0, [
                                            /* Right */1,
                                            10
                                          ]),
                                        /* Lit_precision */[3],
                                        /* End_of_format */0
                                      ]),
                                    "%10.3f"
                                  ]), 32333.02),
                          " 32333.020"
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "alternate_1",
              (function (param) {
                  return /* Eq */Block.__(0, [
                            Curry._1(Format.asprintf(/* Format */[
                                      /* Int */Block.__(4, [
                                          /* Int_x */6,
                                          /* Lit_padding */Block.__(0, [
                                              /* Right */1,
                                              0
                                            ]),
                                          /* No_precision */0,
                                          /* End_of_format */0
                                        ]),
                                      "%0x"
                                    ]), 32333),
                            "7e4d"
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "alternate_2",
                (function (param) {
                    return /* Eq */Block.__(0, [
                              Curry._1(Format.asprintf(/* Format */[
                                        /* Int */Block.__(4, [
                                            /* Int_Cx */7,
                                            /* Lit_padding */Block.__(0, [
                                                /* Right */1,
                                                0
                                              ]),
                                            /* No_precision */0,
                                            /* End_of_format */0
                                          ]),
                                        "%#0x"
                                      ]), 32333),
                              "0x7e4d"
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "alternate_3",
                  (function (param) {
                      return /* Eq */Block.__(0, [
                                /* tuple */[
                                  Curry._1(Format.asprintf(/* Format */[
                                            /* Int */Block.__(4, [
                                                /* Int_Co */11,
                                                /* No_padding */0,
                                                /* No_precision */0,
                                                /* End_of_format */0
                                              ]),
                                            "%#o"
                                          ]), 32),
                                  Curry._1(Format.asprintf(/* Format */[
                                            /* Int */Block.__(4, [
                                                /* Int_o */10,
                                                /* No_padding */0,
                                                /* No_precision */0,
                                                /* End_of_format */0
                                              ]),
                                            "%o"
                                          ]), 32)
                                ],
                                /* tuple */[
                                  "040",
                                  "40"
                                ]
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "justify_0",
                    (function (param) {
                        return /* Eq */Block.__(0, [
                                  Caml_format.caml_format_int("%-8d", 32),
                                  "32      "
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "sign_p",
                      (function (param) {
                          return /* Eq */Block.__(0, [
                                    Curry._1(Format.asprintf(/* Format */[
                                              /* Int */Block.__(4, [
                                                  /* Int_pd */1,
                                                  /* Lit_padding */Block.__(0, [
                                                      /* Right */1,
                                                      4
                                                    ]),
                                                  /* No_precision */0,
                                                  /* End_of_format */0
                                                ]),
                                              "%+4d"
                                            ]), 32),
                                    " +32"
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "sign_2p",
                        (function (param) {
                            return /* Eq */Block.__(0, [
                                      Curry._1(Format.asprintf(/* Format */[
                                                /* Int */Block.__(4, [
                                                    /* Int_sd */2,
                                                    /* Lit_padding */Block.__(0, [
                                                        /* Right */1,
                                                        4
                                                      ]),
                                                    /* No_precision */0,
                                                    /* End_of_format */0
                                                  ]),
                                                "% 4d"
                                              ]), 32),
                                      "  32"
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "sign_3p",
                          (function (param) {
                              return /* Eq */Block.__(0, [
                                        Curry._1(Format.asprintf(/* Format */[
                                                  /* Int32 */Block.__(5, [
                                                      /* Int_u */12,
                                                      /* No_padding */0,
                                                      /* No_precision */0,
                                                      /* End_of_format */0
                                                    ]),
                                                  "%lu"
                                                ]), -1),
                                        "4294967295"
                                      ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "sign_4p",
                            (function (param) {
                                return /* Eq */Block.__(0, [
                                          Curry._1(Format.asprintf(/* Format */[
                                                    /* Int32 */Block.__(5, [
                                                        /* Int_d */0,
                                                        /* No_padding */0,
                                                        /* No_precision */0,
                                                        /* End_of_format */0
                                                      ]),
                                                    "%ld"
                                                  ]), -1),
                                          "-1"
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "width_3",
                              (function (param) {
                                  return /* Eq */Block.__(0, [
                                            Caml_format.caml_format_int("%032d", 32),
                                            "00000000000000000000000000000032"
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "prec_1",
                                (function (param) {
                                    return /* Eq */Block.__(0, [
                                              Curry._1(Format.asprintf(/* Format */[
                                                        /* Int */Block.__(4, [
                                                            /* Int_d */0,
                                                            /* No_padding */0,
                                                            /* Lit_precision */[10],
                                                            /* End_of_format */0
                                                          ]),
                                                        "%.10d"
                                                      ]), 32),
                                              "0000000032"
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "prec_2",
                                  (function (param) {
                                      return /* Eq */Block.__(0, [
                                                Caml_format.caml_format_int("%.10d", 32),
                                                "0000000032"
                                              ]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "prec_3",
                                    (function (param) {
                                        return /* Eq */Block.__(0, [
                                                  Caml_format.caml_format_int("%.d", 32),
                                                  "32"
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "prec_4",
                                      (function (param) {
                                          return /* Eq */Block.__(0, [
                                                    Caml_format.caml_format_int("%.d", 32),
                                                    "32"
                                                  ]);
                                        })
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
    Number.NaN,
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
  return Curry._1(Format.fprintf(ppf, /* Format */[
                  /* String */Block.__(2, [
                      /* No_padding */0,
                      /* End_of_format */0
                    ]),
                  "%s"
                ]), s);
}

function kwd(ppf, s) {
  return Curry._1(Format.fprintf(ppf, /* Format */[
                  /* String */Block.__(2, [
                      /* No_padding */0,
                      /* End_of_format */0
                    ]),
                  "%s"
                ]), s);
}

function pr_exp0(ppf, lam) {
  var exit = 0;
  switch (lam.tag | 0) {
    case 1 : 
        return Curry._2(Format.fprintf(ppf, /* Format */[
                        /* Alpha */Block.__(15, [/* End_of_format */0]),
                        "%a"
                      ]), ident, lam[0]);
    case 0 : 
    case 2 : 
        exit = 1;
        break;
    
  }
  if (exit === 1) {
    return Curry._2(Format.fprintf(ppf, /* Format */[
                    /* Formatting_gen */Block.__(18, [
                        /* Open_box */Block.__(1, [/* Format */[
                              /* String_literal */Block.__(11, [
                                  "<1>",
                                  /* End_of_format */0
                                ]),
                              "<1>"
                            ]]),
                        /* Char_literal */Block.__(12, [
                            /* "(" */40,
                            /* Alpha */Block.__(15, [/* Char_literal */Block.__(12, [
                                    /* ")" */41,
                                    /* Formatting_lit */Block.__(17, [
                                        /* Close_box */0,
                                        /* End_of_format */0
                                      ])
                                  ])])
                          ])
                      ]),
                    "@[<1>(%a)@]"
                  ]), pr_lambda, lam);
  }
  
}

function pr_app(ppf, e) {
  return Curry._2(Format.fprintf(ppf, /* Format */[
                  /* Formatting_gen */Block.__(18, [
                      /* Open_box */Block.__(1, [/* Format */[
                            /* String_literal */Block.__(11, [
                                "<2>",
                                /* End_of_format */0
                              ]),
                            "<2>"
                          ]]),
                      /* Alpha */Block.__(15, [/* Formatting_lit */Block.__(17, [
                              /* Close_box */0,
                              /* End_of_format */0
                            ])])
                    ]),
                  "@[<2>%a@]"
                ]), pr_other_applications, e);
}

function pr_other_applications(ppf, f) {
  switch (f.tag | 0) {
    case 0 : 
    case 1 : 
        return pr_exp0(ppf, f);
    case 2 : 
        return Curry._4(Format.fprintf(ppf, /* Format */[
                        /* Alpha */Block.__(15, [/* Formatting_lit */Block.__(17, [
                                /* Break */Block.__(0, [
                                    "@ ",
                                    1,
                                    0
                                  ]),
                                /* Alpha */Block.__(15, [/* End_of_format */0])
                              ])]),
                        "%a@ %a"
                      ]), pr_app, f[0], pr_exp0, f[1]);
    
  }
}

function pr_lambda(ppf, e) {
  switch (e.tag | 0) {
    case 0 : 
        return Curry._8(Format.fprintf(ppf, /* Format */[
                        /* Formatting_gen */Block.__(18, [
                            /* Open_box */Block.__(1, [/* Format */[
                                  /* String_literal */Block.__(11, [
                                      "<1>",
                                      /* End_of_format */0
                                    ]),
                                  "<1>"
                                ]]),
                            /* Alpha */Block.__(15, [/* Alpha */Block.__(15, [/* Alpha */Block.__(15, [/* Formatting_lit */Block.__(17, [
                                            /* Break */Block.__(0, [
                                                "@ ",
                                                1,
                                                0
                                              ]),
                                            /* Alpha */Block.__(15, [/* Formatting_lit */Block.__(17, [
                                                    /* Close_box */0,
                                                    /* End_of_format */0
                                                  ])])
                                          ])])])])
                          ]),
                        "@[<1>%a%a%a@ %a@]"
                      ]), kwd, "\\", ident, e[0], kwd, ".", pr_lambda, e[1]);
    case 1 : 
    case 2 : 
        return pr_app(ppf, e);
    
  }
}

var string_of_lambda = Curry._1(Format.asprintf(/* Format */[
          /* Alpha */Block.__(15, [/* End_of_format */0]),
          "%a"
        ]), pr_lambda);

var Lambda_suites = /* module */[
  /* ident */ident,
  /* kwd */kwd,
  /* pr_exp0 */pr_exp0,
  /* pr_app */pr_app,
  /* pr_other_applications */pr_other_applications,
  /* pr_lambda */pr_lambda,
  /* string_of_lambda */string_of_lambda
];

var lambda_suites = /* array */[
  /* tuple */[
    /* Var */Block.__(1, ["x"]),
    "x"
  ],
  /* tuple */[
    /* Apply */Block.__(2, [
        /* Var */Block.__(1, ["x"]),
        /* Var */Block.__(1, ["y"])
      ]),
    "x y"
  ],
  /* tuple */[
    /* Lambda */Block.__(0, [
        "z",
        /* Apply */Block.__(2, [
            /* Var */Block.__(1, ["x"]),
            /* Var */Block.__(1, ["y"])
          ])
      ]),
    "\\z. x y"
  ],
  /* tuple */[
    /* Lambda */Block.__(0, [
        "z",
        /* Lambda */Block.__(0, [
            "z",
            /* Apply */Block.__(2, [
                /* Var */Block.__(1, ["x"]),
                /* Var */Block.__(1, ["y"])
              ])
          ])
      ]),
    "\\z. \\z. x y"
  ]
];

function from_lambda_pairs(p) {
  return $$Array.to_list($$Array.mapi((function (i, param) {
                    var b = param[1];
                    var a = param[0];
                    return /* tuple */[
                            Curry._1(Printf.sprintf(/* Format */[
                                      /* String_literal */Block.__(11, [
                                          "lambda_print ",
                                          /* Int */Block.__(4, [
                                              /* Int_d */0,
                                              /* No_padding */0,
                                              /* No_precision */0,
                                              /* End_of_format */0
                                            ])
                                        ]),
                                      "lambda_print %d"
                                    ]), i),
                            (function (param) {
                                return /* Eq */Block.__(0, [
                                          Curry._1(string_of_lambda, a),
                                          b
                                        ]);
                              })
                          ];
                  }), lambda_suites));
}

var ksprintf_suites_000 = /* tuple */[
  "ksprintf",
  (function (param) {
      var f = function (fmt) {
        return Format.ksprintf((function (x) {
                      return x + x;
                    }), fmt);
      };
      return /* Eq */Block.__(0, [
                Curry._2(f(/* Format */[
                          /* String */Block.__(2, [
                              /* No_padding */0,
                              /* Char_literal */Block.__(12, [
                                  /* " " */32,
                                  /* String */Block.__(2, [
                                      /* No_padding */0,
                                      /* String_literal */Block.__(11, [
                                          " a ",
                                          /* End_of_format */0
                                        ])
                                    ])
                                ])
                            ]),
                          "%s %s a "
                        ]), "x", "xx"),
                "x xx a x xx a "
              ]);
    })
];

var ksprintf_suites_001 = /* :: */[
  /* tuple */[
    "sprintf",
    (function (param) {
        return /* Eq */Block.__(0, [
                  Curry._2(Format.sprintf(/* Format */[
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* Char_literal */Block.__(12, [
                                    /* " " */32,
                                    /* Caml_string */Block.__(3, [
                                        /* No_padding */0,
                                        /* End_of_format */0
                                      ])
                                  ])
                              ]),
                            "%s %S"
                          ]), "x", "X"),
                  "x \"X\""
                ]);
      })
  ],
  /* [] */0
];

var ksprintf_suites = /* :: */[
  ksprintf_suites_000,
  ksprintf_suites_001
];

var int64_suites_000 = /* tuple */[
  "i32_simple",
  (function (param) {
      return /* Eq */Block.__(0, [
                Curry._1(Format.asprintf(/* Format */[
                          /* Nativeint */Block.__(6, [
                              /* Int_x */6,
                              /* No_padding */0,
                              /* No_precision */0,
                              /* End_of_format */0
                            ]),
                          "%nx"
                        ]), 4294967295),
                "ffffffff"
              ]);
    })
];

var int64_suites_001 = /* :: */[
  /* tuple */[
    "i32_simple1",
    (function (param) {
        return /* Eq */Block.__(0, [
                  Curry._1(Format.asprintf(/* Format */[
                            /* Nativeint */Block.__(6, [
                                /* Int_o */10,
                                /* No_padding */0,
                                /* No_precision */0,
                                /* End_of_format */0
                              ]),
                            "%no"
                          ]), 4294967295),
                  "37777777777"
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "i64_simple",
      (function (param) {
          return /* Eq */Block.__(0, [
                    Curry._1(Format.asprintf(/* Format */[
                              /* Int64 */Block.__(7, [
                                  /* Int_d */0,
                                  /* No_padding */0,
                                  /* No_precision */0,
                                  /* End_of_format */0
                                ]),
                              "%Ld"
                            ]), /* int64 */[
                          /* hi */0,
                          /* lo */3
                        ]),
                    "3"
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "i64_simple2",
        (function (param) {
            return /* Eq */Block.__(0, [
                      Curry._1(Format.asprintf(/* Format */[
                                /* Int64 */Block.__(7, [
                                    /* Int_x */6,
                                    /* No_padding */0,
                                    /* No_precision */0,
                                    /* End_of_format */0
                                  ]),
                                "%Lx"
                              ]), /* int64 */[
                            /* hi */0,
                            /* lo */33
                          ]),
                      "21"
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "i64_simple3",
          (function (param) {
              return /* Eq */Block.__(0, [
                        Curry._1(Format.asprintf(/* Format */[
                                  /* Int64 */Block.__(7, [
                                      /* Int_i */3,
                                      /* No_padding */0,
                                      /* No_precision */0,
                                      /* End_of_format */0
                                    ]),
                                  "%Li"
                                ]), /* int64 */[
                              /* hi */0,
                              /* lo */33
                            ]),
                        "33"
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "i64_simple4",
            (function (param) {
                return /* Eq */Block.__(0, [
                          Curry._1(Format.asprintf(/* Format */[
                                    /* Int64 */Block.__(7, [
                                        /* Int_X */8,
                                        /* No_padding */0,
                                        /* No_precision */0,
                                        /* End_of_format */0
                                      ]),
                                    "%LX"
                                  ]), /* int64 */[
                                /* hi */0,
                                /* lo */44
                              ]),
                          "2C"
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "i64_simple5",
              (function (param) {
                  return /* Eq */Block.__(0, [
                            Curry._1(Format.asprintf(/* Format */[
                                      /* Int64 */Block.__(7, [
                                          /* Int_x */6,
                                          /* No_padding */0,
                                          /* No_precision */0,
                                          /* End_of_format */0
                                        ]),
                                      "%Lx"
                                    ]), /* int64 */[
                                  /* hi */0,
                                  /* lo */44
                                ]),
                            "2c"
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "i64_simple6",
                (function (param) {
                    return /* Eq */Block.__(0, [
                              Curry._2(Format.asprintf(/* Format */[
                                        /* Int64 */Block.__(7, [
                                            /* Int_x */6,
                                            /* Arg_padding */Block.__(1, [/* Right */1]),
                                            /* No_precision */0,
                                            /* End_of_format */0
                                          ]),
                                        "%*Lx"
                                      ]), 5, /* int64 */[
                                    /* hi */0,
                                    /* lo */44
                                  ]),
                              "   2c"
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "i64_simple7",
                  (function (param) {
                      return /* Eq */Block.__(0, [
                                Caml_format.caml_int64_format("%d", /* int64 */[
                                      /* hi */0,
                                      /* lo */3333
                                    ]),
                                "3333"
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "i64_simple8",
                    (function (param) {
                        return /* Eq */Block.__(0, [
                                  Curry._2(Format.asprintf(/* Format */[
                                            /* Int64 */Block.__(7, [
                                                /* Int_d */0,
                                                /* No_padding */0,
                                                /* No_precision */0,
                                                /* Int64 */Block.__(7, [
                                                    /* Int_d */0,
                                                    /* Lit_padding */Block.__(0, [
                                                        /* Zeros */2,
                                                        18
                                                      ]),
                                                    /* No_precision */0,
                                                    /* End_of_format */0
                                                  ])
                                              ]),
                                            "%Ld%018Ld"
                                          ]), /* int64 */[
                                        /* hi */0,
                                        /* lo */3
                                      ], /* int64 */[
                                        /* hi */0,
                                        /* lo */3
                                      ]),
                                  "3000000000000000003"
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "i64_simple9",
                      (function (param) {
                          return /* Eq */Block.__(0, [
                                    Curry._2(Format.asprintf(/* Format */[
                                              /* Int64 */Block.__(7, [
                                                  /* Int_d */0,
                                                  /* No_padding */0,
                                                  /* No_precision */0,
                                                  /* Int64 */Block.__(7, [
                                                      /* Int_d */0,
                                                      /* Lit_padding */Block.__(0, [
                                                          /* Zeros */2,
                                                          18
                                                        ]),
                                                      /* No_precision */0,
                                                      /* End_of_format */0
                                                    ])
                                                ]),
                                              "%Ld%018Ld"
                                            ]), /* int64 */[
                                          /* hi */107288,
                                          /* lo */1548746752
                                        ], /* int64 */[
                                          /* hi */0,
                                          /* lo */0
                                        ]),
                                    "460800000000000000000000000000000"
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "i64_simple10",
                        (function (param) {
                            return /* Eq */Block.__(0, [
                                      Curry._1(Format.asprintf(/* Format */[
                                                /* Int64 */Block.__(7, [
                                                    /* Int_x */6,
                                                    /* No_padding */0,
                                                    /* No_precision */0,
                                                    /* End_of_format */0
                                                  ]),
                                                "%Lx"
                                              ]), Int64.max_int),
                                      "7fffffffffffffff"
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "i64_simple15",
                          (function (param) {
                              return /* Eq */Block.__(0, [
                                        Curry._1(Format.asprintf(/* Format */[
                                                  /* Int64 */Block.__(7, [
                                                      /* Int_d */0,
                                                      /* No_padding */0,
                                                      /* No_precision */0,
                                                      /* End_of_format */0
                                                    ]),
                                                  "%Ld"
                                                ]), /* int64 */[
                                              /* hi */-1,
                                              /* lo */4294967295
                                            ]),
                                        "-1"
                                      ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "i64_simple16",
                            (function (param) {
                                return /* Eq */Block.__(0, [
                                          Curry._1(Format.asprintf(/* Format */[
                                                    /* Int64 */Block.__(7, [
                                                        /* Int_d */0,
                                                        /* No_padding */0,
                                                        /* No_precision */0,
                                                        /* End_of_format */0
                                                      ]),
                                                    "%Ld"
                                                  ]), /* int64 */[
                                                /* hi */-1,
                                                /* lo */4294956185
                                              ]),
                                          "-11111"
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "i64_simple14",
                              (function (param) {
                                  return /* Eq */Block.__(0, [
                                            Curry._1(Format.asprintf(/* Format */[
                                                      /* Int64 */Block.__(7, [
                                                          /* Int_X */8,
                                                          /* No_padding */0,
                                                          /* No_precision */0,
                                                          /* End_of_format */0
                                                        ]),
                                                      "%LX"
                                                    ]), /* int64 */[
                                                  /* hi */-1,
                                                  /* lo */4294967295
                                                ]),
                                            "FFFFFFFFFFFFFFFF"
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "i64_simple17",
                                (function (param) {
                                    return /* Eq */Block.__(0, [
                                              Curry._1(Format.asprintf(/* Format */[
                                                        /* Int64 */Block.__(7, [
                                                            /* Int_x */6,
                                                            /* No_padding */0,
                                                            /* No_precision */0,
                                                            /* End_of_format */0
                                                          ]),
                                                        "%Lx"
                                                      ]), /* int64 */[
                                                    /* hi */-1,
                                                    /* lo */4294967295
                                                  ]),
                                              "ffffffffffffffff"
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "i64_simple11",
                                  (function (param) {
                                      return /* Eq */Block.__(0, [
                                                Curry._1(Format.asprintf(/* Format */[
                                                          /* Int64 */Block.__(7, [
                                                              /* Int_X */8,
                                                              /* No_padding */0,
                                                              /* No_precision */0,
                                                              /* End_of_format */0
                                                            ]),
                                                          "%LX"
                                                        ]), Int64.max_int),
                                                "7FFFFFFFFFFFFFFF"
                                              ]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "i64_simple12",
                                    (function (param) {
                                        return /* Eq */Block.__(0, [
                                                  Curry._1(Format.asprintf(/* Format */[
                                                            /* Int64 */Block.__(7, [
                                                                /* Int_X */8,
                                                                /* No_padding */0,
                                                                /* No_precision */0,
                                                                /* End_of_format */0
                                                              ]),
                                                            "%LX"
                                                          ]), Int64.min_int),
                                                  "8000000000000000"
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "i64_simple17",
                                      (function (param) {
                                          return /* Eq */Block.__(0, [
                                                    Curry._1(Format.asprintf(/* Format */[
                                                              /* Int64 */Block.__(7, [
                                                                  /* Int_u */12,
                                                                  /* No_padding */0,
                                                                  /* No_precision */0,
                                                                  /* End_of_format */0
                                                                ]),
                                                              "%Lu"
                                                            ]), /* int64 */[
                                                          /* hi */-1,
                                                          /* lo */4294967295
                                                        ]),
                                                    "18446744073709551615"
                                                  ]);
                                        })
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "i64_simple21",
                                        (function (param) {
                                            return /* Eq */Block.__(0, [
                                                      Curry._1(Format.asprintf(/* Format */[
                                                                /* Int64 */Block.__(7, [
                                                                    /* Int_u */12,
                                                                    /* No_padding */0,
                                                                    /* No_precision */0,
                                                                    /* End_of_format */0
                                                                  ]),
                                                                "%Lu"
                                                              ]), /* int64 */[
                                                            /* hi */-1,
                                                            /* lo */4294957296
                                                          ]),
                                                      "18446744073709541616"
                                                    ]);
                                          })
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "i64_simple19",
                                          (function (param) {
                                              return /* Eq */Block.__(0, [
                                                        Curry._1(Format.asprintf(/* Format */[
                                                                  /* Int64 */Block.__(7, [
                                                                      /* Int_o */10,
                                                                      /* No_padding */0,
                                                                      /* No_precision */0,
                                                                      /* End_of_format */0
                                                                    ]),
                                                                  "%Lo"
                                                                ]), Int64.min_int),
                                                        "1000000000000000000000"
                                                      ]);
                                            })
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "i64_simple13",
                                            (function (param) {
                                                return /* Eq */Block.__(0, [
                                                          Curry._1(Format.asprintf(/* Format */[
                                                                    /* Int64 */Block.__(7, [
                                                                        /* Int_X */8,
                                                                        /* No_padding */0,
                                                                        /* No_precision */0,
                                                                        /* End_of_format */0
                                                                      ]),
                                                                    "%LX"
                                                                  ]), Caml_int64.add(Int64.min_int, /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */1
                                                                  ])),
                                                          "8000000000000001"
                                                        ]);
                                              })
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "i64_simple20",
                                              (function (param) {
                                                  return /* Eq */Block.__(0, [
                                                            Curry._1(Format.asprintf(/* Format */[
                                                                      /* Int64 */Block.__(7, [
                                                                          /* Int_x */6,
                                                                          /* Lit_padding */Block.__(0, [
                                                                              /* Right */1,
                                                                              12
                                                                            ]),
                                                                          /* No_precision */0,
                                                                          /* End_of_format */0
                                                                        ]),
                                                                      "%12Lx"
                                                                    ]), /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */3
                                                                ]),
                                                            "           3"
                                                          ]);
                                                })
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "i64_simple21",
                                                (function (param) {
                                                    return /* Eq */Block.__(0, [
                                                              Curry._1(Format.asprintf(/* Format */[
                                                                        /* Int64 */Block.__(7, [
                                                                            /* Int_X */8,
                                                                            /* No_padding */0,
                                                                            /* No_precision */0,
                                                                            /* End_of_format */0
                                                                          ]),
                                                                        "%LX"
                                                                      ]), /* int64 */[
                                                                    /* hi */1859194407,
                                                                    /* lo */1163551168
                                                                  ]),
                                                              "6ED10E27455A61C0"
                                                            ]);
                                                  })
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "missing_neline",
                                                  (function (param) {
                                                      return /* Eq */Block.__(0, [
                                                                Curry._1(Format.asprintf(/* Format */[
                                                                          /* Int64 */Block.__(7, [
                                                                              /* Int_d */0,
                                                                              /* No_padding */0,
                                                                              /* No_precision */0,
                                                                              /* Char_literal */Block.__(12, [
                                                                                  /* "\n" */10,
                                                                                  /* End_of_format */0
                                                                                ])
                                                                            ]),
                                                                          "%Ld\n"
                                                                        ]), /* int64 */[
                                                                      /* hi */0,
                                                                      /* lo */32
                                                                    ]),
                                                                "32\n"
                                                              ]);
                                                    })
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "missing_newline2",
                                                    (function (param) {
                                                        var buf = $$Buffer.create(30);
                                                        return /* Eq */Block.__(0, [
                                                                  (Curry._1(Printf.bprintf(buf, /* Format */[
                                                                              /* Int64 */Block.__(7, [
                                                                                  /* Int_d */0,
                                                                                  /* No_padding */0,
                                                                                  /* No_precision */0,
                                                                                  /* Char_literal */Block.__(12, [
                                                                                      /* "\n" */10,
                                                                                      /* End_of_format */0
                                                                                    ])
                                                                                ]),
                                                                              "%Ld\n"
                                                                            ]), /* int64 */[
                                                                          /* hi */0,
                                                                          /* lo */32
                                                                        ]), $$Buffer.contents(buf)),
                                                                  "32\n"
                                                                ]);
                                                      })
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
      ]
    ]
  ]
];

var int64_suites = /* :: */[
  int64_suites_000,
  int64_suites_001
];

var of_string_data = /* array */[
  /* tuple */[
    /* int64 */[
      /* hi */0,
      /* lo */0
    ],
    "0"
  ],
  /* tuple */[
    /* int64 */[
      /* hi */0,
      /* lo */3
    ],
    "3"
  ],
  /* tuple */[
    /* int64 */[
      /* hi */0,
      /* lo */33
    ],
    "33"
  ],
  /* tuple */[
    /* int64 */[
      /* hi */0,
      /* lo */333
    ],
    "33_3"
  ],
  /* tuple */[
    /* int64 */[
      /* hi */0,
      /* lo */33333
    ],
    "33_33_3"
  ],
  /* tuple */[
    /* int64 */[
      /* hi */77,
      /* lo */2620851541
    ],
    "333333333333"
  ],
  /* tuple */[
    /* int64 */[
      /* hi */-1,
      /* lo */4294967295
    ],
    "0xffff_ffff_ffff_ffff"
  ],
  /* tuple */[
    /* int64 */[
      /* hi */0,
      /* lo */113
    ],
    "0b01110001"
  ],
  /* tuple */[
    /* int64 */[
      /* hi */0,
      /* lo */1
    ],
    "-0xffff_ffff_ffff_ffff"
  ]
];

Mt.from_pair_suites("Caml_format_test", Pervasives.$at(suites, Pervasives.$at(formatter_suites, Pervasives.$at(from_lambda_pairs(lambda_suites), Pervasives.$at(ksprintf_suites, Pervasives.$at($$Array.to_list($$Array.mapi((function (i, param) {
                                    var str_result = param[2];
                                    var f = param[1];
                                    var fmt = param[0];
                                    return /* tuple */[
                                            Curry._1(Printf.sprintf(/* Format */[
                                                      /* String_literal */Block.__(11, [
                                                          "float_format ",
                                                          /* Int */Block.__(4, [
                                                              /* Int_d */0,
                                                              /* No_padding */0,
                                                              /* No_precision */0,
                                                              /* End_of_format */0
                                                            ])
                                                        ]),
                                                      "float_format %d"
                                                    ]), i),
                                            (function (param) {
                                                return /* Eq */Block.__(0, [
                                                          Caml_format.caml_format_float(fmt, f),
                                                          str_result
                                                        ]);
                                              })
                                          ];
                                  }), float_data)), Pervasives.$at(int64_suites, $$Array.to_list($$Array.mapi((function (i, param) {
                                        var b = param[1];
                                        var a = param[0];
                                        return /* tuple */[
                                                Curry._1(Printf.sprintf(/* Format */[
                                                          /* String_literal */Block.__(11, [
                                                              "int64_of_string ",
                                                              /* Int */Block.__(4, [
                                                                  /* Int_d */0,
                                                                  /* No_padding */0,
                                                                  /* No_precision */0,
                                                                  /* Char_literal */Block.__(12, [
                                                                      /* " " */32,
                                                                      /* End_of_format */0
                                                                    ])
                                                                ])
                                                            ]),
                                                          "int64_of_string %d "
                                                        ]), i),
                                                (function (param) {
                                                    return /* Eq */Block.__(0, [
                                                              Caml_format.caml_int64_of_string(b),
                                                              a
                                                            ]);
                                                  })
                                              ];
                                      }), of_string_data)))))))));

var a = Format.asprintf;

var float_suites = /* :: */[
  "float_nan",
  /* [] */0
];

var hh = /* int64 */[
  /* hi */214748364,
  /* lo */3435973836
];

var hhh = /* int64 */[
  /* hi */268435456,
  /* lo */0
];

exports.of_string = of_string;
exports.from_float_of_string = from_float_of_string;
exports.from_of_string = from_of_string;
exports.u = u;
exports.to_str = to_str;
exports.v = v;
exports.suites = suites;
exports.ff = ff;
exports.a = a;
exports.formatter_suites = formatter_suites;
exports.float_data = float_data;
exports.float_suites = float_suites;
exports.Lambda_suites = Lambda_suites;
exports.lambda_suites = lambda_suites;
exports.from_lambda_pairs = from_lambda_pairs;
exports.ksprintf_suites = ksprintf_suites;
exports.int64_suites = int64_suites;
exports.hh = hh;
exports.hhh = hhh;
exports.of_string_data = of_string_data;
/* v Not a pure module */
