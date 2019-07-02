'use strict';

var Mt = require("./mt.js");
var Arg = require("../../lib/js/arg.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");
var Caml_obj = require("../../lib/js/caml_obj.js");

var current = /* record */[/* contents */0];

var accum = /* record */[/* contents : [] */0];

function record(fmt) {
  return Printf.kprintf((function (s) {
                accum[0] = /* :: */[
                  s,
                  accum[0]
                ];
                return /* () */0;
              }), fmt);
}

function f_unit(param) {
  return record(/* Format */[
              /* String_literal */Block.__(11, [
                  "unit()",
                  /* End_of_format */0
                ]),
              "unit()"
            ]);
}

function f_bool(b) {
  return Curry._1(record(/* Format */[
                  /* String_literal */Block.__(11, [
                      "bool(",
                      /* Bool */Block.__(9, [
                          /* No_padding */0,
                          /* Char_literal */Block.__(12, [
                              /* ")" */41,
                              /* End_of_format */0
                            ])
                        ])
                    ]),
                  "bool(%b)"
                ]), b);
}

var r_set = /* record */[/* contents */false];

var r_clear = /* record */[/* contents */true];

function f_string(s) {
  return Curry._1(record(/* Format */[
                  /* String_literal */Block.__(11, [
                      "string(",
                      /* String */Block.__(2, [
                          /* No_padding */0,
                          /* Char_literal */Block.__(12, [
                              /* ")" */41,
                              /* End_of_format */0
                            ])
                        ])
                    ]),
                  "string(%s)"
                ]), s);
}

var r_string = /* record */[/* contents */""];

function f_int(i) {
  return Curry._1(record(/* Format */[
                  /* String_literal */Block.__(11, [
                      "int(",
                      /* Int */Block.__(4, [
                          /* Int_d */0,
                          /* No_padding */0,
                          /* No_precision */0,
                          /* Char_literal */Block.__(12, [
                              /* ")" */41,
                              /* End_of_format */0
                            ])
                        ])
                    ]),
                  "int(%d)"
                ]), i);
}

var r_int = /* record */[/* contents */0];

function f_float(f) {
  return Curry._1(record(/* Format */[
                  /* String_literal */Block.__(11, [
                      "float(",
                      /* Float */Block.__(8, [
                          /* Float_g */9,
                          /* No_padding */0,
                          /* No_precision */0,
                          /* Char_literal */Block.__(12, [
                              /* ")" */41,
                              /* End_of_format */0
                            ])
                        ])
                    ]),
                  "float(%g)"
                ]), f);
}

var r_float = /* record */[/* contents */0.0];

function f_symbol(s) {
  return Curry._1(record(/* Format */[
                  /* String_literal */Block.__(11, [
                      "symbol(",
                      /* String */Block.__(2, [
                          /* No_padding */0,
                          /* Char_literal */Block.__(12, [
                              /* ")" */41,
                              /* End_of_format */0
                            ])
                        ])
                    ]),
                  "symbol(%s)"
                ]), s);
}

function f_rest(s) {
  return Curry._1(record(/* Format */[
                  /* String_literal */Block.__(11, [
                      "rest(",
                      /* String */Block.__(2, [
                          /* No_padding */0,
                          /* Char_literal */Block.__(12, [
                              /* ")" */41,
                              /* End_of_format */0
                            ])
                        ])
                    ]),
                  "rest(%s)"
                ]), s);
}

function f_anon(s) {
  return Curry._1(record(/* Format */[
                  /* String_literal */Block.__(11, [
                      "anon(",
                      /* String */Block.__(2, [
                          /* No_padding */0,
                          /* Char_literal */Block.__(12, [
                              /* ")" */41,
                              /* End_of_format */0
                            ])
                        ])
                    ]),
                  "anon(%s)"
                ]), s);
}

var spec_000 = /* tuple */[
  "-u",
  /* Unit */Block.__(0, [f_unit]),
  "Unit (0)"
];

var spec_001 = /* :: */[
  /* tuple */[
    "-b",
    /* Bool */Block.__(1, [f_bool]),
    "Bool (1)"
  ],
  /* :: */[
    /* tuple */[
      "-s",
      /* Set */Block.__(2, [r_set]),
      "Set (0)"
    ],
    /* :: */[
      /* tuple */[
        "-c",
        /* Clear */Block.__(3, [r_clear]),
        "Clear (0)"
      ],
      /* :: */[
        /* tuple */[
          "-str",
          /* String */Block.__(4, [f_string]),
          "String (1)"
        ],
        /* :: */[
          /* tuple */[
            "-sstr",
            /* Set_string */Block.__(5, [r_string]),
            "Set_string (1)"
          ],
          /* :: */[
            /* tuple */[
              "-i",
              /* Int */Block.__(6, [f_int]),
              "Int (1)"
            ],
            /* :: */[
              /* tuple */[
                "-si",
                /* Set_int */Block.__(7, [r_int]),
                "Set_int (1)"
              ],
              /* :: */[
                /* tuple */[
                  "-f",
                  /* Float */Block.__(8, [f_float]),
                  "Float (1)"
                ],
                /* :: */[
                  /* tuple */[
                    "-sf",
                    /* Set_float */Block.__(9, [r_float]),
                    "Set_float (1)"
                  ],
                  /* :: */[
                    /* tuple */[
                      "-t",
                      /* Tuple */Block.__(10, [/* :: */[
                            /* Bool */Block.__(1, [f_bool]),
                            /* :: */[
                              /* String */Block.__(4, [f_string]),
                              /* :: */[
                                /* Int */Block.__(6, [f_int]),
                                /* [] */0
                              ]
                            ]
                          ]]),
                      "Tuple (3)"
                    ],
                    /* :: */[
                      /* tuple */[
                        "-sym",
                        /* Symbol */Block.__(11, [
                            /* :: */[
                              "a",
                              /* :: */[
                                "b",
                                /* :: */[
                                  "c",
                                  /* [] */0
                                ]
                              ]
                            ],
                            f_symbol
                          ]),
                        "Symbol (1)"
                      ],
                      /* :: */[
                        /* tuple */[
                          "-rest",
                          /* Rest */Block.__(12, [f_rest]),
                          "Rest (*)"
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
];

var spec = /* :: */[
  spec_000,
  spec_001
];

var args1 = /* array */[
  "prog",
  "anon1",
  "-u",
  "-b",
  "true",
  "-s",
  "anon2",
  "-c",
  "-str",
  "foo",
  "-sstr",
  "bar",
  "-i",
  "19",
  "-si",
  "42",
  "-f",
  "3.14",
  "-sf",
  "2.72",
  "anon3",
  "-t",
  "false",
  "gee",
  "1436",
  "-sym",
  "c",
  "anon4",
  "-rest",
  "r1",
  "r2"
];

var args2 = /* array */[
  "prog",
  "anon1",
  "-u",
  "-b=true",
  "-s",
  "anon2",
  "-c",
  "-str=foo",
  "-sstr=bar",
  "-i=19",
  "-si=42",
  "-f=3.14",
  "-sf=2.72",
  "anon3",
  "-t",
  "false",
  "gee",
  "1436",
  "-sym=c",
  "anon4",
  "-rest",
  "r1",
  "r2"
];

function error(s) {
  return Curry._1(Printf.printf(/* Format */[
                  /* String_literal */Block.__(11, [
                      "error (",
                      /* String */Block.__(2, [
                          /* No_padding */0,
                          /* String_literal */Block.__(11, [
                              ")\n",
                              /* End_of_format */0
                            ])
                        ])
                    ]),
                  "error (%s)\n"
                ]), s);
}

function check(r, v, msg) {
  if (Caml_obj.caml_notequal(r[0], v)) {
    return error(msg);
  } else {
    return 0;
  }
}

function test(argv) {
  current[0] = 0;
  r_set[0] = false;
  r_clear[0] = true;
  r_string[0] = "";
  r_int[0] = 0;
  r_float[0] = 0.0;
  accum[0] = /* [] */0;
  Arg.parse_argv(current, argv, spec, f_anon, "usage");
  var result = List.rev(accum[0]);
  var reference = /* :: */[
    "anon(anon1)",
    /* :: */[
      "unit()",
      /* :: */[
        "bool(true)",
        /* :: */[
          "anon(anon2)",
          /* :: */[
            "string(foo)",
            /* :: */[
              "int(19)",
              /* :: */[
                "float(3.14)",
                /* :: */[
                  "anon(anon3)",
                  /* :: */[
                    "bool(false)",
                    /* :: */[
                      "string(gee)",
                      /* :: */[
                        "int(1436)",
                        /* :: */[
                          "symbol(c)",
                          /* :: */[
                            "anon(anon4)",
                            /* :: */[
                              "rest(r1)",
                              /* :: */[
                                "rest(r2)",
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
  ];
  if (Caml_obj.caml_notequal(result, reference)) {
    var f = function (x, y) {
      return Curry._3(Printf.printf(/* Format */[
                      /* String */Block.__(2, [
                          /* Lit_padding */Block.__(0, [
                              /* Right */1,
                              20
                            ]),
                          /* Char_literal */Block.__(12, [
                              /* " " */32,
                              /* Char */Block.__(0, [/* Char_literal */Block.__(12, [
                                      /* " " */32,
                                      /* String */Block.__(2, [
                                          /* Lit_padding */Block.__(0, [
                                              /* Left */0,
                                              20
                                            ]),
                                          /* Char_literal */Block.__(12, [
                                              /* "\n" */10,
                                              /* Flush */Block.__(10, [/* End_of_format */0])
                                            ])
                                        ])
                                    ])])
                            ])
                        ]),
                      "%20s %c %-20s\n%!"
                    ]), x, x === y ? /* "=" */61 : /* "#" */35, y);
    };
    List.iter2(f, result, reference);
  }
  check(r_set, true, "Set");
  check(r_clear, false, "Clear");
  check(r_string, "bar", "Set_string");
  check(r_int, 42, "Set_int");
  return check(r_float, 2.72, "Set_float");
}

test(args1);

test(args2);

Mt.from_pair_suites("Libarg_test", /* [] */0);

var suites = /* [] */0;

exports.current = current;
exports.accum = accum;
exports.record = record;
exports.f_unit = f_unit;
exports.f_bool = f_bool;
exports.r_set = r_set;
exports.r_clear = r_clear;
exports.f_string = f_string;
exports.r_string = r_string;
exports.f_int = f_int;
exports.r_int = r_int;
exports.f_float = f_float;
exports.r_float = r_float;
exports.f_symbol = f_symbol;
exports.f_rest = f_rest;
exports.f_anon = f_anon;
exports.spec = spec;
exports.args1 = args1;
exports.args2 = args2;
exports.error = error;
exports.check = check;
exports.test = test;
exports.suites = suites;
/*  Not a pure module */
