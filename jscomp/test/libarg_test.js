// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj   = require("../runtime/caml_obj");
var Mt         = require("./mt");
var Arg        = require("../stdlib/arg");
var Printf     = require("../stdlib/printf");
var Caml_curry = require("../runtime/caml_curry");
var List       = require("../stdlib/list");

var current = [0];

var accum = [/* [] */0];

function record(fmt) {
  return Printf.kprintf(function (s) {
              accum[0] = /* :: */[
                s,
                accum[0]
              ];
              return /* () */0;
            }, fmt);
}

function f_unit() {
  return record(/* Format */[
              /* String_literal */{
                0: "unit()",
                1: /* End_of_format */0,
                length: 2,
                tag: 11
              },
              "unit()"
            ]);
}

function f_bool(b) {
  return Caml_curry.app1(record(/* Format */[
                  /* String_literal */{
                    0: "bool(",
                    1: /* Bool */{
                      0: /* Char_literal */{
                        0: /* ")" */41,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 12
                      },
                      length: 1,
                      tag: 9
                    },
                    length: 2,
                    tag: 11
                  },
                  "bool(%b)"
                ]), b);
}

var r_set = [/* false */0];

var r_clear = [/* true */1];

function f_string(s) {
  return Caml_curry.app1(record(/* Format */[
                  /* String_literal */{
                    0: "string(",
                    1: /* String */{
                      0: /* No_padding */0,
                      1: /* Char_literal */{
                        0: /* ")" */41,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 12
                      },
                      length: 2,
                      tag: 2
                    },
                    length: 2,
                    tag: 11
                  },
                  "string(%s)"
                ]), s);
}

var r_string = [""];

function f_int(i) {
  return Caml_curry.app1(record(/* Format */[
                  /* String_literal */{
                    0: "int(",
                    1: /* Int */{
                      0: /* Int_d */0,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* Char_literal */{
                        0: /* ")" */41,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 12
                      },
                      length: 4,
                      tag: 4
                    },
                    length: 2,
                    tag: 11
                  },
                  "int(%d)"
                ]), i);
}

var r_int = [0];

function f_float(f) {
  return Caml_curry.app1(record(/* Format */[
                  /* String_literal */{
                    0: "float(",
                    1: /* Float */{
                      0: /* Float_g */9,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* Char_literal */{
                        0: /* ")" */41,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 12
                      },
                      length: 4,
                      tag: 8
                    },
                    length: 2,
                    tag: 11
                  },
                  "float(%g)"
                ]), f);
}

var r_float = [0.0];

function f_symbol(s) {
  return Caml_curry.app1(record(/* Format */[
                  /* String_literal */{
                    0: "symbol(",
                    1: /* String */{
                      0: /* No_padding */0,
                      1: /* Char_literal */{
                        0: /* ")" */41,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 12
                      },
                      length: 2,
                      tag: 2
                    },
                    length: 2,
                    tag: 11
                  },
                  "symbol(%s)"
                ]), s);
}

function f_rest(s) {
  return Caml_curry.app1(record(/* Format */[
                  /* String_literal */{
                    0: "rest(",
                    1: /* String */{
                      0: /* No_padding */0,
                      1: /* Char_literal */{
                        0: /* ")" */41,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 12
                      },
                      length: 2,
                      tag: 2
                    },
                    length: 2,
                    tag: 11
                  },
                  "rest(%s)"
                ]), s);
}

function f_anon(s) {
  return Caml_curry.app1(record(/* Format */[
                  /* String_literal */{
                    0: "anon(",
                    1: /* String */{
                      0: /* No_padding */0,
                      1: /* Char_literal */{
                        0: /* ")" */41,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 12
                      },
                      length: 2,
                      tag: 2
                    },
                    length: 2,
                    tag: 11
                  },
                  "anon(%s)"
                ]), s);
}

var spec_000 = /* tuple */[
  "-u",
  /* Unit */{
    0: f_unit,
    length: 1,
    tag: 0
  },
  "Unit (0)"
];

var spec_001 = /* :: */[
  /* tuple */[
    "-b",
    /* Bool */{
      0: f_bool,
      length: 1,
      tag: 1
    },
    "Bool (1)"
  ],
  /* :: */[
    /* tuple */[
      "-s",
      /* Set */{
        0: r_set,
        length: 1,
        tag: 2
      },
      "Set (0)"
    ],
    /* :: */[
      /* tuple */[
        "-c",
        /* Clear */{
          0: r_clear,
          length: 1,
          tag: 3
        },
        "Clear (0)"
      ],
      /* :: */[
        /* tuple */[
          "-str",
          /* String */{
            0: f_string,
            length: 1,
            tag: 4
          },
          "String (1)"
        ],
        /* :: */[
          /* tuple */[
            "-sstr",
            /* Set_string */{
              0: r_string,
              length: 1,
              tag: 5
            },
            "Set_string (1)"
          ],
          /* :: */[
            /* tuple */[
              "-i",
              /* Int */{
                0: f_int,
                length: 1,
                tag: 6
              },
              "Int (1)"
            ],
            /* :: */[
              /* tuple */[
                "-si",
                /* Set_int */{
                  0: r_int,
                  length: 1,
                  tag: 7
                },
                "Set_int (1)"
              ],
              /* :: */[
                /* tuple */[
                  "-f",
                  /* Float */{
                    0: f_float,
                    length: 1,
                    tag: 8
                  },
                  "Float (1)"
                ],
                /* :: */[
                  /* tuple */[
                    "-sf",
                    /* Set_float */{
                      0: r_float,
                      length: 1,
                      tag: 9
                    },
                    "Set_float (1)"
                  ],
                  /* :: */[
                    /* tuple */[
                      "-t",
                      /* Tuple */{
                        0: /* :: */[
                          /* Bool */{
                            0: f_bool,
                            length: 1,
                            tag: 1
                          },
                          /* :: */[
                            /* String */{
                              0: f_string,
                              length: 1,
                              tag: 4
                            },
                            /* :: */[
                              /* Int */{
                                0: f_int,
                                length: 1,
                                tag: 6
                              },
                              /* [] */0
                            ]
                          ]
                        ],
                        length: 1,
                        tag: 10
                      },
                      "Tuple (3)"
                    ],
                    /* :: */[
                      /* tuple */[
                        "-sym",
                        /* Symbol */{
                          0: /* :: */[
                            "a",
                            /* :: */[
                              "b",
                              /* :: */[
                                "c",
                                /* [] */0
                              ]
                            ]
                          ],
                          1: f_symbol,
                          length: 2,
                          tag: 11
                        },
                        "Symbol (1)"
                      ],
                      /* :: */[
                        /* tuple */[
                          "-rest",
                          /* Rest */{
                            0: f_rest,
                            length: 1,
                            tag: 12
                          },
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
  return Caml_curry.app1(Printf.printf(/* Format */[
                  /* String_literal */{
                    0: "error (",
                    1: /* String */{
                      0: /* No_padding */0,
                      1: /* String_literal */{
                        0: ")\n",
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 11
                      },
                      length: 2,
                      tag: 2
                    },
                    length: 2,
                    tag: 11
                  },
                  "error (%s)\n"
                ]), s);
}

function check(r, v, msg) {
  if (Caml_obj.caml_notequal(r[0], v)) {
    return error(msg);
  }
  else {
    return 0;
  }
}

function test(argv) {
  current[0] = 0;
  r_set[0] = /* false */0;
  r_clear[0] = /* true */1;
  r_string[0] = "";
  r_int[0] = 0;
  r_float[0] = 0.0;
  accum[0] = /* [] */0;
  Arg.parse_argv(/* Some */[current], argv, spec, f_anon, "usage");
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
      return Caml_curry.app3(Printf.printf(/* Format */[
                      /* String */{
                        0: /* Lit_padding */{
                          0: /* Right */1,
                          1: 20,
                          length: 2,
                          tag: 0
                        },
                        1: /* Char_literal */{
                          0: /* " " */32,
                          1: /* Char */{
                            0: /* Char_literal */{
                              0: /* " " */32,
                              1: /* String */{
                                0: /* Lit_padding */{
                                  0: /* Left */0,
                                  1: 20,
                                  length: 2,
                                  tag: 0
                                },
                                1: /* Char_literal */{
                                  0: /* "\n" */10,
                                  1: /* Flush */{
                                    0: /* End_of_format */0,
                                    length: 1,
                                    tag: 10
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
                            length: 1,
                            tag: 0
                          },
                          length: 2,
                          tag: 12
                        },
                        length: 2,
                        tag: 2
                      },
                      "%20s %c %-20s\n%!"
                    ]), x, x === y ? /* "=" */61 : /* "#" */35, y);
    };
    List.iter2(f, result, reference);
  }
  check(r_set, /* true */1, "Set");
  check(r_clear, /* false */0, "Clear");
  check(r_string, "bar", "Set_string");
  check(r_int, 42, "Set_int");
  return check(r_float, 2.72, "Set_float");
}

test(args1);

var suites_000 = /* tuple */[
  "should raise",
  function () {
    return /* ThrowAny */{
            0: function () {
              return test(args2);
            },
            length: 1,
            tag: 3
          };
  }
];

var suites = /* :: */[
  suites_000,
  /* [] */0
];

Mt.from_pair_suites("libarg_test.ml", suites);

exports.current  = current;
exports.accum    = accum;
exports.record   = record;
exports.f_unit   = f_unit;
exports.f_bool   = f_bool;
exports.r_set    = r_set;
exports.r_clear  = r_clear;
exports.f_string = f_string;
exports.r_string = r_string;
exports.f_int    = f_int;
exports.r_int    = r_int;
exports.f_float  = f_float;
exports.r_float  = r_float;
exports.f_symbol = f_symbol;
exports.f_rest   = f_rest;
exports.f_anon   = f_anon;
exports.spec     = spec;
exports.args1    = args1;
exports.args2    = args2;
exports.error    = error;
exports.check    = check;
exports.test     = test;
exports.suites   = suites;
/*  Not a pure module */
