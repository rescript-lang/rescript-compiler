'use strict';

var Mt = require("./mt.js");
var Arg = require("../../lib/js/arg.js");
var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");
var Caml_obj = require("../../lib/js/caml_obj.js");

var current = /* record */[/* contents */0];

var accum = /* record */[/* contents */"[]"];

function record(fmt) {
  return Printf.kprintf((function (s) {
                accum[0] = /* constructor */{
                  tag: "::",
                  Arg0: s,
                  Arg1: accum[0]
                };
                return /* () */0;
              }), fmt);
}

function f_unit(param) {
  return record(/* constructor */{
              tag: "Format",
              Arg0: /* constructor */{
                tag: "String_literal",
                Arg0: "unit()",
                Arg1: "End_of_format"
              },
              Arg1: "unit()"
            });
}

function f_bool(b) {
  return Curry._1(record(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String_literal",
                    Arg0: "bool(",
                    Arg1: /* constructor */{
                      tag: "Bool",
                      Arg0: /* constructor */{
                        tag: "Char_literal",
                        Arg0: /* ")" */41,
                        Arg1: "End_of_format"
                      }
                    }
                  },
                  Arg1: "bool(%b)"
                }), b);
}

var r_set = /* record */[/* contents */false];

var r_clear = /* record */[/* contents */true];

function f_string(s) {
  return Curry._1(record(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String_literal",
                    Arg0: "string(",
                    Arg1: /* constructor */{
                      tag: "String",
                      Arg0: "No_padding",
                      Arg1: /* constructor */{
                        tag: "Char_literal",
                        Arg0: /* ")" */41,
                        Arg1: "End_of_format"
                      }
                    }
                  },
                  Arg1: "string(%s)"
                }), s);
}

var r_string = /* record */[/* contents */""];

function f_int(i) {
  return Curry._1(record(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String_literal",
                    Arg0: "int(",
                    Arg1: /* constructor */{
                      tag: "Int",
                      Arg0: "Int_d",
                      Arg1: "No_padding",
                      Arg2: "No_precision",
                      Arg3: /* constructor */{
                        tag: "Char_literal",
                        Arg0: /* ")" */41,
                        Arg1: "End_of_format"
                      }
                    }
                  },
                  Arg1: "int(%d)"
                }), i);
}

var r_int = /* record */[/* contents */0];

function f_float(f) {
  return Curry._1(record(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String_literal",
                    Arg0: "float(",
                    Arg1: /* constructor */{
                      tag: "Float",
                      Arg0: "Float_g",
                      Arg1: "No_padding",
                      Arg2: "No_precision",
                      Arg3: /* constructor */{
                        tag: "Char_literal",
                        Arg0: /* ")" */41,
                        Arg1: "End_of_format"
                      }
                    }
                  },
                  Arg1: "float(%g)"
                }), f);
}

var r_float = /* record */[/* contents */0.0];

function f_symbol(s) {
  return Curry._1(record(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String_literal",
                    Arg0: "symbol(",
                    Arg1: /* constructor */{
                      tag: "String",
                      Arg0: "No_padding",
                      Arg1: /* constructor */{
                        tag: "Char_literal",
                        Arg0: /* ")" */41,
                        Arg1: "End_of_format"
                      }
                    }
                  },
                  Arg1: "symbol(%s)"
                }), s);
}

function f_rest(s) {
  return Curry._1(record(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String_literal",
                    Arg0: "rest(",
                    Arg1: /* constructor */{
                      tag: "String",
                      Arg0: "No_padding",
                      Arg1: /* constructor */{
                        tag: "Char_literal",
                        Arg0: /* ")" */41,
                        Arg1: "End_of_format"
                      }
                    }
                  },
                  Arg1: "rest(%s)"
                }), s);
}

function f_anon(s) {
  return Curry._1(record(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String_literal",
                    Arg0: "anon(",
                    Arg1: /* constructor */{
                      tag: "String",
                      Arg0: "No_padding",
                      Arg1: /* constructor */{
                        tag: "Char_literal",
                        Arg0: /* ")" */41,
                        Arg1: "End_of_format"
                      }
                    }
                  },
                  Arg1: "anon(%s)"
                }), s);
}

var spec = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "-u",
    /* constructor */{
      tag: "Unit",
      Arg0: f_unit
    },
    "Unit (0)"
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "-b",
      /* constructor */{
        tag: "Bool",
        Arg0: f_bool
      },
      "Bool (1)"
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "-s",
        /* constructor */{
          tag: "Set",
          Arg0: r_set
        },
        "Set (0)"
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "-c",
          /* constructor */{
            tag: "Clear",
            Arg0: r_clear
          },
          "Clear (0)"
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "-str",
            /* constructor */{
              tag: "String",
              Arg0: f_string
            },
            "String (1)"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "-sstr",
              /* constructor */{
                tag: "Set_string",
                Arg0: r_string
              },
              "Set_string (1)"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "-i",
                /* constructor */{
                  tag: "Int",
                  Arg0: f_int
                },
                "Int (1)"
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "-si",
                  /* constructor */{
                    tag: "Set_int",
                    Arg0: r_int
                  },
                  "Set_int (1)"
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "-f",
                    /* constructor */{
                      tag: "Float",
                      Arg0: f_float
                    },
                    "Float (1)"
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "-sf",
                      /* constructor */{
                        tag: "Set_float",
                        Arg0: r_float
                      },
                      "Set_float (1)"
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "-t",
                        /* constructor */{
                          tag: "Tuple",
                          Arg0: /* constructor */{
                            tag: "::",
                            Arg0: /* constructor */{
                              tag: "Bool",
                              Arg0: f_bool
                            },
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: /* constructor */{
                                tag: "String",
                                Arg0: f_string
                              },
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* constructor */{
                                  tag: "Int",
                                  Arg0: f_int
                                },
                                Arg1: "[]"
                              }
                            }
                          }
                        },
                        "Tuple (3)"
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          "-sym",
                          /* constructor */{
                            tag: "Symbol",
                            Arg0: /* constructor */{
                              tag: "::",
                              Arg0: "a",
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: "b",
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: "c",
                                  Arg1: "[]"
                                }
                              }
                            },
                            Arg1: f_symbol
                          },
                          "Symbol (1)"
                        ],
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            "-rest",
                            /* constructor */{
                              tag: "Rest",
                              Arg0: f_rest
                            },
                            "Rest (*)"
                          ],
                          Arg1: "[]"
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};

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
  return Curry._1(Printf.printf(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String_literal",
                    Arg0: "error (",
                    Arg1: /* constructor */{
                      tag: "String",
                      Arg0: "No_padding",
                      Arg1: /* constructor */{
                        tag: "String_literal",
                        Arg0: ")\n",
                        Arg1: "End_of_format"
                      }
                    }
                  },
                  Arg1: "error (%s)\n"
                }), s);
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
  accum[0] = "[]";
  Arg.parse_argv(current, argv, spec, f_anon, "usage");
  var result = List.rev(accum[0]);
  var reference = /* constructor */{
    tag: "::",
    Arg0: "anon(anon1)",
    Arg1: /* constructor */{
      tag: "::",
      Arg0: "unit()",
      Arg1: /* constructor */{
        tag: "::",
        Arg0: "bool(true)",
        Arg1: /* constructor */{
          tag: "::",
          Arg0: "anon(anon2)",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "string(foo)",
            Arg1: /* constructor */{
              tag: "::",
              Arg0: "int(19)",
              Arg1: /* constructor */{
                tag: "::",
                Arg0: "float(3.14)",
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: "anon(anon3)",
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: "bool(false)",
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: "string(gee)",
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: "int(1436)",
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: "symbol(c)",
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: "anon(anon4)",
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: "rest(r1)",
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: "rest(r2)",
                                Arg1: "[]"
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  };
  if (Caml_obj.caml_notequal(result, reference)) {
    var f = function (x, y) {
      return Curry._3(Printf.printf(/* constructor */{
                      tag: "Format",
                      Arg0: /* constructor */{
                        tag: "String",
                        Arg0: /* constructor */{
                          tag: "Lit_padding",
                          Arg0: "Right",
                          Arg1: 20
                        },
                        Arg1: /* constructor */{
                          tag: "Char_literal",
                          Arg0: /* " " */32,
                          Arg1: /* constructor */{
                            tag: "Char",
                            Arg0: /* constructor */{
                              tag: "Char_literal",
                              Arg0: /* " " */32,
                              Arg1: /* constructor */{
                                tag: "String",
                                Arg0: /* constructor */{
                                  tag: "Lit_padding",
                                  Arg0: "Left",
                                  Arg1: 20
                                },
                                Arg1: /* constructor */{
                                  tag: "Char_literal",
                                  Arg0: /* "\n" */10,
                                  Arg1: /* constructor */{
                                    tag: "Flush",
                                    Arg0: "End_of_format"
                                  }
                                }
                              }
                            }
                          }
                        }
                      },
                      Arg1: "%20s %c %-20s\n%!"
                    }), x, x === y ? /* "=" */61 : /* "#" */35, y);
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

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "should raise",
    (function (param) {
        return /* constructor */{
                tag: "ThrowAny",
                Arg0: (function (param) {
                    return test(args2);
                  })
              };
      })
  ],
  Arg1: "[]"
};

Mt.from_pair_suites("Libarg_test", suites);

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
