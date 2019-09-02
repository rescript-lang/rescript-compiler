'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Sexpm = require("./sexpm.js");
var Format = require("../../lib/js/format.js");

var suites = /* record */[/* contents */"[]"];

var test_id = /* record */[/* contents */0];

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: x,
                  Arg1: y
                };
        })
    ],
    Arg1: suites[0]
  };
  return /* () */0;
}

function print_or_error(fmt, x) {
  if (x[0] >= 106380200) {
    return Curry._1(Format.fprintf(fmt, /* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "Formatting_gen",
                      Arg0: /* constructor */{
                        tag: "Open_box",
                        Arg0: /* constructor */{
                          tag: "Format",
                          Arg0: "End_of_format",
                          Arg1: ""
                        }
                      },
                      Arg1: /* constructor */{
                        tag: "String_literal",
                        Arg0: "Error:",
                        Arg1: /* constructor */{
                          tag: "String",
                          Arg0: "No_padding",
                          Arg1: /* constructor */{
                            tag: "Formatting_lit",
                            Arg0: "Close_box",
                            Arg1: /* constructor */{
                              tag: "Formatting_lit",
                              Arg0: "Flush_newline",
                              Arg1: "End_of_format"
                            }
                          }
                        }
                      }
                    },
                    Arg1: "@[Error:%s@]@."
                  }), x[1]);
  } else {
    return Curry._2(Format.fprintf(fmt, /* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "Formatting_gen",
                      Arg0: /* constructor */{
                        tag: "Open_box",
                        Arg0: /* constructor */{
                          tag: "Format",
                          Arg0: "End_of_format",
                          Arg1: ""
                        }
                      },
                      Arg1: /* constructor */{
                        tag: "String_literal",
                        Arg0: "Ok:",
                        Arg1: /* constructor */{
                          tag: "Alpha",
                          Arg0: /* constructor */{
                            tag: "Formatting_lit",
                            Arg0: "Close_box",
                            Arg1: /* constructor */{
                              tag: "Formatting_lit",
                              Arg0: "Flush_newline",
                              Arg1: "End_of_format"
                            }
                          }
                        }
                      }
                    },
                    Arg1: "@[Ok:%a@]@."
                  }), Sexpm.print, x[1]);
  }
}

var a = Sexpm.parse_string("(x x gh 3 3)");

eq("File \"sexpm_test.ml\", line 17, characters 7-14", /* tuple */[
      /* `Ok */[
        17724,
        /* `List */[
          848054398,
          /* constructor */{
            tag: "::",
            Arg0: /* `Atom */[
              726615281,
              "x"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* `Atom */[
                726615281,
                "x"
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* `Atom */[
                  726615281,
                  "gh"
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* `Atom */[
                    726615281,
                    "3"
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* `Atom */[
                      726615281,
                      "3"
                    ],
                    Arg1: "[]"
                  }
                }
              }
            }
          }
        ]
      ],
      a
    ]);

eq("File \"sexpm_test.ml\", line 21, characters 7-14", /* tuple */[
      Curry._2(Format.asprintf(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "Alpha",
                    Arg0: "End_of_format"
                  },
                  Arg1: "%a"
                }), print_or_error, a).trim(),
      "Ok:(x x gh 3 3)\n".trim()
    ]);

Mt.from_pair_suites("Sexpm_test", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.print_or_error = print_or_error;
/* a Not a pure module */
