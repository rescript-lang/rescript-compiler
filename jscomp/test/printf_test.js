'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Format = require("../../lib/js/format.js");
var Printf = require("../../lib/js/printf.js");

function print_pair(fmt, param) {
  return Curry._2(Format.fprintf(fmt, /* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "Char_literal",
                    Arg0: /* "(" */40,
                    Arg1: /* constructor */{
                      tag: "Int",
                      Arg0: "Int_d",
                      Arg1: "No_padding",
                      Arg2: "No_precision",
                      Arg3: /* constructor */{
                        tag: "Char_literal",
                        Arg0: /* "," */44,
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
                      }
                    }
                  },
                  Arg1: "(%d,%d)"
                }), param[0], param[1]);
}

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "sprintf_simple",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: "3232",
                Arg1: Curry._2(Printf.sprintf(/* constructor */{
                          tag: "Format",
                          Arg0: /* constructor */{
                            tag: "String",
                            Arg0: "No_padding",
                            Arg1: /* constructor */{
                              tag: "Int",
                              Arg0: "Int_d",
                              Arg1: "No_padding",
                              Arg2: "No_precision",
                              Arg3: "End_of_format"
                            }
                          },
                          Arg1: "%s%d"
                        }), "32", 32)
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "print_asprintf",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: "xx",
                  Arg1: Format.asprintf(/* constructor */{
                        tag: "Format",
                        Arg0: /* constructor */{
                          tag: "String_literal",
                          Arg0: "xx",
                          Arg1: "End_of_format"
                        },
                        Arg1: "xx"
                      })
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "print_pair",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: "(1,2)",
                    Arg1: Curry._2(Format.asprintf(/* constructor */{
                              tag: "Format",
                              Arg0: /* constructor */{
                                tag: "Alpha",
                                Arg0: "End_of_format"
                              },
                              Arg1: "%a"
                            }), print_pair, /* tuple */[
                          1,
                          2
                        ])
                  };
          })
      ],
      Arg1: "[]"
    }
  }
};

var v = Format.asprintf(/* constructor */{
      tag: "Format",
      Arg0: /* constructor */{
        tag: "String_literal",
        Arg0: "xx",
        Arg1: "End_of_format"
      },
      Arg1: "xx"
    });

Mt.from_pair_suites("Printf_test", suites);

exports.print_pair = print_pair;
exports.suites = suites;
exports.v = v;
/* v Not a pure module */
