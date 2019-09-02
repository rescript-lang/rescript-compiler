'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");
var Mt_global = require("./mt_global.js");

var suites = /* record */[/* contents */"[]"];

var test_id = /* record */[/* contents */0];

function eq(f, param) {
  return Mt_global.collect_eq(test_id, suites, f, param[0], param[1]);
}

var s = Curry._1(Printf.sprintf(/* constructor */{
          tag: "Format",
          Arg0: /* constructor */{
            tag: "Format_arg",
            Arg0: undefined,
            Arg1: /* constructor */{
              tag: "String_ty",
              Arg0: "End_of_fmtty"
            },
            Arg2: /* constructor */{
              tag: "Char_literal",
              Arg0: /* "." */46,
              Arg1: "End_of_format"
            }
          },
          Arg1: "%{%s%}."
        }), /* constructor */{
      tag: "Format",
      Arg0: /* constructor */{
        tag: "String_literal",
        Arg0: "32",
        Arg1: /* constructor */{
          tag: "String",
          Arg0: "No_padding",
          Arg1: "End_of_format"
        }
      },
      Arg1: "32%s"
    });

eq("File \"sprintf_reg_test.ml\", line 8, characters 5-12", /* tuple */[
      s,
      "%s."
    ]);

var s$1 = Curry._2(Printf.sprintf(/* constructor */{
          tag: "Format",
          Arg0: /* constructor */{
            tag: "Int",
            Arg0: "Int_i",
            Arg1: "No_padding",
            Arg2: "No_precision",
            Arg3: /* constructor */{
              tag: "Char_literal",
              Arg0: /* " " */32,
              Arg1: /* constructor */{
                tag: "Format_arg",
                Arg0: undefined,
                Arg1: /* constructor */{
                  tag: "String_ty",
                  Arg0: "End_of_fmtty"
                },
                Arg2: "End_of_format"
              }
            }
          },
          Arg1: "%i %{%s%}"
        }), 1, /* constructor */{
      tag: "Format",
      Arg0: /* constructor */{
        tag: "String_literal",
        Arg0: "spells one ",
        Arg1: /* constructor */{
          tag: "String",
          Arg0: "No_padding",
          Arg1: "End_of_format"
        }
      },
      Arg1: "spells one %s"
    });

eq("File \"sprintf_reg_test.ml\", line 14, characters 5-12", /* tuple */[
      s$1,
      "1 %s"
    ]);

Mt.from_pair_suites("Sprintf_reg_test", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/* s Not a pure module */
