'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Scanf = require("../../lib/js/scanf.js");
var Mt_global = require("./mt_global.js");

var suites = /* record */[/* contents */"[]"];

var test_id = /* record */[/* contents */0];

function eq(f, param) {
  return Mt_global.collect_eq(test_id, suites, f, param[0], param[1]);
}

eq("File \"scanf_test.ml\", line 6, characters 5-12", /* tuple */[
      Curry._1(Scanf.sscanf("32 31", /* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "Int",
                  Arg0: "Int_d",
                  Arg1: "No_padding",
                  Arg2: "No_precision",
                  Arg3: /* constructor */{
                    tag: "Char_literal",
                    Arg0: /* " " */32,
                    Arg1: /* constructor */{
                      tag: "Int",
                      Arg0: "Int_d",
                      Arg1: "No_padding",
                      Arg2: "No_precision",
                      Arg3: "End_of_format"
                    }
                  }
                },
                Arg1: "%d %d"
              }), (function (x, y) {
              return x + y | 0;
            })),
      63
    ]);

eq("File \"scanf_test.ml\", line 7, characters 5-12", /* tuple */[
      Curry._1(Scanf.sscanf("12306459064359371967", /* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "Int64",
                  Arg0: "Int_u",
                  Arg1: "No_padding",
                  Arg2: "No_precision",
                  Arg3: "End_of_format"
                },
                Arg1: "%Lu"
              }), (function (i) {
              return i;
            })),
      /* int64 */[
        /* hi */-1429646511,
        /* lo */235324607
      ]
    ]);

Mt.from_pair_suites("Scanf_test", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
