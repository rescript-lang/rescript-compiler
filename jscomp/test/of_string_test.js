'use strict';

var Mt = require("./mt.js");
var Pervasives = require("../../lib/js/pervasives.js");

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "string_of_float_1",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: "10.",
                Arg1: Pervasives.string_of_float(10)
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "string_of_int",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: "10",
                  Arg1: String(10)
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "valid_float_lexem",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: "10.",
                    Arg1: Pervasives.valid_float_lexem("10")
                  };
          })
      ],
      Arg1: "[]"
    }
  }
};

Mt.from_pair_suites("Of_string_test", suites);

exports.suites = suites;
/*  Not a pure module */
