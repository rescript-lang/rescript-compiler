// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt        = require("./mt");
var Mt_global = require("./mt_global");
var Curry     = require("../runtime/curry");
var Printf    = require("../stdlib/printf");

var suites = [/* [] */0];

var test_id = [0];

function eq(f, param) {
  return Mt_global.collect_eq(test_id, suites, f, param[0], param[1]);
}

var s = Curry._1(Printf.sprintf(/* Format */[
          /* Format_arg */{
            0: /* None */0,
            1: /* String_ty */{
              0: /* End_of_fmtty */0,
              length: 1,
              tag: 1
            },
            2: /* Char_literal */{
              0: /* "." */46,
              1: /* End_of_format */0,
              length: 2,
              tag: 12
            },
            length: 3,
            tag: 13
          },
          "%{%s%}."
        ]), /* Format */[
      /* String_literal */{
        0: "32",
        1: /* String */{
          0: /* No_padding */0,
          1: /* End_of_format */0,
          length: 2,
          tag: 2
        },
        length: 2,
        tag: 11
      },
      "32%s"
    ]);

eq('File "sprintf_reg_test.ml", line 8, characters 5-12', /* tuple */[
      s,
      "%s."
    ]);

var s$1 = Curry._2(Printf.sprintf(/* Format */[
          /* Int */{
            0: /* Int_i */3,
            1: /* No_padding */0,
            2: /* No_precision */0,
            3: /* Char_literal */{
              0: /* " " */32,
              1: /* Format_arg */{
                0: /* None */0,
                1: /* String_ty */{
                  0: /* End_of_fmtty */0,
                  length: 1,
                  tag: 1
                },
                2: /* End_of_format */0,
                length: 3,
                tag: 13
              },
              length: 2,
              tag: 12
            },
            length: 4,
            tag: 4
          },
          "%i %{%s%}"
        ]), 1, /* Format */[
      /* String_literal */{
        0: "spells one ",
        1: /* String */{
          0: /* No_padding */0,
          1: /* End_of_format */0,
          length: 2,
          tag: 2
        },
        length: 2,
        tag: 11
      },
      "spells one %s"
    ]);

eq('File "sprintf_reg_test.ml", line 14, characters 5-12', /* tuple */[
      s$1,
      "1 %s"
    ]);

Mt.from_pair_suites("sprintf_reg_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
/* s Not a pure module */
