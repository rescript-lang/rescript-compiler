'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");
var Mt_global = require("./mt_global.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(f, param) {
  return Mt_global.collect_eq(test_id, suites, f, param[0], param[1]);
}

var s = Curry._1(Printf.sprintf(/* Format */[
          /* Format_arg */Block.__(13, [
              undefined,
              /* String_ty */Block.__(1, [/* End_of_fmtty */0]),
              /* Char_literal */Block.__(12, [
                  /* "." */46,
                  /* End_of_format */0
                ])
            ]),
          "%{%s%}."
        ]), /* Format */[
      /* String_literal */Block.__(11, [
          "32",
          /* String */Block.__(2, [
              /* No_padding */0,
              /* End_of_format */0
            ])
        ]),
      "32%s"
    ]);

eq("File \"sprintf_reg_test.ml\", line 8, characters 5-12", /* tuple */[
      s,
      "%s."
    ]);

var s$1 = Curry._2(Printf.sprintf(/* Format */[
          /* Int */Block.__(4, [
              /* Int_i */3,
              /* No_padding */0,
              /* No_precision */0,
              /* Char_literal */Block.__(12, [
                  /* " " */32,
                  /* Format_arg */Block.__(13, [
                      undefined,
                      /* String_ty */Block.__(1, [/* End_of_fmtty */0]),
                      /* End_of_format */0
                    ])
                ])
            ]),
          "%i %{%s%}"
        ]), 1, /* Format */[
      /* String_literal */Block.__(11, [
          "spells one ",
          /* String */Block.__(2, [
              /* No_padding */0,
              /* End_of_format */0
            ])
        ]),
      "spells one %s"
    ]);

eq("File \"sprintf_reg_test.ml\", line 14, characters 5-12", /* tuple */[
      s$1,
      "1 %s"
    ]);

Mt.from_pair_suites("Sprintf_reg_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/* s Not a pure module */
