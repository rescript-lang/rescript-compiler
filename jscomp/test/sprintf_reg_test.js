'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");
var Mt_global = require("./mt_global.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(f, param) {
  return Mt_global.collect_eq(test_id, suites, f, param[0], param[1]);
}

var s = Curry._1(Printf.sprintf(/* Format */{
          _0: {
            TAG: /* Format_arg */13,
            _0: undefined,
            _1: {
              TAG: /* String_ty */1,
              _0: /* End_of_fmtty */0
            },
            _2: {
              TAG: /* Char_literal */12,
              _0: /* "." */46,
              _1: /* End_of_format */0
            }
          },
          _1: "%{%s%}."
        }), /* Format */{
      _0: {
        TAG: /* String_literal */11,
        _0: "32",
        _1: {
          TAG: /* String */2,
          _0: /* No_padding */0,
          _1: /* End_of_format */0
        }
      },
      _1: "32%s"
    });

eq("File \"sprintf_reg_test.ml\", line 8, characters 5-12", [
      s,
      "%s."
    ]);

var s$1 = Curry._2(Printf.sprintf(/* Format */{
          _0: {
            TAG: /* Int */4,
            _0: /* Int_i */3,
            _1: /* No_padding */0,
            _2: /* No_precision */0,
            _3: {
              TAG: /* Char_literal */12,
              _0: /* " " */32,
              _1: {
                TAG: /* Format_arg */13,
                _0: undefined,
                _1: {
                  TAG: /* String_ty */1,
                  _0: /* End_of_fmtty */0
                },
                _2: /* End_of_format */0
              }
            }
          },
          _1: "%i %{%s%}"
        }), 1, /* Format */{
      _0: {
        TAG: /* String_literal */11,
        _0: "spells one ",
        _1: {
          TAG: /* String */2,
          _0: /* No_padding */0,
          _1: /* End_of_format */0
        }
      },
      _1: "spells one %s"
    });

eq("File \"sprintf_reg_test.ml\", line 14, characters 5-12", [
      s$1,
      "1 %s"
    ]);

Mt.from_pair_suites("Sprintf_reg_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/* s Not a pure module */
