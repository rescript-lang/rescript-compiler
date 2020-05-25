'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Format = require("../../lib/js/format.js");
var Printf = require("../../lib/js/printf.js");

function print_pair(fmt, param) {
  return Curry._2(Format.fprintf(fmt, /* Format */{
                  _0: {
                    tag: /* Char_literal */12,
                    _0: /* "(" */40,
                    _1: {
                      tag: /* Int */4,
                      _0: /* Int_d */0,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        tag: /* Char_literal */12,
                        _0: /* "," */44,
                        _1: {
                          tag: /* Int */4,
                          _0: /* Int_d */0,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: {
                            tag: /* Char_literal */12,
                            _0: /* ")" */41,
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    }
                  },
                  _1: "(%d,%d)"
                }), param[0], param[1]);
}

var suites_0 = [
  "sprintf_simple",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: "3232",
              _1: Curry._2(Printf.sprintf(/* Format */{
                        _0: {
                          tag: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            tag: /* Int */4,
                            _0: /* Int_d */0,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: /* End_of_format */0
                          }
                        },
                        _1: "%s%d"
                      }), "32", 32)
            };
    })
];

var suites_1 = /* :: */{
  _0: [
    "print_asprintf",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: "xx",
                _1: Format.asprintf(/* Format */{
                      _0: {
                        tag: /* String_literal */11,
                        _0: "xx",
                        _1: /* End_of_format */0
                      },
                      _1: "xx"
                    })
              };
      })
  ],
  _1: /* :: */{
    _0: [
      "print_pair",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: "(1,2)",
                  _1: Curry._2(Format.asprintf(/* Format */{
                            _0: {
                              tag: /* Alpha */15,
                              _0: /* End_of_format */0
                            },
                            _1: "%a"
                          }), print_pair, [
                        1,
                        2
                      ])
                };
        })
    ],
    _1: /* [] */0
  }
};

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

var v = Format.asprintf(/* Format */{
      _0: {
        tag: /* String_literal */11,
        _0: "xx",
        _1: /* End_of_format */0
      },
      _1: "xx"
    });

Mt.from_pair_suites("Printf_test", suites);

exports.print_pair = print_pair;
exports.suites = suites;
exports.v = v;
/* v Not a pure module */
