'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_format = require("../../lib/js/caml_format.js");

var of_string = [
  [
    0,
    "0"
  ],
  [
    3,
    "03"
  ],
  [
    -3,
    "-03"
  ],
  [
    -63,
    "-0x3f"
  ],
  [
    -31,
    "-0x1f"
  ],
  [
    47,
    "0X2f"
  ],
  [
    11,
    "0O13"
  ],
  [
    8,
    "0o10"
  ],
  [
    3,
    "0b11"
  ],
  [
    1,
    "0b01"
  ],
  [
    0,
    "0b00"
  ],
  [
    -3,
    "-0b11"
  ],
  [
    -5,
    "-0B101"
  ],
  [
    332,
    "0332"
  ],
  [
    -32,
    "-32"
  ],
  [
    1,
    "-0xffff_ffff"
  ],
  [
    -1,
    "0xffff_ffff"
  ]
];

function from_float_of_string(xs) {
  return $$Array.mapi((function (i, param) {
                return Pervasives.string_of_float;
              }), xs);
}

function from_of_string(xs) {
  return $$Array.to_list($$Array.mapi((function (i, param) {
                    var b = param[1];
                    var a = param[0];
                    return [
                            "of_string " + i,
                            (function (param) {
                                return {
                                        TAG: /* Eq */0,
                                        _0: Caml_format.int_of_string(b),
                                        _1: a
                                      };
                              })
                          ];
                  }), of_string));
}

var to_str = Caml_format.int_of_string;

var pairs = [
  [
    /* FP_infinite */3,
    "infinity"
  ],
  [
    /* FP_infinite */3,
    "+infinity"
  ],
  [
    /* FP_infinite */3,
    "-infinity"
  ],
  [
    /* FP_zero */2,
    "0"
  ],
  [
    /* FP_zero */2,
    "0."
  ]
];

var pairs$1 = [
  [
    3232,
    "32_32.0"
  ],
  [
    1.000,
    "1.000"
  ],
  [
    12.000,
    "12.000"
  ]
];

var suites = Pervasives.$at(from_of_string(of_string), Pervasives.$at({
          hd: [
            "isnan_of_string",
            (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: true,
                        _1: Pervasives.classify_float(Caml_format.float_of_string("nan")) === /* FP_nan */4
                      };
              })
          ],
          tl: /* [] */0
        }, Pervasives.$at($$Array.to_list($$Array.mapi((function (i, param) {
                        var b = param[1];
                        var a = param[0];
                        return [
                                "infinity_of_string " + i,
                                (function (param) {
                                    return {
                                            TAG: /* Eq */0,
                                            _0: a,
                                            _1: Pervasives.classify_float(Caml_format.float_of_string(b))
                                          };
                                  })
                              ];
                      }), pairs)), Pervasives.$at({
                  hd: [
                    "throw",
                    (function (param) {
                        return {
                                TAG: /* ThrowAny */7,
                                _0: (function (param) {
                                    Caml_format.float_of_string("");
                                    
                                  })
                              };
                      })
                  ],
                  tl: {
                    hd: [
                      "format_int",
                      (function (param) {
                          return {
                                  TAG: /* Eq */0,
                                  _0: "                              33",
                                  _1: Caml_format.format_int("%32d", 33)
                                };
                        })
                    ],
                    tl: /* [] */0
                  }
                }, $$Array.to_list($$Array.mapi((function (i, param) {
                            var b = param[1];
                            var a = param[0];
                            return [
                                    "normal_float_of_string " + i,
                                    (function (param) {
                                        return {
                                                TAG: /* Eq */0,
                                                _0: a,
                                                _1: Caml_format.float_of_string(b)
                                              };
                                      })
                                  ];
                          }), pairs$1))))));

function ff(param) {
  return Caml_format.format_int("%32d", param);
}

var float_data = [
  [
    "%f",
    32,
    "32.000000"
  ],
  [
    "%f",
    Number.NaN,
    "nan"
  ],
  [
    "%f",
    Pervasives.infinity,
    "inf"
  ],
  [
    "%f",
    Pervasives.neg_infinity,
    "-inf"
  ],
  [
    "%1.e",
    13000,
    "1e+04"
  ],
  [
    "%1.3e",
    2.3e-05,
    "2.300e-05"
  ],
  [
    "%3.10e",
    3e+56,
    "3.0000000000e+56"
  ],
  [
    "%3.10f",
    20000000000,
    "20000000000.0000000000"
  ],
  [
    "%3.3f",
    -3300,
    "-3300.000"
  ],
  [
    "%1.g",
    13000,
    "1e+04"
  ],
  [
    "%1.3g",
    2.3e-05,
    "2.3e-05"
  ],
  [
    "%3.10g",
    3e+56,
    "3e+56"
  ],
  [
    "%3.10g",
    20000000000,
    "2e+10"
  ],
  [
    "%3.3g",
    -3300,
    "-3.3e+03"
  ],
  [
    "%3.3g",
    -0.0033,
    "-0.0033"
  ],
  [
    "%3.10g",
    30000000000,
    "3e+10"
  ],
  [
    "%3.0g",
    30000000000,
    "3e+10"
  ],
  [
    "%3.g",
    30000000000,
    "3e+10"
  ],
  [
    "%3.g",
    3,
    "  3"
  ],
  [
    "%1.1g",
    2.1,
    "2"
  ],
  [
    "%1.2g",
    2.1,
    "2.1"
  ]
];

var int64_suites_0 = [
  "i64_simple7",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: Caml_int64.to_string([
                    0,
                    3333
                  ]),
              _1: "3333"
            };
    })
];

var int64_suites_1 = {
  hd: [
    "i64_simple15",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: Caml_int64.to_string(Caml_int64.neg_one),
                _1: "-1"
              };
      })
  ],
  tl: {
    hd: [
      "i64_simple16",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: Caml_int64.to_string([
                        -1,
                        4294956185
                      ]),
                  _1: "-11111"
                };
        })
    ],
    tl: /* [] */0
  }
};

var int64_suites = {
  hd: int64_suites_0,
  tl: int64_suites_1
};

var of_string_data = [
  [
    Caml_int64.zero,
    "0"
  ],
  [
    [
      0,
      3
    ],
    "3"
  ],
  [
    [
      0,
      33
    ],
    "33"
  ],
  [
    [
      0,
      333
    ],
    "33_3"
  ],
  [
    [
      0,
      33333
    ],
    "33_33_3"
  ],
  [
    [
      77,
      2620851541
    ],
    "333333333333"
  ],
  [
    Caml_int64.neg_one,
    "0xffff_ffff_ffff_ffff"
  ],
  [
    [
      0,
      113
    ],
    "0b01110001"
  ],
  [
    Caml_int64.one,
    "-0xffff_ffff_ffff_ffff"
  ]
];

Mt.from_pair_suites("Caml_format_test", Pervasives.$at(suites, Pervasives.$at($$Array.to_list($$Array.mapi((function (i, param) {
                        var str_result = param[2];
                        var f = param[1];
                        var fmt = param[0];
                        return [
                                "loat_format " + i,
                                (function (param) {
                                    return {
                                            TAG: /* Eq */0,
                                            _0: Caml_format.format_float(fmt, f),
                                            _1: str_result
                                          };
                                  })
                              ];
                      }), float_data)), Pervasives.$at(int64_suites, $$Array.to_list($$Array.mapi((function (i, param) {
                            var b = param[1];
                            var a = param[0];
                            return [
                                    "int64_of_string " + i + " ",
                                    (function (param) {
                                        return {
                                                TAG: /* Eq */0,
                                                _0: Caml_format.int64_of_string(b),
                                                _1: a
                                              };
                                      })
                                  ];
                          }), of_string_data))))));

var float_suites = {
  hd: "float_nan",
  tl: /* [] */0
};

var hh = [
  214748364,
  3435973836
];

var hhh = [
  268435456,
  0
];

exports.of_string = of_string;
exports.from_float_of_string = from_float_of_string;
exports.from_of_string = from_of_string;
exports.to_str = to_str;
exports.suites = suites;
exports.ff = ff;
exports.float_data = float_data;
exports.float_suites = float_suites;
exports.int64_suites = int64_suites;
exports.hh = hh;
exports.hhh = hhh;
exports.of_string_data = of_string_data;
/* suites Not a pure module */
