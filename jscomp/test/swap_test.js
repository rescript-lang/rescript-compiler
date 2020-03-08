'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Format = require("../../lib/js/format.js");
var Printf = require("../../lib/js/printf.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Pervasives = require("../../lib/js/pervasives.js");

var tests_16 = [
  /* tuple */[
    1,
    256
  ],
  /* tuple */[
    2,
    512
  ],
  /* tuple */[
    4,
    1024
  ],
  /* tuple */[
    8,
    2048
  ],
  /* tuple */[
    16,
    4096
  ],
  /* tuple */[
    32,
    8192
  ],
  /* tuple */[
    64,
    16384
  ],
  /* tuple */[
    128,
    32768
  ],
  /* tuple */[
    256,
    1
  ],
  /* tuple */[
    512,
    2
  ],
  /* tuple */[
    1024,
    4
  ],
  /* tuple */[
    2048,
    8
  ],
  /* tuple */[
    4096,
    16
  ],
  /* tuple */[
    8192,
    32
  ],
  /* tuple */[
    16384,
    64
  ],
  /* tuple */[
    32768,
    128
  ]
];

var tests_32 = [
  /* tuple */[
    1,
    16777216
  ],
  /* tuple */[
    2,
    33554432
  ],
  /* tuple */[
    4,
    67108864
  ],
  /* tuple */[
    8,
    134217728
  ],
  /* tuple */[
    16,
    268435456
  ],
  /* tuple */[
    32,
    536870912
  ],
  /* tuple */[
    64,
    1073741824
  ],
  /* tuple */[
    128,
    -2147483648
  ],
  /* tuple */[
    256,
    65536
  ],
  /* tuple */[
    512,
    131072
  ],
  /* tuple */[
    1024,
    262144
  ],
  /* tuple */[
    2048,
    524288
  ],
  /* tuple */[
    4096,
    1048576
  ],
  /* tuple */[
    8192,
    2097152
  ],
  /* tuple */[
    16384,
    4194304
  ],
  /* tuple */[
    32768,
    8388608
  ],
  /* tuple */[
    65536,
    256
  ],
  /* tuple */[
    131072,
    512
  ],
  /* tuple */[
    262144,
    1024
  ],
  /* tuple */[
    524288,
    2048
  ],
  /* tuple */[
    1048576,
    4096
  ],
  /* tuple */[
    2097152,
    8192
  ],
  /* tuple */[
    4194304,
    16384
  ],
  /* tuple */[
    8388608,
    32768
  ],
  /* tuple */[
    16777216,
    1
  ],
  /* tuple */[
    33554432,
    2
  ],
  /* tuple */[
    67108864,
    4
  ],
  /* tuple */[
    134217728,
    8
  ],
  /* tuple */[
    268435456,
    16
  ],
  /* tuple */[
    536870912,
    32
  ],
  /* tuple */[
    1073741824,
    64
  ],
  /* tuple */[
    -2147483648,
    128
  ]
];

var tests_64 = [
  /* tuple */[
    Caml_int64.one,
    Caml_int64.mk(0, 16777216)
  ],
  /* tuple */[
    Caml_int64.mk(2, 0),
    Caml_int64.mk(0, 33554432)
  ],
  /* tuple */[
    Caml_int64.mk(4, 0),
    Caml_int64.mk(0, 67108864)
  ],
  /* tuple */[
    Caml_int64.mk(8, 0),
    Caml_int64.mk(0, 134217728)
  ],
  /* tuple */[
    Caml_int64.mk(16, 0),
    Caml_int64.mk(0, 268435456)
  ],
  /* tuple */[
    Caml_int64.mk(32, 0),
    Caml_int64.mk(0, 536870912)
  ],
  /* tuple */[
    Caml_int64.mk(64, 0),
    Caml_int64.mk(0, 1073741824)
  ],
  /* tuple */[
    Caml_int64.mk(128, 0),
    Caml_int64.min_int
  ],
  /* tuple */[
    Caml_int64.mk(256, 0),
    Caml_int64.mk(0, 65536)
  ],
  /* tuple */[
    Caml_int64.mk(512, 0),
    Caml_int64.mk(0, 131072)
  ],
  /* tuple */[
    Caml_int64.mk(1024, 0),
    Caml_int64.mk(0, 262144)
  ],
  /* tuple */[
    Caml_int64.mk(2048, 0),
    Caml_int64.mk(0, 524288)
  ],
  /* tuple */[
    Caml_int64.mk(4096, 0),
    Caml_int64.mk(0, 1048576)
  ],
  /* tuple */[
    Caml_int64.mk(8192, 0),
    Caml_int64.mk(0, 2097152)
  ],
  /* tuple */[
    Caml_int64.mk(16384, 0),
    Caml_int64.mk(0, 4194304)
  ],
  /* tuple */[
    Caml_int64.mk(32768, 0),
    Caml_int64.mk(0, 8388608)
  ],
  /* tuple */[
    Caml_int64.mk(65536, 0),
    Caml_int64.mk(0, 256)
  ],
  /* tuple */[
    Caml_int64.mk(131072, 0),
    Caml_int64.mk(0, 512)
  ],
  /* tuple */[
    Caml_int64.mk(262144, 0),
    Caml_int64.mk(0, 1024)
  ],
  /* tuple */[
    Caml_int64.mk(524288, 0),
    Caml_int64.mk(0, 2048)
  ],
  /* tuple */[
    Caml_int64.mk(1048576, 0),
    Caml_int64.mk(0, 4096)
  ],
  /* tuple */[
    Caml_int64.mk(2097152, 0),
    Caml_int64.mk(0, 8192)
  ],
  /* tuple */[
    Caml_int64.mk(4194304, 0),
    Caml_int64.mk(0, 16384)
  ],
  /* tuple */[
    Caml_int64.mk(8388608, 0),
    Caml_int64.mk(0, 32768)
  ],
  /* tuple */[
    Caml_int64.mk(16777216, 0),
    Caml_int64.mk(0, 1)
  ],
  /* tuple */[
    Caml_int64.mk(33554432, 0),
    Caml_int64.mk(0, 2)
  ],
  /* tuple */[
    Caml_int64.mk(67108864, 0),
    Caml_int64.mk(0, 4)
  ],
  /* tuple */[
    Caml_int64.mk(134217728, 0),
    Caml_int64.mk(0, 8)
  ],
  /* tuple */[
    Caml_int64.mk(268435456, 0),
    Caml_int64.mk(0, 16)
  ],
  /* tuple */[
    Caml_int64.mk(536870912, 0),
    Caml_int64.mk(0, 32)
  ],
  /* tuple */[
    Caml_int64.mk(1073741824, 0),
    Caml_int64.mk(0, 64)
  ],
  /* tuple */[
    Caml_int64.mk(-2147483648, 0),
    Caml_int64.mk(0, 128)
  ],
  /* tuple */[
    Caml_int64.mk(0, 1),
    Caml_int64.mk(16777216, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 2),
    Caml_int64.mk(33554432, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 4),
    Caml_int64.mk(67108864, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 8),
    Caml_int64.mk(134217728, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 16),
    Caml_int64.mk(268435456, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 32),
    Caml_int64.mk(536870912, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 64),
    Caml_int64.mk(1073741824, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 128),
    Caml_int64.mk(-2147483648, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 256),
    Caml_int64.mk(65536, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 512),
    Caml_int64.mk(131072, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 1024),
    Caml_int64.mk(262144, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 2048),
    Caml_int64.mk(524288, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 4096),
    Caml_int64.mk(1048576, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 8192),
    Caml_int64.mk(2097152, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 16384),
    Caml_int64.mk(4194304, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 32768),
    Caml_int64.mk(8388608, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 65536),
    Caml_int64.mk(256, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 131072),
    Caml_int64.mk(512, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 262144),
    Caml_int64.mk(1024, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 524288),
    Caml_int64.mk(2048, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 1048576),
    Caml_int64.mk(4096, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 2097152),
    Caml_int64.mk(8192, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 4194304),
    Caml_int64.mk(16384, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 8388608),
    Caml_int64.mk(32768, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 16777216),
    Caml_int64.one
  ],
  /* tuple */[
    Caml_int64.mk(0, 33554432),
    Caml_int64.mk(2, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 67108864),
    Caml_int64.mk(4, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 134217728),
    Caml_int64.mk(8, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 268435456),
    Caml_int64.mk(16, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 536870912),
    Caml_int64.mk(32, 0)
  ],
  /* tuple */[
    Caml_int64.mk(0, 1073741824),
    Caml_int64.mk(64, 0)
  ],
  /* tuple */[
    Caml_int64.min_int,
    Caml_int64.mk(128, 0)
  ]
];

var suites_16 = List.map((function (param) {
        var b = param[1];
        var a = param[0];
        return /* tuple */[
                Curry._1(Printf.sprintf(/* Format */[
                          /* String_literal */Block.__(11, [
                              "swap16 ",
                              /* Int */Block.__(4, [
                                  /* Int_d */0,
                                  /* No_padding */0,
                                  /* No_precision */0,
                                  /* End_of_format */0
                                ])
                            ]),
                          "swap16 %d"
                        ]), a),
                (function (param) {
                    return /* Eq */Block.__(0, [
                              Caml_int32.caml_bswap16(a),
                              b
                            ]);
                  })
              ];
      }), $$Array.to_list(tests_16));

var suites_32 = List.map((function (param) {
        var b = param[1];
        var a = param[0];
        return /* tuple */[
                Curry._1(Printf.sprintf(/* Format */[
                          /* String_literal */Block.__(11, [
                              "swap32 ",
                              /* Int */Block.__(4, [
                                  /* Int_d */0,
                                  /* No_padding */0,
                                  /* No_precision */0,
                                  /* End_of_format */0
                                ])
                            ]),
                          "swap32 %d"
                        ]), a),
                (function (param) {
                    return /* Eq */Block.__(0, [
                              Caml_int32.caml_int32_bswap(a),
                              b
                            ]);
                  })
              ];
      }), $$Array.to_list(tests_32));

var suites_64 = List.map((function (param) {
        var b = param[1];
        var a = param[0];
        return /* tuple */[
                Curry._1(Printf.sprintf(/* Format */[
                          /* String_literal */Block.__(11, [
                              "swap64 ",
                              /* Int */Block.__(4, [
                                  /* Int_d */0,
                                  /* No_padding */0,
                                  /* No_precision */0,
                                  /* End_of_format */0
                                ])
                            ]),
                          "swap64 %d"
                        ]), Caml_int64.to_int32(a)),
                (function (param) {
                    return /* Eq */Block.__(0, [
                              Caml_int64.swap(a),
                              b
                            ]);
                  })
              ];
      }), $$Array.to_list(tests_64));

var d16_000 = /* Format */[
  /* Int */Block.__(4, [
      /* Int_x */6,
      /* No_padding */0,
      /* No_precision */0,
      /* End_of_format */0
    ]),
  "%x"
];

var d16_001 = Caml_int32.caml_bswap16;

var d16_002 = [
  /* tuple */[
    287454020,
    "4433"
  ],
  /* tuple */[
    61680,
    "f0f0"
  ]
];

var d16 = /* tuple */[
  d16_000,
  d16_001,
  d16_002
];

var d32_000 = /* Format */[
  /* Int32 */Block.__(5, [
      /* Int_x */6,
      /* No_padding */0,
      /* No_precision */0,
      /* End_of_format */0
    ]),
  "%lx"
];

var d32_001 = Caml_int32.caml_int32_bswap;

var d32_002 = [
  /* tuple */[
    287454020,
    "44332211"
  ],
  /* tuple */[
    -252645136,
    "f0f0f0f0"
  ]
];

var d32 = /* tuple */[
  d32_000,
  d32_001,
  d32_002
];

function f(s, param) {
  var swap = param[1];
  var x = param[0];
  return $$Array.to_list($$Array.mapi((function (i, param) {
                    var b = param[1];
                    var a = param[0];
                    return /* tuple */[
                            Curry._2(Format.asprintf(/* Format */[
                                      /* String */Block.__(2, [
                                          /* No_padding */0,
                                          /* Char_literal */Block.__(12, [
                                              /* " " */32,
                                              /* Int */Block.__(4, [
                                                  /* Int_i */3,
                                                  /* No_padding */0,
                                                  /* No_precision */0,
                                                  /* End_of_format */0
                                                ])
                                            ])
                                        ]),
                                      "%s %i"
                                    ]), s, i),
                            (function (param) {
                                return /* Eq */Block.__(0, [
                                          Curry._1(Format.asprintf(x), Curry._1(swap, a)),
                                          b
                                        ]);
                              })
                          ];
                  }), param[2]));
}

Mt.from_pair_suites("Swap_test", Pervasives.$at(suites_16, Pervasives.$at(suites_32, Pervasives.$at(suites_64, Pervasives.$at(f("d16", d16), f("d32", d32))))));

exports.tests_16 = tests_16;
exports.tests_32 = tests_32;
exports.tests_64 = tests_64;
exports.suites_16 = suites_16;
exports.suites_32 = suites_32;
exports.suites_64 = suites_64;
exports.d16 = d16;
exports.d32 = d32;
exports.f = f;
/* suites_16 Not a pure module */
