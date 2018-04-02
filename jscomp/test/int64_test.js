'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Int32 = require("../../lib/js/int32.js");
var Int64 = require("../../lib/js/int64.js");
var Format = require("../../lib/js/format.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Ext_array_test = require("./ext_array_test.js");

function f(u, v) {
  return u > v;
}

var v = Caml_int64.add(Caml_int64.of_int32(Int32.max_int), Int64.one);

var h = Caml_int64.neg(v);

var a = /* int64 */[
  /* hi */0,
  /* lo */2147483647
];

function commutative_add(result, a, b) {
  return /* Eq */Block.__(0, [
            /* tuple */[
              result,
              result
            ],
            /* tuple */[
              Caml_int64.add(a, b),
              Caml_int64.add(b, a)
            ]
          ]);
}

var generic_compare = Caml_obj.caml_compare;

var shift_left_tests_000 = $$Array.map((function (i) {
        return Caml_int64.lsl_(/* int64 */[
                    /* hi */0,
                    /* lo */1
                  ], i);
      }), Ext_array_test.range(0, 63));

var shift_left_tests_001 = /* array */[
  /* int64 */[
    /* hi */0,
    /* lo */1
  ],
  /* int64 */[
    /* hi */0,
    /* lo */2
  ],
  /* int64 */[
    /* hi */0,
    /* lo */4
  ],
  /* int64 */[
    /* hi */0,
    /* lo */8
  ],
  /* int64 */[
    /* hi */0,
    /* lo */16
  ],
  /* int64 */[
    /* hi */0,
    /* lo */32
  ],
  /* int64 */[
    /* hi */0,
    /* lo */64
  ],
  /* int64 */[
    /* hi */0,
    /* lo */128
  ],
  /* int64 */[
    /* hi */0,
    /* lo */256
  ],
  /* int64 */[
    /* hi */0,
    /* lo */512
  ],
  /* int64 */[
    /* hi */0,
    /* lo */1024
  ],
  /* int64 */[
    /* hi */0,
    /* lo */2048
  ],
  /* int64 */[
    /* hi */0,
    /* lo */4096
  ],
  /* int64 */[
    /* hi */0,
    /* lo */8192
  ],
  /* int64 */[
    /* hi */0,
    /* lo */16384
  ],
  /* int64 */[
    /* hi */0,
    /* lo */32768
  ],
  /* int64 */[
    /* hi */0,
    /* lo */65536
  ],
  /* int64 */[
    /* hi */0,
    /* lo */131072
  ],
  /* int64 */[
    /* hi */0,
    /* lo */262144
  ],
  /* int64 */[
    /* hi */0,
    /* lo */524288
  ],
  /* int64 */[
    /* hi */0,
    /* lo */1048576
  ],
  /* int64 */[
    /* hi */0,
    /* lo */2097152
  ],
  /* int64 */[
    /* hi */0,
    /* lo */4194304
  ],
  /* int64 */[
    /* hi */0,
    /* lo */8388608
  ],
  /* int64 */[
    /* hi */0,
    /* lo */16777216
  ],
  /* int64 */[
    /* hi */0,
    /* lo */33554432
  ],
  /* int64 */[
    /* hi */0,
    /* lo */67108864
  ],
  /* int64 */[
    /* hi */0,
    /* lo */134217728
  ],
  /* int64 */[
    /* hi */0,
    /* lo */268435456
  ],
  /* int64 */[
    /* hi */0,
    /* lo */536870912
  ],
  /* int64 */[
    /* hi */0,
    /* lo */1073741824
  ],
  /* int64 */[
    /* hi */0,
    /* lo */2147483648
  ],
  /* int64 */[
    /* hi */1,
    /* lo */0
  ],
  /* int64 */[
    /* hi */2,
    /* lo */0
  ],
  /* int64 */[
    /* hi */4,
    /* lo */0
  ],
  /* int64 */[
    /* hi */8,
    /* lo */0
  ],
  /* int64 */[
    /* hi */16,
    /* lo */0
  ],
  /* int64 */[
    /* hi */32,
    /* lo */0
  ],
  /* int64 */[
    /* hi */64,
    /* lo */0
  ],
  /* int64 */[
    /* hi */128,
    /* lo */0
  ],
  /* int64 */[
    /* hi */256,
    /* lo */0
  ],
  /* int64 */[
    /* hi */512,
    /* lo */0
  ],
  /* int64 */[
    /* hi */1024,
    /* lo */0
  ],
  /* int64 */[
    /* hi */2048,
    /* lo */0
  ],
  /* int64 */[
    /* hi */4096,
    /* lo */0
  ],
  /* int64 */[
    /* hi */8192,
    /* lo */0
  ],
  /* int64 */[
    /* hi */16384,
    /* lo */0
  ],
  /* int64 */[
    /* hi */32768,
    /* lo */0
  ],
  /* int64 */[
    /* hi */65536,
    /* lo */0
  ],
  /* int64 */[
    /* hi */131072,
    /* lo */0
  ],
  /* int64 */[
    /* hi */262144,
    /* lo */0
  ],
  /* int64 */[
    /* hi */524288,
    /* lo */0
  ],
  /* int64 */[
    /* hi */1048576,
    /* lo */0
  ],
  /* int64 */[
    /* hi */2097152,
    /* lo */0
  ],
  /* int64 */[
    /* hi */4194304,
    /* lo */0
  ],
  /* int64 */[
    /* hi */8388608,
    /* lo */0
  ],
  /* int64 */[
    /* hi */16777216,
    /* lo */0
  ],
  /* int64 */[
    /* hi */33554432,
    /* lo */0
  ],
  /* int64 */[
    /* hi */67108864,
    /* lo */0
  ],
  /* int64 */[
    /* hi */134217728,
    /* lo */0
  ],
  /* int64 */[
    /* hi */268435456,
    /* lo */0
  ],
  /* int64 */[
    /* hi */536870912,
    /* lo */0
  ],
  /* int64 */[
    /* hi */1073741824,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-2147483648,
    /* lo */0
  ]
];

var shift_left_tests = /* tuple */[
  shift_left_tests_000,
  shift_left_tests_001
];

var shift_right_tests_000 = $$Array.map((function (i) {
        return Caml_int64.asr_(/* int64 */[
                    /* hi */-2147483648,
                    /* lo */0
                  ], i);
      }), Ext_array_test.range(0, 63));

var shift_right_tests_001 = /* array */[
  /* int64 */[
    /* hi */-2147483648,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-1073741824,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-536870912,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-268435456,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-134217728,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-67108864,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-33554432,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-16777216,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-8388608,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-4194304,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-2097152,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-1048576,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-524288,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-262144,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-131072,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-65536,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-32768,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-16384,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-8192,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-4096,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-2048,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-1024,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-512,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-256,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-128,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-64,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-32,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-16,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-8,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-4,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-2,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */0
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */2147483648
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */3221225472
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */3758096384
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4026531840
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4160749568
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4227858432
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4261412864
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4278190080
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4286578688
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4290772992
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4292870144
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4293918720
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294443008
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294705152
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294836224
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294901760
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294934528
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294950912
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294959104
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294963200
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294965248
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294966272
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294966784
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294967040
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294967168
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294967232
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294967264
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294967280
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294967288
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294967292
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294967294
  ],
  /* int64 */[
    /* hi */-1,
    /* lo */4294967295
  ]
];

var shift_right_tests = /* tuple */[
  shift_right_tests_000,
  shift_right_tests_001
];

var shift_right_logical_suites_000 = $$Array.map((function (i) {
        return Caml_int64.lsr_(/* int64 */[
                    /* hi */-2147483648,
                    /* lo */0
                  ], i);
      }), Ext_array_test.range(0, 63));

var shift_right_logical_suites_001 = /* array */[
  /* int64 */[
    /* hi */-2147483648,
    /* lo */0
  ],
  /* int64 */[
    /* hi */1073741824,
    /* lo */0
  ],
  /* int64 */[
    /* hi */536870912,
    /* lo */0
  ],
  /* int64 */[
    /* hi */268435456,
    /* lo */0
  ],
  /* int64 */[
    /* hi */134217728,
    /* lo */0
  ],
  /* int64 */[
    /* hi */67108864,
    /* lo */0
  ],
  /* int64 */[
    /* hi */33554432,
    /* lo */0
  ],
  /* int64 */[
    /* hi */16777216,
    /* lo */0
  ],
  /* int64 */[
    /* hi */8388608,
    /* lo */0
  ],
  /* int64 */[
    /* hi */4194304,
    /* lo */0
  ],
  /* int64 */[
    /* hi */2097152,
    /* lo */0
  ],
  /* int64 */[
    /* hi */1048576,
    /* lo */0
  ],
  /* int64 */[
    /* hi */524288,
    /* lo */0
  ],
  /* int64 */[
    /* hi */262144,
    /* lo */0
  ],
  /* int64 */[
    /* hi */131072,
    /* lo */0
  ],
  /* int64 */[
    /* hi */65536,
    /* lo */0
  ],
  /* int64 */[
    /* hi */32768,
    /* lo */0
  ],
  /* int64 */[
    /* hi */16384,
    /* lo */0
  ],
  /* int64 */[
    /* hi */8192,
    /* lo */0
  ],
  /* int64 */[
    /* hi */4096,
    /* lo */0
  ],
  /* int64 */[
    /* hi */2048,
    /* lo */0
  ],
  /* int64 */[
    /* hi */1024,
    /* lo */0
  ],
  /* int64 */[
    /* hi */512,
    /* lo */0
  ],
  /* int64 */[
    /* hi */256,
    /* lo */0
  ],
  /* int64 */[
    /* hi */128,
    /* lo */0
  ],
  /* int64 */[
    /* hi */64,
    /* lo */0
  ],
  /* int64 */[
    /* hi */32,
    /* lo */0
  ],
  /* int64 */[
    /* hi */16,
    /* lo */0
  ],
  /* int64 */[
    /* hi */8,
    /* lo */0
  ],
  /* int64 */[
    /* hi */4,
    /* lo */0
  ],
  /* int64 */[
    /* hi */2,
    /* lo */0
  ],
  /* int64 */[
    /* hi */1,
    /* lo */0
  ],
  /* int64 */[
    /* hi */0,
    /* lo */2147483648
  ],
  /* int64 */[
    /* hi */0,
    /* lo */1073741824
  ],
  /* int64 */[
    /* hi */0,
    /* lo */536870912
  ],
  /* int64 */[
    /* hi */0,
    /* lo */268435456
  ],
  /* int64 */[
    /* hi */0,
    /* lo */134217728
  ],
  /* int64 */[
    /* hi */0,
    /* lo */67108864
  ],
  /* int64 */[
    /* hi */0,
    /* lo */33554432
  ],
  /* int64 */[
    /* hi */0,
    /* lo */16777216
  ],
  /* int64 */[
    /* hi */0,
    /* lo */8388608
  ],
  /* int64 */[
    /* hi */0,
    /* lo */4194304
  ],
  /* int64 */[
    /* hi */0,
    /* lo */2097152
  ],
  /* int64 */[
    /* hi */0,
    /* lo */1048576
  ],
  /* int64 */[
    /* hi */0,
    /* lo */524288
  ],
  /* int64 */[
    /* hi */0,
    /* lo */262144
  ],
  /* int64 */[
    /* hi */0,
    /* lo */131072
  ],
  /* int64 */[
    /* hi */0,
    /* lo */65536
  ],
  /* int64 */[
    /* hi */0,
    /* lo */32768
  ],
  /* int64 */[
    /* hi */0,
    /* lo */16384
  ],
  /* int64 */[
    /* hi */0,
    /* lo */8192
  ],
  /* int64 */[
    /* hi */0,
    /* lo */4096
  ],
  /* int64 */[
    /* hi */0,
    /* lo */2048
  ],
  /* int64 */[
    /* hi */0,
    /* lo */1024
  ],
  /* int64 */[
    /* hi */0,
    /* lo */512
  ],
  /* int64 */[
    /* hi */0,
    /* lo */256
  ],
  /* int64 */[
    /* hi */0,
    /* lo */128
  ],
  /* int64 */[
    /* hi */0,
    /* lo */64
  ],
  /* int64 */[
    /* hi */0,
    /* lo */32
  ],
  /* int64 */[
    /* hi */0,
    /* lo */16
  ],
  /* int64 */[
    /* hi */0,
    /* lo */8
  ],
  /* int64 */[
    /* hi */0,
    /* lo */4
  ],
  /* int64 */[
    /* hi */0,
    /* lo */2
  ],
  /* int64 */[
    /* hi */0,
    /* lo */1
  ]
];

var shift_right_logical_suites = /* tuple */[
  shift_right_logical_suites_000,
  shift_right_logical_suites_001
];

function fib(_n, _a, _b) {
  while(true) {
    var b = _b;
    var a = _a;
    var n = _n;
    if (n === 0) {
      return a;
    } else {
      _b = Caml_int64.add(a, b);
      _a = b;
      _n = n - 1 | 0;
      continue ;
    }
  };
}

function fac(_n, _acc) {
  while(true) {
    var acc = _acc;
    var n = _n;
    if (n === 0) {
      return acc;
    } else {
      _acc = Caml_int64.mul(acc, Caml_int64.of_int32(n));
      _n = n - 1 | 0;
      continue ;
    }
  };
}

var suites = Pervasives.$at(/* :: */[
      /* tuple */[
        "add_one",
        (function () {
            return /* Eq */Block.__(0, [
                      v,
                      /* int64 */[
                        /* hi */0,
                        /* lo */2147483648
                      ]
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "add_2",
          (function () {
              return /* Eq */Block.__(0, [
                        /* int64 */[
                          /* hi */0,
                          /* lo */4294967294
                        ],
                        Caml_int64.add(a, a)
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "add_3",
            (function () {
                return /* Eq */Block.__(0, [
                          /* int64 */[
                            /* hi */0,
                            /* lo */0
                          ],
                          /* int64 */[
                            /* hi */0,
                            /* lo */0
                          ]
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "add_4",
              (function () {
                  return commutative_add(/* int64 */[
                              /* hi */-1,
                              /* lo */4294967294
                            ], /* int64 */[
                              /* hi */-1,
                              /* lo */4294967293
                            ], /* int64 */[
                              /* hi */0,
                              /* lo */1
                            ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "add_5",
                (function () {
                    return commutative_add(/* int64 */[
                                /* hi */-1,
                                /* lo */4294967293
                              ], /* int64 */[
                                /* hi */-1,
                                /* lo */4294967293
                              ], /* int64 */[
                                /* hi */0,
                                /* lo */0
                              ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "add_6",
                  (function () {
                      return commutative_add(/* int64 */[
                                  /* hi */0,
                                  /* lo */4
                                ], /* int64 */[
                                  /* hi */-1,
                                  /* lo */4294967293
                                ], /* int64 */[
                                  /* hi */0,
                                  /* lo */7
                                ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "add_7",
                    (function () {
                        return commutative_add(/* int64 */[
                                    /* hi */1,
                                    /* lo */0
                                  ], /* int64 */[
                                    /* hi */0,
                                    /* lo */2147483648
                                  ], /* int64 */[
                                    /* hi */0,
                                    /* lo */2147483648
                                  ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "add_8",
                      (function () {
                          return commutative_add(/* int64 */[
                                      /* hi */1,
                                      /* lo */0
                                    ], /* int64 */[
                                      /* hi */0,
                                      /* lo */4294967295
                                    ], /* int64 */[
                                      /* hi */0,
                                      /* lo */1
                                    ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "add_9",
                        (function () {
                            return commutative_add(/* int64 */[
                                        /* hi */0,
                                        /* lo */4294967295
                                      ], /* int64 */[
                                        /* hi */0,
                                        /* lo */2147483648
                                      ], /* int64 */[
                                        /* hi */0,
                                        /* lo */2147483647
                                      ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "add_10",
                          (function () {
                              return commutative_add(/* int64 */[
                                          /* hi */0,
                                          /* lo */2147483648
                                        ], /* int64 */[
                                          /* hi */0,
                                          /* lo */2147483648
                                        ], /* int64 */[
                                          /* hi */0,
                                          /* lo */0
                                        ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "add_11",
                            (function () {
                                return commutative_add(/* int64 */[
                                            /* hi */0,
                                            /* lo */4294967295
                                          ], /* int64 */[
                                            /* hi */0,
                                            /* lo */4294967295
                                          ], /* int64 */[
                                            /* hi */0,
                                            /* lo */0
                                          ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "to_int32",
                              (function () {
                                  return /* Eq */Block.__(0, [
                                            3,
                                            3
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "to_int",
                                (function () {
                                    return /* Eq */Block.__(0, [
                                              3,
                                              3
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "of_int",
                                  (function () {
                                      return /* Eq */Block.__(0, [
                                                /* int64 */[
                                                  /* hi */0,
                                                  /* lo */3
                                                ],
                                                /* int64 */[
                                                  /* hi */0,
                                                  /* lo */3
                                                ]
                                              ]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "lognot",
                                    (function () {
                                        return /* Eq */Block.__(0, [
                                                  /* int64 */[
                                                    /* hi */-1,
                                                    /* lo */4294967293
                                                  ],
                                                  /* int64 */[
                                                    /* hi */-1,
                                                    /* lo */4294967293
                                                  ]
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "neg",
                                      (function () {
                                          return /* Eq */Block.__(0, [
                                                    /* int64 */[
                                                      /* hi */-1,
                                                      /* lo */4294967294
                                                    ],
                                                    /* int64 */[
                                                      /* hi */-1,
                                                      /* lo */4294967294
                                                    ]
                                                  ]);
                                        })
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "sub1",
                                        (function () {
                                            return /* Eq */Block.__(0, [
                                                      /* int64 */[
                                                        /* hi */0,
                                                        /* lo */2
                                                      ],
                                                      /* int64 */[
                                                        /* hi */0,
                                                        /* lo */2
                                                      ]
                                                    ]);
                                          })
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "xor1",
                                          (function () {
                                              return /* Eq */Block.__(0, [
                                                        /* tuple */[
                                                          /* int64 */[
                                                            /* hi */0,
                                                            /* lo */286331153
                                                          ],
                                                          Caml_int64.xor(a, /* int64 */[
                                                                /* hi */0,
                                                                /* lo */4009750271
                                                              ])
                                                        ],
                                                        /* tuple */[
                                                          /* int64 */[
                                                            /* hi */0,
                                                            /* lo */286331153
                                                          ],
                                                          /* int64 */[
                                                            /* hi */0,
                                                            /* lo */2432700672
                                                          ]
                                                        ]
                                                      ]);
                                            })
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "or",
                                            (function () {
                                                return /* Eq */Block.__(0, [
                                                          /* int64 */[
                                                            /* hi */0,
                                                            /* lo */4294967295
                                                          ],
                                                          /* int64 */[
                                                            /* hi */0,
                                                            /* lo */4294967295
                                                          ]
                                                        ]);
                                              })
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "and",
                                              (function () {
                                                  return /* Eq */Block.__(0, [
                                                            /* int64 */[
                                                              /* hi */0,
                                                              /* lo */4008636142
                                                            ],
                                                            /* int64 */[
                                                              /* hi */0,
                                                              /* lo */4008636142
                                                            ]
                                                          ]);
                                                })
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "lsl",
                                                (function () {
                                                    return /* Eq */Block.__(0, [
                                                              $$Array.map((function (x) {
                                                                      return Caml_int64.lsl_(/* int64 */[
                                                                                  /* hi */0,
                                                                                  /* lo */1
                                                                                ], x);
                                                                    }), $$Array.init(64, (function (i) {
                                                                          return i;
                                                                        }))),
                                                              /* array */[
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */1
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */2
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */4
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */8
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */16
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */32
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */64
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */128
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */256
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */512
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */1024
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */2048
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */4096
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */8192
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */16384
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */32768
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */65536
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */131072
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */262144
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */524288
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */1048576
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */2097152
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */4194304
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */8388608
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */16777216
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */33554432
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */67108864
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */134217728
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */268435456
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */536870912
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */1073741824
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */0,
                                                                  /* lo */2147483648
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */1,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */2,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */4,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */8,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */16,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */32,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */64,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */128,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */256,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */512,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */1024,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */2048,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */4096,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */8192,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */16384,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */32768,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */65536,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */131072,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */262144,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */524288,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */1048576,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */2097152,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */4194304,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */8388608,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */16777216,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */33554432,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */67108864,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */134217728,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */268435456,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */536870912,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */1073741824,
                                                                  /* lo */0
                                                                ],
                                                                /* int64 */[
                                                                  /* hi */-2147483648,
                                                                  /* lo */0
                                                                ]
                                                              ]
                                                            ]);
                                                  })
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "lsr",
                                                  (function () {
                                                      return /* Eq */Block.__(0, [
                                                                $$Array.map((function (x) {
                                                                        return Caml_int64.lsr_(/* int64 */[
                                                                                    /* hi */-1,
                                                                                    /* lo */4294967295
                                                                                  ], x);
                                                                      }), $$Array.init(64, (function (i) {
                                                                            return i;
                                                                          }))),
                                                                /* array */[
                                                                  /* int64 */[
                                                                    /* hi */-1,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */2147483647,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */1073741823,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */536870911,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */268435455,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */134217727,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */67108863,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */33554431,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */16777215,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */8388607,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */4194303,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */2097151,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */1048575,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */524287,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */262143,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */131071,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */65535,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */32767,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */16383,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */8191,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */4095,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */2047,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */1023,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */511,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */255,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */127,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */63,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */31,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */15,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */7,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */3,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */1,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */4294967295
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */2147483647
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */1073741823
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */536870911
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */268435455
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */134217727
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */67108863
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */33554431
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */16777215
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */8388607
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */4194303
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */2097151
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */1048575
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */524287
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */262143
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */131071
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */65535
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */32767
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */16383
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */8191
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */4095
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */2047
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */1023
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */511
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */255
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */127
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */63
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */31
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */15
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */7
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */3
                                                                  ],
                                                                  /* int64 */[
                                                                    /* hi */0,
                                                                    /* lo */1
                                                                  ]
                                                                ]
                                                              ]);
                                                    })
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "asr",
                                                    (function () {
                                                        return /* Eq */Block.__(0, [
                                                                  $$Array.map((function (x) {
                                                                          return Caml_int64.asr_(/* int64 */[
                                                                                      /* hi */-1,
                                                                                      /* lo */4294967295
                                                                                    ], x);
                                                                        }), $$Array.init(64, (function (i) {
                                                                              return i;
                                                                            }))),
                                                                  /* array */[
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */-1,
                                                                      /* lo */4294967295
                                                                    ]
                                                                  ]
                                                                ]);
                                                      })
                                                  ],
                                                  /* :: */[
                                                    /* tuple */[
                                                      "mul simple",
                                                      (function () {
                                                          return /* Eq */Block.__(0, [
                                                                    /* int64 */[
                                                                      /* hi */0,
                                                                      /* lo */6
                                                                    ],
                                                                    /* int64 */[
                                                                      /* hi */0,
                                                                      /* lo */6
                                                                    ]
                                                                  ]);
                                                        })
                                                    ],
                                                    /* :: */[
                                                      /* tuple */[
                                                        "of_int32",
                                                        (function () {
                                                            return /* Eq */Block.__(0, [
                                                                      $$Array.map(Caml_int64.of_int32, /* array */[
                                                                            0,
                                                                            -2147483648
                                                                          ]),
                                                                      /* array */[
                                                                        /* int64 */[
                                                                          /* hi */0,
                                                                          /* lo */0
                                                                        ],
                                                                        /* int64 */[
                                                                          /* hi */-1,
                                                                          /* lo */2147483648
                                                                        ]
                                                                      ]
                                                                    ]);
                                                          })
                                                      ],
                                                      /* :: */[
                                                        /* tuple */[
                                                          "to_int32",
                                                          (function () {
                                                              return /* Eq */Block.__(0, [
                                                                        $$Array.map((function (prim) {
                                                                                return prim[1] | 0;
                                                                              }), /* array */[
                                                                              /* int64 */[
                                                                                /* hi */0,
                                                                                /* lo */0
                                                                              ],
                                                                              /* int64 */[
                                                                                /* hi */0,
                                                                                /* lo */2147483648
                                                                              ]
                                                                            ]),
                                                                        /* array */[
                                                                          0,
                                                                          -2147483648
                                                                        ]
                                                                      ]);
                                                            })
                                                        ],
                                                        /* :: */[
                                                          /* tuple */[
                                                            "discard_sign",
                                                            (function () {
                                                                return /* Eq */Block.__(0, [
                                                                          Caml_int64.discard_sign(/* int64 */[
                                                                                /* hi */-1,
                                                                                /* lo */4294967295
                                                                              ]),
                                                                          /* int64 */[
                                                                            /* hi */2147483647,
                                                                            /* lo */4294967295
                                                                          ]
                                                                        ]);
                                                              })
                                                          ],
                                                          /* :: */[
                                                            /* tuple */[
                                                              "div_mod",
                                                              (function () {
                                                                  return /* Eq */Block.__(0, [
                                                                            Caml_int64.div_mod(/* int64 */[
                                                                                  /* hi */0,
                                                                                  /* lo */7
                                                                                ], /* int64 */[
                                                                                  /* hi */0,
                                                                                  /* lo */3
                                                                                ]),
                                                                            /* tuple */[
                                                                              /* int64 */[
                                                                                /* hi */0,
                                                                                /* lo */2
                                                                              ],
                                                                              /* int64 */[
                                                                                /* hi */0,
                                                                                /* lo */1
                                                                              ]
                                                                            ]
                                                                          ]);
                                                                })
                                                            ],
                                                            /* :: */[
                                                              /* tuple */[
                                                                "to_hex",
                                                                (function () {
                                                                    return /* Eq */Block.__(0, [
                                                                              Caml_int64.to_hex(/* int64 */[
                                                                                    /* hi */-1,
                                                                                    /* lo */4294967295
                                                                                  ]),
                                                                              "ffffffffffffffff"
                                                                            ]);
                                                                  })
                                                              ],
                                                              /* :: */[
                                                                /* tuple */[
                                                                  "generic_compare",
                                                                  (function () {
                                                                      return /* Eq */Block.__(0, [
                                                                                Caml_obj.caml_compare(/* int64 */[
                                                                                      /* hi */1,
                                                                                      /* lo */0
                                                                                    ], /* int64 */[
                                                                                      /* hi */0,
                                                                                      /* lo */1
                                                                                    ]) > 0,
                                                                                true
                                                                              ]);
                                                                    })
                                                                ],
                                                                /* :: */[
                                                                  /* tuple */[
                                                                    "test_compier_literal",
                                                                    (function () {
                                                                        return /* Eq */Block.__(0, [
                                                                                  /* int64 */[
                                                                                    /* hi */0,
                                                                                    /* lo */4294967295
                                                                                  ],
                                                                                  /* int64 */[
                                                                                    /* hi */0,
                                                                                    /* lo */4294967295
                                                                                  ]
                                                                                ]);
                                                                      })
                                                                  ],
                                                                  /* :: */[
                                                                    /* tuple */[
                                                                      "generic_compare2",
                                                                      (function () {
                                                                          return /* Eq */Block.__(0, [
                                                                                    Caml_obj.caml_compare(/* int64 */[
                                                                                          /* hi */0,
                                                                                          /* lo */2147483648
                                                                                        ], /* int64 */[
                                                                                          /* hi */0,
                                                                                          /* lo */1
                                                                                        ]) > 0,
                                                                                    true
                                                                                  ]);
                                                                        })
                                                                    ],
                                                                    /* :: */[
                                                                      /* tuple */[
                                                                        "shift_left",
                                                                        (function () {
                                                                            return /* Eq */Block.__(0, [
                                                                                      /* int64 */[
                                                                                        /* hi */0,
                                                                                        /* lo */4294967040
                                                                                      ],
                                                                                      /* int64 */[
                                                                                        /* hi */0,
                                                                                        /* lo */4294967040
                                                                                      ]
                                                                                    ]);
                                                                          })
                                                                      ],
                                                                      /* :: */[
                                                                        /* tuple */[
                                                                          "shift_right",
                                                                          (function () {
                                                                              return /* Eq */Block.__(0, [
                                                                                        4294967295,
                                                                                        4294967295
                                                                                      ]);
                                                                            })
                                                                        ],
                                                                        /* :: */[
                                                                          /* tuple */[
                                                                            "fib_int64",
                                                                            (function () {
                                                                                return /* Eq */Block.__(0, [
                                                                                          fib(1000, /* int64 */[
                                                                                                /* hi */0,
                                                                                                /* lo */1
                                                                                              ], /* int64 */[
                                                                                                /* hi */0,
                                                                                                /* lo */2
                                                                                              ]),
                                                                                          /* int64 */[
                                                                                            /* hi */-1990564327,
                                                                                            /* lo */2874523960
                                                                                          ]
                                                                                        ]);
                                                                              })
                                                                          ],
                                                                          /* :: */[
                                                                            /* tuple */[
                                                                              "fac_int64",
                                                                              (function () {
                                                                                  return /* Eq */Block.__(0, [
                                                                                            fac(30, /* int64 */[
                                                                                                  /* hi */0,
                                                                                                  /* lo */1
                                                                                                ]),
                                                                                            /* int64 */[
                                                                                              /* hi */-2040662563,
                                                                                              /* lo */1409286144
                                                                                            ]
                                                                                          ]);
                                                                                })
                                                                            ],
                                                                            /* [] */0
                                                                          ]
                                                                        ]
                                                                      ]
                                                                    ]
                                                                  ]
                                                                ]
                                                              ]
                                                            ]
                                                          ]
                                                        ]
                                                      ]
                                                    ]
                                                  ]
                                                ]
                                              ]
                                            ]
                                          ]
                                        ]
                                      ]
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ], Pervasives.$at($$Array.to_list(Ext_array_test.map2i((function (i, a, b) {
                    return /* tuple */[
                            Curry._1(Format.asprintf(/* Format */[
                                      /* String_literal */Block.__(11, [
                                          "shift_left_cases ",
                                          /* Int */Block.__(4, [
                                              /* Int_d */0,
                                              /* No_padding */0,
                                              /* No_precision */0,
                                              /* End_of_format */0
                                            ])
                                        ]),
                                      "shift_left_cases %d"
                                    ]), i),
                            (function () {
                                return /* Eq */Block.__(0, [
                                          a,
                                          b
                                        ]);
                              })
                          ];
                  }), shift_left_tests_000, shift_left_tests_001)), Pervasives.$at($$Array.to_list(Ext_array_test.map2i((function (i, a, b) {
                        return /* tuple */[
                                Curry._1(Format.asprintf(/* Format */[
                                          /* String_literal */Block.__(11, [
                                              "shift_right_cases ",
                                              /* Int */Block.__(4, [
                                                  /* Int_d */0,
                                                  /* No_padding */0,
                                                  /* No_precision */0,
                                                  /* End_of_format */0
                                                ])
                                            ]),
                                          "shift_right_cases %d"
                                        ]), i),
                                (function () {
                                    return /* Eq */Block.__(0, [
                                              a,
                                              b
                                            ]);
                                  })
                              ];
                      }), shift_right_tests_000, shift_right_tests_001)), $$Array.to_list(Ext_array_test.map2i((function (i, a, b) {
                        return /* tuple */[
                                Curry._1(Format.asprintf(/* Format */[
                                          /* String_literal */Block.__(11, [
                                              "shift_right_logical_cases ",
                                              /* Int */Block.__(4, [
                                                  /* Int_d */0,
                                                  /* No_padding */0,
                                                  /* No_precision */0,
                                                  /* End_of_format */0
                                                ])
                                            ]),
                                          "shift_right_logical_cases %d"
                                        ]), i),
                                (function () {
                                    return /* Eq */Block.__(0, [
                                              a,
                                              b
                                            ]);
                                  })
                              ];
                      }), shift_right_logical_suites_000, shift_right_logical_suites_001)))));

Mt.from_pair_suites("int64_test.ml", suites);

exports.f = f;
exports.v = v;
exports.h = h;
exports.a = a;
exports.commutative_add = commutative_add;
exports.generic_compare = generic_compare;
exports.shift_left_tests = shift_left_tests;
exports.shift_right_tests = shift_right_tests;
exports.shift_right_logical_suites = shift_right_logical_suites;
exports.fib = fib;
exports.fac = fac;
exports.suites = suites;
/* shift_left_tests Not a pure module */
