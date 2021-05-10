'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Int32 = require("../../lib/js/int32.js");
var Int64 = require("../../lib/js/int64.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Ext_array_test = require("./ext_array_test.js");

function f(u, v) {
  return u > v;
}

var v = Caml_int64.add(Caml_int64.of_int32(Int32.max_int), Int64.one);

var h = Caml_int64.neg(v);

var a = [
  0,
  2147483647
];

function commutative_add(result, a, b) {
  return {
          TAG: /* Eq */0,
          _0: [
            result,
            result
          ],
          _1: [
            Caml_int64.add(a, b),
            Caml_int64.add(b, a)
          ]
        };
}

var generic_compare = Caml_obj.compare;

var shift_left_tests_0 = $$Array.map((function (i) {
        return Caml_int64.lsl_(Caml_int64.one, i);
      }), Ext_array_test.range(0, 63));

var shift_left_tests_1 = [
  Caml_int64.one,
  [
    0,
    2
  ],
  [
    0,
    4
  ],
  [
    0,
    8
  ],
  [
    0,
    16
  ],
  [
    0,
    32
  ],
  [
    0,
    64
  ],
  [
    0,
    128
  ],
  [
    0,
    256
  ],
  [
    0,
    512
  ],
  [
    0,
    1024
  ],
  [
    0,
    2048
  ],
  [
    0,
    4096
  ],
  [
    0,
    8192
  ],
  [
    0,
    16384
  ],
  [
    0,
    32768
  ],
  [
    0,
    65536
  ],
  [
    0,
    131072
  ],
  [
    0,
    262144
  ],
  [
    0,
    524288
  ],
  [
    0,
    1048576
  ],
  [
    0,
    2097152
  ],
  [
    0,
    4194304
  ],
  [
    0,
    8388608
  ],
  [
    0,
    16777216
  ],
  [
    0,
    33554432
  ],
  [
    0,
    67108864
  ],
  [
    0,
    134217728
  ],
  [
    0,
    268435456
  ],
  [
    0,
    536870912
  ],
  [
    0,
    1073741824
  ],
  [
    0,
    2147483648
  ],
  [
    1,
    0
  ],
  [
    2,
    0
  ],
  [
    4,
    0
  ],
  [
    8,
    0
  ],
  [
    16,
    0
  ],
  [
    32,
    0
  ],
  [
    64,
    0
  ],
  [
    128,
    0
  ],
  [
    256,
    0
  ],
  [
    512,
    0
  ],
  [
    1024,
    0
  ],
  [
    2048,
    0
  ],
  [
    4096,
    0
  ],
  [
    8192,
    0
  ],
  [
    16384,
    0
  ],
  [
    32768,
    0
  ],
  [
    65536,
    0
  ],
  [
    131072,
    0
  ],
  [
    262144,
    0
  ],
  [
    524288,
    0
  ],
  [
    1048576,
    0
  ],
  [
    2097152,
    0
  ],
  [
    4194304,
    0
  ],
  [
    8388608,
    0
  ],
  [
    16777216,
    0
  ],
  [
    33554432,
    0
  ],
  [
    67108864,
    0
  ],
  [
    134217728,
    0
  ],
  [
    268435456,
    0
  ],
  [
    536870912,
    0
  ],
  [
    1073741824,
    0
  ],
  Caml_int64.min_int
];

var shift_left_tests = [
  shift_left_tests_0,
  shift_left_tests_1
];

var shift_right_tests_0 = $$Array.map((function (i) {
        return Caml_int64.asr_(Caml_int64.min_int, i);
      }), Ext_array_test.range(0, 63));

var shift_right_tests_1 = [
  Caml_int64.min_int,
  [
    -1073741824,
    0
  ],
  [
    -536870912,
    0
  ],
  [
    -268435456,
    0
  ],
  [
    -134217728,
    0
  ],
  [
    -67108864,
    0
  ],
  [
    -33554432,
    0
  ],
  [
    -16777216,
    0
  ],
  [
    -8388608,
    0
  ],
  [
    -4194304,
    0
  ],
  [
    -2097152,
    0
  ],
  [
    -1048576,
    0
  ],
  [
    -524288,
    0
  ],
  [
    -262144,
    0
  ],
  [
    -131072,
    0
  ],
  [
    -65536,
    0
  ],
  [
    -32768,
    0
  ],
  [
    -16384,
    0
  ],
  [
    -8192,
    0
  ],
  [
    -4096,
    0
  ],
  [
    -2048,
    0
  ],
  [
    -1024,
    0
  ],
  [
    -512,
    0
  ],
  [
    -256,
    0
  ],
  [
    -128,
    0
  ],
  [
    -64,
    0
  ],
  [
    -32,
    0
  ],
  [
    -16,
    0
  ],
  [
    -8,
    0
  ],
  [
    -4,
    0
  ],
  [
    -2,
    0
  ],
  [
    -1,
    0
  ],
  [
    -1,
    2147483648
  ],
  [
    -1,
    3221225472
  ],
  [
    -1,
    3758096384
  ],
  [
    -1,
    4026531840
  ],
  [
    -1,
    4160749568
  ],
  [
    -1,
    4227858432
  ],
  [
    -1,
    4261412864
  ],
  [
    -1,
    4278190080
  ],
  [
    -1,
    4286578688
  ],
  [
    -1,
    4290772992
  ],
  [
    -1,
    4292870144
  ],
  [
    -1,
    4293918720
  ],
  [
    -1,
    4294443008
  ],
  [
    -1,
    4294705152
  ],
  [
    -1,
    4294836224
  ],
  [
    -1,
    4294901760
  ],
  [
    -1,
    4294934528
  ],
  [
    -1,
    4294950912
  ],
  [
    -1,
    4294959104
  ],
  [
    -1,
    4294963200
  ],
  [
    -1,
    4294965248
  ],
  [
    -1,
    4294966272
  ],
  [
    -1,
    4294966784
  ],
  [
    -1,
    4294967040
  ],
  [
    -1,
    4294967168
  ],
  [
    -1,
    4294967232
  ],
  [
    -1,
    4294967264
  ],
  [
    -1,
    4294967280
  ],
  [
    -1,
    4294967288
  ],
  [
    -1,
    4294967292
  ],
  [
    -1,
    4294967294
  ],
  Caml_int64.neg_one
];

var shift_right_tests = [
  shift_right_tests_0,
  shift_right_tests_1
];

var shift_right_logical_suites_0 = $$Array.map((function (i) {
        return Caml_int64.lsr_(Caml_int64.min_int, i);
      }), Ext_array_test.range(0, 63));

var shift_right_logical_suites_1 = [
  Caml_int64.min_int,
  [
    1073741824,
    0
  ],
  [
    536870912,
    0
  ],
  [
    268435456,
    0
  ],
  [
    134217728,
    0
  ],
  [
    67108864,
    0
  ],
  [
    33554432,
    0
  ],
  [
    16777216,
    0
  ],
  [
    8388608,
    0
  ],
  [
    4194304,
    0
  ],
  [
    2097152,
    0
  ],
  [
    1048576,
    0
  ],
  [
    524288,
    0
  ],
  [
    262144,
    0
  ],
  [
    131072,
    0
  ],
  [
    65536,
    0
  ],
  [
    32768,
    0
  ],
  [
    16384,
    0
  ],
  [
    8192,
    0
  ],
  [
    4096,
    0
  ],
  [
    2048,
    0
  ],
  [
    1024,
    0
  ],
  [
    512,
    0
  ],
  [
    256,
    0
  ],
  [
    128,
    0
  ],
  [
    64,
    0
  ],
  [
    32,
    0
  ],
  [
    16,
    0
  ],
  [
    8,
    0
  ],
  [
    4,
    0
  ],
  [
    2,
    0
  ],
  [
    1,
    0
  ],
  [
    0,
    2147483648
  ],
  [
    0,
    1073741824
  ],
  [
    0,
    536870912
  ],
  [
    0,
    268435456
  ],
  [
    0,
    134217728
  ],
  [
    0,
    67108864
  ],
  [
    0,
    33554432
  ],
  [
    0,
    16777216
  ],
  [
    0,
    8388608
  ],
  [
    0,
    4194304
  ],
  [
    0,
    2097152
  ],
  [
    0,
    1048576
  ],
  [
    0,
    524288
  ],
  [
    0,
    262144
  ],
  [
    0,
    131072
  ],
  [
    0,
    65536
  ],
  [
    0,
    32768
  ],
  [
    0,
    16384
  ],
  [
    0,
    8192
  ],
  [
    0,
    4096
  ],
  [
    0,
    2048
  ],
  [
    0,
    1024
  ],
  [
    0,
    512
  ],
  [
    0,
    256
  ],
  [
    0,
    128
  ],
  [
    0,
    64
  ],
  [
    0,
    32
  ],
  [
    0,
    16
  ],
  [
    0,
    8
  ],
  [
    0,
    4
  ],
  [
    0,
    2
  ],
  Caml_int64.one
];

var shift_right_logical_suites = [
  shift_right_logical_suites_0,
  shift_right_logical_suites_1
];

function fib(_n, _a, _b) {
  while(true) {
    var b = _b;
    var a = _a;
    var n = _n;
    if (n === 0) {
      return a;
    }
    _b = Caml_int64.add(a, b);
    _a = b;
    _n = n - 1 | 0;
    continue ;
  };
}

function fac(_n, _acc) {
  while(true) {
    var acc = _acc;
    var n = _n;
    if (n === 0) {
      return acc;
    }
    _acc = Caml_int64.mul(acc, Caml_int64.of_int32(n));
    _n = n - 1 | 0;
    continue ;
  };
}

var suites = Pervasives.$at({
      hd: [
        "add_one",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: v,
                    _1: [
                      0,
                      2147483648
                    ]
                  };
          })
      ],
      tl: {
        hd: [
          "add_2",
          (function (param) {
              return {
                      TAG: /* Eq */0,
                      _0: [
                        0,
                        4294967294
                      ],
                      _1: Caml_int64.add(a, a)
                    };
            })
        ],
        tl: {
          hd: [
            "add_3",
            (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: Caml_int64.zero,
                        _1: Caml_int64.zero
                      };
              })
          ],
          tl: {
            hd: [
              "add_4",
              (function (param) {
                  return commutative_add([
                              -1,
                              4294967294
                            ], [
                              -1,
                              4294967293
                            ], Caml_int64.one);
                })
            ],
            tl: {
              hd: [
                "add_5",
                (function (param) {
                    return commutative_add([
                                -1,
                                4294967293
                              ], [
                                -1,
                                4294967293
                              ], Caml_int64.zero);
                  })
              ],
              tl: {
                hd: [
                  "add_6",
                  (function (param) {
                      return commutative_add([
                                  0,
                                  4
                                ], [
                                  -1,
                                  4294967293
                                ], [
                                  0,
                                  7
                                ]);
                    })
                ],
                tl: {
                  hd: [
                    "add_7",
                    (function (param) {
                        return commutative_add([
                                    1,
                                    0
                                  ], [
                                    0,
                                    2147483648
                                  ], [
                                    0,
                                    2147483648
                                  ]);
                      })
                  ],
                  tl: {
                    hd: [
                      "add_8",
                      (function (param) {
                          return commutative_add([
                                      1,
                                      0
                                    ], [
                                      0,
                                      4294967295
                                    ], Caml_int64.one);
                        })
                    ],
                    tl: {
                      hd: [
                        "add_9",
                        (function (param) {
                            return commutative_add([
                                        0,
                                        4294967295
                                      ], [
                                        0,
                                        2147483648
                                      ], [
                                        0,
                                        2147483647
                                      ]);
                          })
                      ],
                      tl: {
                        hd: [
                          "add_10",
                          (function (param) {
                              return commutative_add([
                                          0,
                                          2147483648
                                        ], [
                                          0,
                                          2147483648
                                        ], Caml_int64.zero);
                            })
                        ],
                        tl: {
                          hd: [
                            "add_11",
                            (function (param) {
                                return commutative_add([
                                            0,
                                            4294967295
                                          ], [
                                            0,
                                            4294967295
                                          ], Caml_int64.zero);
                              })
                          ],
                          tl: {
                            hd: [
                              "to_int32",
                              (function (param) {
                                  return {
                                          TAG: /* Eq */0,
                                          _0: 3,
                                          _1: Caml_int64.to_int32([
                                                0,
                                                3
                                              ])
                                        };
                                })
                            ],
                            tl: {
                              hd: [
                                "to_int",
                                (function (param) {
                                    return {
                                            TAG: /* Eq */0,
                                            _0: 3,
                                            _1: Caml_int64.to_int32([
                                                  0,
                                                  3
                                                ])
                                          };
                                  })
                              ],
                              tl: {
                                hd: [
                                  "of_int",
                                  (function (param) {
                                      return {
                                              TAG: /* Eq */0,
                                              _0: [
                                                0,
                                                3
                                              ],
                                              _1: [
                                                0,
                                                3
                                              ]
                                            };
                                    })
                                ],
                                tl: {
                                  hd: [
                                    "lognot",
                                    (function (param) {
                                        return {
                                                TAG: /* Eq */0,
                                                _0: [
                                                  -1,
                                                  4294967293
                                                ],
                                                _1: [
                                                  -1,
                                                  4294967293
                                                ]
                                              };
                                      })
                                  ],
                                  tl: {
                                    hd: [
                                      "neg",
                                      (function (param) {
                                          return {
                                                  TAG: /* Eq */0,
                                                  _0: [
                                                    -1,
                                                    4294967294
                                                  ],
                                                  _1: [
                                                    -1,
                                                    4294967294
                                                  ]
                                                };
                                        })
                                    ],
                                    tl: {
                                      hd: [
                                        "File \"int64_test.ml\", line 80, characters 4-11",
                                        (function (param) {
                                            return {
                                                    TAG: /* Eq */0,
                                                    _0: Int64.min_int,
                                                    _1: Caml_int64.neg(Int64.min_int)
                                                  };
                                          })
                                      ],
                                      tl: {
                                        hd: [
                                          "File \"int64_test.ml\", line 81, characters 4-11",
                                          (function (param) {
                                              return {
                                                      TAG: /* Eq */0,
                                                      _0: Int64.max_int,
                                                      _1: Caml_int64.neg(Caml_int64.add(Int64.min_int, Caml_int64.one))
                                                    };
                                            })
                                        ],
                                        tl: {
                                          hd: [
                                            "sub1",
                                            (function (param) {
                                                return {
                                                        TAG: /* Eq */0,
                                                        _0: [
                                                          0,
                                                          2
                                                        ],
                                                        _1: [
                                                          0,
                                                          2
                                                        ]
                                                      };
                                              })
                                          ],
                                          tl: {
                                            hd: [
                                              "xor1",
                                              (function (param) {
                                                  return {
                                                          TAG: /* Eq */0,
                                                          _0: [
                                                            [
                                                              0,
                                                              286331153
                                                            ],
                                                            Caml_int64.xor(a, [
                                                                  0,
                                                                  4009750271
                                                                ])
                                                          ],
                                                          _1: [
                                                            [
                                                              0,
                                                              286331153
                                                            ],
                                                            [
                                                              0,
                                                              2432700672
                                                            ]
                                                          ]
                                                        };
                                                })
                                            ],
                                            tl: {
                                              hd: [
                                                "or",
                                                (function (param) {
                                                    return {
                                                            TAG: /* Eq */0,
                                                            _0: [
                                                              0,
                                                              4294967295
                                                            ],
                                                            _1: [
                                                              0,
                                                              4294967295
                                                            ]
                                                          };
                                                  })
                                              ],
                                              tl: {
                                                hd: [
                                                  "and",
                                                  (function (param) {
                                                      return {
                                                              TAG: /* Eq */0,
                                                              _0: [
                                                                0,
                                                                4008636142
                                                              ],
                                                              _1: [
                                                                0,
                                                                4008636142
                                                              ]
                                                            };
                                                    })
                                                ],
                                                tl: {
                                                  hd: [
                                                    "lsl",
                                                    (function (param) {
                                                        return {
                                                                TAG: /* Eq */0,
                                                                _0: $$Array.map((function (x) {
                                                                        return Caml_int64.lsl_(Caml_int64.one, x);
                                                                      }), $$Array.init(64, (function (i) {
                                                                            return i;
                                                                          }))),
                                                                _1: [
                                                                  Caml_int64.one,
                                                                  [
                                                                    0,
                                                                    2
                                                                  ],
                                                                  [
                                                                    0,
                                                                    4
                                                                  ],
                                                                  [
                                                                    0,
                                                                    8
                                                                  ],
                                                                  [
                                                                    0,
                                                                    16
                                                                  ],
                                                                  [
                                                                    0,
                                                                    32
                                                                  ],
                                                                  [
                                                                    0,
                                                                    64
                                                                  ],
                                                                  [
                                                                    0,
                                                                    128
                                                                  ],
                                                                  [
                                                                    0,
                                                                    256
                                                                  ],
                                                                  [
                                                                    0,
                                                                    512
                                                                  ],
                                                                  [
                                                                    0,
                                                                    1024
                                                                  ],
                                                                  [
                                                                    0,
                                                                    2048
                                                                  ],
                                                                  [
                                                                    0,
                                                                    4096
                                                                  ],
                                                                  [
                                                                    0,
                                                                    8192
                                                                  ],
                                                                  [
                                                                    0,
                                                                    16384
                                                                  ],
                                                                  [
                                                                    0,
                                                                    32768
                                                                  ],
                                                                  [
                                                                    0,
                                                                    65536
                                                                  ],
                                                                  [
                                                                    0,
                                                                    131072
                                                                  ],
                                                                  [
                                                                    0,
                                                                    262144
                                                                  ],
                                                                  [
                                                                    0,
                                                                    524288
                                                                  ],
                                                                  [
                                                                    0,
                                                                    1048576
                                                                  ],
                                                                  [
                                                                    0,
                                                                    2097152
                                                                  ],
                                                                  [
                                                                    0,
                                                                    4194304
                                                                  ],
                                                                  [
                                                                    0,
                                                                    8388608
                                                                  ],
                                                                  [
                                                                    0,
                                                                    16777216
                                                                  ],
                                                                  [
                                                                    0,
                                                                    33554432
                                                                  ],
                                                                  [
                                                                    0,
                                                                    67108864
                                                                  ],
                                                                  [
                                                                    0,
                                                                    134217728
                                                                  ],
                                                                  [
                                                                    0,
                                                                    268435456
                                                                  ],
                                                                  [
                                                                    0,
                                                                    536870912
                                                                  ],
                                                                  [
                                                                    0,
                                                                    1073741824
                                                                  ],
                                                                  [
                                                                    0,
                                                                    2147483648
                                                                  ],
                                                                  [
                                                                    1,
                                                                    0
                                                                  ],
                                                                  [
                                                                    2,
                                                                    0
                                                                  ],
                                                                  [
                                                                    4,
                                                                    0
                                                                  ],
                                                                  [
                                                                    8,
                                                                    0
                                                                  ],
                                                                  [
                                                                    16,
                                                                    0
                                                                  ],
                                                                  [
                                                                    32,
                                                                    0
                                                                  ],
                                                                  [
                                                                    64,
                                                                    0
                                                                  ],
                                                                  [
                                                                    128,
                                                                    0
                                                                  ],
                                                                  [
                                                                    256,
                                                                    0
                                                                  ],
                                                                  [
                                                                    512,
                                                                    0
                                                                  ],
                                                                  [
                                                                    1024,
                                                                    0
                                                                  ],
                                                                  [
                                                                    2048,
                                                                    0
                                                                  ],
                                                                  [
                                                                    4096,
                                                                    0
                                                                  ],
                                                                  [
                                                                    8192,
                                                                    0
                                                                  ],
                                                                  [
                                                                    16384,
                                                                    0
                                                                  ],
                                                                  [
                                                                    32768,
                                                                    0
                                                                  ],
                                                                  [
                                                                    65536,
                                                                    0
                                                                  ],
                                                                  [
                                                                    131072,
                                                                    0
                                                                  ],
                                                                  [
                                                                    262144,
                                                                    0
                                                                  ],
                                                                  [
                                                                    524288,
                                                                    0
                                                                  ],
                                                                  [
                                                                    1048576,
                                                                    0
                                                                  ],
                                                                  [
                                                                    2097152,
                                                                    0
                                                                  ],
                                                                  [
                                                                    4194304,
                                                                    0
                                                                  ],
                                                                  [
                                                                    8388608,
                                                                    0
                                                                  ],
                                                                  [
                                                                    16777216,
                                                                    0
                                                                  ],
                                                                  [
                                                                    33554432,
                                                                    0
                                                                  ],
                                                                  [
                                                                    67108864,
                                                                    0
                                                                  ],
                                                                  [
                                                                    134217728,
                                                                    0
                                                                  ],
                                                                  [
                                                                    268435456,
                                                                    0
                                                                  ],
                                                                  [
                                                                    536870912,
                                                                    0
                                                                  ],
                                                                  [
                                                                    1073741824,
                                                                    0
                                                                  ],
                                                                  Caml_int64.min_int
                                                                ]
                                                              };
                                                      })
                                                  ],
                                                  tl: {
                                                    hd: [
                                                      "lsr",
                                                      (function (param) {
                                                          return {
                                                                  TAG: /* Eq */0,
                                                                  _0: $$Array.map((function (x) {
                                                                          return Caml_int64.lsr_(Caml_int64.neg_one, x);
                                                                        }), $$Array.init(64, (function (i) {
                                                                              return i;
                                                                            }))),
                                                                  _1: [
                                                                    Caml_int64.neg_one,
                                                                    Caml_int64.max_int,
                                                                    [
                                                                      1073741823,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      536870911,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      268435455,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      134217727,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      67108863,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      33554431,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      16777215,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      8388607,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      4194303,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      2097151,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      1048575,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      524287,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      262143,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      131071,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      65535,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      32767,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      16383,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      8191,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      4095,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      2047,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      1023,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      511,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      255,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      127,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      63,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      31,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      15,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      7,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      3,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      1,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      0,
                                                                      4294967295
                                                                    ],
                                                                    [
                                                                      0,
                                                                      2147483647
                                                                    ],
                                                                    [
                                                                      0,
                                                                      1073741823
                                                                    ],
                                                                    [
                                                                      0,
                                                                      536870911
                                                                    ],
                                                                    [
                                                                      0,
                                                                      268435455
                                                                    ],
                                                                    [
                                                                      0,
                                                                      134217727
                                                                    ],
                                                                    [
                                                                      0,
                                                                      67108863
                                                                    ],
                                                                    [
                                                                      0,
                                                                      33554431
                                                                    ],
                                                                    [
                                                                      0,
                                                                      16777215
                                                                    ],
                                                                    [
                                                                      0,
                                                                      8388607
                                                                    ],
                                                                    [
                                                                      0,
                                                                      4194303
                                                                    ],
                                                                    [
                                                                      0,
                                                                      2097151
                                                                    ],
                                                                    [
                                                                      0,
                                                                      1048575
                                                                    ],
                                                                    [
                                                                      0,
                                                                      524287
                                                                    ],
                                                                    [
                                                                      0,
                                                                      262143
                                                                    ],
                                                                    [
                                                                      0,
                                                                      131071
                                                                    ],
                                                                    [
                                                                      0,
                                                                      65535
                                                                    ],
                                                                    [
                                                                      0,
                                                                      32767
                                                                    ],
                                                                    [
                                                                      0,
                                                                      16383
                                                                    ],
                                                                    [
                                                                      0,
                                                                      8191
                                                                    ],
                                                                    [
                                                                      0,
                                                                      4095
                                                                    ],
                                                                    [
                                                                      0,
                                                                      2047
                                                                    ],
                                                                    [
                                                                      0,
                                                                      1023
                                                                    ],
                                                                    [
                                                                      0,
                                                                      511
                                                                    ],
                                                                    [
                                                                      0,
                                                                      255
                                                                    ],
                                                                    [
                                                                      0,
                                                                      127
                                                                    ],
                                                                    [
                                                                      0,
                                                                      63
                                                                    ],
                                                                    [
                                                                      0,
                                                                      31
                                                                    ],
                                                                    [
                                                                      0,
                                                                      15
                                                                    ],
                                                                    [
                                                                      0,
                                                                      7
                                                                    ],
                                                                    [
                                                                      0,
                                                                      3
                                                                    ],
                                                                    Caml_int64.one
                                                                  ]
                                                                };
                                                        })
                                                    ],
                                                    tl: {
                                                      hd: [
                                                        "asr",
                                                        (function (param) {
                                                            return {
                                                                    TAG: /* Eq */0,
                                                                    _0: $$Array.map((function (x) {
                                                                            return Caml_int64.asr_(Caml_int64.neg_one, x);
                                                                          }), $$Array.init(64, (function (i) {
                                                                                return i;
                                                                              }))),
                                                                    _1: [
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.neg_one
                                                                    ]
                                                                  };
                                                          })
                                                      ],
                                                      tl: {
                                                        hd: [
                                                          "mul simple",
                                                          (function (param) {
                                                              return {
                                                                      TAG: /* Eq */0,
                                                                      _0: [
                                                                        0,
                                                                        6
                                                                      ],
                                                                      _1: [
                                                                        0,
                                                                        6
                                                                      ]
                                                                    };
                                                            })
                                                        ],
                                                        tl: {
                                                          hd: [
                                                            "of_int32",
                                                            (function (param) {
                                                                return {
                                                                        TAG: /* Eq */0,
                                                                        _0: $$Array.map(Caml_int64.of_int32, [
                                                                              0,
                                                                              -2147483648
                                                                            ]),
                                                                        _1: [
                                                                          Caml_int64.zero,
                                                                          [
                                                                            -1,
                                                                            2147483648
                                                                          ]
                                                                        ]
                                                                      };
                                                              })
                                                          ],
                                                          tl: {
                                                            hd: [
                                                              "of_int32_singleton",
                                                              (function (param) {
                                                                  return {
                                                                          TAG: /* Eq */0,
                                                                          _0: [
                                                                            -1,
                                                                            4294967293
                                                                          ],
                                                                          _1: [
                                                                            -1,
                                                                            4294967293
                                                                          ]
                                                                        };
                                                                })
                                                            ],
                                                            tl: {
                                                              hd: [
                                                                "File \"int64_test.ml\", line 134, characters 4-11",
                                                                (function (param) {
                                                                    return {
                                                                            TAG: /* Eq */0,
                                                                            _0: [
                                                                              0,
                                                                              3
                                                                            ],
                                                                            _1: [
                                                                              0,
                                                                              3
                                                                            ]
                                                                          };
                                                                  })
                                                              ],
                                                              tl: {
                                                                hd: [
                                                                  "to_int32",
                                                                  (function (param) {
                                                                      return {
                                                                              TAG: /* Eq */0,
                                                                              _0: $$Array.map(Caml_int64.to_int32, [
                                                                                    Caml_int64.zero,
                                                                                    [
                                                                                      0,
                                                                                      2147483648
                                                                                    ]
                                                                                  ]),
                                                                              _1: [
                                                                                0,
                                                                                -2147483648
                                                                              ]
                                                                            };
                                                                    })
                                                                ],
                                                                tl: {
                                                                  hd: [
                                                                    "discard_sign",
                                                                    (function (param) {
                                                                        return {
                                                                                TAG: /* Eq */0,
                                                                                _0: Caml_int64.discard_sign(Caml_int64.neg_one),
                                                                                _1: Caml_int64.max_int
                                                                              };
                                                                      })
                                                                  ],
                                                                  tl: {
                                                                    hd: [
                                                                      "div_mod",
                                                                      (function (param) {
                                                                          return {
                                                                                  TAG: /* Eq */0,
                                                                                  _0: Caml_int64.div_mod([
                                                                                        0,
                                                                                        7
                                                                                      ], [
                                                                                        0,
                                                                                        3
                                                                                      ]),
                                                                                  _1: [
                                                                                    [
                                                                                      0,
                                                                                      2
                                                                                    ],
                                                                                    Caml_int64.one
                                                                                  ]
                                                                                };
                                                                        })
                                                                    ],
                                                                    tl: {
                                                                      hd: [
                                                                        "to_hex",
                                                                        (function (param) {
                                                                            return {
                                                                                    TAG: /* Eq */0,
                                                                                    _0: Caml_int64.to_hex(Caml_int64.neg_one),
                                                                                    _1: "ffffffffffffffff"
                                                                                  };
                                                                          })
                                                                      ],
                                                                      tl: {
                                                                        hd: [
                                                                          "generic_compare",
                                                                          (function (param) {
                                                                              return {
                                                                                      TAG: /* Eq */0,
                                                                                      _0: Caml_obj.compare([
                                                                                            1,
                                                                                            0
                                                                                          ], Caml_int64.one) > 0,
                                                                                      _1: true
                                                                                    };
                                                                            })
                                                                        ],
                                                                        tl: {
                                                                          hd: [
                                                                            "test_compier_literal",
                                                                            (function (param) {
                                                                                return {
                                                                                        TAG: /* Eq */0,
                                                                                        _0: [
                                                                                          0,
                                                                                          4294967295
                                                                                        ],
                                                                                        _1: [
                                                                                          0,
                                                                                          4294967295
                                                                                        ]
                                                                                      };
                                                                              })
                                                                          ],
                                                                          tl: {
                                                                            hd: [
                                                                              "generic_compare2",
                                                                              (function (param) {
                                                                                  return {
                                                                                          TAG: /* Eq */0,
                                                                                          _0: Caml_obj.compare([
                                                                                                0,
                                                                                                2147483648
                                                                                              ], Caml_int64.one) > 0,
                                                                                          _1: true
                                                                                        };
                                                                                })
                                                                            ],
                                                                            tl: {
                                                                              hd: [
                                                                                "shift_left",
                                                                                (function (param) {
                                                                                    return {
                                                                                            TAG: /* Eq */0,
                                                                                            _0: [
                                                                                              0,
                                                                                              4294967040
                                                                                            ],
                                                                                            _1: [
                                                                                              0,
                                                                                              4294967040
                                                                                            ]
                                                                                          };
                                                                                  })
                                                                              ],
                                                                              tl: {
                                                                                hd: [
                                                                                  "fib_int64",
                                                                                  (function (param) {
                                                                                      return {
                                                                                              TAG: /* Eq */0,
                                                                                              _0: fib(1000, Caml_int64.one, [
                                                                                                    0,
                                                                                                    2
                                                                                                  ]),
                                                                                              _1: [
                                                                                                -1990564327,
                                                                                                2874523960
                                                                                              ]
                                                                                            };
                                                                                    })
                                                                                ],
                                                                                tl: {
                                                                                  hd: [
                                                                                    "fac_int64",
                                                                                    (function (param) {
                                                                                        return {
                                                                                                TAG: /* Eq */0,
                                                                                                _0: fac(30, Caml_int64.one),
                                                                                                _1: [
                                                                                                  -2040662563,
                                                                                                  1409286144
                                                                                                ]
                                                                                              };
                                                                                      })
                                                                                  ],
                                                                                  tl: {
                                                                                    hd: [
                                                                                      "File \"int64_test.ml\", line 163, characters 6-13",
                                                                                      (function (param) {
                                                                                          return {
                                                                                                  TAG: /* Eq */0,
                                                                                                  _0: Caml_int64.add(Int64.max_int, Int64.max_int),
                                                                                                  _1: [
                                                                                                    -1,
                                                                                                    4294967294
                                                                                                  ]
                                                                                                };
                                                                                        })
                                                                                    ],
                                                                                    tl: {
                                                                                      hd: [
                                                                                        "File \"int64_test.ml\", line 166, characters 6-13",
                                                                                        (function (param) {
                                                                                            return {
                                                                                                    TAG: /* Eq */0,
                                                                                                    _0: Caml_int64.add(Int64.min_int, Int64.min_int),
                                                                                                    _1: Caml_int64.zero
                                                                                                  };
                                                                                          })
                                                                                      ],
                                                                                      tl: {
                                                                                        hd: [
                                                                                          "File \"int64_test.ml\", line 170, characters 6-13",
                                                                                          (function (param) {
                                                                                              return {
                                                                                                      TAG: /* Eq */0,
                                                                                                      _0: Caml_int64.neg_one,
                                                                                                      _1: Caml_int64.neg_one
                                                                                                    };
                                                                                            })
                                                                                        ],
                                                                                        tl: /* [] */0
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }, Pervasives.$at($$Array.to_list(Ext_array_test.map2i((function (i, a, b) {
                    return [
                            "shift_left_cases " + i,
                            (function (param) {
                                return {
                                        TAG: /* Eq */0,
                                        _0: a,
                                        _1: b
                                      };
                              })
                          ];
                  }), shift_left_tests_0, shift_left_tests_1)), Pervasives.$at($$Array.to_list(Ext_array_test.map2i((function (i, a, b) {
                        return [
                                "shift_right_cases " + i,
                                (function (param) {
                                    return {
                                            TAG: /* Eq */0,
                                            _0: a,
                                            _1: b
                                          };
                                  })
                              ];
                      }), shift_right_tests_0, shift_right_tests_1)), $$Array.to_list(Ext_array_test.map2i((function (i, a, b) {
                        return [
                                "shift_right_logical_cases " + i,
                                (function (param) {
                                    return {
                                            TAG: /* Eq */0,
                                            _0: a,
                                            _1: b
                                          };
                                  })
                              ];
                      }), shift_right_logical_suites_0, shift_right_logical_suites_1)))));

var suites$1 = {
  contents: suites
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites$1, loc, x, y);
}

function id(loc, x) {
  var float_value = Caml_int64.float_of_bits(x);
  var match = Pervasives.classify_float(float_value);
  if (match >= 4) {
    return ;
  } else {
    return eq(loc, Caml_int64.bits_of_float(float_value), x);
  }
}

eq("File \"int64_test.ml\", line 202, characters 5-12", Caml_int64.bits_of_float(0.3), [
      1070805811,
      858993459
    ]);

eq("File \"int64_test.ml\", line 203, characters 5-12", Caml_int64.float_of_bits([
          1070805811,
          858993459
        ]), 0.3);

id("File \"int64_test.ml\", line 204, characters 5-12", Caml_int64.neg_one);

id("File \"int64_test.ml\", line 205, characters 5-12", [
      -1,
      4294967196
    ]);

id("File \"int64_test.ml\", line 206, characters 5-12", [
      0,
      4294967295
    ]);

id("File \"int64_test.ml\", line 207, characters 5-12", [
      0,
      536870911
    ]);

id("File \"int64_test.ml\", line 208, characters 5-12", [
      0,
      536870655
    ]);

eq("File \"int64_test.ml\", line 209, characters 5-12", Caml_int64.div(Int64.min_int, [
          0,
          10
        ]), [
      -214748365,
      858993460
    ]);

eq("File \"int64_test.ml\", line 210, characters 5-12", Caml_int64.to_string(Caml_int64.div(Int64.min_int, [
              0,
              10
            ])), "-922337203685477580");

eq("File \"int64_test.ml\", line 211, characters 5-12", Caml_int64.mul(Int64.min_int, [
          0,
          10
        ]), Caml_int64.zero);

eq("File \"int64_test.ml\", line 212, characters 5-12", Caml_int64.mul([
          0,
          10
        ], Int64.min_int), Caml_int64.zero);

eq("File \"int64_test.ml\", line 213, characters 5-12", Caml_int64.mul(Caml_int64.one, Int64.min_int), Int64.min_int);

eq("File \"int64_test.ml\", line 214, characters 5-12", Caml_int64.mul(Int64.max_int, [
          0,
          10
        ]), [
      -1,
      4294967286
    ]);

eq("File \"int64_test.ml\", line 215, characters 5-12", Caml_int64.succ(Int64.max_int), Int64.min_int);

eq("File \"int64_test.ml\", line 216, characters 5-12", Caml_int64.succ(Int64.min_int), [
      -2147483648,
      1
    ]);

eq("File \"int64_test.ml\", line 217, characters 5-12", Caml_int64.succ([
          0,
          4294967295
        ]), [
      1,
      0
    ]);

Mt.from_pair_suites("Int64_test", suites$1.contents);

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
exports.suites = suites$1;
exports.test_id = test_id;
exports.eq = eq;
exports.id = id;
/* shift_left_tests Not a pure module */
