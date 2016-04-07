// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_int64 = require("../runtime/caml_int64");
var Caml_obj   = require("../runtime/caml_obj");
var Pervasives = require("../stdlib/pervasives");
var Mt         = require("./mt");
var Int32      = require("../stdlib/int32");
var Int64      = require("../stdlib/int64");
var $$Array    = require("../stdlib/array");
var Caml_curry = require("../runtime/caml_curry");
var Ext_array  = require("./ext_array");
var Format     = require("../stdlib/format");

function f(u, v) {
  return +(u > v);
}

var v = Caml_int64.add(Caml_int64.of_int32(Int32.max_int), Int64.one);

var h = Caml_int64.neg(v);

var a = /* int64 */[
  0,
  2147483647
];

function commutative_add(result, a, b) {
  return /* Eq */{
          0: /* tuple */[
            result,
            result
          ],
          1: /* tuple */[
            Caml_int64.add(a, b),
            Caml_int64.add(b, a)
          ],
          length: 2,
          tag: 0
        };
}

function generic_compare(prim, prim$1) {
  return Caml_obj.caml_compare(prim, prim$1);
}

var shift_left_tests_000 = $$Array.map(function (i) {
      return Caml_int64.lsl_(/* int64 */[
                  0,
                  1
                ], i);
    }, Ext_array.range(0, 63));

var shift_left_tests_001 = /* array */[
  /* int64 */[
    0,
    1
  ],
  /* int64 */[
    0,
    2
  ],
  /* int64 */[
    0,
    4
  ],
  /* int64 */[
    0,
    8
  ],
  /* int64 */[
    0,
    16
  ],
  /* int64 */[
    0,
    32
  ],
  /* int64 */[
    0,
    64
  ],
  /* int64 */[
    0,
    128
  ],
  /* int64 */[
    0,
    256
  ],
  /* int64 */[
    0,
    512
  ],
  /* int64 */[
    0,
    1024
  ],
  /* int64 */[
    0,
    2048
  ],
  /* int64 */[
    0,
    4096
  ],
  /* int64 */[
    0,
    8192
  ],
  /* int64 */[
    0,
    16384
  ],
  /* int64 */[
    0,
    32768
  ],
  /* int64 */[
    0,
    65536
  ],
  /* int64 */[
    0,
    131072
  ],
  /* int64 */[
    0,
    262144
  ],
  /* int64 */[
    0,
    524288
  ],
  /* int64 */[
    0,
    1048576
  ],
  /* int64 */[
    0,
    2097152
  ],
  /* int64 */[
    0,
    4194304
  ],
  /* int64 */[
    0,
    8388608
  ],
  /* int64 */[
    0,
    16777216
  ],
  /* int64 */[
    0,
    33554432
  ],
  /* int64 */[
    0,
    67108864
  ],
  /* int64 */[
    0,
    134217728
  ],
  /* int64 */[
    0,
    268435456
  ],
  /* int64 */[
    0,
    536870912
  ],
  /* int64 */[
    0,
    1073741824
  ],
  /* int64 */[
    0,
    2147483648
  ],
  /* int64 */[
    1,
    0
  ],
  /* int64 */[
    2,
    0
  ],
  /* int64 */[
    4,
    0
  ],
  /* int64 */[
    8,
    0
  ],
  /* int64 */[
    16,
    0
  ],
  /* int64 */[
    32,
    0
  ],
  /* int64 */[
    64,
    0
  ],
  /* int64 */[
    128,
    0
  ],
  /* int64 */[
    256,
    0
  ],
  /* int64 */[
    512,
    0
  ],
  /* int64 */[
    1024,
    0
  ],
  /* int64 */[
    2048,
    0
  ],
  /* int64 */[
    4096,
    0
  ],
  /* int64 */[
    8192,
    0
  ],
  /* int64 */[
    16384,
    0
  ],
  /* int64 */[
    32768,
    0
  ],
  /* int64 */[
    65536,
    0
  ],
  /* int64 */[
    131072,
    0
  ],
  /* int64 */[
    262144,
    0
  ],
  /* int64 */[
    524288,
    0
  ],
  /* int64 */[
    1048576,
    0
  ],
  /* int64 */[
    2097152,
    0
  ],
  /* int64 */[
    4194304,
    0
  ],
  /* int64 */[
    8388608,
    0
  ],
  /* int64 */[
    16777216,
    0
  ],
  /* int64 */[
    33554432,
    0
  ],
  /* int64 */[
    67108864,
    0
  ],
  /* int64 */[
    134217728,
    0
  ],
  /* int64 */[
    268435456,
    0
  ],
  /* int64 */[
    536870912,
    0
  ],
  /* int64 */[
    1073741824,
    0
  ],
  /* int64 */[
    -2147483648,
    0
  ]
];

var shift_left_tests = /* tuple */[
  shift_left_tests_000,
  shift_left_tests_001
];

var shift_right_tests_000 = $$Array.map(function (i) {
      return Caml_int64.asr_(/* int64 */[
                  -2147483648,
                  0
                ], i);
    }, Ext_array.range(0, 63));

var shift_right_tests_001 = /* array */[
  /* int64 */[
    -2147483648,
    0
  ],
  /* int64 */[
    -1073741824,
    0
  ],
  /* int64 */[
    -536870912,
    0
  ],
  /* int64 */[
    -268435456,
    0
  ],
  /* int64 */[
    -134217728,
    0
  ],
  /* int64 */[
    -67108864,
    0
  ],
  /* int64 */[
    -33554432,
    0
  ],
  /* int64 */[
    -16777216,
    0
  ],
  /* int64 */[
    -8388608,
    0
  ],
  /* int64 */[
    -4194304,
    0
  ],
  /* int64 */[
    -2097152,
    0
  ],
  /* int64 */[
    -1048576,
    0
  ],
  /* int64 */[
    -524288,
    0
  ],
  /* int64 */[
    -262144,
    0
  ],
  /* int64 */[
    -131072,
    0
  ],
  /* int64 */[
    -65536,
    0
  ],
  /* int64 */[
    -32768,
    0
  ],
  /* int64 */[
    -16384,
    0
  ],
  /* int64 */[
    -8192,
    0
  ],
  /* int64 */[
    -4096,
    0
  ],
  /* int64 */[
    -2048,
    0
  ],
  /* int64 */[
    -1024,
    0
  ],
  /* int64 */[
    -512,
    0
  ],
  /* int64 */[
    -256,
    0
  ],
  /* int64 */[
    -128,
    0
  ],
  /* int64 */[
    -64,
    0
  ],
  /* int64 */[
    -32,
    0
  ],
  /* int64 */[
    -16,
    0
  ],
  /* int64 */[
    -8,
    0
  ],
  /* int64 */[
    -4,
    0
  ],
  /* int64 */[
    -2,
    0
  ],
  /* int64 */[
    -1,
    0
  ],
  /* int64 */[
    -1,
    2147483648
  ],
  /* int64 */[
    -1,
    3221225472
  ],
  /* int64 */[
    -1,
    3758096384
  ],
  /* int64 */[
    -1,
    4026531840
  ],
  /* int64 */[
    -1,
    4160749568
  ],
  /* int64 */[
    -1,
    4227858432
  ],
  /* int64 */[
    -1,
    4261412864
  ],
  /* int64 */[
    -1,
    4278190080
  ],
  /* int64 */[
    -1,
    4286578688
  ],
  /* int64 */[
    -1,
    4290772992
  ],
  /* int64 */[
    -1,
    4292870144
  ],
  /* int64 */[
    -1,
    4293918720
  ],
  /* int64 */[
    -1,
    4294443008
  ],
  /* int64 */[
    -1,
    4294705152
  ],
  /* int64 */[
    -1,
    4294836224
  ],
  /* int64 */[
    -1,
    4294901760
  ],
  /* int64 */[
    -1,
    4294934528
  ],
  /* int64 */[
    -1,
    4294950912
  ],
  /* int64 */[
    -1,
    4294959104
  ],
  /* int64 */[
    -1,
    4294963200
  ],
  /* int64 */[
    -1,
    4294965248
  ],
  /* int64 */[
    -1,
    4294966272
  ],
  /* int64 */[
    -1,
    4294966784
  ],
  /* int64 */[
    -1,
    4294967040
  ],
  /* int64 */[
    -1,
    4294967168
  ],
  /* int64 */[
    -1,
    4294967232
  ],
  /* int64 */[
    -1,
    4294967264
  ],
  /* int64 */[
    -1,
    4294967280
  ],
  /* int64 */[
    -1,
    4294967288
  ],
  /* int64 */[
    -1,
    4294967292
  ],
  /* int64 */[
    -1,
    4294967294
  ],
  /* int64 */[
    -1,
    4294967295
  ]
];

var shift_right_tests = /* tuple */[
  shift_right_tests_000,
  shift_right_tests_001
];

var shift_right_logical_suites_000 = $$Array.map(function (i) {
      return Caml_int64.lsr_(/* int64 */[
                  -2147483648,
                  0
                ], i);
    }, Ext_array.range(0, 63));

var shift_right_logical_suites_001 = /* array */[
  /* int64 */[
    -2147483648,
    0
  ],
  /* int64 */[
    1073741824,
    0
  ],
  /* int64 */[
    536870912,
    0
  ],
  /* int64 */[
    268435456,
    0
  ],
  /* int64 */[
    134217728,
    0
  ],
  /* int64 */[
    67108864,
    0
  ],
  /* int64 */[
    33554432,
    0
  ],
  /* int64 */[
    16777216,
    0
  ],
  /* int64 */[
    8388608,
    0
  ],
  /* int64 */[
    4194304,
    0
  ],
  /* int64 */[
    2097152,
    0
  ],
  /* int64 */[
    1048576,
    0
  ],
  /* int64 */[
    524288,
    0
  ],
  /* int64 */[
    262144,
    0
  ],
  /* int64 */[
    131072,
    0
  ],
  /* int64 */[
    65536,
    0
  ],
  /* int64 */[
    32768,
    0
  ],
  /* int64 */[
    16384,
    0
  ],
  /* int64 */[
    8192,
    0
  ],
  /* int64 */[
    4096,
    0
  ],
  /* int64 */[
    2048,
    0
  ],
  /* int64 */[
    1024,
    0
  ],
  /* int64 */[
    512,
    0
  ],
  /* int64 */[
    256,
    0
  ],
  /* int64 */[
    128,
    0
  ],
  /* int64 */[
    64,
    0
  ],
  /* int64 */[
    32,
    0
  ],
  /* int64 */[
    16,
    0
  ],
  /* int64 */[
    8,
    0
  ],
  /* int64 */[
    4,
    0
  ],
  /* int64 */[
    2,
    0
  ],
  /* int64 */[
    1,
    0
  ],
  /* int64 */[
    0,
    2147483648
  ],
  /* int64 */[
    0,
    1073741824
  ],
  /* int64 */[
    0,
    536870912
  ],
  /* int64 */[
    0,
    268435456
  ],
  /* int64 */[
    0,
    134217728
  ],
  /* int64 */[
    0,
    67108864
  ],
  /* int64 */[
    0,
    33554432
  ],
  /* int64 */[
    0,
    16777216
  ],
  /* int64 */[
    0,
    8388608
  ],
  /* int64 */[
    0,
    4194304
  ],
  /* int64 */[
    0,
    2097152
  ],
  /* int64 */[
    0,
    1048576
  ],
  /* int64 */[
    0,
    524288
  ],
  /* int64 */[
    0,
    262144
  ],
  /* int64 */[
    0,
    131072
  ],
  /* int64 */[
    0,
    65536
  ],
  /* int64 */[
    0,
    32768
  ],
  /* int64 */[
    0,
    16384
  ],
  /* int64 */[
    0,
    8192
  ],
  /* int64 */[
    0,
    4096
  ],
  /* int64 */[
    0,
    2048
  ],
  /* int64 */[
    0,
    1024
  ],
  /* int64 */[
    0,
    512
  ],
  /* int64 */[
    0,
    256
  ],
  /* int64 */[
    0,
    128
  ],
  /* int64 */[
    0,
    64
  ],
  /* int64 */[
    0,
    32
  ],
  /* int64 */[
    0,
    16
  ],
  /* int64 */[
    0,
    8
  ],
  /* int64 */[
    0,
    4
  ],
  /* int64 */[
    0,
    2
  ],
  /* int64 */[
    0,
    1
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
    if (n) {
      _b = Caml_int64.add(a, b);
      _a = b;
      _n = n - 1;
      continue ;
      
    }
    else {
      return a;
    }
  };
}

function fac(_n, _acc) {
  while(true) {
    var acc = _acc;
    var n = _n;
    if (n) {
      _acc = Caml_int64.mul(acc, Caml_int64.of_int32(n));
      _n = n - 1;
      continue ;
      
    }
    else {
      return acc;
    }
  };
}

var suites = Pervasives.$at(/* :: */[
      /* tuple */[
        "add_one",
        function () {
          return /* Eq */{
                  0: v,
                  1: /* int64 */[
                    0,
                    2147483648
                  ],
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* :: */[
        /* tuple */[
          "add_2",
          function () {
            return /* Eq */{
                    0: /* int64 */[
                      0,
                      4294967294
                    ],
                    1: Caml_int64.add(a, a),
                    length: 2,
                    tag: 0
                  };
          }
        ],
        /* :: */[
          /* tuple */[
            "add_3",
            function () {
              return /* Eq */{
                      0: /* int64 */[
                        0,
                        0
                      ],
                      1: Caml_int64.add(/* int64 */[
                            0,
                            0
                          ], /* int64 */[
                            0,
                            0
                          ]),
                      length: 2,
                      tag: 0
                    };
            }
          ],
          /* :: */[
            /* tuple */[
              "add_4",
              function () {
                return commutative_add(/* int64 */[
                            -1,
                            4294967294
                          ], /* int64 */[
                            -1,
                            4294967293
                          ], /* int64 */[
                            0,
                            1
                          ]);
              }
            ],
            /* :: */[
              /* tuple */[
                "add_5",
                function () {
                  return commutative_add(/* int64 */[
                              -1,
                              4294967293
                            ], /* int64 */[
                              -1,
                              4294967293
                            ], /* int64 */[
                              0,
                              0
                            ]);
                }
              ],
              /* :: */[
                /* tuple */[
                  "add_6",
                  function () {
                    return commutative_add(/* int64 */[
                                0,
                                4
                              ], /* int64 */[
                                -1,
                                4294967293
                              ], /* int64 */[
                                0,
                                7
                              ]);
                  }
                ],
                /* :: */[
                  /* tuple */[
                    "add_7",
                    function () {
                      return commutative_add(/* int64 */[
                                  1,
                                  0
                                ], /* int64 */[
                                  0,
                                  2147483648
                                ], /* int64 */[
                                  0,
                                  2147483648
                                ]);
                    }
                  ],
                  /* :: */[
                    /* tuple */[
                      "add_8",
                      function () {
                        return commutative_add(/* int64 */[
                                    1,
                                    0
                                  ], /* int64 */[
                                    0,
                                    4294967295
                                  ], /* int64 */[
                                    0,
                                    1
                                  ]);
                      }
                    ],
                    /* :: */[
                      /* tuple */[
                        "add_9",
                        function () {
                          return commutative_add(/* int64 */[
                                      0,
                                      4294967295
                                    ], /* int64 */[
                                      0,
                                      2147483648
                                    ], /* int64 */[
                                      0,
                                      2147483647
                                    ]);
                        }
                      ],
                      /* :: */[
                        /* tuple */[
                          "add_10",
                          function () {
                            return commutative_add(/* int64 */[
                                        0,
                                        2147483648
                                      ], /* int64 */[
                                        0,
                                        2147483648
                                      ], /* int64 */[
                                        0,
                                        0
                                      ]);
                          }
                        ],
                        /* :: */[
                          /* tuple */[
                            "add_11",
                            function () {
                              return commutative_add(/* int64 */[
                                          0,
                                          4294967295
                                        ], /* int64 */[
                                          0,
                                          4294967295
                                        ], /* int64 */[
                                          0,
                                          0
                                        ]);
                            }
                          ],
                          /* :: */[
                            /* tuple */[
                              "to_int32",
                              function () {
                                return /* Eq */{
                                        0: 3,
                                        1: 3,
                                        length: 2,
                                        tag: 0
                                      };
                              }
                            ],
                            /* :: */[
                              /* tuple */[
                                "to_int",
                                function () {
                                  return /* Eq */{
                                          0: 3,
                                          1: 3,
                                          length: 2,
                                          tag: 0
                                        };
                                }
                              ],
                              /* :: */[
                                /* tuple */[
                                  "of_int",
                                  function () {
                                    return /* Eq */{
                                            0: /* int64 */[
                                              0,
                                              3
                                            ],
                                            1: /* int64 */[
                                              0,
                                              3
                                            ],
                                            length: 2,
                                            tag: 0
                                          };
                                  }
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "lognot",
                                    function () {
                                      return /* Eq */{
                                              0: /* int64 */[
                                                -1,
                                                4294967293
                                              ],
                                              1: /* int64 */[
                                                -1,
                                                ((2 ^ 4294967295) >>> 0)
                                              ],
                                              length: 2,
                                              tag: 0
                                            };
                                    }
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "neg",
                                      function () {
                                        return /* Eq */{
                                                0: /* int64 */[
                                                  -1,
                                                  4294967294
                                                ],
                                                1: Caml_int64.neg(/* int64 */[
                                                      0,
                                                      2
                                                    ]),
                                                length: 2,
                                                tag: 0
                                              };
                                      }
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "sub1",
                                        function () {
                                          return /* Eq */{
                                                  0: /* int64 */[
                                                    0,
                                                    2
                                                  ],
                                                  1: Caml_int64.sub(/* int64 */[
                                                        0,
                                                        3
                                                      ], /* int64 */[
                                                        0,
                                                        1
                                                      ]),
                                                  length: 2,
                                                  tag: 0
                                                };
                                        }
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "xor1",
                                          function () {
                                            return /* Eq */{
                                                    0: /* tuple */[
                                                      /* int64 */[
                                                        0,
                                                        ((4009750271 ^ 4293853166) >>> 0)
                                                      ],
                                                      /* int64 */[
                                                        0 ^ 0,
                                                        ((2147483647 ^ 4009750271) >>> 0)
                                                      ]
                                                    ],
                                                    1: /* tuple */[
                                                      /* int64 */[
                                                        0,
                                                        286331153
                                                      ],
                                                      /* int64 */[
                                                        0,
                                                        2432700672
                                                      ]
                                                    ],
                                                    length: 2,
                                                    tag: 0
                                                  };
                                          }
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "or",
                                            function () {
                                              return /* Eq */{
                                                      0: /* int64 */[
                                                        0,
                                                        ((4009750271 | 4293853166) >>> 0)
                                                      ],
                                                      1: /* int64 */[
                                                        0,
                                                        4294967295
                                                      ],
                                                      length: 2,
                                                      tag: 0
                                                    };
                                            }
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "and",
                                              function () {
                                                return /* Eq */{
                                                        0: /* int64 */[
                                                          0 & 0,
                                                          ((4009750271 & 4293853166) >>> 0)
                                                        ],
                                                        1: /* int64 */[
                                                          0,
                                                          4008636142
                                                        ],
                                                        length: 2,
                                                        tag: 0
                                                      };
                                              }
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "lsl",
                                                function () {
                                                  return /* Eq */{
                                                          0: $$Array.map(function (x) {
                                                                return Caml_int64.lsl_(/* int64 */[
                                                                            0,
                                                                            1
                                                                          ], x);
                                                              }, $$Array.init(64, function (i) {
                                                                    return i;
                                                                  })),
                                                          1: /* array */[
                                                            /* int64 */[
                                                              0,
                                                              1
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              2
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              4
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              8
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              16
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              32
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              64
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              128
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              256
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              512
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              1024
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              2048
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              4096
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              8192
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              16384
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              32768
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              65536
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              131072
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              262144
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              524288
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              1048576
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              2097152
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              4194304
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              8388608
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              16777216
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              33554432
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              67108864
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              134217728
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              268435456
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              536870912
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              1073741824
                                                            ],
                                                            /* int64 */[
                                                              0,
                                                              2147483648
                                                            ],
                                                            /* int64 */[
                                                              1,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              2,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              4,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              8,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              16,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              32,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              64,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              128,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              256,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              512,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              1024,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              2048,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              4096,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              8192,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              16384,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              32768,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              65536,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              131072,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              262144,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              524288,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              1048576,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              2097152,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              4194304,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              8388608,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              16777216,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              33554432,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              67108864,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              134217728,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              268435456,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              536870912,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              1073741824,
                                                              0
                                                            ],
                                                            /* int64 */[
                                                              -2147483648,
                                                              0
                                                            ]
                                                          ],
                                                          length: 2,
                                                          tag: 0
                                                        };
                                                }
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "lsr",
                                                  function () {
                                                    return /* Eq */{
                                                            0: $$Array.map(function (x) {
                                                                  return Caml_int64.lsr_(/* int64 */[
                                                                              -1,
                                                                              4294967295
                                                                            ], x);
                                                                }, $$Array.init(64, function (i) {
                                                                      return i;
                                                                    })),
                                                            1: /* array */[
                                                              /* int64 */[
                                                                -1,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                2147483647,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                1073741823,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                536870911,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                268435455,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                134217727,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                67108863,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                33554431,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                16777215,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                8388607,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                4194303,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                2097151,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                1048575,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                524287,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                262143,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                131071,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                65535,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                32767,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                16383,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                8191,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                4095,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                2047,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                1023,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                511,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                255,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                127,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                63,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                31,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                15,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                7,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                3,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                1,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                4294967295
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                2147483647
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                1073741823
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                536870911
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                268435455
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                134217727
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                67108863
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                33554431
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                16777215
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                8388607
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                4194303
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                2097151
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                1048575
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                524287
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                262143
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                131071
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                65535
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                32767
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                16383
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                8191
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                4095
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                2047
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                1023
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                511
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                255
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                127
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                63
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                31
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                15
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                7
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                3
                                                              ],
                                                              /* int64 */[
                                                                0,
                                                                1
                                                              ]
                                                            ],
                                                            length: 2,
                                                            tag: 0
                                                          };
                                                  }
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "asr",
                                                    function () {
                                                      return /* Eq */{
                                                              0: $$Array.map(function (x) {
                                                                    return Caml_int64.asr_(/* int64 */[
                                                                                -1,
                                                                                4294967295
                                                                              ], x);
                                                                  }, $$Array.init(64, function (i) {
                                                                        return i;
                                                                      })),
                                                              1: /* array */[
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ],
                                                                /* int64 */[
                                                                  -1,
                                                                  4294967295
                                                                ]
                                                              ],
                                                              length: 2,
                                                              tag: 0
                                                            };
                                                    }
                                                  ],
                                                  /* :: */[
                                                    /* tuple */[
                                                      "mul simple",
                                                      function () {
                                                        return /* Eq */{
                                                                0: /* int64 */[
                                                                  0,
                                                                  6
                                                                ],
                                                                1: Caml_int64.mul(/* int64 */[
                                                                      0,
                                                                      3
                                                                    ], /* int64 */[
                                                                      0,
                                                                      2
                                                                    ]),
                                                                length: 2,
                                                                tag: 0
                                                              };
                                                      }
                                                    ],
                                                    /* :: */[
                                                      /* tuple */[
                                                        "of_int32",
                                                        function () {
                                                          return /* Eq */{
                                                                  0: $$Array.map(function (prim) {
                                                                        return Caml_int64.of_int32(prim);
                                                                      }, /* array */[
                                                                        0,
                                                                        -2147483648
                                                                      ]),
                                                                  1: /* array */[
                                                                    /* int64 */[
                                                                      0,
                                                                      0
                                                                    ],
                                                                    /* int64 */[
                                                                      -1,
                                                                      2147483648
                                                                    ]
                                                                  ],
                                                                  length: 2,
                                                                  tag: 0
                                                                };
                                                        }
                                                      ],
                                                      /* :: */[
                                                        /* tuple */[
                                                          "to_int32",
                                                          function () {
                                                            return /* Eq */{
                                                                    0: $$Array.map(function (prim) {
                                                                          return prim[1] | 0;
                                                                        }, /* array */[
                                                                          /* int64 */[
                                                                            0,
                                                                            0
                                                                          ],
                                                                          /* int64 */[
                                                                            0,
                                                                            2147483648
                                                                          ]
                                                                        ]),
                                                                    1: /* array */[
                                                                      0,
                                                                      -2147483648
                                                                    ],
                                                                    length: 2,
                                                                    tag: 0
                                                                  };
                                                          }
                                                        ],
                                                        /* :: */[
                                                          /* tuple */[
                                                            "discard_sign",
                                                            function () {
                                                              return /* Eq */{
                                                                      0: Caml_int64.discard_sign(/* int64 */[
                                                                            -1,
                                                                            4294967295
                                                                          ]),
                                                                      1: /* int64 */[
                                                                        2147483647,
                                                                        4294967295
                                                                      ],
                                                                      length: 2,
                                                                      tag: 0
                                                                    };
                                                            }
                                                          ],
                                                          /* :: */[
                                                            /* tuple */[
                                                              "div_mod",
                                                              function () {
                                                                return /* Eq */{
                                                                        0: Caml_int64.div_mod(/* int64 */[
                                                                              0,
                                                                              7
                                                                            ], /* int64 */[
                                                                              0,
                                                                              3
                                                                            ]),
                                                                        1: /* tuple */[
                                                                          /* int64 */[
                                                                            0,
                                                                            2
                                                                          ],
                                                                          /* int64 */[
                                                                            0,
                                                                            1
                                                                          ]
                                                                        ],
                                                                        length: 2,
                                                                        tag: 0
                                                                      };
                                                              }
                                                            ],
                                                            /* :: */[
                                                              /* tuple */[
                                                                "to_hex",
                                                                function () {
                                                                  return /* Eq */{
                                                                          0: Caml_int64.to_hex(/* int64 */[
                                                                                -1,
                                                                                4294967295
                                                                              ]),
                                                                          1: "ffffffffffffffff",
                                                                          length: 2,
                                                                          tag: 0
                                                                        };
                                                                }
                                                              ],
                                                              /* :: */[
                                                                /* tuple */[
                                                                  "generic_compare",
                                                                  function () {
                                                                    return /* Eq */{
                                                                            0: +(Caml_obj.caml_compare(/* int64 */[
                                                                                    1,
                                                                                    0
                                                                                  ], /* int64 */[
                                                                                    0,
                                                                                    1
                                                                                  ]) > 0),
                                                                            1: /* true */1,
                                                                            length: 2,
                                                                            tag: 0
                                                                          };
                                                                  }
                                                                ],
                                                                /* :: */[
                                                                  /* tuple */[
                                                                    "test_compier_literal",
                                                                    function () {
                                                                      return /* Eq */{
                                                                              0: /* int64 */[
                                                                                0,
                                                                                4294967295
                                                                              ],
                                                                              1: Caml_int64.add(Caml_int64.add(/* int64 */[
                                                                                        0,
                                                                                        2147483647
                                                                                      ], /* int64 */[
                                                                                        0,
                                                                                        2147483647
                                                                                      ]), /* int64 */[
                                                                                    0,
                                                                                    1
                                                                                  ]),
                                                                              length: 2,
                                                                              tag: 0
                                                                            };
                                                                    }
                                                                  ],
                                                                  /* :: */[
                                                                    /* tuple */[
                                                                      "generic_compare2",
                                                                      function () {
                                                                        return /* Eq */{
                                                                                0: +(Caml_obj.caml_compare(/* int64 */[
                                                                                        0,
                                                                                        2147483648
                                                                                      ], /* int64 */[
                                                                                        0,
                                                                                        1
                                                                                      ]) > 0),
                                                                                1: /* true */1,
                                                                                length: 2,
                                                                                tag: 0
                                                                              };
                                                                      }
                                                                    ],
                                                                    /* :: */[
                                                                      /* tuple */[
                                                                        "shift_left",
                                                                        function () {
                                                                          return /* Eq */{
                                                                                  0: Caml_int64.lsl_(/* int64 */[
                                                                                        0,
                                                                                        16777215
                                                                                      ], 8),
                                                                                  1: /* int64 */[
                                                                                    0,
                                                                                    4294967040
                                                                                  ],
                                                                                  length: 2,
                                                                                  tag: 0
                                                                                };
                                                                        }
                                                                      ],
                                                                      /* :: */[
                                                                        /* tuple */[
                                                                          "shift_right",
                                                                          function () {
                                                                            return /* Eq */{
                                                                                    0: 4294967295,
                                                                                    1: 4294967295,
                                                                                    length: 2,
                                                                                    tag: 0
                                                                                  };
                                                                          }
                                                                        ],
                                                                        /* :: */[
                                                                          /* tuple */[
                                                                            "fib_int64",
                                                                            function () {
                                                                              return /* Eq */{
                                                                                      0: fib(1000, /* int64 */[
                                                                                            0,
                                                                                            1
                                                                                          ], /* int64 */[
                                                                                            0,
                                                                                            2
                                                                                          ]),
                                                                                      1: /* int64 */[
                                                                                        -1990564327,
                                                                                        2874523960
                                                                                      ],
                                                                                      length: 2,
                                                                                      tag: 0
                                                                                    };
                                                                            }
                                                                          ],
                                                                          /* :: */[
                                                                            /* tuple */[
                                                                              "fac_int64",
                                                                              function () {
                                                                                return /* Eq */{
                                                                                        0: fac(30, /* int64 */[
                                                                                              0,
                                                                                              1
                                                                                            ]),
                                                                                        1: /* int64 */[
                                                                                          -2040662563,
                                                                                          1409286144
                                                                                        ],
                                                                                        length: 2,
                                                                                        tag: 0
                                                                                      };
                                                                              }
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
    ], Pervasives.$at($$Array.to_list(Ext_array.map2i(function (i, a, b) {
                  return /* tuple */[
                          Caml_curry.app1(Format.asprintf(/* Format */{
                                    0: /* String_literal */{
                                      0: "shift_left_cases ",
                                      1: /* Int */{
                                        0: /* Int_d */0,
                                        1: /* No_padding */0,
                                        2: /* No_precision */0,
                                        3: /* End_of_format */0,
                                        length: 4,
                                        tag: 4
                                      },
                                      length: 2,
                                      tag: 11
                                    },
                                    1: "shift_left_cases %d",
                                    length: 2,
                                    tag: 0
                                  }), i),
                          function () {
                            return /* Eq */{
                                    0: a,
                                    1: b,
                                    length: 2,
                                    tag: 0
                                  };
                          }
                        ];
                }, shift_left_tests_000, shift_left_tests_001)), Pervasives.$at($$Array.to_list(Ext_array.map2i(function (i, a, b) {
                      return /* tuple */[
                              Caml_curry.app1(Format.asprintf(/* Format */{
                                        0: /* String_literal */{
                                          0: "shift_right_cases ",
                                          1: /* Int */{
                                            0: /* Int_d */0,
                                            1: /* No_padding */0,
                                            2: /* No_precision */0,
                                            3: /* End_of_format */0,
                                            length: 4,
                                            tag: 4
                                          },
                                          length: 2,
                                          tag: 11
                                        },
                                        1: "shift_right_cases %d",
                                        length: 2,
                                        tag: 0
                                      }), i),
                              function () {
                                return /* Eq */{
                                        0: a,
                                        1: b,
                                        length: 2,
                                        tag: 0
                                      };
                              }
                            ];
                    }, shift_right_tests_000, shift_right_tests_001)), $$Array.to_list(Ext_array.map2i(function (i, a, b) {
                      return /* tuple */[
                              Caml_curry.app1(Format.asprintf(/* Format */{
                                        0: /* String_literal */{
                                          0: "shift_right_logical_cases ",
                                          1: /* Int */{
                                            0: /* Int_d */0,
                                            1: /* No_padding */0,
                                            2: /* No_precision */0,
                                            3: /* End_of_format */0,
                                            length: 4,
                                            tag: 4
                                          },
                                          length: 2,
                                          tag: 11
                                        },
                                        1: "shift_right_logical_cases %d",
                                        length: 2,
                                        tag: 0
                                      }), i),
                              function () {
                                return /* Eq */{
                                        0: a,
                                        1: b,
                                        length: 2,
                                        tag: 0
                                      };
                              }
                            ];
                    }, shift_right_logical_suites_000, shift_right_logical_suites_001)))));

Mt.from_pair_suites("int64_test.ml", suites);

exports.f                          = f;
exports.v                          = v;
exports.h                          = h;
exports.a                          = a;
exports.commutative_add            = commutative_add;
exports.generic_compare            = generic_compare;
exports.shift_left_tests           = shift_left_tests;
exports.shift_right_tests          = shift_right_tests;
exports.shift_right_logical_suites = shift_right_logical_suites;
exports.fib                        = fib;
exports.fac                        = fac;
exports.suites                     = suites;
/* shift_left_tests Not a pure module */
