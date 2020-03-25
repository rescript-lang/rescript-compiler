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

var a = Caml_int64.mk(2147483647, 0);

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
        return Caml_int64.lsl_(Caml_int64.one, i);
      }), Ext_array_test.range(0, 63));

var shift_left_tests_001 = [
  Caml_int64.one,
  Caml_int64.mk(2, 0),
  Caml_int64.mk(4, 0),
  Caml_int64.mk(8, 0),
  Caml_int64.mk(16, 0),
  Caml_int64.mk(32, 0),
  Caml_int64.mk(64, 0),
  Caml_int64.mk(128, 0),
  Caml_int64.mk(256, 0),
  Caml_int64.mk(512, 0),
  Caml_int64.mk(1024, 0),
  Caml_int64.mk(2048, 0),
  Caml_int64.mk(4096, 0),
  Caml_int64.mk(8192, 0),
  Caml_int64.mk(16384, 0),
  Caml_int64.mk(32768, 0),
  Caml_int64.mk(65536, 0),
  Caml_int64.mk(131072, 0),
  Caml_int64.mk(262144, 0),
  Caml_int64.mk(524288, 0),
  Caml_int64.mk(1048576, 0),
  Caml_int64.mk(2097152, 0),
  Caml_int64.mk(4194304, 0),
  Caml_int64.mk(8388608, 0),
  Caml_int64.mk(16777216, 0),
  Caml_int64.mk(33554432, 0),
  Caml_int64.mk(67108864, 0),
  Caml_int64.mk(134217728, 0),
  Caml_int64.mk(268435456, 0),
  Caml_int64.mk(536870912, 0),
  Caml_int64.mk(1073741824, 0),
  Caml_int64.mk(-2147483648, 0),
  Caml_int64.mk(0, 1),
  Caml_int64.mk(0, 2),
  Caml_int64.mk(0, 4),
  Caml_int64.mk(0, 8),
  Caml_int64.mk(0, 16),
  Caml_int64.mk(0, 32),
  Caml_int64.mk(0, 64),
  Caml_int64.mk(0, 128),
  Caml_int64.mk(0, 256),
  Caml_int64.mk(0, 512),
  Caml_int64.mk(0, 1024),
  Caml_int64.mk(0, 2048),
  Caml_int64.mk(0, 4096),
  Caml_int64.mk(0, 8192),
  Caml_int64.mk(0, 16384),
  Caml_int64.mk(0, 32768),
  Caml_int64.mk(0, 65536),
  Caml_int64.mk(0, 131072),
  Caml_int64.mk(0, 262144),
  Caml_int64.mk(0, 524288),
  Caml_int64.mk(0, 1048576),
  Caml_int64.mk(0, 2097152),
  Caml_int64.mk(0, 4194304),
  Caml_int64.mk(0, 8388608),
  Caml_int64.mk(0, 16777216),
  Caml_int64.mk(0, 33554432),
  Caml_int64.mk(0, 67108864),
  Caml_int64.mk(0, 134217728),
  Caml_int64.mk(0, 268435456),
  Caml_int64.mk(0, 536870912),
  Caml_int64.mk(0, 1073741824),
  Caml_int64.min_int
];

var shift_left_tests = /* tuple */[
  shift_left_tests_000,
  shift_left_tests_001
];

var shift_right_tests_000 = $$Array.map((function (i) {
        return Caml_int64.asr_(Caml_int64.min_int, i);
      }), Ext_array_test.range(0, 63));

var shift_right_tests_001 = [
  Caml_int64.min_int,
  Caml_int64.mk(0, -1073741824),
  Caml_int64.mk(0, -536870912),
  Caml_int64.mk(0, -268435456),
  Caml_int64.mk(0, -134217728),
  Caml_int64.mk(0, -67108864),
  Caml_int64.mk(0, -33554432),
  Caml_int64.mk(0, -16777216),
  Caml_int64.mk(0, -8388608),
  Caml_int64.mk(0, -4194304),
  Caml_int64.mk(0, -2097152),
  Caml_int64.mk(0, -1048576),
  Caml_int64.mk(0, -524288),
  Caml_int64.mk(0, -262144),
  Caml_int64.mk(0, -131072),
  Caml_int64.mk(0, -65536),
  Caml_int64.mk(0, -32768),
  Caml_int64.mk(0, -16384),
  Caml_int64.mk(0, -8192),
  Caml_int64.mk(0, -4096),
  Caml_int64.mk(0, -2048),
  Caml_int64.mk(0, -1024),
  Caml_int64.mk(0, -512),
  Caml_int64.mk(0, -256),
  Caml_int64.mk(0, -128),
  Caml_int64.mk(0, -64),
  Caml_int64.mk(0, -32),
  Caml_int64.mk(0, -16),
  Caml_int64.mk(0, -8),
  Caml_int64.mk(0, -4),
  Caml_int64.mk(0, -2),
  Caml_int64.mk(0, -1),
  Caml_int64.mk(-2147483648, -1),
  Caml_int64.mk(-1073741824, -1),
  Caml_int64.mk(-536870912, -1),
  Caml_int64.mk(-268435456, -1),
  Caml_int64.mk(-134217728, -1),
  Caml_int64.mk(-67108864, -1),
  Caml_int64.mk(-33554432, -1),
  Caml_int64.mk(-16777216, -1),
  Caml_int64.mk(-8388608, -1),
  Caml_int64.mk(-4194304, -1),
  Caml_int64.mk(-2097152, -1),
  Caml_int64.mk(-1048576, -1),
  Caml_int64.mk(-524288, -1),
  Caml_int64.mk(-262144, -1),
  Caml_int64.mk(-131072, -1),
  Caml_int64.mk(-65536, -1),
  Caml_int64.mk(-32768, -1),
  Caml_int64.mk(-16384, -1),
  Caml_int64.mk(-8192, -1),
  Caml_int64.mk(-4096, -1),
  Caml_int64.mk(-2048, -1),
  Caml_int64.mk(-1024, -1),
  Caml_int64.mk(-512, -1),
  Caml_int64.mk(-256, -1),
  Caml_int64.mk(-128, -1),
  Caml_int64.mk(-64, -1),
  Caml_int64.mk(-32, -1),
  Caml_int64.mk(-16, -1),
  Caml_int64.mk(-8, -1),
  Caml_int64.mk(-4, -1),
  Caml_int64.mk(-2, -1),
  Caml_int64.neg_one
];

var shift_right_tests = /* tuple */[
  shift_right_tests_000,
  shift_right_tests_001
];

var shift_right_logical_suites_000 = $$Array.map((function (i) {
        return Caml_int64.lsr_(Caml_int64.min_int, i);
      }), Ext_array_test.range(0, 63));

var shift_right_logical_suites_001 = [
  Caml_int64.min_int,
  Caml_int64.mk(0, 1073741824),
  Caml_int64.mk(0, 536870912),
  Caml_int64.mk(0, 268435456),
  Caml_int64.mk(0, 134217728),
  Caml_int64.mk(0, 67108864),
  Caml_int64.mk(0, 33554432),
  Caml_int64.mk(0, 16777216),
  Caml_int64.mk(0, 8388608),
  Caml_int64.mk(0, 4194304),
  Caml_int64.mk(0, 2097152),
  Caml_int64.mk(0, 1048576),
  Caml_int64.mk(0, 524288),
  Caml_int64.mk(0, 262144),
  Caml_int64.mk(0, 131072),
  Caml_int64.mk(0, 65536),
  Caml_int64.mk(0, 32768),
  Caml_int64.mk(0, 16384),
  Caml_int64.mk(0, 8192),
  Caml_int64.mk(0, 4096),
  Caml_int64.mk(0, 2048),
  Caml_int64.mk(0, 1024),
  Caml_int64.mk(0, 512),
  Caml_int64.mk(0, 256),
  Caml_int64.mk(0, 128),
  Caml_int64.mk(0, 64),
  Caml_int64.mk(0, 32),
  Caml_int64.mk(0, 16),
  Caml_int64.mk(0, 8),
  Caml_int64.mk(0, 4),
  Caml_int64.mk(0, 2),
  Caml_int64.mk(0, 1),
  Caml_int64.mk(-2147483648, 0),
  Caml_int64.mk(1073741824, 0),
  Caml_int64.mk(536870912, 0),
  Caml_int64.mk(268435456, 0),
  Caml_int64.mk(134217728, 0),
  Caml_int64.mk(67108864, 0),
  Caml_int64.mk(33554432, 0),
  Caml_int64.mk(16777216, 0),
  Caml_int64.mk(8388608, 0),
  Caml_int64.mk(4194304, 0),
  Caml_int64.mk(2097152, 0),
  Caml_int64.mk(1048576, 0),
  Caml_int64.mk(524288, 0),
  Caml_int64.mk(262144, 0),
  Caml_int64.mk(131072, 0),
  Caml_int64.mk(65536, 0),
  Caml_int64.mk(32768, 0),
  Caml_int64.mk(16384, 0),
  Caml_int64.mk(8192, 0),
  Caml_int64.mk(4096, 0),
  Caml_int64.mk(2048, 0),
  Caml_int64.mk(1024, 0),
  Caml_int64.mk(512, 0),
  Caml_int64.mk(256, 0),
  Caml_int64.mk(128, 0),
  Caml_int64.mk(64, 0),
  Caml_int64.mk(32, 0),
  Caml_int64.mk(16, 0),
  Caml_int64.mk(8, 0),
  Caml_int64.mk(4, 0),
  Caml_int64.mk(2, 0),
  Caml_int64.one
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

var suites = Pervasives.$at(/* :: */[
      /* tuple */[
        "add_one",
        (function (param) {
            return /* Eq */Block.__(0, [
                      v,
                      Caml_int64.mk(-2147483648, 0)
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "add_2",
          (function (param) {
              return /* Eq */Block.__(0, [
                        Caml_int64.mk(-2, 0),
                        Caml_int64.add(a, a)
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "add_3",
            (function (param) {
                return /* Eq */Block.__(0, [
                          Caml_int64.zero,
                          Caml_int64.zero
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "add_4",
              (function (param) {
                  return commutative_add(Caml_int64.mk(-2, -1), Caml_int64.mk(-3, -1), Caml_int64.one);
                })
            ],
            /* :: */[
              /* tuple */[
                "add_5",
                (function (param) {
                    return commutative_add(Caml_int64.mk(-3, -1), Caml_int64.mk(-3, -1), Caml_int64.zero);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "add_6",
                  (function (param) {
                      return commutative_add(Caml_int64.mk(4, 0), Caml_int64.mk(-3, -1), Caml_int64.mk(7, 0));
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "add_7",
                    (function (param) {
                        return commutative_add(Caml_int64.mk(0, 1), Caml_int64.mk(-2147483648, 0), Caml_int64.mk(-2147483648, 0));
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "add_8",
                      (function (param) {
                          return commutative_add(Caml_int64.mk(0, 1), Caml_int64.mk(-1, 0), Caml_int64.one);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "add_9",
                        (function (param) {
                            return commutative_add(Caml_int64.mk(-1, 0), Caml_int64.mk(-2147483648, 0), Caml_int64.mk(2147483647, 0));
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "add_10",
                          (function (param) {
                              return commutative_add(Caml_int64.mk(-2147483648, 0), Caml_int64.mk(-2147483648, 0), Caml_int64.zero);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "add_11",
                            (function (param) {
                                return commutative_add(Caml_int64.mk(-1, 0), Caml_int64.mk(-1, 0), Caml_int64.zero);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "to_int32",
                              (function (param) {
                                  return /* Eq */Block.__(0, [
                                            3,
                                            Caml_int64.to_int32(Caml_int64.mk(3, 0))
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "to_int",
                                (function (param) {
                                    return /* Eq */Block.__(0, [
                                              3,
                                              Caml_int64.to_int32(Caml_int64.mk(3, 0))
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "of_int",
                                  (function (param) {
                                      return /* Eq */Block.__(0, [
                                                Caml_int64.mk(3, 0),
                                                Caml_int64.mk(3, 0)
                                              ]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "lognot",
                                    (function (param) {
                                        return /* Eq */Block.__(0, [
                                                  Caml_int64.mk(-3, -1),
                                                  Caml_int64.mk(-3, -1)
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "neg",
                                      (function (param) {
                                          return /* Eq */Block.__(0, [
                                                    Caml_int64.mk(-2, -1),
                                                    Caml_int64.mk(-2, -1)
                                                  ]);
                                        })
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "File \"int64_test.ml\", line 80, characters 4-11",
                                        (function (param) {
                                            return /* Eq */Block.__(0, [
                                                      Int64.min_int,
                                                      Caml_int64.neg(Int64.min_int)
                                                    ]);
                                          })
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "File \"int64_test.ml\", line 81, characters 4-11",
                                          (function (param) {
                                              return /* Eq */Block.__(0, [
                                                        Int64.max_int,
                                                        Caml_int64.neg(Caml_int64.add(Int64.min_int, Caml_int64.one))
                                                      ]);
                                            })
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "sub1",
                                            (function (param) {
                                                return /* Eq */Block.__(0, [
                                                          Caml_int64.mk(2, 0),
                                                          Caml_int64.mk(2, 0)
                                                        ]);
                                              })
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "xor1",
                                              (function (param) {
                                                  return /* Eq */Block.__(0, [
                                                            /* tuple */[
                                                              Caml_int64.mk(286331153, 0),
                                                              Caml_int64.xor(a, Caml_int64.mk(-285217025, 0))
                                                            ],
                                                            /* tuple */[
                                                              Caml_int64.mk(286331153, 0),
                                                              Caml_int64.mk(-1862266624, 0)
                                                            ]
                                                          ]);
                                                })
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "or",
                                                (function (param) {
                                                    return /* Eq */Block.__(0, [
                                                              Caml_int64.mk(-1, 0),
                                                              Caml_int64.mk(-1, 0)
                                                            ]);
                                                  })
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "and",
                                                  (function (param) {
                                                      return /* Eq */Block.__(0, [
                                                                Caml_int64.mk(-286331154, 0),
                                                                Caml_int64.mk(-286331154, 0)
                                                              ]);
                                                    })
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "lsl",
                                                    (function (param) {
                                                        return /* Eq */Block.__(0, [
                                                                  $$Array.map((function (x) {
                                                                          return Caml_int64.lsl_(Caml_int64.one, x);
                                                                        }), $$Array.init(64, (function (i) {
                                                                              return i;
                                                                            }))),
                                                                  [
                                                                    Caml_int64.one,
                                                                    Caml_int64.mk(2, 0),
                                                                    Caml_int64.mk(4, 0),
                                                                    Caml_int64.mk(8, 0),
                                                                    Caml_int64.mk(16, 0),
                                                                    Caml_int64.mk(32, 0),
                                                                    Caml_int64.mk(64, 0),
                                                                    Caml_int64.mk(128, 0),
                                                                    Caml_int64.mk(256, 0),
                                                                    Caml_int64.mk(512, 0),
                                                                    Caml_int64.mk(1024, 0),
                                                                    Caml_int64.mk(2048, 0),
                                                                    Caml_int64.mk(4096, 0),
                                                                    Caml_int64.mk(8192, 0),
                                                                    Caml_int64.mk(16384, 0),
                                                                    Caml_int64.mk(32768, 0),
                                                                    Caml_int64.mk(65536, 0),
                                                                    Caml_int64.mk(131072, 0),
                                                                    Caml_int64.mk(262144, 0),
                                                                    Caml_int64.mk(524288, 0),
                                                                    Caml_int64.mk(1048576, 0),
                                                                    Caml_int64.mk(2097152, 0),
                                                                    Caml_int64.mk(4194304, 0),
                                                                    Caml_int64.mk(8388608, 0),
                                                                    Caml_int64.mk(16777216, 0),
                                                                    Caml_int64.mk(33554432, 0),
                                                                    Caml_int64.mk(67108864, 0),
                                                                    Caml_int64.mk(134217728, 0),
                                                                    Caml_int64.mk(268435456, 0),
                                                                    Caml_int64.mk(536870912, 0),
                                                                    Caml_int64.mk(1073741824, 0),
                                                                    Caml_int64.mk(-2147483648, 0),
                                                                    Caml_int64.mk(0, 1),
                                                                    Caml_int64.mk(0, 2),
                                                                    Caml_int64.mk(0, 4),
                                                                    Caml_int64.mk(0, 8),
                                                                    Caml_int64.mk(0, 16),
                                                                    Caml_int64.mk(0, 32),
                                                                    Caml_int64.mk(0, 64),
                                                                    Caml_int64.mk(0, 128),
                                                                    Caml_int64.mk(0, 256),
                                                                    Caml_int64.mk(0, 512),
                                                                    Caml_int64.mk(0, 1024),
                                                                    Caml_int64.mk(0, 2048),
                                                                    Caml_int64.mk(0, 4096),
                                                                    Caml_int64.mk(0, 8192),
                                                                    Caml_int64.mk(0, 16384),
                                                                    Caml_int64.mk(0, 32768),
                                                                    Caml_int64.mk(0, 65536),
                                                                    Caml_int64.mk(0, 131072),
                                                                    Caml_int64.mk(0, 262144),
                                                                    Caml_int64.mk(0, 524288),
                                                                    Caml_int64.mk(0, 1048576),
                                                                    Caml_int64.mk(0, 2097152),
                                                                    Caml_int64.mk(0, 4194304),
                                                                    Caml_int64.mk(0, 8388608),
                                                                    Caml_int64.mk(0, 16777216),
                                                                    Caml_int64.mk(0, 33554432),
                                                                    Caml_int64.mk(0, 67108864),
                                                                    Caml_int64.mk(0, 134217728),
                                                                    Caml_int64.mk(0, 268435456),
                                                                    Caml_int64.mk(0, 536870912),
                                                                    Caml_int64.mk(0, 1073741824),
                                                                    Caml_int64.min_int
                                                                  ]
                                                                ]);
                                                      })
                                                  ],
                                                  /* :: */[
                                                    /* tuple */[
                                                      "lsr",
                                                      (function (param) {
                                                          return /* Eq */Block.__(0, [
                                                                    $$Array.map((function (x) {
                                                                            return Caml_int64.lsr_(Caml_int64.neg_one, x);
                                                                          }), $$Array.init(64, (function (i) {
                                                                                return i;
                                                                              }))),
                                                                    [
                                                                      Caml_int64.neg_one,
                                                                      Caml_int64.max_int,
                                                                      Caml_int64.mk(-1, 1073741823),
                                                                      Caml_int64.mk(-1, 536870911),
                                                                      Caml_int64.mk(-1, 268435455),
                                                                      Caml_int64.mk(-1, 134217727),
                                                                      Caml_int64.mk(-1, 67108863),
                                                                      Caml_int64.mk(-1, 33554431),
                                                                      Caml_int64.mk(-1, 16777215),
                                                                      Caml_int64.mk(-1, 8388607),
                                                                      Caml_int64.mk(-1, 4194303),
                                                                      Caml_int64.mk(-1, 2097151),
                                                                      Caml_int64.mk(-1, 1048575),
                                                                      Caml_int64.mk(-1, 524287),
                                                                      Caml_int64.mk(-1, 262143),
                                                                      Caml_int64.mk(-1, 131071),
                                                                      Caml_int64.mk(-1, 65535),
                                                                      Caml_int64.mk(-1, 32767),
                                                                      Caml_int64.mk(-1, 16383),
                                                                      Caml_int64.mk(-1, 8191),
                                                                      Caml_int64.mk(-1, 4095),
                                                                      Caml_int64.mk(-1, 2047),
                                                                      Caml_int64.mk(-1, 1023),
                                                                      Caml_int64.mk(-1, 511),
                                                                      Caml_int64.mk(-1, 255),
                                                                      Caml_int64.mk(-1, 127),
                                                                      Caml_int64.mk(-1, 63),
                                                                      Caml_int64.mk(-1, 31),
                                                                      Caml_int64.mk(-1, 15),
                                                                      Caml_int64.mk(-1, 7),
                                                                      Caml_int64.mk(-1, 3),
                                                                      Caml_int64.mk(-1, 1),
                                                                      Caml_int64.mk(-1, 0),
                                                                      Caml_int64.mk(2147483647, 0),
                                                                      Caml_int64.mk(1073741823, 0),
                                                                      Caml_int64.mk(536870911, 0),
                                                                      Caml_int64.mk(268435455, 0),
                                                                      Caml_int64.mk(134217727, 0),
                                                                      Caml_int64.mk(67108863, 0),
                                                                      Caml_int64.mk(33554431, 0),
                                                                      Caml_int64.mk(16777215, 0),
                                                                      Caml_int64.mk(8388607, 0),
                                                                      Caml_int64.mk(4194303, 0),
                                                                      Caml_int64.mk(2097151, 0),
                                                                      Caml_int64.mk(1048575, 0),
                                                                      Caml_int64.mk(524287, 0),
                                                                      Caml_int64.mk(262143, 0),
                                                                      Caml_int64.mk(131071, 0),
                                                                      Caml_int64.mk(65535, 0),
                                                                      Caml_int64.mk(32767, 0),
                                                                      Caml_int64.mk(16383, 0),
                                                                      Caml_int64.mk(8191, 0),
                                                                      Caml_int64.mk(4095, 0),
                                                                      Caml_int64.mk(2047, 0),
                                                                      Caml_int64.mk(1023, 0),
                                                                      Caml_int64.mk(511, 0),
                                                                      Caml_int64.mk(255, 0),
                                                                      Caml_int64.mk(127, 0),
                                                                      Caml_int64.mk(63, 0),
                                                                      Caml_int64.mk(31, 0),
                                                                      Caml_int64.mk(15, 0),
                                                                      Caml_int64.mk(7, 0),
                                                                      Caml_int64.mk(3, 0),
                                                                      Caml_int64.one
                                                                    ]
                                                                  ]);
                                                        })
                                                    ],
                                                    /* :: */[
                                                      /* tuple */[
                                                        "asr",
                                                        (function (param) {
                                                            return /* Eq */Block.__(0, [
                                                                      $$Array.map((function (x) {
                                                                              return Caml_int64.asr_(Caml_int64.neg_one, x);
                                                                            }), $$Array.init(64, (function (i) {
                                                                                  return i;
                                                                                }))),
                                                                      [
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
                                                                    ]);
                                                          })
                                                      ],
                                                      /* :: */[
                                                        /* tuple */[
                                                          "mul simple",
                                                          (function (param) {
                                                              return /* Eq */Block.__(0, [
                                                                        Caml_int64.mk(6, 0),
                                                                        Caml_int64.mk(6, 0)
                                                                      ]);
                                                            })
                                                        ],
                                                        /* :: */[
                                                          /* tuple */[
                                                            "of_int32",
                                                            (function (param) {
                                                                return /* Eq */Block.__(0, [
                                                                          $$Array.map(Caml_int64.of_int32, [
                                                                                0,
                                                                                -2147483648
                                                                              ]),
                                                                          [
                                                                            Caml_int64.zero,
                                                                            Caml_int64.mk(-2147483648, -1)
                                                                          ]
                                                                        ]);
                                                              })
                                                          ],
                                                          /* :: */[
                                                            /* tuple */[
                                                              "of_int32_singleton",
                                                              (function (param) {
                                                                  return /* Eq */Block.__(0, [
                                                                            Caml_int64.mk(-3, -1),
                                                                            Caml_int64.mk(-3, -1)
                                                                          ]);
                                                                })
                                                            ],
                                                            /* :: */[
                                                              /* tuple */[
                                                                "File \"int64_test.ml\", line 134, characters 4-11",
                                                                (function (param) {
                                                                    return /* Eq */Block.__(0, [
                                                                              Caml_int64.mk(3, 0),
                                                                              Caml_int64.mk(3, 0)
                                                                            ]);
                                                                  })
                                                              ],
                                                              /* :: */[
                                                                /* tuple */[
                                                                  "to_int32",
                                                                  (function (param) {
                                                                      return /* Eq */Block.__(0, [
                                                                                $$Array.map(Caml_int64.to_int32, [
                                                                                      Caml_int64.zero,
                                                                                      Caml_int64.mk(-2147483648, 0)
                                                                                    ]),
                                                                                [
                                                                                  0,
                                                                                  -2147483648
                                                                                ]
                                                                              ]);
                                                                    })
                                                                ],
                                                                /* :: */[
                                                                  /* tuple */[
                                                                    "discard_sign",
                                                                    (function (param) {
                                                                        return /* Eq */Block.__(0, [
                                                                                  Caml_int64.discard_sign(Caml_int64.neg_one),
                                                                                  Caml_int64.max_int
                                                                                ]);
                                                                      })
                                                                  ],
                                                                  /* :: */[
                                                                    /* tuple */[
                                                                      "div_mod",
                                                                      (function (param) {
                                                                          return /* Eq */Block.__(0, [
                                                                                    Caml_int64.div_mod(Caml_int64.mk(7, 0), Caml_int64.mk(3, 0)),
                                                                                    /* tuple */[
                                                                                      Caml_int64.mk(2, 0),
                                                                                      Caml_int64.one
                                                                                    ]
                                                                                  ]);
                                                                        })
                                                                    ],
                                                                    /* :: */[
                                                                      /* tuple */[
                                                                        "to_hex",
                                                                        (function (param) {
                                                                            return /* Eq */Block.__(0, [
                                                                                      Caml_int64.to_hex(Caml_int64.neg_one),
                                                                                      "ffffffffffffffff"
                                                                                    ]);
                                                                          })
                                                                      ],
                                                                      /* :: */[
                                                                        /* tuple */[
                                                                          "generic_compare",
                                                                          (function (param) {
                                                                              return /* Eq */Block.__(0, [
                                                                                        Caml_obj.caml_compare(Caml_int64.mk(0, 1), Caml_int64.one) > 0,
                                                                                        true
                                                                                      ]);
                                                                            })
                                                                        ],
                                                                        /* :: */[
                                                                          /* tuple */[
                                                                            "test_compier_literal",
                                                                            (function (param) {
                                                                                return /* Eq */Block.__(0, [
                                                                                          Caml_int64.mk(-1, 0),
                                                                                          Caml_int64.mk(-1, 0)
                                                                                        ]);
                                                                              })
                                                                          ],
                                                                          /* :: */[
                                                                            /* tuple */[
                                                                              "generic_compare2",
                                                                              (function (param) {
                                                                                  return /* Eq */Block.__(0, [
                                                                                            Caml_obj.caml_compare(Caml_int64.mk(-2147483648, 0), Caml_int64.one) > 0,
                                                                                            true
                                                                                          ]);
                                                                                })
                                                                            ],
                                                                            /* :: */[
                                                                              /* tuple */[
                                                                                "shift_left",
                                                                                (function (param) {
                                                                                    return /* Eq */Block.__(0, [
                                                                                              Caml_int64.mk(-256, 0),
                                                                                              Caml_int64.mk(-256, 0)
                                                                                            ]);
                                                                                  })
                                                                              ],
                                                                              /* :: */[
                                                                                /* tuple */[
                                                                                  "shift_right",
                                                                                  (function (param) {
                                                                                      return /* Eq */Block.__(0, [
                                                                                                4294967295,
                                                                                                4294967295
                                                                                              ]);
                                                                                    })
                                                                                ],
                                                                                /* :: */[
                                                                                  /* tuple */[
                                                                                    "fib_int64",
                                                                                    (function (param) {
                                                                                        return /* Eq */Block.__(0, [
                                                                                                  fib(1000, Caml_int64.one, Caml_int64.mk(2, 0)),
                                                                                                  Caml_int64.mk(-1420443336, -1990564327)
                                                                                                ]);
                                                                                      })
                                                                                  ],
                                                                                  /* :: */[
                                                                                    /* tuple */[
                                                                                      "fac_int64",
                                                                                      (function (param) {
                                                                                          return /* Eq */Block.__(0, [
                                                                                                    fac(30, Caml_int64.one),
                                                                                                    Caml_int64.mk(1409286144, -2040662563)
                                                                                                  ]);
                                                                                        })
                                                                                    ],
                                                                                    /* :: */[
                                                                                      /* tuple */[
                                                                                        "File \"int64_test.ml\", line 163, characters 6-13",
                                                                                        (function (param) {
                                                                                            return /* Eq */Block.__(0, [
                                                                                                      Caml_int64.add(Int64.max_int, Int64.max_int),
                                                                                                      Caml_int64.mk(-2, -1)
                                                                                                    ]);
                                                                                          })
                                                                                      ],
                                                                                      /* :: */[
                                                                                        /* tuple */[
                                                                                          "File \"int64_test.ml\", line 166, characters 6-13",
                                                                                          (function (param) {
                                                                                              return /* Eq */Block.__(0, [
                                                                                                        Caml_int64.add(Int64.min_int, Int64.min_int),
                                                                                                        Caml_int64.zero
                                                                                                      ]);
                                                                                            })
                                                                                        ],
                                                                                        /* :: */[
                                                                                          /* tuple */[
                                                                                            "File \"int64_test.ml\", line 170, characters 6-13",
                                                                                            (function (param) {
                                                                                                return /* Eq */Block.__(0, [
                                                                                                          Caml_int64.neg_one,
                                                                                                          Caml_int64.neg_one
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
                            (function (param) {
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
                                (function (param) {
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
                                (function (param) {
                                    return /* Eq */Block.__(0, [
                                              a,
                                              b
                                            ]);
                                  })
                              ];
                      }), shift_right_logical_suites_000, shift_right_logical_suites_001)))));

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

eq("File \"int64_test.ml\", line 202, characters 5-12", Caml_int64.bits_of_float(0.3), Caml_int64.mk(858993459, 1070805811));

eq("File \"int64_test.ml\", line 203, characters 5-12", Caml_int64.float_of_bits(Caml_int64.mk(858993459, 1070805811)), 0.3);

id("File \"int64_test.ml\", line 204, characters 5-12", Caml_int64.neg_one);

id("File \"int64_test.ml\", line 205, characters 5-12", Caml_int64.mk(-100, -1));

id("File \"int64_test.ml\", line 206, characters 5-12", Caml_int64.mk(-1, 0));

id("File \"int64_test.ml\", line 207, characters 5-12", Caml_int64.mk(536870911, 0));

id("File \"int64_test.ml\", line 208, characters 5-12", Caml_int64.mk(536870655, 0));

eq("File \"int64_test.ml\", line 209, characters 5-12", Caml_int64.div(Int64.min_int, Caml_int64.mk(10, 0)), Caml_int64.mk(858993460, -214748365));

eq("File \"int64_test.ml\", line 210, characters 5-12", Caml_int64.to_string(Caml_int64.div(Int64.min_int, Caml_int64.mk(10, 0))), "-922337203685477580");

eq("File \"int64_test.ml\", line 211, characters 5-12", Caml_int64.mul(Int64.min_int, Caml_int64.mk(10, 0)), Caml_int64.zero);

eq("File \"int64_test.ml\", line 212, characters 5-12", Caml_int64.mul(Caml_int64.mk(10, 0), Int64.min_int), Caml_int64.zero);

eq("File \"int64_test.ml\", line 213, characters 5-12", Caml_int64.mul(Caml_int64.one, Int64.min_int), Int64.min_int);

eq("File \"int64_test.ml\", line 214, characters 5-12", Caml_int64.mul(Int64.max_int, Caml_int64.mk(10, 0)), Caml_int64.mk(-10, -1));

eq("File \"int64_test.ml\", line 215, characters 5-12", Caml_int64.succ(Int64.max_int), Int64.min_int);

eq("File \"int64_test.ml\", line 216, characters 5-12", Caml_int64.succ(Int64.min_int), Caml_int64.mk(1, -2147483648));

eq("File \"int64_test.ml\", line 217, characters 5-12", Caml_int64.succ(Caml_int64.mk(-1, 0)), Caml_int64.mk(0, 1));

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
