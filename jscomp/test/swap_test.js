// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_int64     = require("../runtime/caml_int64");
var Caml_obj       = require("../runtime/caml_obj");
var Pervasives     = require("../stdlib/pervasives");
var Mt             = require("./mt");
var Printf         = require("../stdlib/printf");
var Caml_primitive = require("../runtime/caml_primitive");
var $$Array        = require("../stdlib/array");
var Caml_curry     = require("../runtime/caml_curry");
var List           = require("../stdlib/list");
var Format         = require("../stdlib/format");

var tests_16 = Caml_obj.caml_obj_dup(/* array */[
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
    ]);

var tests_32 = Caml_obj.caml_obj_dup(/* array */[
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
    ]);

var tests_64 = Caml_obj.caml_obj_dup(/* array */[
      /* tuple */[
        /* int64 */[
          0,
          1
        ],
        /* int64 */[
          16777216,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          2
        ],
        /* int64 */[
          33554432,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          4
        ],
        /* int64 */[
          67108864,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          8
        ],
        /* int64 */[
          134217728,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          16
        ],
        /* int64 */[
          268435456,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          32
        ],
        /* int64 */[
          536870912,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          64
        ],
        /* int64 */[
          1073741824,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          128
        ],
        /* int64 */[
          -2147483648,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          256
        ],
        /* int64 */[
          65536,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          512
        ],
        /* int64 */[
          131072,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          1024
        ],
        /* int64 */[
          262144,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          2048
        ],
        /* int64 */[
          524288,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          4096
        ],
        /* int64 */[
          1048576,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          8192
        ],
        /* int64 */[
          2097152,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          16384
        ],
        /* int64 */[
          4194304,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          32768
        ],
        /* int64 */[
          8388608,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          65536
        ],
        /* int64 */[
          256,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          131072
        ],
        /* int64 */[
          512,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          262144
        ],
        /* int64 */[
          1024,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          524288
        ],
        /* int64 */[
          2048,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          1048576
        ],
        /* int64 */[
          4096,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          2097152
        ],
        /* int64 */[
          8192,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          4194304
        ],
        /* int64 */[
          16384,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          8388608
        ],
        /* int64 */[
          32768,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          16777216
        ],
        /* int64 */[
          1,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          33554432
        ],
        /* int64 */[
          2,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          67108864
        ],
        /* int64 */[
          4,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          134217728
        ],
        /* int64 */[
          8,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          268435456
        ],
        /* int64 */[
          16,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          536870912
        ],
        /* int64 */[
          32,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          1073741824
        ],
        /* int64 */[
          64,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          0,
          2147483648
        ],
        /* int64 */[
          128,
          0
        ]
      ],
      /* tuple */[
        /* int64 */[
          1,
          0
        ],
        /* int64 */[
          0,
          16777216
        ]
      ],
      /* tuple */[
        /* int64 */[
          2,
          0
        ],
        /* int64 */[
          0,
          33554432
        ]
      ],
      /* tuple */[
        /* int64 */[
          4,
          0
        ],
        /* int64 */[
          0,
          67108864
        ]
      ],
      /* tuple */[
        /* int64 */[
          8,
          0
        ],
        /* int64 */[
          0,
          134217728
        ]
      ],
      /* tuple */[
        /* int64 */[
          16,
          0
        ],
        /* int64 */[
          0,
          268435456
        ]
      ],
      /* tuple */[
        /* int64 */[
          32,
          0
        ],
        /* int64 */[
          0,
          536870912
        ]
      ],
      /* tuple */[
        /* int64 */[
          64,
          0
        ],
        /* int64 */[
          0,
          1073741824
        ]
      ],
      /* tuple */[
        /* int64 */[
          128,
          0
        ],
        /* int64 */[
          0,
          2147483648
        ]
      ],
      /* tuple */[
        /* int64 */[
          256,
          0
        ],
        /* int64 */[
          0,
          65536
        ]
      ],
      /* tuple */[
        /* int64 */[
          512,
          0
        ],
        /* int64 */[
          0,
          131072
        ]
      ],
      /* tuple */[
        /* int64 */[
          1024,
          0
        ],
        /* int64 */[
          0,
          262144
        ]
      ],
      /* tuple */[
        /* int64 */[
          2048,
          0
        ],
        /* int64 */[
          0,
          524288
        ]
      ],
      /* tuple */[
        /* int64 */[
          4096,
          0
        ],
        /* int64 */[
          0,
          1048576
        ]
      ],
      /* tuple */[
        /* int64 */[
          8192,
          0
        ],
        /* int64 */[
          0,
          2097152
        ]
      ],
      /* tuple */[
        /* int64 */[
          16384,
          0
        ],
        /* int64 */[
          0,
          4194304
        ]
      ],
      /* tuple */[
        /* int64 */[
          32768,
          0
        ],
        /* int64 */[
          0,
          8388608
        ]
      ],
      /* tuple */[
        /* int64 */[
          65536,
          0
        ],
        /* int64 */[
          0,
          256
        ]
      ],
      /* tuple */[
        /* int64 */[
          131072,
          0
        ],
        /* int64 */[
          0,
          512
        ]
      ],
      /* tuple */[
        /* int64 */[
          262144,
          0
        ],
        /* int64 */[
          0,
          1024
        ]
      ],
      /* tuple */[
        /* int64 */[
          524288,
          0
        ],
        /* int64 */[
          0,
          2048
        ]
      ],
      /* tuple */[
        /* int64 */[
          1048576,
          0
        ],
        /* int64 */[
          0,
          4096
        ]
      ],
      /* tuple */[
        /* int64 */[
          2097152,
          0
        ],
        /* int64 */[
          0,
          8192
        ]
      ],
      /* tuple */[
        /* int64 */[
          4194304,
          0
        ],
        /* int64 */[
          0,
          16384
        ]
      ],
      /* tuple */[
        /* int64 */[
          8388608,
          0
        ],
        /* int64 */[
          0,
          32768
        ]
      ],
      /* tuple */[
        /* int64 */[
          16777216,
          0
        ],
        /* int64 */[
          0,
          1
        ]
      ],
      /* tuple */[
        /* int64 */[
          33554432,
          0
        ],
        /* int64 */[
          0,
          2
        ]
      ],
      /* tuple */[
        /* int64 */[
          67108864,
          0
        ],
        /* int64 */[
          0,
          4
        ]
      ],
      /* tuple */[
        /* int64 */[
          134217728,
          0
        ],
        /* int64 */[
          0,
          8
        ]
      ],
      /* tuple */[
        /* int64 */[
          268435456,
          0
        ],
        /* int64 */[
          0,
          16
        ]
      ],
      /* tuple */[
        /* int64 */[
          536870912,
          0
        ],
        /* int64 */[
          0,
          32
        ]
      ],
      /* tuple */[
        /* int64 */[
          1073741824,
          0
        ],
        /* int64 */[
          0,
          64
        ]
      ],
      /* tuple */[
        /* int64 */[
          -2147483648,
          0
        ],
        /* int64 */[
          0,
          128
        ]
      ]
    ]);

var suites_16 = List.map(function (param) {
      var b = param[1];
      var a = param[0];
      return /* tuple */[
              Caml_curry.app1(Printf.sprintf(/* Format */{
                        0: /* String_literal */{
                          0: "swap16 ",
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
                        1: "swap16 %d",
                        length: 2,
                        tag: 0
                      }), a),
              function () {
                return /* Eq */{
                        0: Caml_primitive.caml_bswap16(a),
                        1: b,
                        length: 2,
                        tag: 0
                      };
              }
            ];
    }, $$Array.to_list(tests_16));

var suites_32 = List.map(function (param) {
      var b = param[1];
      var a = param[0];
      return /* tuple */[
              Caml_curry.app1(Printf.sprintf(/* Format */{
                        0: /* String_literal */{
                          0: "swap32 ",
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
                        1: "swap32 %d",
                        length: 2,
                        tag: 0
                      }), a),
              function () {
                return /* Eq */{
                        0: Caml_primitive.caml_int32_bswap(a),
                        1: b,
                        length: 2,
                        tag: 0
                      };
              }
            ];
    }, $$Array.to_list(tests_32));

var suites_64 = List.map(function (param) {
      var b = param[1];
      var a = param[0];
      return /* tuple */[
              Caml_curry.app1(Printf.sprintf(/* Format */{
                        0: /* String_literal */{
                          0: "swap64 ",
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
                        1: "swap64 %d",
                        length: 2,
                        tag: 0
                      }), a[1] | 0),
              function () {
                return /* Eq */{
                        0: Caml_int64.swap(a),
                        1: b,
                        length: 2,
                        tag: 0
                      };
              }
            ];
    }, $$Array.to_list(tests_64));

var d16_000 = /* Format */{
  0: /* Int */{
    0: /* Int_x */6,
    1: /* No_padding */0,
    2: /* No_precision */0,
    3: /* End_of_format */0,
    length: 4,
    tag: 4
  },
  1: "%x",
  length: 2,
  tag: 0
};

function d16_001(prim) {
  return Caml_primitive.caml_bswap16(prim);
}

var d16_002 = /* array */[
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

var d32_000 = /* Format */{
  0: /* Int32 */{
    0: /* Int_x */6,
    1: /* No_padding */0,
    2: /* No_precision */0,
    3: /* End_of_format */0,
    length: 4,
    tag: 5
  },
  1: "%lx",
  length: 2,
  tag: 0
};

function d32_001(prim) {
  return Caml_primitive.caml_int32_bswap(prim);
}

var d32_002 = /* array */[
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
  return $$Array.to_list($$Array.mapi(function (i, param) {
                  var b = param[1];
                  var a = param[0];
                  return /* tuple */[
                          Caml_curry.app2(Format.asprintf(/* Format */{
                                    0: /* String */{
                                      0: /* No_padding */0,
                                      1: /* Char_literal */{
                                        0: /* " " */32,
                                        1: /* Int */{
                                          0: /* Int_i */3,
                                          1: /* No_padding */0,
                                          2: /* No_precision */0,
                                          3: /* End_of_format */0,
                                          length: 4,
                                          tag: 4
                                        },
                                        length: 2,
                                        tag: 12
                                      },
                                      length: 2,
                                      tag: 2
                                    },
                                    1: "%s %i",
                                    length: 2,
                                    tag: 0
                                  }), s, i),
                          function () {
                            return /* Eq */{
                                    0: Caml_curry.app1(Format.asprintf(x), Caml_curry.app1(swap, a)),
                                    1: b,
                                    length: 2,
                                    tag: 0
                                  };
                          }
                        ];
                }, param[2]));
}

Mt.from_pair_suites("swap_test.ml", Pervasives.$at(suites_16, Pervasives.$at(suites_32, Pervasives.$at(suites_64, Pervasives.$at(f("d16", d16), f("d32", d32))))));

exports.tests_16  = tests_16;
exports.tests_32  = tests_32;
exports.tests_64  = tests_64;
exports.suites_16 = suites_16;
exports.suites_32 = suites_32;
exports.suites_64 = suites_64;
exports.d16       = d16;
exports.d32       = d32;
exports.f         = f;
/* suites_16 Not a pure module */
