// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj       = require("../runtime/caml_obj");
var Pervasives     = require("../stdlib/pervasives");
var Mt             = require("./mt");
var Printf         = require("../stdlib/printf");
var Caml_primitive = require("../runtime/caml_primitive");
var $$Array        = require("../stdlib/array");
var Caml_curry     = require("../runtime/caml_curry");
var List           = require("../stdlib/list");

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

Mt.from_pair_suites("swap_test.ml", Pervasives.$at(suites_16, suites_32));

exports.tests_16  = tests_16;
exports.tests_32  = tests_32;
exports.suites_16 = suites_16;
exports.suites_32 = suites_32;
/* suites_16 Not a pure module */
