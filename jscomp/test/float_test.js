// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_float = require("../runtime/caml_float");
var Pervasives = require("../stdlib/pervasives");
var Mt         = require("./mt");
var Printf     = require("../stdlib/printf");
var $$Array    = require("../stdlib/array");
var Caml_curry = require("../runtime/caml_curry");

var match = Caml_float.caml_frexp_float(12.0);

var results = $$Array.append(/* array */[
      /* tuple */[
        Math.log10(2),
        0.301029995663981198
      ],
      /* tuple */[
        Caml_float.caml_ldexp_float(1, 6),
        64
      ],
      /* tuple */[
        Caml_float.caml_ldexp_float(1, 5),
        32
      ],
      /* tuple */[
        Caml_float.caml_ldexp_float(1.e-5, 1024),
        1.79769313486231605e+303
      ],
      /* tuple */[
        Caml_float.caml_ldexp_float(1, -1024),
        5.56268464626800346e-309
      ],
      /* tuple */[
        Caml_float.caml_hypot_float(3, 4),
        5
      ],
      /* tuple */[
        Caml_float.caml_hypot_float(4, 3),
        5
      ],
      /* tuple */[
        Caml_float.caml_hypot_float(5, 12),
        13
      ],
      /* tuple */[
        Caml_float.caml_hypot_float(12, 5),
        13
      ]
    ], /* array */[
      /* tuple */[
        match[0],
        0.75
      ],
      /* tuple */[
        match[1],
        4
      ]
    ]);

function from_pairs(ps) {
  return $$Array.to_list($$Array.mapi(function (i, param) {
                  var b = param[1];
                  var a = param[0];
                  return /* tuple */[
                          Caml_curry.app1(Printf.sprintf(/* Format */{
                                    0: /* String_literal */{
                                      0: "pair ",
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
                                    1: "pair %d",
                                    length: 2,
                                    tag: 0
                                  }), i),
                          function () {
                            return /* Approx */{
                                    0: a,
                                    1: b,
                                    length: 2,
                                    tag: 2
                                  };
                          }
                        ];
                }, ps));
}

Mt.from_pair_suites("float_test.ml", Pervasives.$at(/* :: */[
          /* tuple */[
            "mod_float",
            function () {
              return /* Approx */{
                      0: 3.2 % 0.5,
                      1: 0.200000000000000178,
                      length: 2,
                      tag: 2
                    };
            }
          ],
          /* [] */0
        ], from_pairs(results)));

var epsilon_float = 2.22044604925031308e-16;

exports.epsilon_float = epsilon_float;
exports.results       = results;
exports.from_pairs    = from_pairs;
/* results Not a pure module */
