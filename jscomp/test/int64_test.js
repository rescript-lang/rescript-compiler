// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_int64 = require("../runtime/caml_int64");
var Mt         = require("./mt");
var Int32      = require("../stdlib/int32");
var Int64      = require("../stdlib/int64");

function f(u, v) {
  return +(u > v);
}

var v = Caml_int64.add(Caml_int64.of_int32(Int32.max_int), Int64.one);

var h = Caml_int64.neg(v);

var a = /* int64 */[
  2147483647,
  0
];

var suites_000 = /* tuple */[
  "add_one",
  function () {
    return /* Eq */{
            0: v,
            1: /* int64 */[
              -2147483648,
              0
            ],
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "add_2",
    function () {
      return /* Eq */{
              0: /* int64 */[
                -2,
                0
              ],
              1: Caml_int64.add(a, a),
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
                      3,
                      0
                    ],
                    1: /* int64 */[
                      3,
                      0
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
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("int64_test.ml", suites);

exports.f      = f;
exports.v      = v;
exports.h      = h;
exports.a      = a;
exports.suites = suites;
/*  Not a pure module */
