// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt    = require("./mt");
var Int32 = require("../stdlib/int32");

function hash_variant(s) {
  var accu = 0;
  for(var i = 0 ,i_finish = s.length - 1; i<= i_finish; ++i){
    accu = 223 * accu + s.charCodeAt(i) & (1 << 31) - 1;
  }
  if (accu > 1073741823) {
    return accu - (1 << 31) | 0;
  }
  else {
    return accu;
  }
}

function fib(n) {
  if (n !== 0 && n !== 1) {
    return fib(n - 1 | 0) + fib(n - 2 | 0) | 0;
  }
  else {
    return 1;
  }
}

Mt.from_pair_suites("int_overflow_test.ml", /* :: */[
      /* tuple */[
        "plus_overflow",
        function () {
          return /* Eq */{
                  0: /* true */1,
                  1: +((Int32.max_int + 1 | 0) === Int32.min_int),
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* :: */[
        /* tuple */[
          "minus_overflow",
          function () {
            return /* Eq */{
                    0: /* true */1,
                    1: +((Int32.min_int - Int32.one | 0) === Int32.max_int),
                    length: 2,
                    tag: 0
                  };
          }
        ],
        /* :: */[
          /* tuple */[
            "flow_again",
            function () {
              return /* Eq */{
                      0: 2147483646,
                      1: (Int32.max_int + Int32.max_int | 0) + Int32.min_int | 0,
                      length: 2,
                      tag: 0
                    };
            }
          ],
          /* :: */[
            /* tuple */[
              "flow_again",
              function () {
                return /* Eq */{
                        0: -2,
                        1: Int32.max_int + Int32.max_int | 0,
                        length: 2,
                        tag: 0
                      };
              }
            ],
            /* :: */[
              /* tuple */[
                "hash_test",
                function () {
                  return /* Eq */{
                          0: hash_variant("xxyyzzuuxxzzyy00112233"),
                          1: 544087776,
                          length: 2,
                          tag: 0
                        };
                }
              ],
              /* :: */[
                /* tuple */[
                  "hash_test2",
                  function () {
                    return /* Eq */{
                            0: hash_variant("xxyyzxzzyy"),
                            1: -449896130,
                            length: 2,
                            tag: 0
                          };
                  }
                ],
                /* :: */[
                  /* tuple */[
                    "int_literal_flow",
                    function () {
                      return /* Eq */{
                              0: -1,
                              1: -1,
                              length: 2,
                              tag: 0
                            };
                    }
                  ],
                  /* :: */[
                    /* tuple */[
                      "int_literal_flow2",
                      function () {
                        return /* Eq */{
                                0: -1,
                                1: -1,
                                length: 2,
                                tag: 0
                              };
                      }
                    ],
                    /* :: */[
                      /* tuple */[
                        "int_literal_flow3",
                        function () {
                          return /* Eq */{
                                  0: -1,
                                  1: -1,
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
    ]);

exports.hash_variant = hash_variant;
exports.fib          = fib;
/*  Not a pure module */
