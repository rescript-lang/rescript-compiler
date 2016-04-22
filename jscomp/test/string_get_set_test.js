// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_int64  = require("../runtime/caml_int64");
var Mt          = require("./mt");
var Caml_string = require("../runtime/caml_string");

Mt.from_pair_suites("string_get_set_test.ml", /* :: */[
      /* tuple */[
        'File "string_get_set_test.ml", line 8, characters 4-11',
        function () {
          return /* Eq */{
                  0: Caml_string.caml_string_get16("2\0", 0),
                  1: 50,
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* :: */[
        /* tuple */[
          'File "string_get_set_test.ml", line 9, characters 4-11',
          function () {
            return /* Eq */{
                    0: Caml_string.caml_string_get16("20", 0),
                    1: 12338,
                    length: 2,
                    tag: 0
                  };
          }
        ],
        /* :: */[
          /* tuple */[
            'File "string_get_set_test.ml", line 10, characters 4-11',
            function () {
              return /* Eq */{
                      0: Caml_string.caml_string_get32("0123", 0),
                      1: 858927408,
                      length: 2,
                      tag: 0
                    };
            }
          ],
          /* :: */[
            /* tuple */[
              'File "string_get_set_test.ml", line 11, characters 4-11',
              function () {
                return /* Eq */{
                        0: Caml_string.caml_string_get32("0123", 0),
                        1: 858927408,
                        length: 2,
                        tag: 0
                      };
              }
            ],
            /* :: */[
              /* tuple */[
                'File "string_get_set_test.ml", line 12, characters 4-11',
                function () {
                  return /* Eq */{
                          0: Caml_string.caml_string_get32("3210", 0),
                          1: 808530483,
                          length: 2,
                          tag: 0
                        };
                }
              ],
              /* :: */[
                /* tuple */[
                  'File "string_get_set_test.ml", line 13, characters 4-11',
                  function () {
                    return /* Eq */{
                            0: Caml_int64.get64("12345678", 0),
                            1: /* int64 */[
                              943142453,
                              875770417
                            ],
                            length: 2,
                            tag: 0
                          };
                  }
                ],
                /* :: */[
                  /* tuple */[
                    'File "string_get_set_test.ml", line 14, characters 4-11',
                    function () {
                      return /* Eq */{
                              0: Caml_int64.get64("87654321", 0),
                              1: /* int64 */[
                                825373492,
                                892745528
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
    ]);

/*  Not a pure module */
