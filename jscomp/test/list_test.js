// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj = require("../runtime/caml_obj");
var Mt       = require("./mt");
var $$Array  = require("../stdlib/array");
var List     = require("../stdlib/list");

var list_suites_000 = /* tuple */[
  "length",
  function () {
    return /* Eq */{
            0: 1,
            1: List.length(/* :: */[
                  /* tuple */[
                    0,
                    1,
                    2,
                    3,
                    4
                  ],
                  /* [] */0
                ]),
            length: 2,
            tag: 0
          };
  }
];

var list_suites_001 = /* :: */[
  /* tuple */[
    "length2",
    function () {
      return /* Eq */{
              0: 5,
              1: List.length(/* :: */[
                    0,
                    /* :: */[
                      1,
                      /* :: */[
                        2,
                        /* :: */[
                          3,
                          /* :: */[
                            4,
                            /* [] */0
                          ]
                        ]
                      ]
                    ]
                  ]),
              length: 2,
              tag: 0
            };
    }
  ],
  /* :: */[
    /* tuple */[
      "long_length",
      function () {
        var v = 100000;
        return /* Eq */{
                0: v,
                1: List.length($$Array.to_list($$Array.init(v, function () {
                              return 0;
                            }))),
                length: 2,
                tag: 0
              };
      }
    ],
    /* :: */[
      /* tuple */[
        "sort",
        function () {
          return /* Eq */{
                  0: List.sort(function (x, y) {
                        return Caml_obj.caml_int_compare(x, y);
                      }, /* :: */[
                        4,
                        /* :: */[
                          1,
                          /* :: */[
                            2,
                            /* :: */[
                              3,
                              /* [] */0
                            ]
                          ]
                        ]
                      ]),
                  1: /* :: */[
                    1,
                    /* :: */[
                      2,
                      /* :: */[
                        3,
                        /* :: */[
                          4,
                          /* [] */0
                        ]
                      ]
                    ]
                  ],
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* [] */0
    ]
  ]
];

var list_suites = /* :: */[
  list_suites_000,
  list_suites_001
];

Mt.from_pair_suites("list_test.ml", list_suites);

exports.list_suites = list_suites;
/*  Not a pure module */
