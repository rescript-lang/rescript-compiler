// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Lam_methname = require("./lam_methname");
var Mt           = require("./mt");

var suites_000 = /* tuple */[
  "normal",
  function () {
    return /* Eq */{
            0: Lam_methname.process("xx"),
            1: /* tuple */[
              /* Unknown */{
                0: /* None */0,
                length: 1,
                tag: 2
              },
              "xx"
            ],
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "js",
    function () {
      return /* Eq */{
              0: Lam_methname.process("xx__js"),
              1: /* tuple */[
                /* Js */{
                  0: /* None */0,
                  length: 1,
                  tag: 0
                },
                "xx"
              ],
              length: 2,
              tag: 0
            };
    }
  ],
  /* :: */[
    /* tuple */[
      "js_set",
      function () {
        return /* Eq */{
                0: Lam_methname.process("xx__set"),
                1: /* tuple */[
                  /* Js_set */2,
                  "xx"
                ],
                length: 2,
                tag: 0
              };
      }
    ],
    /* :: */[
      /* tuple */[
        "js_none",
        function () {
          return /* Eq */{
                  0: Lam_methname.process("xx__"),
                  1: /* tuple */[
                    /* Js */{
                      0: /* None */0,
                      length: 1,
                      tag: 0
                    },
                    "xx"
                  ],
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* :: */[
        /* tuple */[
          "js1",
          function () {
            return /* Eq */{
                    0: Lam_methname.process("xx__js_1"),
                    1: /* tuple */[
                      /* Js */{
                        0: /* Some */[1],
                        length: 1,
                        tag: 0
                      },
                      "xx"
                    ],
                    length: 2,
                    tag: 0
                  };
          }
        ],
        /* :: */[
          /* tuple */[
            "js2",
            function () {
              return /* Eq */{
                      0: Lam_methname.process("xx__set"),
                      1: /* tuple */[
                        /* Js_set */2,
                        "xx"
                      ],
                      length: 2,
                      tag: 0
                    };
            }
          ],
          /* :: */[
            /* tuple */[
              "js3",
              function () {
                return /* Eq */{
                        0: Lam_methname.process("xx__2"),
                        1: /* tuple */[
                          /* Js */{
                            0: /* Some */[2],
                            length: 1,
                            tag: 0
                          },
                          "xx"
                        ],
                        length: 2,
                        tag: 0
                      };
              }
            ],
            /* :: */[
              /* tuple */[
                "ml1",
                function () {
                  return /* Eq */{
                          0: Lam_methname.process("xx__2_ml"),
                          1: /* tuple */[
                            /* Ml */{
                              0: /* Some */[2],
                              length: 1,
                              tag: 1
                            },
                            "xx"
                          ],
                          length: 2,
                          tag: 0
                        };
                }
              ],
              /* :: */[
                /* tuple */[
                  "index",
                  function () {
                    return /* Eq */{
                            0: Lam_methname.process("index__"),
                            1: /* tuple */[
                              /* Js_index */0,
                              "index"
                            ],
                            length: 2,
                            tag: 0
                          };
                  }
                ],
                /* :: */[
                  /* tuple */[
                    "set_index",
                    function () {
                      return /* Eq */{
                              0: Lam_methname.process("index__set"),
                              1: /* tuple */[
                                /* Js_set_index */1,
                                "index"
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
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("lam_methname_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
