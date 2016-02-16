// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj = require("../runtime/caml_obj");
var Mt       = require("./mt");

var suites_000 = /* tuple */[
  "option",
  function () {
    return /* Eq */{
            0: /* true */1,
            1: Caml_obj.caml_lessthan(/* None */0, /* Some */[1]),
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "option2",
    function () {
      return /* Eq */{
              0: /* true */1,
              1: Caml_obj.caml_lessthan(/* Some */[1], /* Some */[2]),
              length: 2,
              tag: 0
            };
    }
  ],
  /* :: */[
    /* tuple */[
      "list0",
      function () {
        return /* Eq */{
                0: /* true */1,
                1: Caml_obj.caml_greaterthan(/* :: */[
                      1,
                      /* [] */0
                    ], /* [] */0),
                length: 2,
                tag: 0
              };
      }
    ],
    /* :: */[
      /* tuple */[
        "listeq",
        function () {
          return /* Eq */{
                  0: /* true */1,
                  1: Caml_obj.caml_equal(/* :: */[
                        1,
                        /* :: */[
                          2,
                          /* :: */[
                            3,
                            /* [] */0
                          ]
                        ]
                      ], /* :: */[
                        1,
                        /* :: */[
                          2,
                          /* :: */[
                            3,
                            /* [] */0
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
          "listneq",
          function () {
            return /* Eq */{
                    0: /* true */1,
                    1: Caml_obj.caml_greaterthan(/* :: */[
                          1,
                          /* :: */[
                            2,
                            /* :: */[
                              3,
                              /* [] */0
                            ]
                          ]
                        ], /* :: */[
                          1,
                          /* :: */[
                            2,
                            /* :: */[
                              2,
                              /* [] */0
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
            "custom_u",
            function () {
              return /* Eq */{
                      0: /* true */1,
                      1: Caml_obj.caml_greaterthan(/* tuple */[
                            /* A */{
                              0: 3,
                              length: 1,
                              tag: 0
                            },
                            /* B */{
                              0: 2,
                              1: /* false */0,
                              length: 2,
                              tag: 1
                            },
                            /* C */{
                              0: 1,
                              length: 1,
                              tag: 2
                            }
                          ], /* tuple */[
                            /* A */{
                              0: 3,
                              length: 1,
                              tag: 0
                            },
                            /* B */{
                              0: 2,
                              1: /* false */0,
                              length: 2,
                              tag: 1
                            },
                            /* C */{
                              0: 0,
                              length: 1,
                              tag: 2
                            }
                          ]),
                      length: 2,
                      tag: 0
                    };
            }
          ],
          /* :: */[
            /* tuple */[
              "custom_u2",
              function () {
                return /* Eq */{
                        0: /* true */1,
                        1: Caml_obj.caml_equal(/* tuple */[
                              /* A */{
                                0: 3,
                                length: 1,
                                tag: 0
                              },
                              /* B */{
                                0: 2,
                                1: /* false */0,
                                length: 2,
                                tag: 1
                              },
                              /* C */{
                                0: 1,
                                length: 1,
                                tag: 2
                              }
                            ], /* tuple */[
                              /* A */{
                                0: 3,
                                length: 1,
                                tag: 0
                              },
                              /* B */{
                                0: 2,
                                1: /* false */0,
                                length: 2,
                                tag: 1
                              },
                              /* C */{
                                0: 1,
                                length: 1,
                                tag: 2
                              }
                            ]),
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
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("caml_compare_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
