// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt       = require("./mt");
var Ext_list = require("./ext_list");

var suites_000 = /* tuple */[
  "drop",
  function () {
    return /* Eq */{
            0: Ext_list.drop(3, /* :: */[
                  0,
                  /* :: */[
                    1,
                    /* :: */[
                      2,
                      /* [] */0
                    ]
                  ]
                ]),
            1: /* [] */0,
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "drop1",
    function () {
      return /* Eq */{
              0: Ext_list.drop(2, /* :: */[
                    0,
                    /* :: */[
                      1,
                      /* :: */[
                        2,
                        /* [] */0
                      ]
                    ]
                  ]),
              1: /* :: */[
                2,
                /* [] */0
              ],
              length: 2,
              tag: 0
            };
    }
  ],
  /* :: */[
    /* tuple */[
      "flat_map",
      function () {
        return /* Eq */{
                0: /* :: */[
                  0,
                  /* :: */[
                    0,
                    /* :: */[
                      1,
                      /* :: */[
                        1,
                        /* :: */[
                          0,
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ],
                1: Ext_list.flat_map(function (x) {
                      if (x % 2) {
                        return /* :: */[
                                1,
                                /* :: */[
                                  1,
                                  /* [] */0
                                ]
                              ];
                      }
                      else {
                        return /* :: */[
                                0,
                                /* [] */0
                              ];
                      }
                    }, /* :: */[
                      0,
                      /* :: */[
                        0,
                        /* :: */[
                          3,
                          /* :: */[
                            0,
                            /* [] */0
                          ]
                        ]
                      ]
                    ]),
                length: 2,
                tag: 0
              };
      }
    ],
    /* [] */0
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("a_list_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
