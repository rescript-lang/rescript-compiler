// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt       = require("./mt");
var Ext_list = require("./ext_list");

var suites_001 = [
  /* tuple */0,
  "drop",
  function () {
    return [
            /* Eq */0,
            Ext_list.drop(3, [
                  /* :: */0,
                  0,
                  [
                    /* :: */0,
                    1,
                    [
                      /* :: */0,
                      2,
                      /* [] */0
                    ]
                  ]
                ]),
            /* [] */0
          ];
  }
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "drop1",
    function () {
      return [
              /* Eq */0,
              Ext_list.drop(2, [
                    /* :: */0,
                    0,
                    [
                      /* :: */0,
                      1,
                      [
                        /* :: */0,
                        2,
                        /* [] */0
                      ]
                    ]
                  ]),
              [
                /* :: */0,
                2,
                /* [] */0
              ]
            ];
    }
  ],
  [
    /* :: */0,
    [
      /* tuple */0,
      "flat_map",
      function () {
        return [
                /* Eq */0,
                [
                  /* :: */0,
                  0,
                  [
                    /* :: */0,
                    0,
                    [
                      /* :: */0,
                      1,
                      [
                        /* :: */0,
                        1,
                        [
                          /* :: */0,
                          0,
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ],
                Ext_list.flat_map(function (x) {
                      if (x % 2) {
                        return [
                                /* :: */0,
                                1,
                                [
                                  /* :: */0,
                                  1,
                                  /* [] */0
                                ]
                              ];
                      }
                      else {
                        return [
                                /* :: */0,
                                0,
                                /* [] */0
                              ];
                      }
                    }, [
                      /* :: */0,
                      0,
                      [
                        /* :: */0,
                        0,
                        [
                          /* :: */0,
                          3,
                          [
                            /* :: */0,
                            0,
                            /* [] */0
                          ]
                        ]
                      ]
                    ])
              ];
      }
    ],
    /* [] */0
  ]
];

var suites = [
  /* :: */0,
  suites_001,
  suites_002
];

Mt.from_pair_suites("a_list_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
