// GENERATED CODE BY BUCKLESCRIPT VERSION 0.6.0 , PLEASE EDIT WITH CARE
'use strict';

var Caml_obj = require("../caml_obj");
var Mt       = require("./mt");
var Block    = require("../block");

var suites_000 = /* tuple */[
  "option",
  function () {
    return /* Eq */Block.__(0, [
              /* true */1,
              Caml_obj.caml_lessthan(/* None */0, /* Some */[1])
            ]);
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "option2",
    function () {
      return /* Eq */Block.__(0, [
                /* true */1,
                Caml_obj.caml_lessthan(/* Some */[1], /* Some */[2])
              ]);
    }
  ],
  /* :: */[
    /* tuple */[
      "list0",
      function () {
        return /* Eq */Block.__(0, [
                  /* true */1,
                  Caml_obj.caml_greaterthan(/* :: */[
                        1,
                        /* [] */0
                      ], /* [] */0)
                ]);
      }
    ],
    /* :: */[
      /* tuple */[
        "listeq",
        function () {
          return /* Eq */Block.__(0, [
                    /* true */1,
                    Caml_obj.caml_equal(/* :: */[
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
                        ])
                  ]);
        }
      ],
      /* :: */[
        /* tuple */[
          "listneq",
          function () {
            return /* Eq */Block.__(0, [
                      /* true */1,
                      Caml_obj.caml_greaterthan(/* :: */[
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
                          ])
                    ]);
          }
        ],
        /* :: */[
          /* tuple */[
            "custom_u",
            function () {
              return /* Eq */Block.__(0, [
                        /* true */1,
                        Caml_obj.caml_greaterthan(/* tuple */[
                              /* A */Block.__(0, [3]),
                              /* B */Block.__(1, [
                                  2,
                                  /* false */0
                                ]),
                              /* C */Block.__(2, [1])
                            ], /* tuple */[
                              /* A */Block.__(0, [3]),
                              /* B */Block.__(1, [
                                  2,
                                  /* false */0
                                ]),
                              /* C */Block.__(2, [0])
                            ])
                      ]);
            }
          ],
          /* :: */[
            /* tuple */[
              "custom_u2",
              function () {
                return /* Eq */Block.__(0, [
                          /* true */1,
                          Caml_obj.caml_equal(/* tuple */[
                                /* A */Block.__(0, [3]),
                                /* B */Block.__(1, [
                                    2,
                                    /* false */0
                                  ]),
                                /* C */Block.__(2, [1])
                              ], /* tuple */[
                                /* A */Block.__(0, [3]),
                                /* B */Block.__(1, [
                                    2,
                                    /* false */0
                                  ]),
                                /* C */Block.__(2, [1])
                              ])
                        ]);
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
