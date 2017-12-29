'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");

var list_suites_000 = /* tuple */[
  "length",
  (function () {
      return /* Eq */Block.__(0, [
                1,
                List.length(/* :: */[
                      /* tuple */[
                        0,
                        1,
                        2,
                        3,
                        4
                      ],
                      /* [] */0
                    ])
              ]);
    })
];

var list_suites_001 = /* :: */[
  /* tuple */[
    "length2",
    (function () {
        return /* Eq */Block.__(0, [
                  5,
                  List.length(/* :: */[
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
                      ])
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "long_length",
      (function () {
          return /* Eq */Block.__(0, [
                    30000,
                    List.length($$Array.to_list($$Array.init(30000, (function () {
                                    return 0;
                                  }))))
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "sort",
        (function () {
            return /* Eq */Block.__(0, [
                      List.sort(Caml_primitive.caml_int_compare, /* :: */[
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
                    ]);
          })
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
