'use strict';

var Mt         = require("./mt.js");
var List       = require("../../lib/js/list.js");
var Block      = require("../../lib/js/block.js");
var Ext_string = require("./ext_string.js");

var suites_000 = /* tuple */[
  "split",
  (function () {
      return /* Eq */Block.__(0, [
                Ext_string.split(/* Some */[/* true */1], "hihi", /* "i" */105),
                /* :: */[
                  "h",
                  /* :: */[
                    "h",
                    /* :: */[
                      "",
                      /* [] */0
                    ]
                  ]
                ]
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "split_non_empty",
    (function () {
        return /* Eq */Block.__(0, [
                  Ext_string.split(/* None */0, "hihi", /* "i" */105),
                  /* :: */[
                    "h",
                    /* :: */[
                      "h",
                      /* [] */0
                    ]
                  ]
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "split_empty",
      (function () {
          return /* Eq */Block.__(0, [
                    Ext_string.split(/* Some */[/* true */1], "", /* "i" */105),
                    /* [] */0
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "split_normal",
        (function () {
            return /* Eq */Block.__(0, [
                      Ext_string.split(/* Some */[/* true */1], "h i i", /* " " */32),
                      /* :: */[
                        "h",
                        /* :: */[
                          "i",
                          /* :: */[
                            "i",
                            /* [] */0
                          ]
                        ]
                      ]
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "split_by",
          (function () {
              return /* Eq */Block.__(0, [
                        List.filter((function (s) {
                                  return +(s !== "");
                                }))(Ext_string.split_by(/* None */0, (function (x) {
                                    if (x === /* " " */32) {
                                      return /* true */1;
                                    } else {
                                      return +(x === /* "\t" */9);
                                    }
                                  }), "h hgso hgso \t hi")),
                        /* :: */[
                          "h",
                          /* :: */[
                            "hgso",
                            /* :: */[
                              "hgso",
                              /* :: */[
                                "hi",
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
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("a_string_test.ml", suites);

var split = Ext_string.split;

var split_by = Ext_string.split_by;

exports.split    = split;
exports.split_by = split_by;
exports.suites   = suites;
/*  Not a pure module */
