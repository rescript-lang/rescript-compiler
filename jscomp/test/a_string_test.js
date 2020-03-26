'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Ext_string_test = require("./ext_string_test.js");

var suites_000 = /* tuple */[
  "split",
  (function (param) {
      return /* Eq */Block.__(0, [
                Ext_string_test.split(true, "hihi", /* "i" */105),
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
    (function (param) {
        return /* Eq */Block.__(0, [
                  Ext_string_test.split(undefined, "hihi", /* "i" */105),
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
      (function (param) {
          return /* Eq */Block.__(0, [
                    Ext_string_test.split(true, "", /* "i" */105),
                    /* [] */0
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "split_normal",
        (function (param) {
            return /* Eq */Block.__(0, [
                      Ext_string_test.split(true, "h i i", /* " " */32),
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
          (function (param) {
              return /* Eq */Block.__(0, [
                        List.filter((function (s) {
                                  return s !== "";
                                }))(Ext_string_test.split_by(undefined, (function (x) {
                                    if (x === /* " " */32) {
                                      return true;
                                    } else {
                                      return x === /* "\t" */9;
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

Mt.from_pair_suites("A_string_test", suites);

var split = Ext_string_test.split;

var split_by = Ext_string_test.split_by;

exports.split = split;
exports.split_by = split_by;
exports.suites = suites;
/*  Not a pure module */
