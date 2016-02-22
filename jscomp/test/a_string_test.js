// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt         = require("./mt");
var Ext_string = require("./ext_string");
var List       = require("../stdlib/list");

var split = Ext_string.split;

var split_by = Ext_string.split_by;

var suites_000 = /* tuple */[
  "split",
  function () {
    return /* Eq */{
            0: split(/* Some */[/* true */1], "hihi", /* "i" */105),
            1: /* :: */[
              "h",
              /* :: */[
                "h",
                /* :: */[
                  "",
                  /* [] */0
                ]
              ]
            ],
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "split_non_empty",
    function () {
      return /* Eq */{
              0: split(/* None */0, "hihi", /* "i" */105),
              1: /* :: */[
                "h",
                /* :: */[
                  "h",
                  /* [] */0
                ]
              ],
              length: 2,
              tag: 0
            };
    }
  ],
  /* :: */[
    /* tuple */[
      "split_empty",
      function () {
        return /* Eq */{
                0: split(/* Some */[/* true */1], "", /* "i" */105),
                1: /* [] */0,
                length: 2,
                tag: 0
              };
      }
    ],
    /* :: */[
      /* tuple */[
        "split_normal",
        function () {
          return /* Eq */{
                  0: split(/* Some */[/* true */1], "h i i", /* " " */32),
                  1: /* :: */[
                    "h",
                    /* :: */[
                      "i",
                      /* :: */[
                        "i",
                        /* [] */0
                      ]
                    ]
                  ],
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* :: */[
        /* tuple */[
          "split_by",
          function () {
            return /* Eq */{
                    0: List.filter(function (s) {
                            return +(s !== "");
                          })(split_by(/* None */0, function (x) {
                              if (x === /* " " */32) {
                                return /* true */1;
                              }
                              else {
                                return +(x === /* "\t" */9);
                              }
                            }, "h hgso hgso \t hi")),
                    1: /* :: */[
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
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("a_string_test.ml", suites);

exports.split    = split;
exports.split_by = split_by;
exports.suites   = suites;
/*  Not a pure module */
