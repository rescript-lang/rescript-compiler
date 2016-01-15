// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Mt = require("./mt");
var Ext_string = require("./ext_string");
var List = require("../stdlib/list");

var split = Ext_string.split;

var split_by = Ext_string.split_by;

var suites_001 = [
  /* tuple */0,
  "split",
  function () {
    return Mt.assert_equal(split([
                    /* Some */0,
                    /* true */1
                  ], "hihi", /* "i" */105), [
                /* :: */0,
                "h",
                [
                  /* :: */0,
                  "h",
                  [
                    /* :: */0,
                    "",
                    /* [] */0
                  ]
                ]
              ]);
  }
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "split_non_empty",
    function () {
      return Mt.assert_equal(split(/* None */0, "hihi", /* "i" */105), [
                  /* :: */0,
                  "h",
                  [
                    /* :: */0,
                    "h",
                    /* [] */0
                  ]
                ]);
    }
  ],
  [
    /* :: */0,
    [
      /* tuple */0,
      "splitempty",
      function () {
        return Mt.assert_equal(split([
                        /* Some */0,
                        /* true */1
                      ], "", /* "i" */105), [
                    /* :: */0,
                    "",
                    /* [] */0
                  ]);
      }
    ],
    [
      /* :: */0,
      [
        /* tuple */0,
        "split_normal",
        function () {
          return Mt.assert_equal(split([
                          /* Some */0,
                          /* true */1
                        ], "h i i", /* " " */32), [
                      /* :: */0,
                      "h",
                      [
                        /* :: */0,
                        "i",
                        [
                          /* :: */0,
                          "i",
                          /* [] */0
                        ]
                      ]
                    ]);
        }
      ],
      [
        /* :: */0,
        [
          /* tuple */0,
          "split_by",
          function () {
            return Mt.assert_equal(List.filter(function (s) {
                              return +(s !== "");
                            })(split_by(/* None */0, function (x) {
                                return +(x === /* " " */32 || x === /* "\t" */9);
                              }, "h hgso hgso \t hi")), [
                        /* :: */0,
                        "h",
                        [
                          /* :: */0,
                          "hgso",
                          [
                            /* :: */0,
                            "hgso",
                            [
                              /* :: */0,
                              "hi",
                              /* [] */0
                            ]
                          ]
                        ]
                      ]);
          }
        ],
        /* [] */0
      ]
    ]
  ]
];

var suites = [
  /* :: */0,
  suites_001,
  suites_002
];

Mt.from_suites("a_string_test.ml", suites);

exports.split = split;
exports.split_by = split_by;
exports.suites = suites;
/*  Not a pure module */
