// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Assert     = require("assert");
var Mt         = require("./mt");
var Ext_string = require("./ext_string");
var List       = require("../stdlib/list");

var split = Ext_string.split;

var split_by = Ext_string.split_by;

var suites_000 = /* tuple */[
  "split",
  function () {
    var prim = split(/* Some */[/* true */1], "hihi", /* "i" */105);
    return Assert.deepEqual(prim, /* :: */[
                "h",
                /* :: */[
                  "h",
                  /* :: */[
                    "",
                    /* [] */0
                  ]
                ]
              ]);
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "split_non_empty",
    function () {
      var prim = split(/* None */0, "hihi", /* "i" */105);
      return Assert.deepEqual(prim, /* :: */[
                  "h",
                  /* :: */[
                    "h",
                    /* [] */0
                  ]
                ]);
    }
  ],
  /* :: */[
    /* tuple */[
      "splitempty",
      function () {
        var prim = split(/* Some */[/* true */1], "", /* "i" */105);
        return Assert.deepEqual(prim, /* :: */[
                    "",
                    /* [] */0
                  ]);
      }
    ],
    /* :: */[
      /* tuple */[
        "split_normal",
        function () {
          var prim = split(/* Some */[/* true */1], "h i i", /* " " */32);
          return Assert.deepEqual(prim, /* :: */[
                      "h",
                      /* :: */[
                        "i",
                        /* :: */[
                          "i",
                          /* [] */0
                        ]
                      ]
                    ]);
        }
      ],
      /* :: */[
        /* tuple */[
          "split_by",
          function () {
            var prim = List.filter(function (s) {
                    return +(s !== "");
                  })(split_by(/* None */0, function (x) {
                      if (x === /* " " */32) {
                        return /* true */1;
                      }
                      else {
                        return +(x === /* "\t" */9);
                      }
                    }, "h hgso hgso \t hi"));
            return Assert.deepEqual(prim, /* :: */[
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
                      ]);
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

Mt.from_suites("a_string_test.ml", suites);

exports.split    = split;
exports.split_by = split_by;
exports.suites   = suites;
/*  Not a pure module */
