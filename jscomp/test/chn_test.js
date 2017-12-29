'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

console.log("你好，\n世界");

console.log("\x3f\u003f\b\t\n\v\f\r\0\"\'");

function convert(s) {
  return $$Array.to_list(Array.from(s, (function (x) {
                    var match = x.codePointAt(0);
                    if (match !== undefined) {
                      return match;
                    } else {
                      throw [
                            Caml_builtin_exceptions.assert_failure,
                            [
                              "chn_test.ml",
                              20,
                              18
                            ]
                          ];
                    }
                  })));
}

eq("File \"chn_test.ml\", line 25, characters 7-14", "你好，\n世界", "你好，\n世界");

eq("File \"chn_test.ml\", line 27, characters 7-14", convert("汉字是世界上最美丽的character"), /* :: */[
      27721,
      /* :: */[
        23383,
        /* :: */[
          26159,
          /* :: */[
            19990,
            /* :: */[
              30028,
              /* :: */[
                19978,
                /* :: */[
                  26368,
                  /* :: */[
                    32654,
                    /* :: */[
                      20029,
                      /* :: */[
                        30340,
                        /* :: */[
                          99,
                          /* :: */[
                            104,
                            /* :: */[
                              97,
                              /* :: */[
                                114,
                                /* :: */[
                                  97,
                                  /* :: */[
                                    99,
                                    /* :: */[
                                      116,
                                      /* :: */[
                                        101,
                                        /* :: */[
                                          114,
                                          /* [] */0
                                        ]
                                      ]
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]);

eq("File \"chn_test.ml\", line 48, characters 5-12", convert("\x3f\x3fa"), /* :: */[
      63,
      /* :: */[
        63,
        /* :: */[
          97,
          /* [] */0
        ]
      ]
    ]);

eq("File \"chn_test.ml\", line 50, characters 5-12", convert("??a"), /* :: */[
      63,
      /* :: */[
        63,
        /* :: */[
          97,
          /* [] */0
        ]
      ]
    ]);

eq("File \"chn_test.ml\", line 52, characters 5-12", convert("\u003f\x3fa"), /* :: */[
      63,
      /* :: */[
        63,
        /* :: */[
          97,
          /* [] */0
        ]
      ]
    ]);

eq("File \"chn_test.ml\", line 54, characters 5-12", convert("🚀🚀a"), /* :: */[
      128640,
      /* :: */[
        128640,
        /* :: */[
          97,
          /* [] */0
        ]
      ]
    ]);

eq("File \"chn_test.ml\", line 56, characters 5-12", convert("\uD83D\uDE80a"), /* :: */[
      128640,
      /* :: */[
        97,
        /* [] */0
      ]
    ]);

eq("File \"chn_test.ml\", line 58, characters 5-12", convert("\uD83D\uDE80\x3f"), /* :: */[
      128640,
      /* :: */[
        63,
        /* [] */0
      ]
    ]);

eq("File \"chn_test.ml\", line 63, characters 5-12", convert("\uD83D\uDE80\uD83D\uDE80a"), /* :: */[
      128640,
      /* :: */[
        128640,
        /* :: */[
          97,
          /* [] */0
        ]
      ]
    ]);

eq("No inline string length", "\uD83D\uDE80\0".length, 3);

eq("No inline string access", Caml_string.get("\uD83D\uDE80\0", 0) & 255, 61);

eq("File \"chn_test.ml\", line 79, characters 5-12", convert("\uD83D\uDE80"), /* :: */[
      128640,
      /* [] */0
    ]);

eq("File \"chn_test.ml\", line 81, characters 5-12", convert("\uD83D\uDE80\uD83D\uDE80"), /* :: */[
      128640,
      /* :: */[
        128640,
        /* [] */0
      ]
    ]);

eq("File \"chn_test.ml\", line 82, characters 5-12", convert(" \b\t\n\v\f\ra"), /* :: */[
      32,
      /* :: */[
        8,
        /* :: */[
          9,
          /* :: */[
            10,
            /* :: */[
              11,
              /* :: */[
                12,
                /* :: */[
                  13,
                  /* :: */[
                    97,
                    /* [] */0
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]);

eq("File \"chn_test.ml\", line 89, characters 6-13", convert(" \b\t\n\v\f\r\"\'\\\0a"), /* :: */[
      32,
      /* :: */[
        8,
        /* :: */[
          9,
          /* :: */[
            10,
            /* :: */[
              11,
              /* :: */[
                12,
                /* :: */[
                  13,
                  /* :: */[
                    34,
                    /* :: */[
                      39,
                      /* :: */[
                        92,
                        /* :: */[
                          0,
                          /* :: */[
                            97,
                            /* [] */0
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]);

Mt.from_pair_suites("chn_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.convert = convert;
/*  Not a pure module */
