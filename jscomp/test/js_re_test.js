'use strict';

var Mt         = require("./mt.js");
var Block      = require("../../lib/js/block.js");
var Caml_array = require("../../lib/js/caml_array.js");

var suites_000 = /* tuple */[
  "exec_literal",
  (function () {
      var match = (/[^.]+/).exec("http://xxx.domain.com");
      if (match !== null) {
        return /* Eq */Block.__(0, [
                  "xxx",
                  Caml_array.caml_array_get(match, 0).substring(7)
                ]);
      } else {
        return /* FailWith */Block.__(9, ["regex should match"]);
      }
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "exec_no_match",
    (function () {
        var match = (/https:\/\/(.*)/).exec("http://xxx.domain.com");
        if (match !== null) {
          return /* FailWith */Block.__(9, ["regex should not match"]);
        } else {
          return /* Ok */Block.__(4, [/* true */1]);
        }
      })
  ],
  /* :: */[
    /* tuple */[
      "test_str",
      (function () {
          var res = +new RegExp("foo").test("#foo#");
          return /* Eq */Block.__(0, [
                    /* true */1,
                    res
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "fromStringWithFlags",
        (function () {
            var res = new RegExp("foo", "g");
            return /* Eq */Block.__(0, [
                      /* true */1,
                      +res.global
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "result_index",
          (function () {
              var match = new RegExp("zbar").exec("foobarbazbar");
              if (match !== null) {
                return /* Eq */Block.__(0, [
                          8,
                          match.index
                        ]);
              } else {
                return /* Fail */Block.__(8, [/* () */0]);
              }
            })
        ],
        /* :: */[
          /* tuple */[
            "result_input",
            (function () {
                var input = "foobar";
                var match = (/foo/g).exec(input);
                if (match !== null) {
                  return /* Eq */Block.__(0, [
                            input,
                            match.input
                          ]);
                } else {
                  return /* Fail */Block.__(8, [/* () */0]);
                }
              })
          ],
          /* :: */[
            /* tuple */[
              "t_flags",
              (function () {
                  return /* Eq */Block.__(0, [
                            "gi",
                            (/./ig).flags
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "t_global",
                (function () {
                    return /* Eq */Block.__(0, [
                              /* true */1,
                              +(/./ig).global
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "t_ignoreCase",
                  (function () {
                      return /* Eq */Block.__(0, [
                                /* true */1,
                                +(/./ig).ignoreCase
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "t_lastIndex",
                    (function () {
                        var re = (/na/g);
                        re.exec("banana");
                        return /* Eq */Block.__(0, [
                                  4,
                                  re.lastIndex
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "t_setLastIndex",
                      (function () {
                          var re = (/na/g);
                          var before = re.lastIndex;
                          re.lastIndex = 42;
                          var after = re.lastIndex;
                          return /* Eq */Block.__(0, [
                                    /* tuple */[
                                      0,
                                      42
                                    ],
                                    /* tuple */[
                                      before,
                                      after
                                    ]
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "t_multiline",
                        (function () {
                            return /* Eq */Block.__(0, [
                                      /* false */0,
                                      +(/./ig).multiline
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "t_source",
                          (function () {
                              return /* Eq */Block.__(0, [
                                        "f.+o",
                                        (/f.+o/ig).source
                                      ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "t_sticky",
                            (function () {
                                return /* Eq */Block.__(0, [
                                          /* true */1,
                                          +(/./yg).sticky
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "t_unicode",
                              (function () {
                                  return /* Eq */Block.__(0, [
                                            /* false */0,
                                            +(/./yg).unicode
                                          ]);
                                })
                            ],
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
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_re_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
