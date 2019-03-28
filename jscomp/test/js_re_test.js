'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites_000 = /* tuple */[
  "captures",
  (function (param) {
      var re = (/(\d+)-(?:(\d+))?/g);
      var match = re.exec("3-");
      if (match !== null) {
        var defined = Caml_array.caml_array_get(match, 1);
        var $$undefined = Caml_array.caml_array_get(match, 2);
        return /* Eq */Block.__(0, [
                  /* tuple */[
                    "3",
                    null
                  ],
                  /* tuple */[
                    defined,
                    $$undefined
                  ]
                ]);
      } else {
        return /* Fail */Block.__(8, [/* () */0]);
      }
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "fromString",
    (function (param) {
        var contentOf = function (tag, xmlString) {
          var param = new RegExp("<" + (tag + (">(.*?)<\\/" + (tag + ">")))).exec(xmlString);
          if (param !== null) {
            return Caml_option.nullable_to_opt(Caml_array.caml_array_get(param, 1));
          }
          
        };
        return /* Eq */Block.__(0, [
                  contentOf("div", "<div>Hi</div>"),
                  "Hi"
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "exec_literal",
      (function (param) {
          var match = (/[^.]+/).exec("http://xxx.domain.com");
          if (match !== null) {
            return /* Eq */Block.__(0, [
                      "http://xxx",
                      Caml_array.caml_array_get(match, 0)
                    ]);
          } else {
            return /* FailWith */Block.__(9, ["regex should match"]);
          }
        })
    ],
    /* :: */[
      /* tuple */[
        "exec_no_match",
        (function (param) {
            var match = (/https:\/\/(.*)/).exec("http://xxx.domain.com");
            if (match !== null) {
              return /* FailWith */Block.__(9, ["regex should not match"]);
            } else {
              return /* Ok */Block.__(4, [true]);
            }
          })
      ],
      /* :: */[
        /* tuple */[
          "test_str",
          (function (param) {
              var res = new RegExp("foo").test("#foo#");
              return /* Eq */Block.__(0, [
                        true,
                        res
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "fromStringWithFlags",
            (function (param) {
                var res = new RegExp("foo", "g");
                return /* Eq */Block.__(0, [
                          true,
                          res.global
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "result_index",
              (function (param) {
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
                (function (param) {
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
                  (function (param) {
                      return /* Eq */Block.__(0, [
                                "gi",
                                (/./ig).flags
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "t_global",
                    (function (param) {
                        return /* Eq */Block.__(0, [
                                  true,
                                  (/./ig).global
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "t_ignoreCase",
                      (function (param) {
                          return /* Eq */Block.__(0, [
                                    true,
                                    (/./ig).ignoreCase
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "t_lastIndex",
                        (function (param) {
                            var re = (/na/g);
                            Caml_option.null_to_opt(re.exec("banana"));
                            return /* Eq */Block.__(0, [
                                      4,
                                      re.lastIndex
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "t_setLastIndex",
                          (function (param) {
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
                            (function (param) {
                                return /* Eq */Block.__(0, [
                                          false,
                                          (/./ig).multiline
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "t_source",
                              (function (param) {
                                  return /* Eq */Block.__(0, [
                                            "f.+o",
                                            (/f.+o/ig).source
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "t_sticky",
                                (function (param) {
                                    return /* Eq */Block.__(0, [
                                              true,
                                              (/./yg).sticky
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "t_unicode",
                                  (function (param) {
                                      return /* Eq */Block.__(0, [
                                                false,
                                                (/./yg).unicode
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
    ]
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("Js_re_test", suites);

exports.suites = suites;
/*  Not a pure module */
