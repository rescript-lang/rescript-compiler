'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_option = require("../../lib/js/js_option.js");

function simpleEq(a, b) {
  return a === b;
}

var option_suites_000 = /* tuple */[
  "option_isSome_Some",
  (function (param) {
      return /* Eq */Block.__(0, [
                true,
                true
              ]);
    })
];

var option_suites_001 = /* :: */[
  /* tuple */[
    "option_isSome_None",
    (function (param) {
        return /* Eq */Block.__(0, [
                  false,
                  false
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "option_isNone_Some",
      (function (param) {
          return /* Eq */Block.__(0, [
                    false,
                    false
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "option_isNone_None",
        (function (param) {
            return /* Eq */Block.__(0, [
                      true,
                      true
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "option_isSomeValue_Eq",
          (function (param) {
              return /* Eq */Block.__(0, [
                        true,
                        Js_option.isSomeValue(simpleEq, 2, 2)
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "option_isSomeValue_Diff",
            (function (param) {
                return /* Eq */Block.__(0, [
                          false,
                          Js_option.isSomeValue(simpleEq, 1, 2)
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "option_isSomeValue_DiffNone",
              (function (param) {
                  return /* Eq */Block.__(0, [
                            false,
                            Js_option.isSomeValue(simpleEq, 1, undefined)
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "option_getExn_Some",
                (function (param) {
                    return /* Eq */Block.__(0, [
                              2,
                              Js_option.getExn(2)
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "option_equal_Eq",
                  (function (param) {
                      return /* Eq */Block.__(0, [
                                true,
                                Js_option.equal(simpleEq, 2, 2)
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "option_equal_Diff",
                    (function (param) {
                        return /* Eq */Block.__(0, [
                                  false,
                                  Js_option.equal(simpleEq, 1, 2)
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "option_equal_DiffNone",
                      (function (param) {
                          return /* Eq */Block.__(0, [
                                    false,
                                    Js_option.equal(simpleEq, 1, undefined)
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "option_andThen_SomeSome",
                        (function (param) {
                            return /* Eq */Block.__(0, [
                                      true,
                                      Js_option.isSomeValue(simpleEq, 3, Js_option.andThen((function (a) {
                                                  return a + 1 | 0;
                                                }), 2))
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "option_andThen_SomeNone",
                          (function (param) {
                              return /* Eq */Block.__(0, [
                                        false,
                                        Js_option.isSomeValue(simpleEq, 3, Js_option.andThen((function (param) {
                                                    return undefined;
                                                  }), 2))
                                      ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "option_map_Some",
                            (function (param) {
                                return /* Eq */Block.__(0, [
                                          true,
                                          Js_option.isSomeValue(simpleEq, 3, Js_option.map((function (a) {
                                                      return a + 1 | 0;
                                                    }), 2))
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "option_map_None",
                              (function (param) {
                                  return /* Eq */Block.__(0, [
                                            undefined,
                                            Js_option.map((function (a) {
                                                    return a + 1 | 0;
                                                  }), undefined)
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "option_default_Some",
                                (function (param) {
                                    return /* Eq */Block.__(0, [
                                              2,
                                              Js_option.getWithDefault(3, 2)
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "option_default_None",
                                  (function (param) {
                                      return /* Eq */Block.__(0, [
                                                3,
                                                Js_option.getWithDefault(3, undefined)
                                              ]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "option_filter_Pass",
                                    (function (param) {
                                        return /* Eq */Block.__(0, [
                                                  true,
                                                  Js_option.isSomeValue(simpleEq, 2, Js_option.filter((function (a) {
                                                              return a % 2 === 0;
                                                            }), 2))
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "option_filter_Reject",
                                      (function (param) {
                                          return /* Eq */Block.__(0, [
                                                    undefined,
                                                    Js_option.filter((function (a) {
                                                            return a % 3 === 0;
                                                          }), 2)
                                                  ]);
                                        })
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "option_filter_None",
                                        (function (param) {
                                            return /* Eq */Block.__(0, [
                                                      undefined,
                                                      Js_option.filter((function (a) {
                                                              return a % 3 === 0;
                                                            }), undefined)
                                                    ]);
                                          })
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "option_firstSome_First",
                                          (function (param) {
                                              return /* Eq */Block.__(0, [
                                                        true,
                                                        Js_option.isSomeValue(simpleEq, 3, Js_option.firstSome(3, 2))
                                                      ]);
                                            })
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "option_firstSome_First",
                                            (function (param) {
                                                return /* Eq */Block.__(0, [
                                                          true,
                                                          Js_option.isSomeValue(simpleEq, 2, Js_option.firstSome(undefined, 2))
                                                        ]);
                                              })
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "option_firstSome_None",
                                              (function (param) {
                                                  return /* Eq */Block.__(0, [
                                                            undefined,
                                                            Js_option.firstSome(undefined, undefined)
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
            ]
          ]
        ]
      ]
    ]
  ]
];

var option_suites = /* :: */[
  option_suites_000,
  option_suites_001
];

Mt.from_pair_suites("js_option_test.ml", option_suites);

exports.simpleEq = simpleEq;
exports.option_suites = option_suites;
/*  Not a pure module */
