'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_option = require("../../lib/js/js_option.js");

function simpleEq(a, b) {
  return a === b;
}

var option_suites_000 = /* tuple */[
  "option_isSome_Some",
  (function () {
      return /* Eq */Block.__(0, [
                true,
                true
              ]);
    })
];

var option_suites_001 = /* :: */[
  /* tuple */[
    "option_isSome_None",
    (function () {
        return /* Eq */Block.__(0, [
                  false,
                  false
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "option_isNone_Some",
      (function () {
          return /* Eq */Block.__(0, [
                    false,
                    false
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "option_isNone_None",
        (function () {
            return /* Eq */Block.__(0, [
                      true,
                      true
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "option_isSomeValue_Eq",
          (function () {
              return /* Eq */Block.__(0, [
                        true,
                        Js_option.isSomeValue(simpleEq, 2, /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(2))
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "option_isSomeValue_Diff",
            (function () {
                return /* Eq */Block.__(0, [
                          false,
                          Js_option.isSomeValue(simpleEq, 1, /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(2))
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "option_isSomeValue_DiffNone",
              (function () {
                  return /* Eq */Block.__(0, [
                            false,
                            Js_option.isSomeValue(simpleEq, 1, /* None */null)
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "option_getExn_Some",
                (function () {
                    return /* Eq */Block.__(0, [
                              2,
                              Js_option.getExn(/* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(2))
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "option_equal_Eq",
                  (function () {
                      return /* Eq */Block.__(0, [
                                true,
                                Js_option.equal(simpleEq, /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(2), /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(2))
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "option_equal_Diff",
                    (function () {
                        return /* Eq */Block.__(0, [
                                  false,
                                  Js_option.equal(simpleEq, /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(1), /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(2))
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "option_equal_DiffNone",
                      (function () {
                          return /* Eq */Block.__(0, [
                                    false,
                                    Js_option.equal(simpleEq, /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(1), /* None */null)
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "option_andThen_SomeSome",
                        (function () {
                            return /* Eq */Block.__(0, [
                                      true,
                                      Js_option.isSomeValue(simpleEq, 3, Js_option.andThen((function (a) {
                                                  return /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(a + 1 | 0);
                                                }), /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(2)))
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "option_andThen_SomeNone",
                          (function () {
                              return /* Eq */Block.__(0, [
                                        false,
                                        Js_option.isSomeValue(simpleEq, 3, Js_option.andThen((function () {
                                                    return /* None */null;
                                                  }), /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(2)))
                                      ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "option_map_Some",
                            (function () {
                                return /* Eq */Block.__(0, [
                                          true,
                                          Js_option.isSomeValue(simpleEq, 3, Js_option.map((function (a) {
                                                      return a + 1 | 0;
                                                    }), /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(2)))
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "option_map_None",
                              (function () {
                                  return /* Eq */Block.__(0, [
                                            /* None */null,
                                            Js_option.map((function (a) {
                                                    return a + 1 | 0;
                                                  }), /* None */null)
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "option_default_Some",
                                (function () {
                                    return /* Eq */Block.__(0, [
                                              2,
                                              Js_option.getWithDefault(3, /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(2))
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "option_default_None",
                                  (function () {
                                      return /* Eq */Block.__(0, [
                                                3,
                                                Js_option.getWithDefault(3, /* None */null)
                                              ]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "option_filter_Pass",
                                    (function () {
                                        return /* Eq */Block.__(0, [
                                                  true,
                                                  Js_option.isSomeValue(simpleEq, 2, Js_option.filter((function (a) {
                                                              return a % 2 === 0;
                                                            }), /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(2)))
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "option_filter_Reject",
                                      (function () {
                                          return /* Eq */Block.__(0, [
                                                    /* None */null,
                                                    Js_option.filter((function (a) {
                                                            return a % 3 === 0;
                                                          }), /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(2))
                                                  ]);
                                        })
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "option_filter_None",
                                        (function () {
                                            return /* Eq */Block.__(0, [
                                                      /* None */null,
                                                      Js_option.filter((function (a) {
                                                              return a % 3 === 0;
                                                            }), /* None */null)
                                                    ]);
                                          })
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "option_firstSome_First",
                                          (function () {
                                              return /* Eq */Block.__(0, [
                                                        true,
                                                        Js_option.isSomeValue(simpleEq, 3, Js_option.firstSome(/* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(3), /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(2)))
                                                      ]);
                                            })
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "option_firstSome_First",
                                            (function () {
                                                return /* Eq */Block.__(0, [
                                                          true,
                                                          Js_option.isSomeValue(simpleEq, 2, Js_option.firstSome(/* None */null, /* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(2)))
                                                        ]);
                                              })
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "option_firstSome_None",
                                              (function () {
                                                  return /* Eq */Block.__(0, [
                                                            /* None */null,
                                                            Js_option.firstSome(/* None */null, /* None */null)
                                                          ]);
                                                })
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "File \"js_option_test.ml\", line 81, characters 4-11",
                                                (function () {
                                                    return /* Eq */Block.__(0, [
                                                              true,
                                                              true
                                                            ]);
                                                  })
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "File \"js_option_test.ml\", line 85, characters 4-11",
                                                  (function () {
                                                      var param = Js_option.getExn(/* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(/* None */null));
                                                      return /* Eq */Block.__(0, [
                                                                param ? true : false,
                                                                false
                                                              ]);
                                                    })
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "File \"js_option_test.ml\", line 89, characters 4-11",
                                                    (function () {
                                                        return /* Eq */Block.__(0, [
                                                                  true,
                                                                  true
                                                                ]);
                                                      })
                                                  ],
                                                  /* :: */[
                                                    /* tuple */[
                                                      "File \"js_option_test.ml\", line 93, characters 4-11",
                                                      (function () {
                                                          var param = Js_option.getExn(/* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(/* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(/* None */null)));
                                                          return /* Eq */Block.__(0, [
                                                                    param ? true : false,
                                                                    true
                                                                  ]);
                                                        })
                                                    ],
                                                    /* :: */[
                                                      /* tuple */[
                                                        "File \"js_option_test.ml\", line 97, characters 4-11",
                                                        (function () {
                                                            var param = Js_option.getExn(Js_option.getExn(/* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(/* box */(function (x) {if(x==null || x[".box"]!==undefined) {return {".box":x}} else {return x}})(/* None */null))));
                                                            return /* Eq */Block.__(0, [
                                                                      param ? true : false,
                                                                      false
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
