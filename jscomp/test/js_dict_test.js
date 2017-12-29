'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_dict = require("../../lib/js/js_dict.js");
var Js_primitive = require("../../lib/js/js_primitive.js");

function obj() {
  return {
          foo: 43,
          bar: 86
        };
}

var suites_000 = /* tuple */[
  "empty",
  (function () {
      return /* Eq */Block.__(0, [
                /* array */[],
                Object.keys({ })
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "get",
    (function () {
        return /* Eq */Block.__(0, [
                  /* Some */[43],
                  Js_primitive.undefined_to_opt(({
                            foo: 43,
                            bar: 86
                          })["foo"])
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "get - property not in object",
      (function () {
          return /* Eq */Block.__(0, [
                    /* None */0,
                    Js_primitive.undefined_to_opt(({
                              foo: 43,
                              bar: 86
                            })["baz"])
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "unsafe_get",
        (function () {
            return /* Eq */Block.__(0, [
                      43,
                      ({
                            foo: 43,
                            bar: 86
                          })["foo"]
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "set",
          (function () {
              var o = {
                foo: 43,
                bar: 86
              };
              o["foo"] = 36;
              return /* Eq */Block.__(0, [
                        /* Some */[36],
                        Js_primitive.undefined_to_opt(o["foo"])
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "keys",
            (function () {
                return /* Eq */Block.__(0, [
                          /* array */[
                            "foo",
                            "bar"
                          ],
                          Object.keys({
                                foo: 43,
                                bar: 86
                              })
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "entries",
              (function () {
                  return /* Eq */Block.__(0, [
                            /* array */[
                              /* tuple */[
                                "foo",
                                43
                              ],
                              /* tuple */[
                                "bar",
                                86
                              ]
                            ],
                            Js_dict.entries({
                                  foo: 43,
                                  bar: 86
                                })
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "values",
                (function () {
                    return /* Eq */Block.__(0, [
                              /* int array */[
                                43,
                                86
                              ],
                              Js_dict.values({
                                    foo: 43,
                                    bar: 86
                                  })
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "fromList - []",
                  (function () {
                      return /* Eq */Block.__(0, [
                                { },
                                Js_dict.fromList(/* [] */0)
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "fromList",
                    (function () {
                        return /* Eq */Block.__(0, [
                                  /* array */[
                                    /* tuple */[
                                      "x",
                                      23
                                    ],
                                    /* tuple */[
                                      "y",
                                      46
                                    ]
                                  ],
                                  Js_dict.entries(Js_dict.fromList(/* :: */[
                                            /* tuple */[
                                              "x",
                                              23
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "y",
                                                46
                                              ],
                                              /* [] */0
                                            ]
                                          ]))
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "fromArray - []",
                      (function () {
                          return /* Eq */Block.__(0, [
                                    { },
                                    Js_dict.fromArray(/* array */[])
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "fromArray",
                        (function () {
                            return /* Eq */Block.__(0, [
                                      /* array */[
                                        /* tuple */[
                                          "x",
                                          23
                                        ],
                                        /* tuple */[
                                          "y",
                                          46
                                        ]
                                      ],
                                      Js_dict.entries(Js_dict.fromArray(/* array */[
                                                /* tuple */[
                                                  "x",
                                                  23
                                                ],
                                                /* tuple */[
                                                  "y",
                                                  46
                                                ]
                                              ]))
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "map",
                          (function () {
                              return /* Eq */Block.__(0, [
                                        {
                                          foo: "43",
                                          bar: "86"
                                        },
                                        Js_dict.map((function (i) {
                                                return "" + i;
                                              }), {
                                              foo: 43,
                                              bar: 86
                                            })
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
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_dict_test.ml", suites);

exports.obj = obj;
exports.suites = suites;
/*  Not a pure module */
