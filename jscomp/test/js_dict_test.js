'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_dict = require("../../lib/js/js_dict.js");
var Js_primitive = require("../../lib/js/js_primitive.js");

function obj(param) {
  return {
          foo: 43,
          bar: 86
        };
}

var suites_000 = /* tuple */[
  "empty",
  (function (param) {
      return /* Eq */Block.__(0, [
                /* array */[],
                Object.keys({ })
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "get",
    (function (param) {
        return /* Eq */Block.__(0, [
                  43,
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
      (function (param) {
          return /* Eq */Block.__(0, [
                    undefined,
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
        (function (param) {
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
          (function (param) {
              var o = {
                foo: 43,
                bar: 86
              };
              o["foo"] = 36;
              return /* Eq */Block.__(0, [
                        36,
                        Js_primitive.undefined_to_opt(o["foo"])
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "keys",
            (function (param) {
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
              (function (param) {
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
                (function (param) {
                    return /* Eq */Block.__(0, [
                              /* array */[
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
                  (function (param) {
                      return /* Eq */Block.__(0, [
                                { },
                                Js_dict.fromList(/* [] */0)
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "fromList",
                    (function (param) {
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
                      (function (param) {
                          return /* Eq */Block.__(0, [
                                    { },
                                    Js_dict.fromArray(/* array */[])
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "fromArray",
                        (function (param) {
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
                          (function (param) {
                              return /* Eq */Block.__(0, [
                                        {
                                          foo: "43",
                                          bar: "86"
                                        },
                                        Js_dict.map((function (i) {
                                                return String(i);
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
