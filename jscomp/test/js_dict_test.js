'use strict';

var Mt = require("./mt.js");
var Js_dict = require("../../lib/js/js_dict.js");

function obj(param) {
  return {
          foo: 43,
          bar: 86
        };
}

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "empty",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: /* array */[],
                Arg1: Object.keys({ })
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "get",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: 43,
                  Arg1: Js_dict.get({
                        foo: 43,
                        bar: 86
                      }, "foo")
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "get - property not in object",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: undefined,
                    Arg1: Js_dict.get({
                          foo: 43,
                          bar: 86
                        }, "baz")
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "unsafe_get",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: 43,
                      Arg1: ({
                            foo: 43,
                            bar: 86
                          })["foo"]
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "set",
            (function (param) {
                var o = {
                  foo: 43,
                  bar: 86
                };
                o["foo"] = 36;
                return /* constructor */{
                        tag: "Eq",
                        Arg0: 36,
                        Arg1: Js_dict.get(o, "foo")
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "keys",
              (function (param) {
                  return /* constructor */{
                          tag: "Eq",
                          Arg0: /* array */[
                            "foo",
                            "bar"
                          ],
                          Arg1: Object.keys({
                                foo: 43,
                                bar: 86
                              })
                        };
                })
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "entries",
                (function (param) {
                    return /* constructor */{
                            tag: "Eq",
                            Arg0: /* array */[
                              /* tuple */[
                                "foo",
                                43
                              ],
                              /* tuple */[
                                "bar",
                                86
                              ]
                            ],
                            Arg1: Js_dict.entries({
                                  foo: 43,
                                  bar: 86
                                })
                          };
                  })
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "values",
                  (function (param) {
                      return /* constructor */{
                              tag: "Eq",
                              Arg0: /* array */[
                                43,
                                86
                              ],
                              Arg1: Js_dict.values({
                                    foo: 43,
                                    bar: 86
                                  })
                            };
                    })
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "fromList - []",
                    (function (param) {
                        return /* constructor */{
                                tag: "Eq",
                                Arg0: { },
                                Arg1: Js_dict.fromList("[]")
                              };
                      })
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "fromList",
                      (function (param) {
                          return /* constructor */{
                                  tag: "Eq",
                                  Arg0: /* array */[
                                    /* tuple */[
                                      "x",
                                      23
                                    ],
                                    /* tuple */[
                                      "y",
                                      46
                                    ]
                                  ],
                                  Arg1: Js_dict.entries(Js_dict.fromList(/* constructor */{
                                            tag: "::",
                                            Arg0: /* tuple */[
                                              "x",
                                              23
                                            ],
                                            Arg1: /* constructor */{
                                              tag: "::",
                                              Arg0: /* tuple */[
                                                "y",
                                                46
                                              ],
                                              Arg1: "[]"
                                            }
                                          }))
                                };
                        })
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "fromArray - []",
                        (function (param) {
                            return /* constructor */{
                                    tag: "Eq",
                                    Arg0: { },
                                    Arg1: Js_dict.fromArray(/* array */[])
                                  };
                          })
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          "fromArray",
                          (function (param) {
                              return /* constructor */{
                                      tag: "Eq",
                                      Arg0: /* array */[
                                        /* tuple */[
                                          "x",
                                          23
                                        ],
                                        /* tuple */[
                                          "y",
                                          46
                                        ]
                                      ],
                                      Arg1: Js_dict.entries(Js_dict.fromArray(/* array */[
                                                /* tuple */[
                                                  "x",
                                                  23
                                                ],
                                                /* tuple */[
                                                  "y",
                                                  46
                                                ]
                                              ]))
                                    };
                            })
                        ],
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            "map",
                            (function (param) {
                                return /* constructor */{
                                        tag: "Eq",
                                        Arg0: {
                                          foo: "43",
                                          bar: "86"
                                        },
                                        Arg1: Js_dict.map((function (i) {
                                                return String(i);
                                              }), {
                                              foo: 43,
                                              bar: 86
                                            })
                                      };
                              })
                          ],
                          Arg1: "[]"
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};

Mt.from_pair_suites("Js_dict_test", suites);

exports.obj = obj;
exports.suites = suites;
/*  Not a pure module */
