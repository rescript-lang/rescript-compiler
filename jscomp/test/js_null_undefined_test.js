'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Js_null_undefined = require("../../lib/js/js_null_undefined.js");

var suites_000 = /* tuple */[
  "toOption - null",
  (function (param) {
      return /* Eq */Block.__(0, [
                undefined,
                undefined
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "toOption - undefined",
    (function (param) {
        return /* Eq */Block.__(0, [
                  undefined,
                  undefined
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "toOption - empty",
      (function (param) {
          return /* Eq */Block.__(0, [
                    undefined,
                    undefined
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "toOption - 'a",
        (function (param) {
            return /* Eq */Block.__(0, [
                      "foo",
                      Caml_option.nullable_to_opt("foo")
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "return",
          (function (param) {
              return /* Eq */Block.__(0, [
                        "something",
                        Caml_option.nullable_to_opt("something")
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "test - null",
            (function (param) {
                return /* Eq */Block.__(0, [
                          true,
                          true
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "test - undefined",
              (function (param) {
                  return /* Eq */Block.__(0, [
                            true,
                            true
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "test - empty",
                (function (param) {
                    return /* Eq */Block.__(0, [
                              true,
                              true
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "test - 'a",
                  (function (param) {
                      return /* Eq */Block.__(0, [
                                false,
                                false
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "bind - null",
                    (function (param) {
                        return /* StrictEq */Block.__(2, [
                                  null,
                                  Js_null_undefined.bind(null, (function (v) {
                                          return v;
                                        }))
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "bind - undefined",
                      (function (param) {
                          return /* StrictEq */Block.__(2, [
                                    undefined,
                                    Js_null_undefined.bind(undefined, (function (v) {
                                            return v;
                                          }))
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "bind - empty",
                        (function (param) {
                            return /* StrictEq */Block.__(2, [
                                      undefined,
                                      Js_null_undefined.bind(undefined, (function (v) {
                                              return v;
                                            }))
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "bind - 'a",
                          (function (param) {
                              return /* Eq */Block.__(0, [
                                        4,
                                        Js_null_undefined.bind(2, (function (n) {
                                                return (n << 1);
                                              }))
                                      ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "iter - null",
                            (function (param) {
                                var hit = /* record */{
                                  contents: false
                                };
                                Js_null_undefined.iter(null, (function (param) {
                                        hit.contents = true;
                                        return /* () */0;
                                      }));
                                return /* Eq */Block.__(0, [
                                          false,
                                          hit.contents
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "iter - undefined",
                              (function (param) {
                                  var hit = /* record */{
                                    contents: false
                                  };
                                  Js_null_undefined.iter(undefined, (function (param) {
                                          hit.contents = true;
                                          return /* () */0;
                                        }));
                                  return /* Eq */Block.__(0, [
                                            false,
                                            hit.contents
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "iter - empty",
                                (function (param) {
                                    var hit = /* record */{
                                      contents: false
                                    };
                                    Js_null_undefined.iter(undefined, (function (param) {
                                            hit.contents = true;
                                            return /* () */0;
                                          }));
                                    return /* Eq */Block.__(0, [
                                              false,
                                              hit.contents
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "iter - 'a",
                                  (function (param) {
                                      var hit = /* record */{
                                        contents: 0
                                      };
                                      Js_null_undefined.iter(2, (function (v) {
                                              hit.contents = v;
                                              return /* () */0;
                                            }));
                                      return /* Eq */Block.__(0, [
                                                2,
                                                hit.contents
                                              ]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "fromOption - None",
                                    (function (param) {
                                        return /* Eq */Block.__(0, [
                                                  undefined,
                                                  Js_null_undefined.fromOption(undefined)
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "fromOption - Some",
                                      (function (param) {
                                          return /* Eq */Block.__(0, [
                                                    2,
                                                    Js_null_undefined.fromOption(2)
                                                  ]);
                                        })
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "null <> undefined",
                                        (function (param) {
                                            return /* Ok */Block.__(4, [true]);
                                          })
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "null <> empty",
                                          (function (param) {
                                              return /* Ok */Block.__(4, [true]);
                                            })
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "undefined = empty",
                                            (function (param) {
                                                return /* Ok */Block.__(4, [true]);
                                              })
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "File \"js_null_undefined_test.ml\", line 42, characters 2-9",
                                              (function (param) {
                                                  return /* Ok */Block.__(4, [true]);
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

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("Js_null_undefined_test", suites);

exports.suites = suites;
/*  Not a pure module */
