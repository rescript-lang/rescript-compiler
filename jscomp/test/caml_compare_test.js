'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var function_equal_test;

try {
  function_equal_test = Caml_obj.caml_equal((function (x) {
          return x + 1 | 0;
        }), (function (x) {
          return x + 2 | 0;
        }));
}
catch (raw_exn){
  var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
  function_equal_test = exn[0] === Caml_builtin_exceptions.invalid_argument && exn[1] === "equal: functional value" ? true : false;
}

var small64 = /* int64 */{
  hi: 10,
  lo: 1494771484
};

var big64 = /* int64 */{
  hi: 134217728,
  lo: 0
};

var suites = /* record */{
  contents: /* :: */[
    /* tuple */[
      "File \"caml_compare_test.ml\", line 12, characters 4-11",
      (function (param) {
          return /* Eq */Block.__(0, [
                    true,
                    Caml_obj.caml_lessthan(undefined, 1)
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "option2",
        (function (param) {
            return /* Eq */Block.__(0, [
                      true,
                      Caml_obj.caml_lessthan(1, 2)
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "File \"caml_compare_test.ml\", line 14, characters 4-11",
          (function (param) {
              return /* Eq */Block.__(0, [
                        true,
                        Caml_obj.caml_greaterthan(/* :: */[
                              1,
                              /* [] */0
                            ], /* [] */0)
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "listeq",
            (function (param) {
                return /* Eq */Block.__(0, [
                          true,
                          Caml_obj.caml_equal(/* :: */[
                                1,
                                /* :: */[
                                  2,
                                  /* :: */[
                                    3,
                                    /* [] */0
                                  ]
                                ]
                              ], /* :: */[
                                1,
                                /* :: */[
                                  2,
                                  /* :: */[
                                    3,
                                    /* [] */0
                                  ]
                                ]
                              ])
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "listneq",
              (function (param) {
                  return /* Eq */Block.__(0, [
                            true,
                            Caml_obj.caml_greaterthan(/* :: */[
                                  1,
                                  /* :: */[
                                    2,
                                    /* :: */[
                                      3,
                                      /* [] */0
                                    ]
                                  ]
                                ], /* :: */[
                                  1,
                                  /* :: */[
                                    2,
                                    /* :: */[
                                      2,
                                      /* [] */0
                                    ]
                                  ]
                                ])
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "custom_u",
                (function (param) {
                    return /* Eq */Block.__(0, [
                              true,
                              Caml_obj.caml_greaterthan(/* tuple */[
                                    /* A */Block.__(0, [3]),
                                    /* B */Block.__(1, [
                                        2,
                                        false
                                      ]),
                                    /* C */Block.__(2, [1])
                                  ], /* tuple */[
                                    /* A */Block.__(0, [3]),
                                    /* B */Block.__(1, [
                                        2,
                                        false
                                      ]),
                                    /* C */Block.__(2, [0])
                                  ])
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "custom_u2",
                  (function (param) {
                      return /* Eq */Block.__(0, [
                                true,
                                Caml_obj.caml_equal(/* tuple */[
                                      /* A */Block.__(0, [3]),
                                      /* B */Block.__(1, [
                                          2,
                                          false
                                        ]),
                                      /* C */Block.__(2, [1])
                                    ], /* tuple */[
                                      /* A */Block.__(0, [3]),
                                      /* B */Block.__(1, [
                                          2,
                                          false
                                        ]),
                                      /* C */Block.__(2, [1])
                                    ])
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "function",
                    (function (param) {
                        return /* Eq */Block.__(0, [
                                  true,
                                  function_equal_test
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "File \"caml_compare_test.ml\", line 20, characters 4-11",
                      (function (param) {
                          return /* Eq */Block.__(0, [
                                    true,
                                    Caml_obj.caml_lessthan(undefined, 1)
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "File \"caml_compare_test.ml\", line 31, characters 4-11",
                        (function (param) {
                            return /* Eq */Block.__(0, [
                                      true,
                                      Caml_obj.caml_lessthan(undefined, /* array */[
                                            1,
                                            30
                                          ])
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "File \"caml_compare_test.ml\", line 34, characters 4-11",
                          (function (param) {
                              return /* Eq */Block.__(0, [
                                        true,
                                        Caml_obj.caml_greaterthan(/* array */[
                                              1,
                                              30
                                            ], undefined)
                                      ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "File \"caml_compare_test.ml\", line 37, characters 4-11",
                            (function (param) {
                                return /* Eq */Block.__(0, [
                                          true,
                                          Caml_obj.caml_lessthan(/* :: */[
                                                2,
                                                /* :: */[
                                                  6,
                                                  /* :: */[
                                                    1,
                                                    /* :: */[
                                                      1,
                                                      /* :: */[
                                                        2,
                                                        /* :: */[
                                                          1,
                                                          /* :: */[
                                                            4,
                                                            /* :: */[
                                                              2,
                                                              /* :: */[
                                                                1,
                                                                /* [] */0
                                                              ]
                                                            ]
                                                          ]
                                                        ]
                                                      ]
                                                    ]
                                                  ]
                                                ]
                                              ], /* :: */[
                                                2,
                                                /* :: */[
                                                  6,
                                                  /* :: */[
                                                    1,
                                                    /* :: */[
                                                      1,
                                                      /* :: */[
                                                        2,
                                                        /* :: */[
                                                          1,
                                                          /* :: */[
                                                            4,
                                                            /* :: */[
                                                              2,
                                                              /* :: */[
                                                                1,
                                                                /* :: */[
                                                                  409,
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
                                              ])
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "File \"caml_compare_test.ml\", line 40, characters 4-11",
                              (function (param) {
                                  return /* Eq */Block.__(0, [
                                            true,
                                            Caml_obj.caml_lessthan(/* :: */[
                                                  1,
                                                  /* [] */0
                                                ], /* :: */[
                                                  1,
                                                  /* :: */[
                                                    409,
                                                    /* [] */0
                                                  ]
                                                ])
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "File \"caml_compare_test.ml\", line 43, characters 4-11",
                                (function (param) {
                                    return /* Eq */Block.__(0, [
                                              true,
                                              Caml_obj.caml_lessthan(/* [] */0, /* :: */[
                                                    409,
                                                    /* [] */0
                                                  ])
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "File \"caml_compare_test.ml\", line 46, characters 4-11",
                                  (function (param) {
                                      return /* Eq */Block.__(0, [
                                                true,
                                                Caml_obj.caml_greaterthan(/* :: */[
                                                      2,
                                                      /* :: */[
                                                        6,
                                                        /* :: */[
                                                          1,
                                                          /* :: */[
                                                            1,
                                                            /* :: */[
                                                              2,
                                                              /* :: */[
                                                                1,
                                                                /* :: */[
                                                                  4,
                                                                  /* :: */[
                                                                    2,
                                                                    /* :: */[
                                                                      1,
                                                                      /* :: */[
                                                                        409,
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
                                                    ], /* :: */[
                                                      2,
                                                      /* :: */[
                                                        6,
                                                        /* :: */[
                                                          1,
                                                          /* :: */[
                                                            1,
                                                            /* :: */[
                                                              2,
                                                              /* :: */[
                                                                1,
                                                                /* :: */[
                                                                  4,
                                                                  /* :: */[
                                                                    2,
                                                                    /* :: */[
                                                                      1,
                                                                      /* [] */0
                                                                    ]
                                                                  ]
                                                                ]
                                                              ]
                                                            ]
                                                          ]
                                                        ]
                                                      ]
                                                    ])
                                              ]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "File \"caml_compare_test.ml\", line 50, characters 4-11",
                                    (function (param) {
                                        return /* Eq */Block.__(0, [
                                                  false,
                                                  false
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "File \"caml_compare_test.ml\", line 53, characters 4-11",
                                      (function (param) {
                                          return /* Eq */Block.__(0, [
                                                    false,
                                                    false
                                                  ]);
                                        })
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "File \"caml_compare_test.ml\", line 56, characters 4-11",
                                        (function (param) {
                                            return /* Eq */Block.__(0, [
                                                      false,
                                                      Caml_obj.caml_equal(/* :: */[
                                                            2,
                                                            /* :: */[
                                                              6,
                                                              /* :: */[
                                                                1,
                                                                /* :: */[
                                                                  1,
                                                                  /* :: */[
                                                                    2,
                                                                    /* :: */[
                                                                      1,
                                                                      /* :: */[
                                                                        4,
                                                                        /* :: */[
                                                                          2,
                                                                          /* :: */[
                                                                            1,
                                                                            /* [] */0
                                                                          ]
                                                                        ]
                                                                      ]
                                                                    ]
                                                                  ]
                                                                ]
                                                              ]
                                                            ]
                                                          ], /* :: */[
                                                            2,
                                                            /* :: */[
                                                              6,
                                                              /* :: */[
                                                                1,
                                                                /* :: */[
                                                                  1,
                                                                  /* :: */[
                                                                    2,
                                                                    /* :: */[
                                                                      1,
                                                                      /* :: */[
                                                                        4,
                                                                        /* :: */[
                                                                          2,
                                                                          /* :: */[
                                                                            1,
                                                                            /* :: */[
                                                                              409,
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
                                                          ])
                                                    ]);
                                          })
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "File \"caml_compare_test.ml\", line 59, characters 4-11",
                                          (function (param) {
                                              return /* Eq */Block.__(0, [
                                                        false,
                                                        Caml_obj.caml_equal(/* :: */[
                                                              2,
                                                              /* :: */[
                                                                6,
                                                                /* :: */[
                                                                  1,
                                                                  /* :: */[
                                                                    1,
                                                                    /* :: */[
                                                                      2,
                                                                      /* :: */[
                                                                        1,
                                                                        /* :: */[
                                                                          4,
                                                                          /* :: */[
                                                                            2,
                                                                            /* :: */[
                                                                              1,
                                                                              /* :: */[
                                                                                409,
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
                                                            ], /* :: */[
                                                              2,
                                                              /* :: */[
                                                                6,
                                                                /* :: */[
                                                                  1,
                                                                  /* :: */[
                                                                    1,
                                                                    /* :: */[
                                                                      2,
                                                                      /* :: */[
                                                                        1,
                                                                        /* :: */[
                                                                          4,
                                                                          /* :: */[
                                                                            2,
                                                                            /* :: */[
                                                                              1,
                                                                              /* [] */0
                                                                            ]
                                                                          ]
                                                                        ]
                                                                      ]
                                                                    ]
                                                                  ]
                                                                ]
                                                              ]
                                                            ])
                                                      ]);
                                            })
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "cmp_id",
                                            (function (param) {
                                                return /* Eq */Block.__(0, [
                                                          Caml_obj.caml_compare({
                                                                x: 1,
                                                                y: 2
                                                              }, {
                                                                x: 1,
                                                                y: 2
                                                              }),
                                                          0
                                                        ]);
                                              })
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "cmp_val",
                                              (function (param) {
                                                  return /* Eq */Block.__(0, [
                                                            Caml_obj.caml_compare({
                                                                  x: 1
                                                                }, {
                                                                  x: 2
                                                                }),
                                                            -1
                                                          ]);
                                                })
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "cmp_val2",
                                                (function (param) {
                                                    return /* Eq */Block.__(0, [
                                                              Caml_obj.caml_compare({
                                                                    x: 2
                                                                  }, {
                                                                    x: 1
                                                                  }),
                                                              1
                                                            ]);
                                                  })
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "cmp_empty",
                                                  (function (param) {
                                                      return /* Eq */Block.__(0, [
                                                                Caml_obj.caml_compare(({}), ({})),
                                                                0
                                                              ]);
                                                    })
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "cmp_empty2",
                                                    (function (param) {
                                                        return /* Eq */Block.__(0, [
                                                                  Caml_obj.caml_compare(({}), ({x:1})),
                                                                  -1
                                                                ]);
                                                      })
                                                  ],
                                                  /* :: */[
                                                    /* tuple */[
                                                      "cmp_swap",
                                                      (function (param) {
                                                          return /* Eq */Block.__(0, [
                                                                    Caml_obj.caml_compare({
                                                                          x: 1,
                                                                          y: 2
                                                                        }, {
                                                                          y: 2,
                                                                          x: 1
                                                                        }),
                                                                    0
                                                                  ]);
                                                        })
                                                    ],
                                                    /* :: */[
                                                      /* tuple */[
                                                        "cmp_size",
                                                        (function (param) {
                                                            return /* Eq */Block.__(0, [
                                                                      Caml_obj.caml_compare(({x:1}), ({x:1, y:2})),
                                                                      -1
                                                                    ]);
                                                          })
                                                      ],
                                                      /* :: */[
                                                        /* tuple */[
                                                          "cmp_size2",
                                                          (function (param) {
                                                              return /* Eq */Block.__(0, [
                                                                        Caml_obj.caml_compare(({x:1, y:2}), ({x:1})),
                                                                        1
                                                                      ]);
                                                            })
                                                        ],
                                                        /* :: */[
                                                          /* tuple */[
                                                            "cmp_order",
                                                            (function (param) {
                                                                return /* Eq */Block.__(0, [
                                                                          Caml_obj.caml_compare({
                                                                                x: 0,
                                                                                y: 1
                                                                              }, {
                                                                                x: 1,
                                                                                y: 0
                                                                              }),
                                                                          -1
                                                                        ]);
                                                              })
                                                          ],
                                                          /* :: */[
                                                            /* tuple */[
                                                              "cmp_order2",
                                                              (function (param) {
                                                                  return /* Eq */Block.__(0, [
                                                                            Caml_obj.caml_compare({
                                                                                  x: 1,
                                                                                  y: 0
                                                                                }, {
                                                                                  x: 0,
                                                                                  y: 1
                                                                                }),
                                                                            1
                                                                          ]);
                                                                })
                                                            ],
                                                            /* :: */[
                                                              /* tuple */[
                                                                "cmp_in_list",
                                                                (function (param) {
                                                                    return /* Eq */Block.__(0, [
                                                                              Caml_obj.caml_compare(/* :: */[
                                                                                    {
                                                                                      x: 1
                                                                                    },
                                                                                    /* [] */0
                                                                                  ], /* :: */[
                                                                                    {
                                                                                      x: 2
                                                                                    },
                                                                                    /* [] */0
                                                                                  ]),
                                                                              -1
                                                                            ]);
                                                                  })
                                                              ],
                                                              /* :: */[
                                                                /* tuple */[
                                                                  "cmp_in_list2",
                                                                  (function (param) {
                                                                      return /* Eq */Block.__(0, [
                                                                                Caml_obj.caml_compare(/* :: */[
                                                                                      {
                                                                                        x: 2
                                                                                      },
                                                                                      /* [] */0
                                                                                    ], /* :: */[
                                                                                      {
                                                                                        x: 1
                                                                                      },
                                                                                      /* [] */0
                                                                                    ]),
                                                                                1
                                                                              ]);
                                                                    })
                                                                ],
                                                                /* :: */[
                                                                  /* tuple */[
                                                                    "cmp_with_list",
                                                                    (function (param) {
                                                                        return /* Eq */Block.__(0, [
                                                                                  Caml_obj.caml_compare({
                                                                                        x: /* :: */[
                                                                                          0,
                                                                                          /* [] */0
                                                                                        ]
                                                                                      }, {
                                                                                        x: /* :: */[
                                                                                          1,
                                                                                          /* [] */0
                                                                                        ]
                                                                                      }),
                                                                                  -1
                                                                                ]);
                                                                      })
                                                                  ],
                                                                  /* :: */[
                                                                    /* tuple */[
                                                                      "cmp_with_list2",
                                                                      (function (param) {
                                                                          return /* Eq */Block.__(0, [
                                                                                    Caml_obj.caml_compare({
                                                                                          x: /* :: */[
                                                                                            1,
                                                                                            /* [] */0
                                                                                          ]
                                                                                        }, {
                                                                                          x: /* :: */[
                                                                                            0,
                                                                                            /* [] */0
                                                                                          ]
                                                                                        }),
                                                                                    1
                                                                                  ]);
                                                                        })
                                                                    ],
                                                                    /* :: */[
                                                                      /* tuple */[
                                                                        "eq_id",
                                                                        (function (param) {
                                                                            return /* Ok */Block.__(4, [Caml_obj.caml_equal({
                                                                                            x: 1,
                                                                                            y: 2
                                                                                          }, {
                                                                                            x: 1,
                                                                                            y: 2
                                                                                          })]);
                                                                          })
                                                                      ],
                                                                      /* :: */[
                                                                        /* tuple */[
                                                                          "eq_val",
                                                                          (function (param) {
                                                                              return /* Eq */Block.__(0, [
                                                                                        Caml_obj.caml_equal({
                                                                                              x: 1
                                                                                            }, {
                                                                                              x: 2
                                                                                            }),
                                                                                        false
                                                                                      ]);
                                                                            })
                                                                        ],
                                                                        /* :: */[
                                                                          /* tuple */[
                                                                            "eq_val2",
                                                                            (function (param) {
                                                                                return /* Eq */Block.__(0, [
                                                                                          Caml_obj.caml_equal({
                                                                                                x: 2
                                                                                              }, {
                                                                                                x: 1
                                                                                              }),
                                                                                          false
                                                                                        ]);
                                                                              })
                                                                          ],
                                                                          /* :: */[
                                                                            /* tuple */[
                                                                              "eq_empty",
                                                                              (function (param) {
                                                                                  return /* Eq */Block.__(0, [
                                                                                            Caml_obj.caml_equal(({}), ({})),
                                                                                            true
                                                                                          ]);
                                                                                })
                                                                            ],
                                                                            /* :: */[
                                                                              /* tuple */[
                                                                                "eq_empty2",
                                                                                (function (param) {
                                                                                    return /* Eq */Block.__(0, [
                                                                                              Caml_obj.caml_equal(({}), ({x:1})),
                                                                                              false
                                                                                            ]);
                                                                                  })
                                                                              ],
                                                                              /* :: */[
                                                                                /* tuple */[
                                                                                  "eq_swap",
                                                                                  (function (param) {
                                                                                      return /* Ok */Block.__(4, [Caml_obj.caml_equal({
                                                                                                      x: 1,
                                                                                                      y: 2
                                                                                                    }, {
                                                                                                      y: 2,
                                                                                                      x: 1
                                                                                                    })]);
                                                                                    })
                                                                                ],
                                                                                /* :: */[
                                                                                  /* tuple */[
                                                                                    "eq_size",
                                                                                    (function (param) {
                                                                                        return /* Eq */Block.__(0, [
                                                                                                  Caml_obj.caml_equal(({x:1}), ({x:1, y:2})),
                                                                                                  false
                                                                                                ]);
                                                                                      })
                                                                                  ],
                                                                                  /* :: */[
                                                                                    /* tuple */[
                                                                                      "eq_size2",
                                                                                      (function (param) {
                                                                                          return /* Eq */Block.__(0, [
                                                                                                    Caml_obj.caml_equal(({x:1, y:2}), ({x:1})),
                                                                                                    false
                                                                                                  ]);
                                                                                        })
                                                                                    ],
                                                                                    /* :: */[
                                                                                      /* tuple */[
                                                                                        "eq_in_list",
                                                                                        (function (param) {
                                                                                            return /* Eq */Block.__(0, [
                                                                                                      Caml_obj.caml_equal(/* :: */[
                                                                                                            {
                                                                                                              x: 1
                                                                                                            },
                                                                                                            /* [] */0
                                                                                                          ], /* :: */[
                                                                                                            {
                                                                                                              x: 2
                                                                                                            },
                                                                                                            /* [] */0
                                                                                                          ]),
                                                                                                      false
                                                                                                    ]);
                                                                                          })
                                                                                      ],
                                                                                      /* :: */[
                                                                                        /* tuple */[
                                                                                          "eq_in_list2",
                                                                                          (function (param) {
                                                                                              return /* Eq */Block.__(0, [
                                                                                                        Caml_obj.caml_equal(/* :: */[
                                                                                                              {
                                                                                                                x: 2
                                                                                                              },
                                                                                                              /* [] */0
                                                                                                            ], /* :: */[
                                                                                                              {
                                                                                                                x: 2
                                                                                                              },
                                                                                                              /* [] */0
                                                                                                            ]),
                                                                                                        true
                                                                                                      ]);
                                                                                            })
                                                                                        ],
                                                                                        /* :: */[
                                                                                          /* tuple */[
                                                                                            "eq_with_list",
                                                                                            (function (param) {
                                                                                                return /* Eq */Block.__(0, [
                                                                                                          Caml_obj.caml_equal({
                                                                                                                x: /* :: */[
                                                                                                                  0,
                                                                                                                  /* [] */0
                                                                                                                ]
                                                                                                              }, {
                                                                                                                x: /* :: */[
                                                                                                                  0,
                                                                                                                  /* [] */0
                                                                                                                ]
                                                                                                              }),
                                                                                                          true
                                                                                                        ]);
                                                                                              })
                                                                                          ],
                                                                                          /* :: */[
                                                                                            /* tuple */[
                                                                                              "eq_with_list2",
                                                                                              (function (param) {
                                                                                                  return /* Eq */Block.__(0, [
                                                                                                            Caml_obj.caml_equal({
                                                                                                                  x: /* :: */[
                                                                                                                    0,
                                                                                                                    /* [] */0
                                                                                                                  ]
                                                                                                                }, {
                                                                                                                  x: /* :: */[
                                                                                                                    1,
                                                                                                                    /* [] */0
                                                                                                                  ]
                                                                                                                }),
                                                                                                            false
                                                                                                          ]);
                                                                                                })
                                                                                            ],
                                                                                            /* :: */[
                                                                                              /* tuple */[
                                                                                                "File \"caml_compare_test.ml\", line 90, characters 4-11",
                                                                                                (function (param) {
                                                                                                    return /* Eq */Block.__(0, [
                                                                                                              Caml_obj.caml_compare(null, /* :: */[
                                                                                                                    3,
                                                                                                                    /* [] */0
                                                                                                                  ]),
                                                                                                              -1
                                                                                                            ]);
                                                                                                  })
                                                                                              ],
                                                                                              /* :: */[
                                                                                                /* tuple */[
                                                                                                  "File \"caml_compare_test.ml\", line 93, characters 4-11",
                                                                                                  (function (param) {
                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                Caml_obj.caml_compare(/* :: */[
                                                                                                                      3,
                                                                                                                      /* [] */0
                                                                                                                    ], null),
                                                                                                                1
                                                                                                              ]);
                                                                                                    })
                                                                                                ],
                                                                                                /* :: */[
                                                                                                  /* tuple */[
                                                                                                    "File \"caml_compare_test.ml\", line 96, characters 4-11",
                                                                                                    (function (param) {
                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                  Caml_obj.caml_compare(null, 0),
                                                                                                                  -1
                                                                                                                ]);
                                                                                                      })
                                                                                                  ],
                                                                                                  /* :: */[
                                                                                                    /* tuple */[
                                                                                                      "File \"caml_compare_test.ml\", line 99, characters 4-11",
                                                                                                      (function (param) {
                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                    Caml_obj.caml_compare(0, null),
                                                                                                                    1
                                                                                                                  ]);
                                                                                                        })
                                                                                                    ],
                                                                                                    /* :: */[
                                                                                                      /* tuple */[
                                                                                                        "File \"caml_compare_test.ml\", line 102, characters 4-11",
                                                                                                        (function (param) {
                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                      Caml_obj.caml_compare(undefined, 0),
                                                                                                                      -1
                                                                                                                    ]);
                                                                                                          })
                                                                                                      ],
                                                                                                      /* :: */[
                                                                                                        /* tuple */[
                                                                                                          "File \"caml_compare_test.ml\", line 105, characters 4-11",
                                                                                                          (function (param) {
                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                        Caml_obj.caml_compare(0, undefined),
                                                                                                                        1
                                                                                                                      ]);
                                                                                                            })
                                                                                                        ],
                                                                                                        /* :: */[
                                                                                                          /* tuple */[
                                                                                                            "cmp_int64a",
                                                                                                            (function (param) {
                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                          Caml_int64.compare(/* int64 */{
                                                                                                                                hi: 0,
                                                                                                                                lo: 1
                                                                                                                              }, /* int64 */{
                                                                                                                                hi: 10,
                                                                                                                                lo: 1494771484
                                                                                                                              }),
                                                                                                                          -1
                                                                                                                        ]);
                                                                                                              })
                                                                                                          ],
                                                                                                          /* :: */[
                                                                                                            /* tuple */[
                                                                                                              "cmp_int64b",
                                                                                                              (function (param) {
                                                                                                                  return /* Eq */Block.__(0, [
                                                                                                                            Caml_int64.compare(/* int64 */{
                                                                                                                                  hi: 10,
                                                                                                                                  lo: 1494771484
                                                                                                                                }, /* int64 */{
                                                                                                                                  hi: 0,
                                                                                                                                  lo: 1
                                                                                                                                }),
                                                                                                                            1
                                                                                                                          ]);
                                                                                                                })
                                                                                                            ],
                                                                                                            /* :: */[
                                                                                                              /* tuple */[
                                                                                                                "cmp_int64c",
                                                                                                                (function (param) {
                                                                                                                    return /* Eq */Block.__(0, [
                                                                                                                              Caml_int64.compare(small64, big64),
                                                                                                                              -1
                                                                                                                            ]);
                                                                                                                  })
                                                                                                              ],
                                                                                                              /* :: */[
                                                                                                                /* tuple */[
                                                                                                                  "cmp_int64d",
                                                                                                                  (function (param) {
                                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                                Caml_int64.compare(big64, small64),
                                                                                                                                1
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
      ]
    ]
  ]
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

eq("File \"caml_compare_test.ml\", line 120, characters 6-13", true, Caml_obj.caml_greaterthan(1, undefined));

eq("File \"caml_compare_test.ml\", line 121, characters 6-13", true, Caml_obj.caml_lessthan(/* [] */0, /* :: */[
          1,
          /* [] */0
        ]));

eq("File \"caml_compare_test.ml\", line 122, characters 6-13", false, Caml_obj.caml_greaterthan(undefined, 1));

eq("File \"caml_compare_test.ml\", line 123, characters 6-13", false, Caml_obj.caml_greaterthan(undefined, /* array */[
          1,
          30
        ]));

eq("File \"caml_compare_test.ml\", line 124, characters 6-13", false, Caml_obj.caml_lessthan(/* array */[
          1,
          30
        ], undefined));

Mt.from_pair_suites("Caml_compare_test", suites.contents);

exports.function_equal_test = function_equal_test;
exports.small64 = small64;
exports.big64 = big64;
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/* function_equal_test Not a pure module */
