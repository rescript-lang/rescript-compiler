'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_exn = require("../../lib/js/js_exn.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
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
  var exn = Js_exn.internalToOCamlException(raw_exn);
  function_equal_test = exn[0] === Caml_builtin_exceptions.invalid_argument && exn[1] === "equal: functional value" ? /* true */1 : /* false */0;
}

var suites_000 = /* tuple */[
  "option",
  (function () {
      return /* Eq */Block.__(0, [
                /* true */1,
                Caml_obj.caml_lessthan(/* None */0, /* Some */[1])
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "option2",
    (function () {
        return /* Eq */Block.__(0, [
                  /* true */1,
                  Caml_obj.caml_lessthan(/* Some */[1], /* Some */[2])
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "list0",
      (function () {
          return /* Eq */Block.__(0, [
                    /* true */1,
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
        (function () {
            return /* Eq */Block.__(0, [
                      /* true */1,
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
          (function () {
              return /* Eq */Block.__(0, [
                        /* true */1,
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
            (function () {
                return /* Eq */Block.__(0, [
                          /* true */1,
                          Caml_obj.caml_greaterthan(/* tuple */[
                                /* A */Block.__(0, [3]),
                                /* B */Block.__(1, [
                                    2,
                                    /* false */0
                                  ]),
                                /* C */Block.__(2, [1])
                              ], /* tuple */[
                                /* A */Block.__(0, [3]),
                                /* B */Block.__(1, [
                                    2,
                                    /* false */0
                                  ]),
                                /* C */Block.__(2, [0])
                              ])
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "custom_u2",
              (function () {
                  return /* Eq */Block.__(0, [
                            /* true */1,
                            Caml_obj.caml_equal(/* tuple */[
                                  /* A */Block.__(0, [3]),
                                  /* B */Block.__(1, [
                                      2,
                                      /* false */0
                                    ]),
                                  /* C */Block.__(2, [1])
                                ], /* tuple */[
                                  /* A */Block.__(0, [3]),
                                  /* B */Block.__(1, [
                                      2,
                                      /* false */0
                                    ]),
                                  /* C */Block.__(2, [1])
                                ])
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "function",
                (function () {
                    return /* Eq */Block.__(0, [
                              /* true */1,
                              function_equal_test
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "File \"caml_compare_test.ml\", line 17, characters 4-11",
                  (function () {
                      return /* Eq */Block.__(0, [
                                /* true */1,
                                Caml_obj.caml_lessthan(/* None */0, /* Some */[1])
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "File \"caml_compare_test.ml\", line 28, characters 4-11",
                    (function () {
                        return /* Eq */Block.__(0, [
                                  /* true */1,
                                  Caml_obj.caml_lessthan(/* None */0, /* Some */[/* int array */[
                                          1,
                                          30
                                        ]])
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "File \"caml_compare_test.ml\", line 31, characters 4-11",
                      (function () {
                          return /* Eq */Block.__(0, [
                                    /* true */1,
                                    Caml_obj.caml_greaterthan(/* Some */[/* int array */[
                                            1,
                                            30
                                          ]], /* None */0)
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "File \"caml_compare_test.ml\", line 34, characters 4-11",
                        (function () {
                            return /* Eq */Block.__(0, [
                                      /* true */1,
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
                          "File \"caml_compare_test.ml\", line 37, characters 4-11",
                          (function () {
                              return /* Eq */Block.__(0, [
                                        /* true */1,
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
                            "File \"caml_compare_test.ml\", line 41, characters 4-11",
                            (function () {
                                return /* Eq */Block.__(0, [
                                          /* false */0,
                                          +(/* None */0 === /* Some */[/* int array */[
                                                1,
                                                30
                                              ]])
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "File \"caml_compare_test.ml\", line 44, characters 4-11",
                              (function () {
                                  return /* Eq */Block.__(0, [
                                            /* false */0,
                                            +(/* Some */[/* int array */[
                                                  1,
                                                  30
                                                ]] === /* None */0)
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "File \"caml_compare_test.ml\", line 47, characters 4-11",
                                (function () {
                                    return /* Eq */Block.__(0, [
                                              /* false */0,
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
                                  "File \"caml_compare_test.ml\", line 50, characters 4-11",
                                  (function () {
                                      return /* Eq */Block.__(0, [
                                                /* false */0,
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
                                    (function () {
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
                                      (function () {
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
                                        (function () {
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
                                          (function () {
                                              return /* Eq */Block.__(0, [
                                                        Caml_obj.caml_compare(({}), ({})),
                                                        0
                                                      ]);
                                            })
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "cmp_empty2",
                                            (function () {
                                                return /* Eq */Block.__(0, [
                                                          Caml_obj.caml_compare(({}), ({x:1})),
                                                          -1
                                                        ]);
                                              })
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "cmp_swap",
                                              (function () {
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
                                                (function () {
                                                    return /* Eq */Block.__(0, [
                                                              Caml_obj.caml_compare(({x:1}), ({x:1, y:2})),
                                                              -1
                                                            ]);
                                                  })
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "cmp_size2",
                                                  (function () {
                                                      return /* Eq */Block.__(0, [
                                                                Caml_obj.caml_compare(({x:1, y:2}), ({x:1})),
                                                                1
                                                              ]);
                                                    })
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "cmp_order",
                                                    (function () {
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
                                                      (function () {
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
                                                        (function () {
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
                                                          (function () {
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
                                                            (function () {
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
                                                              (function () {
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
                                                                (function () {
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
                                                                  (function () {
                                                                      return /* Eq */Block.__(0, [
                                                                                Caml_obj.caml_equal({
                                                                                      x: 1
                                                                                    }, {
                                                                                      x: 2
                                                                                    }),
                                                                                /* false */0
                                                                              ]);
                                                                    })
                                                                ],
                                                                /* :: */[
                                                                  /* tuple */[
                                                                    "eq_val2",
                                                                    (function () {
                                                                        return /* Eq */Block.__(0, [
                                                                                  Caml_obj.caml_equal({
                                                                                        x: 2
                                                                                      }, {
                                                                                        x: 1
                                                                                      }),
                                                                                  /* false */0
                                                                                ]);
                                                                      })
                                                                  ],
                                                                  /* :: */[
                                                                    /* tuple */[
                                                                      "eq_empty",
                                                                      (function () {
                                                                          return /* Eq */Block.__(0, [
                                                                                    Caml_obj.caml_equal(({}), ({})),
                                                                                    /* true */1
                                                                                  ]);
                                                                        })
                                                                    ],
                                                                    /* :: */[
                                                                      /* tuple */[
                                                                        "eq_empty2",
                                                                        (function () {
                                                                            return /* Eq */Block.__(0, [
                                                                                      Caml_obj.caml_equal(({}), ({x:1})),
                                                                                      /* false */0
                                                                                    ]);
                                                                          })
                                                                      ],
                                                                      /* :: */[
                                                                        /* tuple */[
                                                                          "eq_swap",
                                                                          (function () {
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
                                                                            (function () {
                                                                                return /* Eq */Block.__(0, [
                                                                                          Caml_obj.caml_equal(({x:1}), ({x:1, y:2})),
                                                                                          /* false */0
                                                                                        ]);
                                                                              })
                                                                          ],
                                                                          /* :: */[
                                                                            /* tuple */[
                                                                              "eq_size2",
                                                                              (function () {
                                                                                  return /* Eq */Block.__(0, [
                                                                                            Caml_obj.caml_equal(({x:1, y:2}), ({x:1})),
                                                                                            /* false */0
                                                                                          ]);
                                                                                })
                                                                            ],
                                                                            /* :: */[
                                                                              /* tuple */[
                                                                                "eq_in_list",
                                                                                (function () {
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
                                                                                              /* false */0
                                                                                            ]);
                                                                                  })
                                                                              ],
                                                                              /* :: */[
                                                                                /* tuple */[
                                                                                  "eq_in_list2",
                                                                                  (function () {
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
                                                                                                /* true */1
                                                                                              ]);
                                                                                    })
                                                                                ],
                                                                                /* :: */[
                                                                                  /* tuple */[
                                                                                    "eq_with_list",
                                                                                    (function () {
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
                                                                                                  /* true */1
                                                                                                ]);
                                                                                      })
                                                                                  ],
                                                                                  /* :: */[
                                                                                    /* tuple */[
                                                                                      "eq_with_list2",
                                                                                      (function () {
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
                                                                                                    /* false */0
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
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("caml_compare_test.ml", suites);

exports.function_equal_test = function_equal_test;
exports.suites = suites;
/* function_equal_test Not a pure module */
