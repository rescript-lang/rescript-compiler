'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_vector = require("../../lib/js/js_vector.js");
var Js_primitive = require("../../lib/js/js_primitive.js");

var suites_000 = /* tuple */[
  "File \"js_array_test.ml\", line 3, characters 4-11",
  (function () {
      var x = /* array */[
        1,
        2,
        3,
        4,
        5
      ];
      return /* Eq */Block.__(0, [
                /* array */[
                  2,
                  4
                ],
                (Js_vector.filterInPlace((function (x) {
                          return x % 2 === 0;
                        }), x), x)
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "File \"js_array_test.ml\", line 11, characters 4-11",
    (function () {
        var x = /* array */[
          1,
          2,
          3,
          4,
          5
        ];
        return /* Eq */Block.__(0, [
                  true,
                  (Js_vector.filterInPlace((function (x) {
                            return x > 10;
                          }), x), x.length === 0)
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "isArray_array",
      (function () {
          return /* Eq */Block.__(0, [
                    true,
                    Array.isArray(/* array */[])
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "isArray_int",
        (function () {
            return /* Eq */Block.__(0, [
                      false,
                      Array.isArray(34)
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "length",
          (function () {
              return /* Eq */Block.__(0, [
                        3,
                        /* array */[
                          1,
                          2,
                          3
                        ].length
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "copyWithin",
            (function () {
                return /* Eq */Block.__(0, [
                          /* array */[
                            1,
                            2,
                            3,
                            1,
                            2
                          ],
                          /* array */[
                              1,
                              2,
                              3,
                              4,
                              5
                            ].copyWithin(-2)
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "copyWithinFrom",
              (function () {
                  return /* Eq */Block.__(0, [
                            /* array */[
                              4,
                              5,
                              3,
                              4,
                              5
                            ],
                            /* array */[
                                1,
                                2,
                                3,
                                4,
                                5
                              ].copyWithin(0, 3)
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "copyWithinFromRange",
                (function () {
                    return /* Eq */Block.__(0, [
                              /* array */[
                                4,
                                2,
                                3,
                                4,
                                5
                              ],
                              /* array */[
                                  1,
                                  2,
                                  3,
                                  4,
                                  5
                                ].copyWithin(0, 3, 4)
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "fillInPlace",
                  (function () {
                      return /* Eq */Block.__(0, [
                                /* array */[
                                  4,
                                  4,
                                  4
                                ],
                                /* array */[
                                    1,
                                    2,
                                    3
                                  ].fill(4)
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "fillFromInPlace",
                    (function () {
                        return /* Eq */Block.__(0, [
                                  /* array */[
                                    1,
                                    4,
                                    4
                                  ],
                                  /* array */[
                                      1,
                                      2,
                                      3
                                    ].fill(4, 1)
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "fillRangeInPlace",
                      (function () {
                          return /* Eq */Block.__(0, [
                                    /* array */[
                                      1,
                                      4,
                                      3
                                    ],
                                    /* array */[
                                        1,
                                        2,
                                        3
                                      ].fill(4, 1, 2)
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "pop",
                        (function () {
                            return /* Eq */Block.__(0, [
                                      /* Some */[3],
                                      Js_primitive.undefined_to_opt(/* array */[
                                              1,
                                              2,
                                              3
                                            ].pop())
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "pop - empty array",
                          (function () {
                              return /* Eq */Block.__(0, [
                                        /* None */0,
                                        Js_primitive.undefined_to_opt(/* array */[].pop())
                                      ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "push",
                            (function () {
                                return /* Eq */Block.__(0, [
                                          4,
                                          /* array */[
                                              1,
                                              2,
                                              3
                                            ].push(4)
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "pushMany",
                              (function () {
                                  return /* Eq */Block.__(0, [
                                            5,
                                            /* array */[
                                                1,
                                                2,
                                                3
                                              ].push(4, 5)
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "reverseInPlace",
                                (function () {
                                    return /* Eq */Block.__(0, [
                                              /* array */[
                                                3,
                                                2,
                                                1
                                              ],
                                              /* array */[
                                                  1,
                                                  2,
                                                  3
                                                ].reverse()
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "shift",
                                  (function () {
                                      return /* Eq */Block.__(0, [
                                                /* Some */[1],
                                                Js_primitive.undefined_to_opt(/* array */[
                                                        1,
                                                        2,
                                                        3
                                                      ].shift())
                                              ]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "shift - empty array",
                                    (function () {
                                        return /* Eq */Block.__(0, [
                                                  /* None */0,
                                                  Js_primitive.undefined_to_opt(/* array */[].shift())
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "sortInPlace",
                                      (function () {
                                          return /* Eq */Block.__(0, [
                                                    /* array */[
                                                      1,
                                                      2,
                                                      3
                                                    ],
                                                    /* array */[
                                                        3,
                                                        1,
                                                        2
                                                      ].sort()
                                                  ]);
                                        })
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "sortInPlaceWith",
                                        (function () {
                                            return /* Eq */Block.__(0, [
                                                      /* array */[
                                                        3,
                                                        2,
                                                        1
                                                      ],
                                                      /* array */[
                                                          3,
                                                          1,
                                                          2
                                                        ].sort((function (a, b) {
                                                              return b - a | 0;
                                                            }))
                                                    ]);
                                          })
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "spliceInPlace",
                                          (function () {
                                              var arr = /* array */[
                                                1,
                                                2,
                                                3,
                                                4
                                              ];
                                              var removed = arr.splice(2, 0, 5);
                                              return /* Eq */Block.__(0, [
                                                        /* tuple */[
                                                          /* array */[
                                                            1,
                                                            2,
                                                            5,
                                                            3,
                                                            4
                                                          ],
                                                          /* array */[]
                                                        ],
                                                        /* tuple */[
                                                          arr,
                                                          removed
                                                        ]
                                                      ]);
                                            })
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "removeFromInPlace",
                                            (function () {
                                                var arr = /* array */[
                                                  1,
                                                  2,
                                                  3,
                                                  4
                                                ];
                                                var removed = arr.splice(2);
                                                return /* Eq */Block.__(0, [
                                                          /* tuple */[
                                                            /* array */[
                                                              1,
                                                              2
                                                            ],
                                                            /* array */[
                                                              3,
                                                              4
                                                            ]
                                                          ],
                                                          /* tuple */[
                                                            arr,
                                                            removed
                                                          ]
                                                        ]);
                                              })
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "removeCountInPlace",
                                              (function () {
                                                  var arr = /* array */[
                                                    1,
                                                    2,
                                                    3,
                                                    4
                                                  ];
                                                  var removed = arr.splice(2, 1);
                                                  return /* Eq */Block.__(0, [
                                                            /* tuple */[
                                                              /* array */[
                                                                1,
                                                                2,
                                                                4
                                                              ],
                                                              /* array */[3]
                                                            ],
                                                            /* tuple */[
                                                              arr,
                                                              removed
                                                            ]
                                                          ]);
                                                })
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "unshift",
                                                (function () {
                                                    return /* Eq */Block.__(0, [
                                                              4,
                                                              /* array */[
                                                                  1,
                                                                  2,
                                                                  3
                                                                ].unshift(4)
                                                            ]);
                                                  })
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "unshiftMany",
                                                  (function () {
                                                      return /* Eq */Block.__(0, [
                                                                5,
                                                                /* array */[
                                                                    1,
                                                                    2,
                                                                    3
                                                                  ].unshift(4, 5)
                                                              ]);
                                                    })
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "append",
                                                    (function () {
                                                        return /* Eq */Block.__(0, [
                                                                  /* array */[
                                                                    1,
                                                                    2,
                                                                    3,
                                                                    4
                                                                  ],
                                                                  /* array */[
                                                                      1,
                                                                      2,
                                                                      3
                                                                    ].concat(4)
                                                                ]);
                                                      })
                                                  ],
                                                  /* :: */[
                                                    /* tuple */[
                                                      "concat",
                                                      (function () {
                                                          return /* Eq */Block.__(0, [
                                                                    /* array */[
                                                                      1,
                                                                      2,
                                                                      3,
                                                                      4,
                                                                      5
                                                                    ],
                                                                    /* array */[
                                                                        1,
                                                                        2,
                                                                        3
                                                                      ].concat(/* array */[
                                                                          4,
                                                                          5
                                                                        ])
                                                                  ]);
                                                        })
                                                    ],
                                                    /* :: */[
                                                      /* tuple */[
                                                        "concatMany",
                                                        (function () {
                                                            return /* Eq */Block.__(0, [
                                                                      /* array */[
                                                                        1,
                                                                        2,
                                                                        3,
                                                                        4,
                                                                        5,
                                                                        6,
                                                                        7
                                                                      ],
                                                                      /* array */[
                                                                          1,
                                                                          2,
                                                                          3
                                                                        ].concat(/* array */[
                                                                            4,
                                                                            5
                                                                          ], /* array */[
                                                                            6,
                                                                            7
                                                                          ])
                                                                    ]);
                                                          })
                                                      ],
                                                      /* :: */[
                                                        /* tuple */[
                                                          "includes",
                                                          (function () {
                                                              return /* Eq */Block.__(0, [
                                                                        true,
                                                                        /* array */[
                                                                            1,
                                                                            2,
                                                                            3
                                                                          ].includes(3)
                                                                      ]);
                                                            })
                                                        ],
                                                        /* :: */[
                                                          /* tuple */[
                                                            "indexOf",
                                                            (function () {
                                                                return /* Eq */Block.__(0, [
                                                                          1,
                                                                          /* array */[
                                                                              1,
                                                                              2,
                                                                              3
                                                                            ].indexOf(2)
                                                                        ]);
                                                              })
                                                          ],
                                                          /* :: */[
                                                            /* tuple */[
                                                              "indexOfFrom",
                                                              (function () {
                                                                  return /* Eq */Block.__(0, [
                                                                            3,
                                                                            /* array */[
                                                                                1,
                                                                                2,
                                                                                3,
                                                                                2
                                                                              ].indexOf(2, 2)
                                                                          ]);
                                                                })
                                                            ],
                                                            /* :: */[
                                                              /* tuple */[
                                                                "join",
                                                                (function () {
                                                                    return /* Eq */Block.__(0, [
                                                                              "1,2,3",
                                                                              /* array */[
                                                                                  1,
                                                                                  2,
                                                                                  3
                                                                                ].join()
                                                                            ]);
                                                                  })
                                                              ],
                                                              /* :: */[
                                                                /* tuple */[
                                                                  "joinWith",
                                                                  (function () {
                                                                      return /* Eq */Block.__(0, [
                                                                                "1;2;3",
                                                                                /* array */[
                                                                                    1,
                                                                                    2,
                                                                                    3
                                                                                  ].join(";")
                                                                              ]);
                                                                    })
                                                                ],
                                                                /* :: */[
                                                                  /* tuple */[
                                                                    "lastIndexOf",
                                                                    (function () {
                                                                        return /* Eq */Block.__(0, [
                                                                                  1,
                                                                                  /* array */[
                                                                                      1,
                                                                                      2,
                                                                                      3
                                                                                    ].lastIndexOf(2)
                                                                                ]);
                                                                      })
                                                                  ],
                                                                  /* :: */[
                                                                    /* tuple */[
                                                                      "lastIndexOfFrom",
                                                                      (function () {
                                                                          return /* Eq */Block.__(0, [
                                                                                    1,
                                                                                    /* array */[
                                                                                        1,
                                                                                        2,
                                                                                        3,
                                                                                        2
                                                                                      ].lastIndexOf(2, 2)
                                                                                  ]);
                                                                        })
                                                                    ],
                                                                    /* :: */[
                                                                      /* tuple */[
                                                                        "slice",
                                                                        (function () {
                                                                            return /* Eq */Block.__(0, [
                                                                                      /* array */[
                                                                                        2,
                                                                                        3
                                                                                      ],
                                                                                      /* array */[
                                                                                          1,
                                                                                          2,
                                                                                          3,
                                                                                          4,
                                                                                          5
                                                                                        ].slice(1, 3)
                                                                                    ]);
                                                                          })
                                                                      ],
                                                                      /* :: */[
                                                                        /* tuple */[
                                                                          "copy",
                                                                          (function () {
                                                                              return /* Eq */Block.__(0, [
                                                                                        /* array */[
                                                                                          1,
                                                                                          2,
                                                                                          3,
                                                                                          4,
                                                                                          5
                                                                                        ],
                                                                                        /* array */[
                                                                                            1,
                                                                                            2,
                                                                                            3,
                                                                                            4,
                                                                                            5
                                                                                          ].slice()
                                                                                      ]);
                                                                            })
                                                                        ],
                                                                        /* :: */[
                                                                          /* tuple */[
                                                                            "sliceFrom",
                                                                            (function () {
                                                                                return /* Eq */Block.__(0, [
                                                                                          /* array */[
                                                                                            3,
                                                                                            4,
                                                                                            5
                                                                                          ],
                                                                                          /* array */[
                                                                                              1,
                                                                                              2,
                                                                                              3,
                                                                                              4,
                                                                                              5
                                                                                            ].slice(2)
                                                                                        ]);
                                                                              })
                                                                          ],
                                                                          /* :: */[
                                                                            /* tuple */[
                                                                              "toString",
                                                                              (function () {
                                                                                  return /* Eq */Block.__(0, [
                                                                                            "1,2,3",
                                                                                            /* array */[
                                                                                                1,
                                                                                                2,
                                                                                                3
                                                                                              ].toString()
                                                                                          ]);
                                                                                })
                                                                            ],
                                                                            /* :: */[
                                                                              /* tuple */[
                                                                                "toLocaleString",
                                                                                (function () {
                                                                                    return /* Eq */Block.__(0, [
                                                                                              "1,2,3",
                                                                                              /* array */[
                                                                                                  1,
                                                                                                  2,
                                                                                                  3
                                                                                                ].toLocaleString()
                                                                                            ]);
                                                                                  })
                                                                              ],
                                                                              /* :: */[
                                                                                /* tuple */[
                                                                                  "every",
                                                                                  (function () {
                                                                                      return /* Eq */Block.__(0, [
                                                                                                true,
                                                                                                /* array */[
                                                                                                    1,
                                                                                                    2,
                                                                                                    3
                                                                                                  ].every((function (n) {
                                                                                                        return n > 0;
                                                                                                      }))
                                                                                              ]);
                                                                                    })
                                                                                ],
                                                                                /* :: */[
                                                                                  /* tuple */[
                                                                                    "everyi",
                                                                                    (function () {
                                                                                        return /* Eq */Block.__(0, [
                                                                                                  false,
                                                                                                  /* array */[
                                                                                                      1,
                                                                                                      2,
                                                                                                      3
                                                                                                    ].every((function (_, i) {
                                                                                                          return i > 0;
                                                                                                        }))
                                                                                                ]);
                                                                                      })
                                                                                  ],
                                                                                  /* :: */[
                                                                                    /* tuple */[
                                                                                      "filter",
                                                                                      (function () {
                                                                                          return /* Eq */Block.__(0, [
                                                                                                    /* array */[
                                                                                                      2,
                                                                                                      4
                                                                                                    ],
                                                                                                    /* array */[
                                                                                                        1,
                                                                                                        2,
                                                                                                        3,
                                                                                                        4
                                                                                                      ].filter((function (n) {
                                                                                                            return n % 2 === 0;
                                                                                                          }))
                                                                                                  ]);
                                                                                        })
                                                                                    ],
                                                                                    /* :: */[
                                                                                      /* tuple */[
                                                                                        "filteri",
                                                                                        (function () {
                                                                                            return /* Eq */Block.__(0, [
                                                                                                      /* array */[
                                                                                                        1,
                                                                                                        3
                                                                                                      ],
                                                                                                      /* array */[
                                                                                                          1,
                                                                                                          2,
                                                                                                          3,
                                                                                                          4
                                                                                                        ].filter((function (_, i) {
                                                                                                              return i % 2 === 0;
                                                                                                            }))
                                                                                                    ]);
                                                                                          })
                                                                                      ],
                                                                                      /* :: */[
                                                                                        /* tuple */[
                                                                                          "find",
                                                                                          (function () {
                                                                                              return /* Eq */Block.__(0, [
                                                                                                        /* Some */[2],
                                                                                                        Js_primitive.undefined_to_opt(/* array */[
                                                                                                                1,
                                                                                                                2,
                                                                                                                3,
                                                                                                                4
                                                                                                              ].find((function (n) {
                                                                                                                    return n % 2 === 0;
                                                                                                                  })))
                                                                                                      ]);
                                                                                            })
                                                                                        ],
                                                                                        /* :: */[
                                                                                          /* tuple */[
                                                                                            "find - no match",
                                                                                            (function () {
                                                                                                return /* Eq */Block.__(0, [
                                                                                                          /* None */0,
                                                                                                          Js_primitive.undefined_to_opt(/* array */[
                                                                                                                  1,
                                                                                                                  2,
                                                                                                                  3,
                                                                                                                  4
                                                                                                                ].find((function (n) {
                                                                                                                      return n % 2 === 5;
                                                                                                                    })))
                                                                                                        ]);
                                                                                              })
                                                                                          ],
                                                                                          /* :: */[
                                                                                            /* tuple */[
                                                                                              "findi",
                                                                                              (function () {
                                                                                                  return /* Eq */Block.__(0, [
                                                                                                            /* Some */[1],
                                                                                                            Js_primitive.undefined_to_opt(/* array */[
                                                                                                                    1,
                                                                                                                    2,
                                                                                                                    3,
                                                                                                                    4
                                                                                                                  ].find((function (_, i) {
                                                                                                                        return i % 2 === 0;
                                                                                                                      })))
                                                                                                          ]);
                                                                                                })
                                                                                            ],
                                                                                            /* :: */[
                                                                                              /* tuple */[
                                                                                                "findi - no match",
                                                                                                (function () {
                                                                                                    return /* Eq */Block.__(0, [
                                                                                                              /* None */0,
                                                                                                              Js_primitive.undefined_to_opt(/* array */[
                                                                                                                      1,
                                                                                                                      2,
                                                                                                                      3,
                                                                                                                      4
                                                                                                                    ].find((function (_, i) {
                                                                                                                          return i % 2 === 5;
                                                                                                                        })))
                                                                                                            ]);
                                                                                                  })
                                                                                              ],
                                                                                              /* :: */[
                                                                                                /* tuple */[
                                                                                                  "findIndex",
                                                                                                  (function () {
                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                1,
                                                                                                                /* array */[
                                                                                                                    1,
                                                                                                                    2,
                                                                                                                    3,
                                                                                                                    4
                                                                                                                  ].findIndex((function (n) {
                                                                                                                        return n % 2 === 0;
                                                                                                                      }))
                                                                                                              ]);
                                                                                                    })
                                                                                                ],
                                                                                                /* :: */[
                                                                                                  /* tuple */[
                                                                                                    "findIndexi",
                                                                                                    (function () {
                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                  0,
                                                                                                                  /* array */[
                                                                                                                      1,
                                                                                                                      2,
                                                                                                                      3,
                                                                                                                      4
                                                                                                                    ].findIndex((function (_, i) {
                                                                                                                          return i % 2 === 0;
                                                                                                                        }))
                                                                                                                ]);
                                                                                                      })
                                                                                                  ],
                                                                                                  /* :: */[
                                                                                                    /* tuple */[
                                                                                                      "forEach",
                                                                                                      (function () {
                                                                                                          var sum = [0];
                                                                                                          /* array */[
                                                                                                              1,
                                                                                                              2,
                                                                                                              3
                                                                                                            ].forEach((function (n) {
                                                                                                                  sum[0] = sum[0] + n | 0;
                                                                                                                  return /* () */0;
                                                                                                                }));
                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                    6,
                                                                                                                    sum[0]
                                                                                                                  ]);
                                                                                                        })
                                                                                                    ],
                                                                                                    /* :: */[
                                                                                                      /* tuple */[
                                                                                                        "forEachi",
                                                                                                        (function () {
                                                                                                            var sum = [0];
                                                                                                            /* array */[
                                                                                                                1,
                                                                                                                2,
                                                                                                                3
                                                                                                              ].forEach((function (_, i) {
                                                                                                                    sum[0] = sum[0] + i | 0;
                                                                                                                    return /* () */0;
                                                                                                                  }));
                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                      3,
                                                                                                                      sum[0]
                                                                                                                    ]);
                                                                                                          })
                                                                                                      ],
                                                                                                      /* :: */[
                                                                                                        /* tuple */[
                                                                                                          "map",
                                                                                                          (function () {
                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                        /* array */[
                                                                                                                          2,
                                                                                                                          4,
                                                                                                                          6,
                                                                                                                          8
                                                                                                                        ],
                                                                                                                        /* array */[
                                                                                                                            1,
                                                                                                                            2,
                                                                                                                            3,
                                                                                                                            4
                                                                                                                          ].map((function (n) {
                                                                                                                                return (n << 1);
                                                                                                                              }))
                                                                                                                      ]);
                                                                                                            })
                                                                                                        ],
                                                                                                        /* :: */[
                                                                                                          /* tuple */[
                                                                                                            "map",
                                                                                                            (function () {
                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                          /* array */[
                                                                                                                            0,
                                                                                                                            2,
                                                                                                                            4,
                                                                                                                            6
                                                                                                                          ],
                                                                                                                          /* array */[
                                                                                                                              1,
                                                                                                                              2,
                                                                                                                              3,
                                                                                                                              4
                                                                                                                            ].map((function (_, i) {
                                                                                                                                  return (i << 1);
                                                                                                                                }))
                                                                                                                        ]);
                                                                                                              })
                                                                                                          ],
                                                                                                          /* :: */[
                                                                                                            /* tuple */[
                                                                                                              "reduce",
                                                                                                              (function () {
                                                                                                                  return /* Eq */Block.__(0, [
                                                                                                                            -10,
                                                                                                                            /* array */[
                                                                                                                                1,
                                                                                                                                2,
                                                                                                                                3,
                                                                                                                                4
                                                                                                                              ].reduce((function (acc, n) {
                                                                                                                                    return acc - n | 0;
                                                                                                                                  }), 0)
                                                                                                                          ]);
                                                                                                                })
                                                                                                            ],
                                                                                                            /* :: */[
                                                                                                              /* tuple */[
                                                                                                                "reducei",
                                                                                                                (function () {
                                                                                                                    return /* Eq */Block.__(0, [
                                                                                                                              -6,
                                                                                                                              /* array */[
                                                                                                                                  1,
                                                                                                                                  2,
                                                                                                                                  3,
                                                                                                                                  4
                                                                                                                                ].reduce((function (acc, _, i) {
                                                                                                                                      return acc - i | 0;
                                                                                                                                    }), 0)
                                                                                                                            ]);
                                                                                                                  })
                                                                                                              ],
                                                                                                              /* :: */[
                                                                                                                /* tuple */[
                                                                                                                  "reduceRight",
                                                                                                                  (function () {
                                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                                -10,
                                                                                                                                /* array */[
                                                                                                                                    1,
                                                                                                                                    2,
                                                                                                                                    3,
                                                                                                                                    4
                                                                                                                                  ].reduceRight((function (acc, n) {
                                                                                                                                        return acc - n | 0;
                                                                                                                                      }), 0)
                                                                                                                              ]);
                                                                                                                    })
                                                                                                                ],
                                                                                                                /* :: */[
                                                                                                                  /* tuple */[
                                                                                                                    "reduceRighti",
                                                                                                                    (function () {
                                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                                  -6,
                                                                                                                                  /* array */[
                                                                                                                                      1,
                                                                                                                                      2,
                                                                                                                                      3,
                                                                                                                                      4
                                                                                                                                    ].reduceRight((function (acc, _, i) {
                                                                                                                                          return acc - i | 0;
                                                                                                                                        }), 0)
                                                                                                                                ]);
                                                                                                                      })
                                                                                                                  ],
                                                                                                                  /* :: */[
                                                                                                                    /* tuple */[
                                                                                                                      "some",
                                                                                                                      (function () {
                                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                                    false,
                                                                                                                                    /* array */[
                                                                                                                                        1,
                                                                                                                                        2,
                                                                                                                                        3,
                                                                                                                                        4
                                                                                                                                      ].some((function (n) {
                                                                                                                                            return n <= 0;
                                                                                                                                          }))
                                                                                                                                  ]);
                                                                                                                        })
                                                                                                                    ],
                                                                                                                    /* :: */[
                                                                                                                      /* tuple */[
                                                                                                                        "somei",
                                                                                                                        (function () {
                                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                                      true,
                                                                                                                                      /* array */[
                                                                                                                                          1,
                                                                                                                                          2,
                                                                                                                                          3,
                                                                                                                                          4
                                                                                                                                        ].some((function (_, i) {
                                                                                                                                              return i <= 0;
                                                                                                                                            }))
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
      ]
    ]
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_array_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
