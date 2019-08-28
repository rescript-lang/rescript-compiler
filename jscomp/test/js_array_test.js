'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_vector = require("../../lib/js/js_vector.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites_000 = /* tuple */[
  "File \"js_array_test.ml\", line 3, characters 4-11",
  (function (param) {
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
    (function (param) {
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
      (function (param) {
          return /* Eq */Block.__(0, [
                    true,
                    Array.isArray(/* array */[])
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "isArray_int",
        (function (param) {
            return /* Eq */Block.__(0, [
                      false,
                      Array.isArray(34)
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "length",
          (function (param) {
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
            (function (param) {
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
              (function (param) {
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
                (function (param) {
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
                  (function (param) {
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
                    (function (param) {
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
                      (function (param) {
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
                        (function (param) {
                            return /* Eq */Block.__(0, [
                                      3,
                                      Caml_option.undefined_to_opt(/* array */[
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
                          (function (param) {
                              return /* Eq */Block.__(0, [
                                        undefined,
                                        Caml_option.undefined_to_opt(/* array */[].pop())
                                      ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "push",
                            (function (param) {
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
                              (function (param) {
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
                                (function (param) {
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
                                  (function (param) {
                                      return /* Eq */Block.__(0, [
                                                1,
                                                Caml_option.undefined_to_opt(/* array */[
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
                                    (function (param) {
                                        return /* Eq */Block.__(0, [
                                                  undefined,
                                                  Caml_option.undefined_to_opt(/* array */[].shift())
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "sortInPlace",
                                      (function (param) {
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
                                        (function (param) {
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
                                          (function (param) {
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
                                            (function (param) {
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
                                              (function (param) {
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
                                                (function (param) {
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
                                                  (function (param) {
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
                                                    (function (param) {
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
                                                                    ].concat(/* array */[4])
                                                                ]);
                                                      })
                                                  ],
                                                  /* :: */[
                                                    /* tuple */[
                                                      "concat",
                                                      (function (param) {
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
                                                        (function (param) {
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
                                                          (function (param) {
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
                                                            (function (param) {
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
                                                              (function (param) {
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
                                                                (function (param) {
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
                                                                  (function (param) {
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
                                                                    (function (param) {
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
                                                                      (function (param) {
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
                                                                        (function (param) {
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
                                                                          (function (param) {
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
                                                                            (function (param) {
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
                                                                              (function (param) {
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
                                                                                (function (param) {
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
                                                                                  (function (param) {
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
                                                                                    (function (param) {
                                                                                        return /* Eq */Block.__(0, [
                                                                                                  false,
                                                                                                  /* array */[
                                                                                                      1,
                                                                                                      2,
                                                                                                      3
                                                                                                    ].every((function (param, i) {
                                                                                                          return i > 0;
                                                                                                        }))
                                                                                                ]);
                                                                                      })
                                                                                  ],
                                                                                  /* :: */[
                                                                                    /* tuple */[
                                                                                      "filter",
                                                                                      (function (param) {
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
                                                                                        (function (param) {
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
                                                                                                        ].filter((function (param, i) {
                                                                                                              return i % 2 === 0;
                                                                                                            }))
                                                                                                    ]);
                                                                                          })
                                                                                      ],
                                                                                      /* :: */[
                                                                                        /* tuple */[
                                                                                          "find",
                                                                                          (function (param) {
                                                                                              return /* Eq */Block.__(0, [
                                                                                                        2,
                                                                                                        Caml_option.undefined_to_opt(/* array */[
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
                                                                                            (function (param) {
                                                                                                return /* Eq */Block.__(0, [
                                                                                                          undefined,
                                                                                                          Caml_option.undefined_to_opt(/* array */[
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
                                                                                              (function (param) {
                                                                                                  return /* Eq */Block.__(0, [
                                                                                                            1,
                                                                                                            Caml_option.undefined_to_opt(/* array */[
                                                                                                                    1,
                                                                                                                    2,
                                                                                                                    3,
                                                                                                                    4
                                                                                                                  ].find((function (param, i) {
                                                                                                                        return i % 2 === 0;
                                                                                                                      })))
                                                                                                          ]);
                                                                                                })
                                                                                            ],
                                                                                            /* :: */[
                                                                                              /* tuple */[
                                                                                                "findi - no match",
                                                                                                (function (param) {
                                                                                                    return /* Eq */Block.__(0, [
                                                                                                              undefined,
                                                                                                              Caml_option.undefined_to_opt(/* array */[
                                                                                                                      1,
                                                                                                                      2,
                                                                                                                      3,
                                                                                                                      4
                                                                                                                    ].find((function (param, i) {
                                                                                                                          return i % 2 === 5;
                                                                                                                        })))
                                                                                                            ]);
                                                                                                  })
                                                                                              ],
                                                                                              /* :: */[
                                                                                                /* tuple */[
                                                                                                  "findIndex",
                                                                                                  (function (param) {
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
                                                                                                    (function (param) {
                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                  0,
                                                                                                                  /* array */[
                                                                                                                      1,
                                                                                                                      2,
                                                                                                                      3,
                                                                                                                      4
                                                                                                                    ].findIndex((function (param, i) {
                                                                                                                          return i % 2 === 0;
                                                                                                                        }))
                                                                                                                ]);
                                                                                                      })
                                                                                                  ],
                                                                                                  /* :: */[
                                                                                                    /* tuple */[
                                                                                                      "forEach",
                                                                                                      (function (param) {
                                                                                                          var sum = /* record */{
                                                                                                            contents: 0
                                                                                                          };
                                                                                                          /* array */[
                                                                                                              1,
                                                                                                              2,
                                                                                                              3
                                                                                                            ].forEach((function (n) {
                                                                                                                  sum.contents = sum.contents + n | 0;
                                                                                                                  return /* () */0;
                                                                                                                }));
                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                    6,
                                                                                                                    sum.contents
                                                                                                                  ]);
                                                                                                        })
                                                                                                    ],
                                                                                                    /* :: */[
                                                                                                      /* tuple */[
                                                                                                        "forEachi",
                                                                                                        (function (param) {
                                                                                                            var sum = /* record */{
                                                                                                              contents: 0
                                                                                                            };
                                                                                                            /* array */[
                                                                                                                1,
                                                                                                                2,
                                                                                                                3
                                                                                                              ].forEach((function (param, i) {
                                                                                                                    sum.contents = sum.contents + i | 0;
                                                                                                                    return /* () */0;
                                                                                                                  }));
                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                      3,
                                                                                                                      sum.contents
                                                                                                                    ]);
                                                                                                          })
                                                                                                      ],
                                                                                                      /* :: */[
                                                                                                        /* tuple */[
                                                                                                          "map",
                                                                                                          (function (param) {
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
                                                                                                            (function (param) {
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
                                                                                                                            ].map((function (param, i) {
                                                                                                                                  return (i << 1);
                                                                                                                                }))
                                                                                                                        ]);
                                                                                                              })
                                                                                                          ],
                                                                                                          /* :: */[
                                                                                                            /* tuple */[
                                                                                                              "reduce",
                                                                                                              (function (param) {
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
                                                                                                                (function (param) {
                                                                                                                    return /* Eq */Block.__(0, [
                                                                                                                              -6,
                                                                                                                              /* array */[
                                                                                                                                  1,
                                                                                                                                  2,
                                                                                                                                  3,
                                                                                                                                  4
                                                                                                                                ].reduce((function (acc, param, i) {
                                                                                                                                      return acc - i | 0;
                                                                                                                                    }), 0)
                                                                                                                            ]);
                                                                                                                  })
                                                                                                              ],
                                                                                                              /* :: */[
                                                                                                                /* tuple */[
                                                                                                                  "reduceRight",
                                                                                                                  (function (param) {
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
                                                                                                                    (function (param) {
                                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                                  -6,
                                                                                                                                  /* array */[
                                                                                                                                      1,
                                                                                                                                      2,
                                                                                                                                      3,
                                                                                                                                      4
                                                                                                                                    ].reduceRight((function (acc, param, i) {
                                                                                                                                          return acc - i | 0;
                                                                                                                                        }), 0)
                                                                                                                                ]);
                                                                                                                      })
                                                                                                                  ],
                                                                                                                  /* :: */[
                                                                                                                    /* tuple */[
                                                                                                                      "some",
                                                                                                                      (function (param) {
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
                                                                                                                        (function (param) {
                                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                                      true,
                                                                                                                                      /* array */[
                                                                                                                                          1,
                                                                                                                                          2,
                                                                                                                                          3,
                                                                                                                                          4
                                                                                                                                        ].some((function (param, i) {
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

Mt.from_pair_suites("Js_array_test", suites);

exports.suites = suites;
/*  Not a pure module */
