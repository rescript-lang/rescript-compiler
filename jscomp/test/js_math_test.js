'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_math = require("../../lib/js/js_math.js");

var suites_000 = /* tuple */[
  "_E",
  (function () {
      return /* ApproxThreshold */Block.__(6, [
                0.001,
                2.718,
                Math.E
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "_LN2",
    (function () {
        return /* ApproxThreshold */Block.__(6, [
                  0.001,
                  0.693,
                  Math.LN2
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "_LN10",
      (function () {
          return /* ApproxThreshold */Block.__(6, [
                    0.001,
                    2.303,
                    Math.LN10
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "_LOG2E",
        (function () {
            return /* ApproxThreshold */Block.__(6, [
                      0.001,
                      1.443,
                      Math.LOG2E
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "_LOG10E",
          (function () {
              return /* ApproxThreshold */Block.__(6, [
                        0.001,
                        0.434,
                        Math.LOG10E
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "_PI",
            (function () {
                return /* ApproxThreshold */Block.__(6, [
                          0.00001,
                          3.14159,
                          Math.PI
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "_SQRT1_2",
              (function () {
                  return /* ApproxThreshold */Block.__(6, [
                            0.001,
                            0.707,
                            Math.SQRT1_2
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "_SQRT2",
                (function () {
                    return /* ApproxThreshold */Block.__(6, [
                              0.001,
                              1.414,
                              Math.SQRT2
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "abs_int",
                  (function () {
                      return /* Eq */Block.__(0, [
                                4,
                                Math.abs(-4)
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "abs_float",
                    (function () {
                        return /* Eq */Block.__(0, [
                                  1.2,
                                  Math.abs(-1.2)
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "acos",
                      (function () {
                          return /* ApproxThreshold */Block.__(6, [
                                    0.001,
                                    1.159,
                                    Math.acos(0.4)
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "acosh",
                        (function () {
                            return /* ApproxThreshold */Block.__(6, [
                                      0.001,
                                      0.622,
                                      Math.acosh(1.2)
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "asin",
                          (function () {
                              return /* ApproxThreshold */Block.__(6, [
                                        0.001,
                                        0.411,
                                        Math.asin(0.4)
                                      ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "asinh",
                            (function () {
                                return /* ApproxThreshold */Block.__(6, [
                                          0.001,
                                          0.390,
                                          Math.asinh(0.4)
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "atan",
                              (function () {
                                  return /* ApproxThreshold */Block.__(6, [
                                            0.001,
                                            0.380,
                                            Math.atan(0.4)
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "atanh",
                                (function () {
                                    return /* ApproxThreshold */Block.__(6, [
                                              0.001,
                                              0.423,
                                              Math.atanh(0.4)
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "atan2",
                                  (function () {
                                      return /* ApproxThreshold */Block.__(6, [
                                                0.001,
                                                0.588,
                                                Math.atan2(0.4, 0.6)
                                              ]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "cbrt",
                                    (function () {
                                        return /* Eq */Block.__(0, [
                                                  2,
                                                  Math.cbrt(8)
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "unsafe_ceil_int",
                                      (function () {
                                          return /* Eq */Block.__(0, [
                                                    4,
                                                    Math.ceil(3.2)
                                                  ]);
                                        })
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "ceil_int",
                                        (function () {
                                            return /* Eq */Block.__(0, [
                                                      4,
                                                      Js_math.ceil_int(3.2)
                                                    ]);
                                          })
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "ceil_float",
                                          (function () {
                                              return /* Eq */Block.__(0, [
                                                        4,
                                                        Math.ceil(3.2)
                                                      ]);
                                            })
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "cos",
                                            (function () {
                                                return /* ApproxThreshold */Block.__(6, [
                                                          0.001,
                                                          0.921,
                                                          Math.cos(0.4)
                                                        ]);
                                              })
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "cosh",
                                              (function () {
                                                  return /* ApproxThreshold */Block.__(6, [
                                                            0.001,
                                                            1.081,
                                                            Math.cosh(0.4)
                                                          ]);
                                                })
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "exp",
                                                (function () {
                                                    return /* ApproxThreshold */Block.__(6, [
                                                              0.001,
                                                              1.491,
                                                              Math.exp(0.4)
                                                            ]);
                                                  })
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "expm1",
                                                  (function () {
                                                      return /* ApproxThreshold */Block.__(6, [
                                                                0.001,
                                                                0.491,
                                                                Math.expm1(0.4)
                                                              ]);
                                                    })
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "unsafe_floor_int",
                                                    (function () {
                                                        return /* Eq */Block.__(0, [
                                                                  3,
                                                                  Math.floor(3.2)
                                                                ]);
                                                      })
                                                  ],
                                                  /* :: */[
                                                    /* tuple */[
                                                      "floor_int",
                                                      (function () {
                                                          return /* Eq */Block.__(0, [
                                                                    3,
                                                                    Js_math.floor_int(3.2)
                                                                  ]);
                                                        })
                                                    ],
                                                    /* :: */[
                                                      /* tuple */[
                                                        "floor_float",
                                                        (function () {
                                                            return /* Eq */Block.__(0, [
                                                                      3,
                                                                      Math.floor(3.2)
                                                                    ]);
                                                          })
                                                      ],
                                                      /* :: */[
                                                        /* tuple */[
                                                          "fround",
                                                          (function () {
                                                              return /* Approx */Block.__(5, [
                                                                        3.2,
                                                                        Math.fround(3.2)
                                                                      ]);
                                                            })
                                                        ],
                                                        /* :: */[
                                                          /* tuple */[
                                                            "hypot",
                                                            (function () {
                                                                return /* ApproxThreshold */Block.__(6, [
                                                                          0.001,
                                                                          0.721,
                                                                          Math.hypot(0.4, 0.6)
                                                                        ]);
                                                              })
                                                          ],
                                                          /* :: */[
                                                            /* tuple */[
                                                              "hypotMany",
                                                              (function () {
                                                                  return /* ApproxThreshold */Block.__(6, [
                                                                            0.001,
                                                                            1.077,
                                                                            Math.hypot(0.4, 0.6, 0.8)
                                                                          ]);
                                                                })
                                                            ],
                                                            /* :: */[
                                                              /* tuple */[
                                                                "imul",
                                                                (function () {
                                                                    return /* Eq */Block.__(0, [
                                                                              8,
                                                                              Math.imul(4, 2)
                                                                            ]);
                                                                  })
                                                              ],
                                                              /* :: */[
                                                                /* tuple */[
                                                                  "log",
                                                                  (function () {
                                                                      return /* ApproxThreshold */Block.__(6, [
                                                                                0.001,
                                                                                -0.916,
                                                                                Math.log(0.4)
                                                                              ]);
                                                                    })
                                                                ],
                                                                /* :: */[
                                                                  /* tuple */[
                                                                    "log1p",
                                                                    (function () {
                                                                        return /* ApproxThreshold */Block.__(6, [
                                                                                  0.001,
                                                                                  0.336,
                                                                                  Math.log1p(0.4)
                                                                                ]);
                                                                      })
                                                                  ],
                                                                  /* :: */[
                                                                    /* tuple */[
                                                                      "log10",
                                                                      (function () {
                                                                          return /* ApproxThreshold */Block.__(6, [
                                                                                    0.001,
                                                                                    -0.397,
                                                                                    Math.log10(0.4)
                                                                                  ]);
                                                                        })
                                                                    ],
                                                                    /* :: */[
                                                                      /* tuple */[
                                                                        "log2",
                                                                        (function () {
                                                                            return /* ApproxThreshold */Block.__(6, [
                                                                                      0.001,
                                                                                      -1.321,
                                                                                      Math.log2(0.4)
                                                                                    ]);
                                                                          })
                                                                      ],
                                                                      /* :: */[
                                                                        /* tuple */[
                                                                          "max_int",
                                                                          (function () {
                                                                              return /* Eq */Block.__(0, [
                                                                                        4,
                                                                                        Math.max(2, 4)
                                                                                      ]);
                                                                            })
                                                                        ],
                                                                        /* :: */[
                                                                          /* tuple */[
                                                                            "maxMany_int",
                                                                            (function () {
                                                                                return /* Eq */Block.__(0, [
                                                                                          4,
                                                                                          Math.max(2, 4, 3)
                                                                                        ]);
                                                                              })
                                                                          ],
                                                                          /* :: */[
                                                                            /* tuple */[
                                                                              "max_float",
                                                                              (function () {
                                                                                  return /* Eq */Block.__(0, [
                                                                                            4.2,
                                                                                            Math.max(2.7, 4.2)
                                                                                          ]);
                                                                                })
                                                                            ],
                                                                            /* :: */[
                                                                              /* tuple */[
                                                                                "maxMany_float",
                                                                                (function () {
                                                                                    return /* Eq */Block.__(0, [
                                                                                              4.2,
                                                                                              Math.max(2.7, 4.2, 3.9)
                                                                                            ]);
                                                                                  })
                                                                              ],
                                                                              /* :: */[
                                                                                /* tuple */[
                                                                                  "min_int",
                                                                                  (function () {
                                                                                      return /* Eq */Block.__(0, [
                                                                                                2,
                                                                                                Math.min(2, 4)
                                                                                              ]);
                                                                                    })
                                                                                ],
                                                                                /* :: */[
                                                                                  /* tuple */[
                                                                                    "minMany_int",
                                                                                    (function () {
                                                                                        return /* Eq */Block.__(0, [
                                                                                                  2,
                                                                                                  Math.min(2, 4, 3)
                                                                                                ]);
                                                                                      })
                                                                                  ],
                                                                                  /* :: */[
                                                                                    /* tuple */[
                                                                                      "min_float",
                                                                                      (function () {
                                                                                          return /* Eq */Block.__(0, [
                                                                                                    2.7,
                                                                                                    Math.min(2.7, 4.2)
                                                                                                  ]);
                                                                                        })
                                                                                    ],
                                                                                    /* :: */[
                                                                                      /* tuple */[
                                                                                        "minMany_float",
                                                                                        (function () {
                                                                                            return /* Eq */Block.__(0, [
                                                                                                      2.7,
                                                                                                      Math.min(2.7, 4.2, 3.9)
                                                                                                    ]);
                                                                                          })
                                                                                      ],
                                                                                      /* :: */[
                                                                                        /* tuple */[
                                                                                          "random",
                                                                                          (function () {
                                                                                              var a = Math.random();
                                                                                              return /* Ok */Block.__(4, [+(a >= 0 && a < 1)]);
                                                                                            })
                                                                                        ],
                                                                                        /* :: */[
                                                                                          /* tuple */[
                                                                                            "random_int",
                                                                                            (function () {
                                                                                                var a = Js_math.random_int(1, 3);
                                                                                                return /* Ok */Block.__(4, [+(a >= 1 && a < 3)]);
                                                                                              })
                                                                                          ],
                                                                                          /* :: */[
                                                                                            /* tuple */[
                                                                                              "unsafe_round",
                                                                                              (function () {
                                                                                                  return /* Eq */Block.__(0, [
                                                                                                            3,
                                                                                                            Math.round(3.2)
                                                                                                          ]);
                                                                                                })
                                                                                            ],
                                                                                            /* :: */[
                                                                                              /* tuple */[
                                                                                                "round",
                                                                                                (function () {
                                                                                                    return /* Eq */Block.__(0, [
                                                                                                              3,
                                                                                                              Math.round(3.2)
                                                                                                            ]);
                                                                                                  })
                                                                                              ],
                                                                                              /* :: */[
                                                                                                /* tuple */[
                                                                                                  "sign_int",
                                                                                                  (function () {
                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                -1,
                                                                                                                Math.sign(-4)
                                                                                                              ]);
                                                                                                    })
                                                                                                ],
                                                                                                /* :: */[
                                                                                                  /* tuple */[
                                                                                                    "sign_float",
                                                                                                    (function () {
                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                  -1,
                                                                                                                  Math.sign(-4.2)
                                                                                                                ]);
                                                                                                      })
                                                                                                  ],
                                                                                                  /* :: */[
                                                                                                    /* tuple */[
                                                                                                      "sign_float -0",
                                                                                                      (function () {
                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                    -0,
                                                                                                                    Math.sign(-0)
                                                                                                                  ]);
                                                                                                        })
                                                                                                    ],
                                                                                                    /* :: */[
                                                                                                      /* tuple */[
                                                                                                        "sin",
                                                                                                        (function () {
                                                                                                            return /* ApproxThreshold */Block.__(6, [
                                                                                                                      0.001,
                                                                                                                      0.389,
                                                                                                                      Math.sin(0.4)
                                                                                                                    ]);
                                                                                                          })
                                                                                                      ],
                                                                                                      /* :: */[
                                                                                                        /* tuple */[
                                                                                                          "sinh",
                                                                                                          (function () {
                                                                                                              return /* ApproxThreshold */Block.__(6, [
                                                                                                                        0.001,
                                                                                                                        0.410,
                                                                                                                        Math.sinh(0.4)
                                                                                                                      ]);
                                                                                                            })
                                                                                                        ],
                                                                                                        /* :: */[
                                                                                                          /* tuple */[
                                                                                                            "sqrt",
                                                                                                            (function () {
                                                                                                                return /* ApproxThreshold */Block.__(6, [
                                                                                                                          0.001,
                                                                                                                          0.632,
                                                                                                                          Math.sqrt(0.4)
                                                                                                                        ]);
                                                                                                              })
                                                                                                          ],
                                                                                                          /* :: */[
                                                                                                            /* tuple */[
                                                                                                              "tan",
                                                                                                              (function () {
                                                                                                                  return /* ApproxThreshold */Block.__(6, [
                                                                                                                            0.001,
                                                                                                                            0.422,
                                                                                                                            Math.tan(0.4)
                                                                                                                          ]);
                                                                                                                })
                                                                                                            ],
                                                                                                            /* :: */[
                                                                                                              /* tuple */[
                                                                                                                "tanh",
                                                                                                                (function () {
                                                                                                                    return /* ApproxThreshold */Block.__(6, [
                                                                                                                              0.001,
                                                                                                                              0.379,
                                                                                                                              Math.tanh(0.4)
                                                                                                                            ]);
                                                                                                                  })
                                                                                                              ],
                                                                                                              /* :: */[
                                                                                                                /* tuple */[
                                                                                                                  "unsafe_trunc",
                                                                                                                  (function () {
                                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                                4,
                                                                                                                                Math.trunc(4.2156)
                                                                                                                              ]);
                                                                                                                    })
                                                                                                                ],
                                                                                                                /* :: */[
                                                                                                                  /* tuple */[
                                                                                                                    "trunc",
                                                                                                                    (function () {
                                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                                  4,
                                                                                                                                  Math.trunc(4.2156)
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
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_math_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
