'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Pervasives = require("../../lib/js/pervasives.js");

var suites_000 = /* tuple */[
  "_NaN <> _NaN",
  (function (param) {
      return /* Eq */Block.__(0, [
                false,
                NaN === NaN
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "isNaN - _NaN",
    (function (param) {
        return /* Eq */Block.__(0, [
                  true,
                  isNaN(NaN)
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "isNaN - 0.",
      (function (param) {
          return /* Eq */Block.__(0, [
                    false,
                    isNaN(0)
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "isFinite - infinity",
        (function (param) {
            return /* Eq */Block.__(0, [
                      false,
                      isFinite(Pervasives.infinity)
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "isFinite - neg_infinity",
          (function (param) {
              return /* Eq */Block.__(0, [
                        false,
                        isFinite(Pervasives.neg_infinity)
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "isFinite - _NaN",
            (function (param) {
                return /* Eq */Block.__(0, [
                          false,
                          isFinite(NaN)
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "isFinite - 0.",
              (function (param) {
                  return /* Eq */Block.__(0, [
                            true,
                            isFinite(0)
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "toExponential",
                (function (param) {
                    return /* Eq */Block.__(0, [
                              "1.23456e+2",
                              (123.456).toExponential()
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "toExponential - large number",
                  (function (param) {
                      return /* Eq */Block.__(0, [
                                "1.2e+21",
                                (1.2e21).toExponential()
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "toExponentialWithPrecision - digits:2",
                    (function (param) {
                        return /* Eq */Block.__(0, [
                                  "1.23e+2",
                                  (123.456).toExponential(2)
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "toExponentialWithPrecision - digits:4",
                      (function (param) {
                          return /* Eq */Block.__(0, [
                                    "1.2346e+2",
                                    (123.456).toExponential(4)
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "toExponentialWithPrecision - digits:20",
                        (function (param) {
                            return /* Eq */Block.__(0, [
                                      "0.00000000000000000000e+0",
                                      (0).toExponential(20)
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "File \"js_float_test.ml\", line 31, characters 3-10",
                          (function (param) {
                              return /* ThrowAny */Block.__(7, [(function (param) {
                                            (0).toExponential(101);
                                            return /* () */0;
                                          })]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "toExponentialWithPrecision - digits:-1",
                            (function (param) {
                                return /* ThrowAny */Block.__(7, [(function (param) {
                                              (0).toExponential(-1);
                                              return /* () */0;
                                            })]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "toFixed",
                              (function (param) {
                                  return /* Eq */Block.__(0, [
                                            "123",
                                            (123.456).toFixed()
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "toFixed - large number",
                                (function (param) {
                                    return /* Eq */Block.__(0, [
                                              "1.2e+21",
                                              (1.2e21).toFixed()
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "toFixedWithPrecision - digits:2",
                                  (function (param) {
                                      return /* Eq */Block.__(0, [
                                                "123.46",
                                                (123.456).toFixed(2)
                                              ]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "toFixedWithPrecision - digits:4",
                                    (function (param) {
                                        return /* Eq */Block.__(0, [
                                                  "123.4560",
                                                  (123.456).toFixed(4)
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "toFixedWithPrecision - digits:20",
                                      (function (param) {
                                          return /* Eq */Block.__(0, [
                                                    "0.00000000000000000000",
                                                    (0).toFixed(20)
                                                  ]);
                                        })
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "toFixedWithPrecision - digits:101",
                                        (function (param) {
                                            return /* ThrowAny */Block.__(7, [(function (param) {
                                                          (0).toFixed(101);
                                                          return /* () */0;
                                                        })]);
                                          })
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "toFixedWithPrecision - digits:-1",
                                          (function (param) {
                                              return /* ThrowAny */Block.__(7, [(function (param) {
                                                            (0).toFixed(-1);
                                                            return /* () */0;
                                                          })]);
                                            })
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "toPrecision",
                                            (function (param) {
                                                return /* Eq */Block.__(0, [
                                                          "123.456",
                                                          (123.456).toPrecision()
                                                        ]);
                                              })
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "toPrecision - large number",
                                              (function (param) {
                                                  return /* Eq */Block.__(0, [
                                                            "1.2e+21",
                                                            (1.2e21).toPrecision()
                                                          ]);
                                                })
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "toPrecisionWithPrecision - digits:2",
                                                (function (param) {
                                                    return /* Eq */Block.__(0, [
                                                              "1.2e+2",
                                                              (123.456).toPrecision(2)
                                                            ]);
                                                  })
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "toPrecisionWithPrecision - digits:4",
                                                  (function (param) {
                                                      return /* Eq */Block.__(0, [
                                                                "123.5",
                                                                (123.456).toPrecision(4)
                                                              ]);
                                                    })
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "toPrecisionWithPrecision - digits:20",
                                                    (function (param) {
                                                        return /* Eq */Block.__(0, [
                                                                  "0.0000000000000000000",
                                                                  (0).toPrecision(20)
                                                                ]);
                                                      })
                                                  ],
                                                  /* :: */[
                                                    /* tuple */[
                                                      "File \"js_float_test.ml\", line 61, characters 3-10",
                                                      (function (param) {
                                                          return /* ThrowAny */Block.__(7, [(function (param) {
                                                                        (0).toPrecision(101);
                                                                        return /* () */0;
                                                                      })]);
                                                        })
                                                    ],
                                                    /* :: */[
                                                      /* tuple */[
                                                        "toPrecisionWithPrecision - digits:-1",
                                                        (function (param) {
                                                            return /* ThrowAny */Block.__(7, [(function (param) {
                                                                          (0).toPrecision(-1);
                                                                          return /* () */0;
                                                                        })]);
                                                          })
                                                      ],
                                                      /* :: */[
                                                        /* tuple */[
                                                          "toString",
                                                          (function (param) {
                                                              return /* Eq */Block.__(0, [
                                                                        "1.23",
                                                                        (1.23).toString()
                                                                      ]);
                                                            })
                                                        ],
                                                        /* :: */[
                                                          /* tuple */[
                                                            "toString - large number",
                                                            (function (param) {
                                                                return /* Eq */Block.__(0, [
                                                                          "1.2e+21",
                                                                          (1.2e21).toString()
                                                                        ]);
                                                              })
                                                          ],
                                                          /* :: */[
                                                            /* tuple */[
                                                              "toStringWithRadix - radix:2",
                                                              (function (param) {
                                                                  return /* Eq */Block.__(0, [
                                                                            "1111011.0111010010111100011010100111111011111001110111",
                                                                            (123.456).toString(2)
                                                                          ]);
                                                                })
                                                            ],
                                                            /* :: */[
                                                              /* tuple */[
                                                                "toStringWithRadix - radix:16",
                                                                (function (param) {
                                                                    return /* Eq */Block.__(0, [
                                                                              "7b.74bc6a7ef9dc",
                                                                              (123.456).toString(16)
                                                                            ]);
                                                                  })
                                                              ],
                                                              /* :: */[
                                                                /* tuple */[
                                                                  "toStringWithRadix - radix:36",
                                                                  (function (param) {
                                                                      return /* Eq */Block.__(0, [
                                                                                "3f",
                                                                                (123).toString(36)
                                                                              ]);
                                                                    })
                                                                ],
                                                                /* :: */[
                                                                  /* tuple */[
                                                                    "toStringWithRadix - radix:37",
                                                                    (function (param) {
                                                                        return /* ThrowAny */Block.__(7, [(function (param) {
                                                                                      (0).toString(37);
                                                                                      return /* () */0;
                                                                                    })]);
                                                                      })
                                                                  ],
                                                                  /* :: */[
                                                                    /* tuple */[
                                                                      "toStringWithRadix - radix:1",
                                                                      (function (param) {
                                                                          return /* ThrowAny */Block.__(7, [(function (param) {
                                                                                        (0).toString(1);
                                                                                        return /* () */0;
                                                                                      })]);
                                                                        })
                                                                    ],
                                                                    /* :: */[
                                                                      /* tuple */[
                                                                        "toStringWithRadix - radix:-1",
                                                                        (function (param) {
                                                                            return /* ThrowAny */Block.__(7, [(function (param) {
                                                                                          (0).toString(-1);
                                                                                          return /* () */0;
                                                                                        })]);
                                                                          })
                                                                      ],
                                                                      /* :: */[
                                                                        /* tuple */[
                                                                          "fromString - 123",
                                                                          (function (param) {
                                                                              return /* Eq */Block.__(0, [
                                                                                        123,
                                                                                        Number("123")
                                                                                      ]);
                                                                            })
                                                                        ],
                                                                        /* :: */[
                                                                          /* tuple */[
                                                                            "fromString - 12.3",
                                                                            (function (param) {
                                                                                return /* Eq */Block.__(0, [
                                                                                          12.3,
                                                                                          Number("12.3")
                                                                                        ]);
                                                                              })
                                                                          ],
                                                                          /* :: */[
                                                                            /* tuple */[
                                                                              "fromString - empty string",
                                                                              (function (param) {
                                                                                  return /* Eq */Block.__(0, [
                                                                                            0,
                                                                                            Number("")
                                                                                          ]);
                                                                                })
                                                                            ],
                                                                            /* :: */[
                                                                              /* tuple */[
                                                                                "fromString - 0x11",
                                                                                (function (param) {
                                                                                    return /* Eq */Block.__(0, [
                                                                                              17,
                                                                                              Number("0x11")
                                                                                            ]);
                                                                                  })
                                                                              ],
                                                                              /* :: */[
                                                                                /* tuple */[
                                                                                  "fromString - 0b11",
                                                                                  (function (param) {
                                                                                      return /* Eq */Block.__(0, [
                                                                                                3,
                                                                                                Number("0b11")
                                                                                              ]);
                                                                                    })
                                                                                ],
                                                                                /* :: */[
                                                                                  /* tuple */[
                                                                                    "fromString - 0o11",
                                                                                    (function (param) {
                                                                                        return /* Eq */Block.__(0, [
                                                                                                  9,
                                                                                                  Number("0o11")
                                                                                                ]);
                                                                                      })
                                                                                  ],
                                                                                  /* :: */[
                                                                                    /* tuple */[
                                                                                      "fromString - invalid string",
                                                                                      (function (param) {
                                                                                          return /* Eq */Block.__(0, [
                                                                                                    true,
                                                                                                    isNaN(Number("foo"))
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

Mt.from_pair_suites("Js_float_test", suites);

exports.suites = suites;
/*  Not a pure module */
