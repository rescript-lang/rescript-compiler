'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var suites_000 = /* tuple */[
  "toExponential",
  (function () {
      return /* Eq */Block.__(0, [
                "1.23456e+5",
                (123456).toExponential()
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "toExponentialWithPrecision - digits:2",
    (function () {
        return /* Eq */Block.__(0, [
                  "1.23e+5",
                  (123456).toExponential(2)
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "toExponentialWithPrecision - digits:4",
      (function () {
          return /* Eq */Block.__(0, [
                    "1.2346e+5",
                    (123456).toExponential(4)
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "toExponentialWithPrecision - digits:20",
        (function () {
            return /* Eq */Block.__(0, [
                      "0.00000000000000000000e+0",
                      (0).toExponential(20)
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "File \"js_int_test.ml\", line 12, characters 3-10",
          (function () {
              return /* ThrowAny */Block.__(7, [(function () {
                            (0).toExponential(101);
                            return /* () */0;
                          })]);
            })
        ],
        /* :: */[
          /* tuple */[
            "toExponentialWithPrecision - digits:-1",
            (function () {
                return /* ThrowAny */Block.__(7, [(function () {
                              (0).toExponential(-1);
                              return /* () */0;
                            })]);
              })
          ],
          /* :: */[
            /* tuple */[
              "toPrecision",
              (function () {
                  return /* Eq */Block.__(0, [
                            "123456",
                            (123456).toPrecision()
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "toPrecisionWithPrecision - digits:2",
                (function () {
                    return /* Eq */Block.__(0, [
                              "1.2e+5",
                              (123456).toPrecision(2)
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "toPrecisionWithPrecision - digits:4",
                  (function () {
                      return /* Eq */Block.__(0, [
                                "1.235e+5",
                                (123456).toPrecision(4)
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "toPrecisionWithPrecision - digits:20",
                    (function () {
                        return /* Eq */Block.__(0, [
                                  "0.0000000000000000000",
                                  (0).toPrecision(20)
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "File \"js_int_test.ml\", line 25, characters 3-10",
                      (function () {
                          return /* ThrowAny */Block.__(7, [(function () {
                                        (0).toPrecision(101);
                                        return /* () */0;
                                      })]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "toPrecisionWithPrecision - digits:-1",
                        (function () {
                            return /* ThrowAny */Block.__(7, [(function () {
                                          (0).toPrecision(-1);
                                          return /* () */0;
                                        })]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "toString",
                          (function () {
                              return /* Eq */Block.__(0, [
                                        "123",
                                        (123).toString()
                                      ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "toStringWithRadix - radix:2",
                            (function () {
                                return /* Eq */Block.__(0, [
                                          "11110001001000000",
                                          (123456).toString(2)
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "toStringWithRadix - radix:16",
                              (function () {
                                  return /* Eq */Block.__(0, [
                                            "1e240",
                                            (123456).toString(16)
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "toStringWithRadix - radix:36",
                                (function () {
                                    return /* Eq */Block.__(0, [
                                              "2n9c",
                                              (123456).toString(36)
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "toStringWithRadix - radix:37",
                                  (function () {
                                      return /* ThrowAny */Block.__(7, [(function () {
                                                    (0).toString(37);
                                                    return /* () */0;
                                                  })]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "toStringWithRadix - radix:1",
                                    (function () {
                                        return /* ThrowAny */Block.__(7, [(function () {
                                                      (0).toString(1);
                                                      return /* () */0;
                                                    })]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "toStringWithRadix - radix:-1",
                                      (function () {
                                          return /* ThrowAny */Block.__(7, [(function () {
                                                        (0).toString(-1);
                                                        return /* () */0;
                                                      })]);
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
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_int_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
