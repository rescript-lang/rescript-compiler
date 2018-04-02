'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");

function mkI8(a) {
  return new Int8Array(a);
}

function via(make, f, arr) {
  return Array.from(Curry._1(f, Curry._1(make, arr)));
}

function viaInt8(_, _$1) {
  return (function (param, param$1) {
      return Array.from(Curry._1(param, new Int8Array(param$1)));
    });
}

var x = new Int8Array(/* array */[
      1,
      2,
      3
    ]);

var suites_000 = /* tuple */[
  "array_buffer - make",
  (function () {
      return /* Eq */Block.__(0, [
                5,
                new ArrayBuffer(5).byteLength
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "array_buffer - byteLength",
    (function () {
        return /* Eq */Block.__(0, [
                  5,
                  new ArrayBuffer(5).byteLength
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "array_buffer - slice",
      (function () {
          return /* Eq */Block.__(0, [
                    2,
                    new ArrayBuffer(5).slice(2, 4).byteLength
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "array_buffer - sliceFrom",
        (function () {
            return /* Eq */Block.__(0, [
                      3,
                      new ArrayBuffer(5).slice(2).byteLength
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "typed_array - unsafe_get",
          (function () {
              return /* Eq */Block.__(0, [
                        4,
                        new Int8Array(/* array */[
                                1,
                                2,
                                3,
                                4,
                                5
                              ])[3]
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "typed_array - unsafe_set",
            (function () {
                var a = new Int8Array(/* array */[
                      1,
                      2,
                      3,
                      4,
                      5
                    ]);
                a[3] = 14;
                return /* Eq */Block.__(0, [
                          14,
                          a[3]
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "typed_array - buffer",
              (function () {
                  return /* Eq */Block.__(0, [
                            new Int8Array(/* array */[
                                  3,
                                  4,
                                  5
                                ]),
                            new Int8Array(new Int8Array(/* array */[
                                      1,
                                      2,
                                      3,
                                      4,
                                      5
                                    ]).buffer, 2)
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "typed_array - byteLength",
                (function () {
                    return /* Eq */Block.__(0, [
                              10,
                              new Int16Array(/* array */[
                                    1,
                                    2,
                                    3,
                                    4,
                                    5
                                  ]).byteLength
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "typed_array - byteOffset",
                  (function () {
                      return /* Eq */Block.__(0, [
                                0,
                                new Int8Array(/* array */[
                                      1,
                                      2,
                                      3,
                                      4,
                                      5
                                    ]).byteOffset
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "typed_array - setArray",
                    (function () {
                        var f = function (a) {
                          a.set(/* array */[
                                9,
                                8,
                                7
                              ]);
                          return a;
                        };
                        return /* Eq */Block.__(0, [
                                  new Int8Array(/* array */[
                                        9,
                                        8,
                                        7,
                                        4,
                                        5
                                      ]),
                                  f(new Int8Array(/* array */[
                                            1,
                                            2,
                                            3,
                                            4,
                                            5
                                          ]))
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "typed_array - setArrayOffset",
                      (function () {
                          var f = function (a) {
                            a.set(/* array */[
                                  9,
                                  8,
                                  7
                                ], 2);
                            return a;
                          };
                          return /* Eq */Block.__(0, [
                                    new Int8Array(/* array */[
                                          1,
                                          2,
                                          9,
                                          8,
                                          7
                                        ]),
                                    f(new Int8Array(/* array */[
                                              1,
                                              2,
                                              3,
                                              4,
                                              5
                                            ]))
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "typed_array - length",
                        (function () {
                            return /* Eq */Block.__(0, [
                                      5,
                                      new Int8Array(/* array */[
                                            1,
                                            2,
                                            3,
                                            4,
                                            5
                                          ]).length
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "typed_array - copyWithin",
                          (function () {
                              return /* Eq */Block.__(0, [
                                        new Int8Array(/* array */[
                                              1,
                                              2,
                                              3,
                                              1,
                                              2
                                            ]),
                                        new Int8Array(/* array */[
                                                1,
                                                2,
                                                3,
                                                4,
                                                5
                                              ]).copyWithin(-2)
                                      ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "typed_array - copyWithinFrom",
                            (function () {
                                return /* Eq */Block.__(0, [
                                          new Int8Array(/* array */[
                                                4,
                                                5,
                                                3,
                                                4,
                                                5
                                              ]),
                                          new Int8Array(/* array */[
                                                  1,
                                                  2,
                                                  3,
                                                  4,
                                                  5
                                                ]).copyWithin(0, 3)
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "typed_array - copyWithinFromRange",
                              (function () {
                                  return /* Eq */Block.__(0, [
                                            new Int8Array(/* array */[
                                                  4,
                                                  2,
                                                  3,
                                                  4,
                                                  5
                                                ]),
                                            new Int8Array(/* array */[
                                                    1,
                                                    2,
                                                    3,
                                                    4,
                                                    5
                                                  ]).copyWithin(0, 3, 4)
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "typed_array - fillInPlace",
                                (function () {
                                    return /* Eq */Block.__(0, [
                                              new Int8Array(/* array */[
                                                    4,
                                                    4,
                                                    4
                                                  ]),
                                              new Int8Array(/* array */[
                                                      1,
                                                      2,
                                                      3
                                                    ]).fill(4)
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "typed_array - fillFromInPlace",
                                  (function () {
                                      return /* Eq */Block.__(0, [
                                                new Int8Array(/* array */[
                                                      1,
                                                      4,
                                                      4
                                                    ]),
                                                new Int8Array(/* array */[
                                                        1,
                                                        2,
                                                        3
                                                      ]).fill(4, 1)
                                              ]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "typed_array - fillRangeInPlace",
                                    (function () {
                                        return /* Eq */Block.__(0, [
                                                  new Int8Array(/* array */[
                                                        1,
                                                        4,
                                                        3
                                                      ]),
                                                  new Int8Array(/* array */[
                                                          1,
                                                          2,
                                                          3
                                                        ]).fill(4, 1, 2)
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "typed_array - reverseInPlace",
                                      (function () {
                                          return /* Eq */Block.__(0, [
                                                    new Int8Array(/* array */[
                                                          3,
                                                          2,
                                                          1
                                                        ]),
                                                    new Int8Array(/* array */[
                                                            1,
                                                            2,
                                                            3
                                                          ]).reverse()
                                                  ]);
                                        })
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "typed_array - sortInPlace",
                                        (function () {
                                            return /* Eq */Block.__(0, [
                                                      new Int8Array(/* array */[
                                                            1,
                                                            2,
                                                            3
                                                          ]),
                                                      new Int8Array(/* array */[
                                                              3,
                                                              1,
                                                              2
                                                            ]).sort()
                                                    ]);
                                          })
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "typed_array - sortInPlaceWith",
                                          (function () {
                                              return /* Eq */Block.__(0, [
                                                        new Int8Array(/* array */[
                                                              3,
                                                              2,
                                                              1
                                                            ]),
                                                        new Int8Array(/* array */[
                                                                3,
                                                                1,
                                                                2
                                                              ]).sort((function (a, b) {
                                                                return b - a | 0;
                                                              }))
                                                      ]);
                                            })
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "typed_array - includes",
                                            (function () {
                                                return /* Eq */Block.__(0, [
                                                          true,
                                                          new Int8Array(/* array */[
                                                                  1,
                                                                  2,
                                                                  3
                                                                ]).includes(3)
                                                        ]);
                                              })
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "typed_array - indexOf",
                                              (function () {
                                                  return /* Eq */Block.__(0, [
                                                            1,
                                                            new Int8Array(/* array */[
                                                                    1,
                                                                    2,
                                                                    3
                                                                  ]).indexOf(2)
                                                          ]);
                                                })
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "typed_array - indexOfFrom",
                                                (function () {
                                                    return /* Eq */Block.__(0, [
                                                              3,
                                                              new Int8Array(/* array */[
                                                                      1,
                                                                      2,
                                                                      3,
                                                                      2
                                                                    ]).indexOf(2, 2)
                                                            ]);
                                                  })
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "typed_array - join",
                                                  (function () {
                                                      return /* Eq */Block.__(0, [
                                                                "1,2,3",
                                                                new Int8Array(/* array */[
                                                                        1,
                                                                        2,
                                                                        3
                                                                      ]).join()
                                                              ]);
                                                    })
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "typed_array - joinWith",
                                                    (function () {
                                                        return /* Eq */Block.__(0, [
                                                                  "1;2;3",
                                                                  new Int8Array(/* array */[
                                                                          1,
                                                                          2,
                                                                          3
                                                                        ]).join(";")
                                                                ]);
                                                      })
                                                  ],
                                                  /* :: */[
                                                    /* tuple */[
                                                      "typed_array - lastIndexOf",
                                                      (function () {
                                                          return /* Eq */Block.__(0, [
                                                                    1,
                                                                    new Int8Array(/* array */[
                                                                            1,
                                                                            2,
                                                                            3
                                                                          ]).lastIndexOf(2)
                                                                  ]);
                                                        })
                                                    ],
                                                    /* :: */[
                                                      /* tuple */[
                                                        "typed_array - lastIndexOfFrom",
                                                        (function () {
                                                            return /* Eq */Block.__(0, [
                                                                      1,
                                                                      new Int8Array(/* array */[
                                                                              1,
                                                                              2,
                                                                              3,
                                                                              2
                                                                            ]).lastIndexOf(2, 2)
                                                                    ]);
                                                          })
                                                      ],
                                                      /* :: */[
                                                        /* tuple */[
                                                          "typed_array - slice",
                                                          (function () {
                                                              return /* Eq */Block.__(0, [
                                                                        new Int8Array(/* array */[
                                                                              2,
                                                                              3
                                                                            ]),
                                                                        new Int8Array(/* array */[
                                                                                1,
                                                                                2,
                                                                                3,
                                                                                4,
                                                                                5
                                                                              ]).slice(1, 3)
                                                                      ]);
                                                            })
                                                        ],
                                                        /* :: */[
                                                          /* tuple */[
                                                            "typed_array - copy",
                                                            (function () {
                                                                return /* Eq */Block.__(0, [
                                                                          new Int8Array(/* array */[
                                                                                1,
                                                                                2,
                                                                                3,
                                                                                4,
                                                                                5
                                                                              ]),
                                                                          new Int8Array(/* array */[
                                                                                  1,
                                                                                  2,
                                                                                  3,
                                                                                  4,
                                                                                  5
                                                                                ]).slice()
                                                                        ]);
                                                              })
                                                          ],
                                                          /* :: */[
                                                            /* tuple */[
                                                              "typed_array - sliceFrom",
                                                              (function () {
                                                                  return /* Eq */Block.__(0, [
                                                                            new Int8Array(/* array */[
                                                                                  3,
                                                                                  4,
                                                                                  5
                                                                                ]),
                                                                            new Int8Array(/* array */[
                                                                                    1,
                                                                                    2,
                                                                                    3,
                                                                                    4,
                                                                                    5
                                                                                  ]).slice(2)
                                                                          ]);
                                                                })
                                                            ],
                                                            /* :: */[
                                                              /* tuple */[
                                                                "typed_array - subarray",
                                                                (function () {
                                                                    return /* Eq */Block.__(0, [
                                                                              new Int8Array(/* array */[
                                                                                    2,
                                                                                    3
                                                                                  ]),
                                                                              new Int8Array(/* array */[
                                                                                      1,
                                                                                      2,
                                                                                      3,
                                                                                      4,
                                                                                      5
                                                                                    ]).subarray(1, 3)
                                                                            ]);
                                                                  })
                                                              ],
                                                              /* :: */[
                                                                /* tuple */[
                                                                  "typed_array - subarrayFrom",
                                                                  (function () {
                                                                      return /* Eq */Block.__(0, [
                                                                                new Int8Array(/* array */[
                                                                                      3,
                                                                                      4,
                                                                                      5
                                                                                    ]),
                                                                                new Int8Array(/* array */[
                                                                                        1,
                                                                                        2,
                                                                                        3,
                                                                                        4,
                                                                                        5
                                                                                      ]).subarray(2)
                                                                              ]);
                                                                    })
                                                                ],
                                                                /* :: */[
                                                                  /* tuple */[
                                                                    "typed_array - toString",
                                                                    (function () {
                                                                        return /* Eq */Block.__(0, [
                                                                                  "1,2,3",
                                                                                  new Int8Array(/* array */[
                                                                                          1,
                                                                                          2,
                                                                                          3
                                                                                        ]).toString()
                                                                                ]);
                                                                      })
                                                                  ],
                                                                  /* :: */[
                                                                    /* tuple */[
                                                                      "typed_array - toLocaleString",
                                                                      (function () {
                                                                          return /* Eq */Block.__(0, [
                                                                                    "1,2,3",
                                                                                    new Int8Array(/* array */[
                                                                                            1,
                                                                                            2,
                                                                                            3
                                                                                          ]).toLocaleString()
                                                                                  ]);
                                                                        })
                                                                    ],
                                                                    /* :: */[
                                                                      /* tuple */[
                                                                        "typed_array - every",
                                                                        (function () {
                                                                            return /* Eq */Block.__(0, [
                                                                                      true,
                                                                                      new Int8Array(/* array */[
                                                                                              1,
                                                                                              2,
                                                                                              3
                                                                                            ]).every((function (n) {
                                                                                              return n > 0;
                                                                                            }))
                                                                                    ]);
                                                                          })
                                                                      ],
                                                                      /* :: */[
                                                                        /* tuple */[
                                                                          "typed_array - everyi",
                                                                          (function () {
                                                                              return /* Eq */Block.__(0, [
                                                                                        false,
                                                                                        new Int8Array(/* array */[
                                                                                                1,
                                                                                                2,
                                                                                                3
                                                                                              ]).every((function (_, i) {
                                                                                                return i > 0;
                                                                                              }))
                                                                                      ]);
                                                                            })
                                                                        ],
                                                                        /* :: */[
                                                                          /* tuple */[
                                                                            "typed_array - filter",
                                                                            (function () {
                                                                                return /* Eq */Block.__(0, [
                                                                                          new Int8Array(/* array */[
                                                                                                2,
                                                                                                4
                                                                                              ]),
                                                                                          new Int8Array(/* array */[
                                                                                                  1,
                                                                                                  2,
                                                                                                  3,
                                                                                                  4
                                                                                                ]).filter((function (n) {
                                                                                                  return n % 2 === 0;
                                                                                                }))
                                                                                        ]);
                                                                              })
                                                                          ],
                                                                          /* :: */[
                                                                            /* tuple */[
                                                                              "typed_array - filteri",
                                                                              (function () {
                                                                                  return /* Eq */Block.__(0, [
                                                                                            new Int8Array(/* array */[
                                                                                                  1,
                                                                                                  3
                                                                                                ]),
                                                                                            new Int8Array(/* array */[
                                                                                                    1,
                                                                                                    2,
                                                                                                    3,
                                                                                                    4
                                                                                                  ]).filter((function (_, i) {
                                                                                                    return i % 2 === 0;
                                                                                                  }))
                                                                                          ]);
                                                                                })
                                                                            ],
                                                                            /* :: */[
                                                                              /* tuple */[
                                                                                "typed_array - find",
                                                                                (function () {
                                                                                    return /* Eq */Block.__(0, [
                                                                                              2,
                                                                                              new Int8Array(/* array */[
                                                                                                      1,
                                                                                                      2,
                                                                                                      3,
                                                                                                      4
                                                                                                    ]).find((function (n) {
                                                                                                      return n % 2 === 0;
                                                                                                    }))
                                                                                            ]);
                                                                                  })
                                                                              ],
                                                                              /* :: */[
                                                                                /* tuple */[
                                                                                  "typed_array - findi",
                                                                                  (function () {
                                                                                      return /* Eq */Block.__(0, [
                                                                                                1,
                                                                                                new Int8Array(/* array */[
                                                                                                        1,
                                                                                                        2,
                                                                                                        3,
                                                                                                        4
                                                                                                      ]).find((function (_, i) {
                                                                                                        return i % 2 === 0;
                                                                                                      }))
                                                                                              ]);
                                                                                    })
                                                                                ],
                                                                                /* :: */[
                                                                                  /* tuple */[
                                                                                    "typed_array - findIndex",
                                                                                    (function () {
                                                                                        return /* Eq */Block.__(0, [
                                                                                                  1,
                                                                                                  new Int8Array(/* array */[
                                                                                                          1,
                                                                                                          2,
                                                                                                          3,
                                                                                                          4
                                                                                                        ]).findIndex((function (n) {
                                                                                                          return n % 2 === 0;
                                                                                                        }))
                                                                                                ]);
                                                                                      })
                                                                                  ],
                                                                                  /* :: */[
                                                                                    /* tuple */[
                                                                                      "typed_array - findIndexi",
                                                                                      (function () {
                                                                                          return /* Eq */Block.__(0, [
                                                                                                    0,
                                                                                                    new Int8Array(/* array */[
                                                                                                            1,
                                                                                                            2,
                                                                                                            3,
                                                                                                            4
                                                                                                          ]).findIndex((function (_, i) {
                                                                                                            return i % 2 === 0;
                                                                                                          }))
                                                                                                  ]);
                                                                                        })
                                                                                    ],
                                                                                    /* :: */[
                                                                                      /* tuple */[
                                                                                        "typed_array - forEach",
                                                                                        (function () {
                                                                                            var sum = [0];
                                                                                            new Int8Array(/* array */[
                                                                                                    1,
                                                                                                    2,
                                                                                                    3
                                                                                                  ]).forEach((function (n) {
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
                                                                                          "typed_array - forEachi",
                                                                                          (function () {
                                                                                              var sum = [0];
                                                                                              new Int8Array(/* array */[
                                                                                                      1,
                                                                                                      2,
                                                                                                      3
                                                                                                    ]).forEach((function (_, i) {
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
                                                                                            "typed_array - map",
                                                                                            (function () {
                                                                                                return /* Eq */Block.__(0, [
                                                                                                          new Int8Array(/* array */[
                                                                                                                2,
                                                                                                                4,
                                                                                                                6,
                                                                                                                8
                                                                                                              ]),
                                                                                                          new Int8Array(/* array */[
                                                                                                                  1,
                                                                                                                  2,
                                                                                                                  3,
                                                                                                                  4
                                                                                                                ]).map((function (n) {
                                                                                                                  return (n << 1);
                                                                                                                }))
                                                                                                        ]);
                                                                                              })
                                                                                          ],
                                                                                          /* :: */[
                                                                                            /* tuple */[
                                                                                              "typed_array - map",
                                                                                              (function () {
                                                                                                  return /* Eq */Block.__(0, [
                                                                                                            new Int8Array(/* array */[
                                                                                                                  0,
                                                                                                                  2,
                                                                                                                  4,
                                                                                                                  6
                                                                                                                ]),
                                                                                                            new Int8Array(/* array */[
                                                                                                                    1,
                                                                                                                    2,
                                                                                                                    3,
                                                                                                                    4
                                                                                                                  ]).map((function (_, i) {
                                                                                                                    return (i << 1);
                                                                                                                  }))
                                                                                                          ]);
                                                                                                })
                                                                                            ],
                                                                                            /* :: */[
                                                                                              /* tuple */[
                                                                                                "typed_array - reduce",
                                                                                                (function () {
                                                                                                    return /* Eq */Block.__(0, [
                                                                                                              -10,
                                                                                                              new Int8Array(/* array */[
                                                                                                                      1,
                                                                                                                      2,
                                                                                                                      3,
                                                                                                                      4
                                                                                                                    ]).reduce((function (acc, n) {
                                                                                                                      return acc - n | 0;
                                                                                                                    }), 0)
                                                                                                            ]);
                                                                                                  })
                                                                                              ],
                                                                                              /* :: */[
                                                                                                /* tuple */[
                                                                                                  "typed_array - reducei",
                                                                                                  (function () {
                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                -6,
                                                                                                                new Int8Array(/* array */[
                                                                                                                        1,
                                                                                                                        2,
                                                                                                                        3,
                                                                                                                        4
                                                                                                                      ]).reduce((function (acc, _, i) {
                                                                                                                        return acc - i | 0;
                                                                                                                      }), 0)
                                                                                                              ]);
                                                                                                    })
                                                                                                ],
                                                                                                /* :: */[
                                                                                                  /* tuple */[
                                                                                                    "typed_array - reduceRight",
                                                                                                    (function () {
                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                  -10,
                                                                                                                  new Int8Array(/* array */[
                                                                                                                          1,
                                                                                                                          2,
                                                                                                                          3,
                                                                                                                          4
                                                                                                                        ]).reduceRight((function (acc, n) {
                                                                                                                          return acc - n | 0;
                                                                                                                        }), 0)
                                                                                                                ]);
                                                                                                      })
                                                                                                  ],
                                                                                                  /* :: */[
                                                                                                    /* tuple */[
                                                                                                      "typed_array - reduceRighti",
                                                                                                      (function () {
                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                    -6,
                                                                                                                    new Int8Array(/* array */[
                                                                                                                            1,
                                                                                                                            2,
                                                                                                                            3,
                                                                                                                            4
                                                                                                                          ]).reduceRight((function (acc, _, i) {
                                                                                                                            return acc - i | 0;
                                                                                                                          }), 0)
                                                                                                                  ]);
                                                                                                        })
                                                                                                    ],
                                                                                                    /* :: */[
                                                                                                      /* tuple */[
                                                                                                        "typed_array - some",
                                                                                                        (function () {
                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                      false,
                                                                                                                      new Int8Array(/* array */[
                                                                                                                              1,
                                                                                                                              2,
                                                                                                                              3,
                                                                                                                              4
                                                                                                                            ]).some((function (n) {
                                                                                                                              return n <= 0;
                                                                                                                            }))
                                                                                                                    ]);
                                                                                                          })
                                                                                                      ],
                                                                                                      /* :: */[
                                                                                                        /* tuple */[
                                                                                                          "typed_array - somei",
                                                                                                          (function () {
                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                        true,
                                                                                                                        new Int8Array(/* array */[
                                                                                                                                1,
                                                                                                                                2,
                                                                                                                                3,
                                                                                                                                4
                                                                                                                              ]).some((function (_, i) {
                                                                                                                                return i <= 0;
                                                                                                                              }))
                                                                                                                      ]);
                                                                                                            })
                                                                                                        ],
                                                                                                        /* :: */[
                                                                                                          /* tuple */[
                                                                                                            "int8_array - _BYTES_PER_ELEMENT",
                                                                                                            (function () {
                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                          1,
                                                                                                                          Int8Array.BYTES_PER_ELEMENT
                                                                                                                        ]);
                                                                                                              })
                                                                                                          ],
                                                                                                          /* :: */[
                                                                                                            /* tuple */[
                                                                                                              "int8_array - make",
                                                                                                              (function () {
                                                                                                                  return /* Eq */Block.__(0, [
                                                                                                                            3,
                                                                                                                            new Int8Array(/* array */[
                                                                                                                                  1,
                                                                                                                                  2,
                                                                                                                                  3
                                                                                                                                ]).byteLength
                                                                                                                          ]);
                                                                                                                })
                                                                                                            ],
                                                                                                            /* :: */[
                                                                                                              /* tuple */[
                                                                                                                "int8_array - fromBuffer",
                                                                                                                (function () {
                                                                                                                    return /* Eq */Block.__(0, [
                                                                                                                              32,
                                                                                                                              new Int8Array(new ArrayBuffer(32)).byteLength
                                                                                                                            ]);
                                                                                                                  })
                                                                                                              ],
                                                                                                              /* :: */[
                                                                                                                /* tuple */[
                                                                                                                  "int8_array - fromBufferOffset",
                                                                                                                  (function () {
                                                                                                                      var buffer = new ArrayBuffer(32);
                                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                                24,
                                                                                                                                new Int8Array(buffer, 8).byteLength
                                                                                                                              ]);
                                                                                                                    })
                                                                                                                ],
                                                                                                                /* :: */[
                                                                                                                  /* tuple */[
                                                                                                                    "int8_array - fromBufferRange",
                                                                                                                    (function () {
                                                                                                                        var buffer = new ArrayBuffer(32);
                                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                                  2,
                                                                                                                                  new Int8Array(buffer, 8, 2).byteLength
                                                                                                                                ]);
                                                                                                                      })
                                                                                                                  ],
                                                                                                                  /* :: */[
                                                                                                                    /* tuple */[
                                                                                                                      "int8_array - fromLength",
                                                                                                                      (function () {
                                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                                    3,
                                                                                                                                    new Int8Array(3).byteLength
                                                                                                                                  ]);
                                                                                                                        })
                                                                                                                    ],
                                                                                                                    /* :: */[
                                                                                                                      /* tuple */[
                                                                                                                        "int8_array - unsafe_set - typed_array sanity check",
                                                                                                                        (function () {
                                                                                                                            var a = new Int8Array(/* array */[
                                                                                                                                  1,
                                                                                                                                  2,
                                                                                                                                  3,
                                                                                                                                  4,
                                                                                                                                  5
                                                                                                                                ]);
                                                                                                                            a[3] = 14;
                                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                                      14,
                                                                                                                                      a[3]
                                                                                                                                    ]);
                                                                                                                          })
                                                                                                                      ],
                                                                                                                      /* :: */[
                                                                                                                        /* tuple */[
                                                                                                                          "uint8_array - _BYTES_PER_ELEMENT",
                                                                                                                          (function () {
                                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                                        1,
                                                                                                                                        Uint8Array.BYTES_PER_ELEMENT
                                                                                                                                      ]);
                                                                                                                            })
                                                                                                                        ],
                                                                                                                        /* :: */[
                                                                                                                          /* tuple */[
                                                                                                                            "uint8_array - make",
                                                                                                                            (function () {
                                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                                          3,
                                                                                                                                          new Uint8Array(/* array */[
                                                                                                                                                1,
                                                                                                                                                2,
                                                                                                                                                3
                                                                                                                                              ]).byteLength
                                                                                                                                        ]);
                                                                                                                              })
                                                                                                                          ],
                                                                                                                          /* :: */[
                                                                                                                            /* tuple */[
                                                                                                                              "uint8_array - fromBuffer",
                                                                                                                              (function () {
                                                                                                                                  return /* Eq */Block.__(0, [
                                                                                                                                            32,
                                                                                                                                            new Uint8Array(new ArrayBuffer(32)).byteLength
                                                                                                                                          ]);
                                                                                                                                })
                                                                                                                            ],
                                                                                                                            /* :: */[
                                                                                                                              /* tuple */[
                                                                                                                                "uint8_array - fromBufferOffset",
                                                                                                                                (function () {
                                                                                                                                    var buffer = new ArrayBuffer(32);
                                                                                                                                    return /* Eq */Block.__(0, [
                                                                                                                                              24,
                                                                                                                                              new Uint8Array(buffer, 8).byteLength
                                                                                                                                            ]);
                                                                                                                                  })
                                                                                                                              ],
                                                                                                                              /* :: */[
                                                                                                                                /* tuple */[
                                                                                                                                  "uint8_array - fromBufferRange",
                                                                                                                                  (function () {
                                                                                                                                      var buffer = new ArrayBuffer(32);
                                                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                                                2,
                                                                                                                                                new Uint8Array(buffer, 8, 2).byteLength
                                                                                                                                              ]);
                                                                                                                                    })
                                                                                                                                ],
                                                                                                                                /* :: */[
                                                                                                                                  /* tuple */[
                                                                                                                                    "uint8_array - fromLength",
                                                                                                                                    (function () {
                                                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                                                  3,
                                                                                                                                                  new Uint8Array(3).byteLength
                                                                                                                                                ]);
                                                                                                                                      })
                                                                                                                                  ],
                                                                                                                                  /* :: */[
                                                                                                                                    /* tuple */[
                                                                                                                                      "uint8_array - unsafe_set - typed_array sanity check",
                                                                                                                                      (function () {
                                                                                                                                          var a = new Uint8Array(/* array */[
                                                                                                                                                1,
                                                                                                                                                2,
                                                                                                                                                3,
                                                                                                                                                4,
                                                                                                                                                5
                                                                                                                                              ]);
                                                                                                                                          a[3] = 14;
                                                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                                                    14,
                                                                                                                                                    a[3]
                                                                                                                                                  ]);
                                                                                                                                        })
                                                                                                                                    ],
                                                                                                                                    /* :: */[
                                                                                                                                      /* tuple */[
                                                                                                                                        "uint8clamped_array - _BYTES_PER_ELEMENT",
                                                                                                                                        (function () {
                                                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                                                      1,
                                                                                                                                                      Uint8ClampedArray.BYTES_PER_ELEMENT
                                                                                                                                                    ]);
                                                                                                                                          })
                                                                                                                                      ],
                                                                                                                                      /* :: */[
                                                                                                                                        /* tuple */[
                                                                                                                                          "uint8clamped_array - make",
                                                                                                                                          (function () {
                                                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                                                        3,
                                                                                                                                                        new Uint8ClampedArray(/* array */[
                                                                                                                                                              1,
                                                                                                                                                              2,
                                                                                                                                                              3
                                                                                                                                                            ]).byteLength
                                                                                                                                                      ]);
                                                                                                                                            })
                                                                                                                                        ],
                                                                                                                                        /* :: */[
                                                                                                                                          /* tuple */[
                                                                                                                                            "uint8clamped_array - fromBuffer",
                                                                                                                                            (function () {
                                                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                                                          32,
                                                                                                                                                          new Uint8ClampedArray(new ArrayBuffer(32)).byteLength
                                                                                                                                                        ]);
                                                                                                                                              })
                                                                                                                                          ],
                                                                                                                                          /* :: */[
                                                                                                                                            /* tuple */[
                                                                                                                                              "uint8clamped_array - fromBufferOffset",
                                                                                                                                              (function () {
                                                                                                                                                  var buffer = new ArrayBuffer(32);
                                                                                                                                                  return /* Eq */Block.__(0, [
                                                                                                                                                            24,
                                                                                                                                                            new Uint8ClampedArray(buffer, 8).byteLength
                                                                                                                                                          ]);
                                                                                                                                                })
                                                                                                                                            ],
                                                                                                                                            /* :: */[
                                                                                                                                              /* tuple */[
                                                                                                                                                "uint8clamped_array - fromBufferRange",
                                                                                                                                                (function () {
                                                                                                                                                    var buffer = new ArrayBuffer(32);
                                                                                                                                                    return /* Eq */Block.__(0, [
                                                                                                                                                              2,
                                                                                                                                                              new Uint8ClampedArray(buffer, 8, 2).byteLength
                                                                                                                                                            ]);
                                                                                                                                                  })
                                                                                                                                              ],
                                                                                                                                              /* :: */[
                                                                                                                                                /* tuple */[
                                                                                                                                                  "uint8clamped_array - fromLength",
                                                                                                                                                  (function () {
                                                                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                                                                3,
                                                                                                                                                                new Uint8ClampedArray(3).byteLength
                                                                                                                                                              ]);
                                                                                                                                                    })
                                                                                                                                                ],
                                                                                                                                                /* :: */[
                                                                                                                                                  /* tuple */[
                                                                                                                                                    "uint8clamped_array - unsafe_set - typed_array sanity check",
                                                                                                                                                    (function () {
                                                                                                                                                        var a = new Uint8ClampedArray(/* array */[
                                                                                                                                                              1,
                                                                                                                                                              2,
                                                                                                                                                              3,
                                                                                                                                                              4,
                                                                                                                                                              5
                                                                                                                                                            ]);
                                                                                                                                                        a[3] = 14;
                                                                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                                                                  14,
                                                                                                                                                                  a[3]
                                                                                                                                                                ]);
                                                                                                                                                      })
                                                                                                                                                  ],
                                                                                                                                                  /* :: */[
                                                                                                                                                    /* tuple */[
                                                                                                                                                      "int16_array - _BYTES_PER_ELEMENT",
                                                                                                                                                      (function () {
                                                                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                                                                    2,
                                                                                                                                                                    Int16Array.BYTES_PER_ELEMENT
                                                                                                                                                                  ]);
                                                                                                                                                        })
                                                                                                                                                    ],
                                                                                                                                                    /* :: */[
                                                                                                                                                      /* tuple */[
                                                                                                                                                        "int16_array - make",
                                                                                                                                                        (function () {
                                                                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                                                                      6,
                                                                                                                                                                      new Int16Array(/* array */[
                                                                                                                                                                            1,
                                                                                                                                                                            2,
                                                                                                                                                                            3
                                                                                                                                                                          ]).byteLength
                                                                                                                                                                    ]);
                                                                                                                                                          })
                                                                                                                                                      ],
                                                                                                                                                      /* :: */[
                                                                                                                                                        /* tuple */[
                                                                                                                                                          "int16_array - fromBuffer",
                                                                                                                                                          (function () {
                                                                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                                                                        32,
                                                                                                                                                                        new Int16Array(new ArrayBuffer(32)).byteLength
                                                                                                                                                                      ]);
                                                                                                                                                            })
                                                                                                                                                        ],
                                                                                                                                                        /* :: */[
                                                                                                                                                          /* tuple */[
                                                                                                                                                            "int16_array - fromBufferOffset",
                                                                                                                                                            (function () {
                                                                                                                                                                var buffer = new ArrayBuffer(32);
                                                                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                                                                          24,
                                                                                                                                                                          new Int16Array(buffer, 8).byteLength
                                                                                                                                                                        ]);
                                                                                                                                                              })
                                                                                                                                                          ],
                                                                                                                                                          /* :: */[
                                                                                                                                                            /* tuple */[
                                                                                                                                                              "int16_array - fromBufferRange",
                                                                                                                                                              (function () {
                                                                                                                                                                  var buffer = new ArrayBuffer(32);
                                                                                                                                                                  return /* Eq */Block.__(0, [
                                                                                                                                                                            4,
                                                                                                                                                                            new Int16Array(buffer, 8, 2).byteLength
                                                                                                                                                                          ]);
                                                                                                                                                                })
                                                                                                                                                            ],
                                                                                                                                                            /* :: */[
                                                                                                                                                              /* tuple */[
                                                                                                                                                                "int16_array - fromLength",
                                                                                                                                                                (function () {
                                                                                                                                                                    return /* Eq */Block.__(0, [
                                                                                                                                                                              6,
                                                                                                                                                                              new Int16Array(3).byteLength
                                                                                                                                                                            ]);
                                                                                                                                                                  })
                                                                                                                                                              ],
                                                                                                                                                              /* :: */[
                                                                                                                                                                /* tuple */[
                                                                                                                                                                  "int16_array - unsafe_set - typed_array sanity check",
                                                                                                                                                                  (function () {
                                                                                                                                                                      var a = new Int16Array(/* array */[
                                                                                                                                                                            1,
                                                                                                                                                                            2,
                                                                                                                                                                            3,
                                                                                                                                                                            4,
                                                                                                                                                                            5
                                                                                                                                                                          ]);
                                                                                                                                                                      a[3] = 14;
                                                                                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                                                                                14,
                                                                                                                                                                                a[3]
                                                                                                                                                                              ]);
                                                                                                                                                                    })
                                                                                                                                                                ],
                                                                                                                                                                /* :: */[
                                                                                                                                                                  /* tuple */[
                                                                                                                                                                    "uint16_array - _BYTES_PER_ELEMENT",
                                                                                                                                                                    (function () {
                                                                                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                                                                                  2,
                                                                                                                                                                                  Uint16Array.BYTES_PER_ELEMENT
                                                                                                                                                                                ]);
                                                                                                                                                                      })
                                                                                                                                                                  ],
                                                                                                                                                                  /* :: */[
                                                                                                                                                                    /* tuple */[
                                                                                                                                                                      "uint16_array - make",
                                                                                                                                                                      (function () {
                                                                                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                                                                                    6,
                                                                                                                                                                                    new Uint16Array(/* array */[
                                                                                                                                                                                          1,
                                                                                                                                                                                          2,
                                                                                                                                                                                          3
                                                                                                                                                                                        ]).byteLength
                                                                                                                                                                                  ]);
                                                                                                                                                                        })
                                                                                                                                                                    ],
                                                                                                                                                                    /* :: */[
                                                                                                                                                                      /* tuple */[
                                                                                                                                                                        "uint16_array - fromBuffer",
                                                                                                                                                                        (function () {
                                                                                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                                                                                      32,
                                                                                                                                                                                      new Uint16Array(new ArrayBuffer(32)).byteLength
                                                                                                                                                                                    ]);
                                                                                                                                                                          })
                                                                                                                                                                      ],
                                                                                                                                                                      /* :: */[
                                                                                                                                                                        /* tuple */[
                                                                                                                                                                          "uint16_array - fromBufferOffset",
                                                                                                                                                                          (function () {
                                                                                                                                                                              var buffer = new ArrayBuffer(32);
                                                                                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                                                                                        24,
                                                                                                                                                                                        new Uint16Array(buffer, 8).byteLength
                                                                                                                                                                                      ]);
                                                                                                                                                                            })
                                                                                                                                                                        ],
                                                                                                                                                                        /* :: */[
                                                                                                                                                                          /* tuple */[
                                                                                                                                                                            "uint16_array - fromBufferRange",
                                                                                                                                                                            (function () {
                                                                                                                                                                                var buffer = new ArrayBuffer(32);
                                                                                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                                                                                          4,
                                                                                                                                                                                          new Uint16Array(buffer, 8, 2).byteLength
                                                                                                                                                                                        ]);
                                                                                                                                                                              })
                                                                                                                                                                          ],
                                                                                                                                                                          /* :: */[
                                                                                                                                                                            /* tuple */[
                                                                                                                                                                              "uint16_array - fromLength",
                                                                                                                                                                              (function () {
                                                                                                                                                                                  return /* Eq */Block.__(0, [
                                                                                                                                                                                            6,
                                                                                                                                                                                            new Uint16Array(3).byteLength
                                                                                                                                                                                          ]);
                                                                                                                                                                                })
                                                                                                                                                                            ],
                                                                                                                                                                            /* :: */[
                                                                                                                                                                              /* tuple */[
                                                                                                                                                                                "uint16_array - unsafe_set - typed_array sanity check",
                                                                                                                                                                                (function () {
                                                                                                                                                                                    var a = new Uint16Array(/* array */[
                                                                                                                                                                                          1,
                                                                                                                                                                                          2,
                                                                                                                                                                                          3,
                                                                                                                                                                                          4,
                                                                                                                                                                                          5
                                                                                                                                                                                        ]);
                                                                                                                                                                                    a[3] = 14;
                                                                                                                                                                                    return /* Eq */Block.__(0, [
                                                                                                                                                                                              14,
                                                                                                                                                                                              a[3]
                                                                                                                                                                                            ]);
                                                                                                                                                                                  })
                                                                                                                                                                              ],
                                                                                                                                                                              /* :: */[
                                                                                                                                                                                /* tuple */[
                                                                                                                                                                                  "int32_array - _BYTES_PER_ELEMENT",
                                                                                                                                                                                  (function () {
                                                                                                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                                                                                                4,
                                                                                                                                                                                                Int32Array.BYTES_PER_ELEMENT
                                                                                                                                                                                              ]);
                                                                                                                                                                                    })
                                                                                                                                                                                ],
                                                                                                                                                                                /* :: */[
                                                                                                                                                                                  /* tuple */[
                                                                                                                                                                                    "int32_array - make",
                                                                                                                                                                                    (function () {
                                                                                                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                                                                                                  12,
                                                                                                                                                                                                  new Int32Array($$Array.map((function (prim) {
                                                                                                                                                                                                              return prim;
                                                                                                                                                                                                            }), /* array */[
                                                                                                                                                                                                            1,
                                                                                                                                                                                                            2,
                                                                                                                                                                                                            3
                                                                                                                                                                                                          ])).byteLength
                                                                                                                                                                                                ]);
                                                                                                                                                                                      })
                                                                                                                                                                                  ],
                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                    /* tuple */[
                                                                                                                                                                                      "int32_array - fromBuffer",
                                                                                                                                                                                      (function () {
                                                                                                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                                                                                                    32,
                                                                                                                                                                                                    new Int32Array(new ArrayBuffer(32)).byteLength
                                                                                                                                                                                                  ]);
                                                                                                                                                                                        })
                                                                                                                                                                                    ],
                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                      /* tuple */[
                                                                                                                                                                                        "int32_array - fromBufferOffset",
                                                                                                                                                                                        (function () {
                                                                                                                                                                                            var buffer = new ArrayBuffer(32);
                                                                                                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                                                                                                      24,
                                                                                                                                                                                                      new Int32Array(buffer, 8).byteLength
                                                                                                                                                                                                    ]);
                                                                                                                                                                                          })
                                                                                                                                                                                      ],
                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                        /* tuple */[
                                                                                                                                                                                          "int32_array - fromBufferRange",
                                                                                                                                                                                          (function () {
                                                                                                                                                                                              var buffer = new ArrayBuffer(32);
                                                                                                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                                                                                                        8,
                                                                                                                                                                                                        new Int32Array(buffer, 8, 2).byteLength
                                                                                                                                                                                                      ]);
                                                                                                                                                                                            })
                                                                                                                                                                                        ],
                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                          /* tuple */[
                                                                                                                                                                                            "int32_array - fromLength",
                                                                                                                                                                                            (function () {
                                                                                                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                                                                                                          12,
                                                                                                                                                                                                          new Int32Array(3).byteLength
                                                                                                                                                                                                        ]);
                                                                                                                                                                                              })
                                                                                                                                                                                          ],
                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                            /* tuple */[
                                                                                                                                                                                              "int32_array - unsafe_set - typed_array sanity check",
                                                                                                                                                                                              (function () {
                                                                                                                                                                                                  var a = new Int32Array($$Array.map((function (prim) {
                                                                                                                                                                                                              return prim;
                                                                                                                                                                                                            }), /* array */[
                                                                                                                                                                                                            1,
                                                                                                                                                                                                            2,
                                                                                                                                                                                                            3,
                                                                                                                                                                                                            4,
                                                                                                                                                                                                            5
                                                                                                                                                                                                          ]));
                                                                                                                                                                                                  a[3] = 14;
                                                                                                                                                                                                  return /* Eq */Block.__(0, [
                                                                                                                                                                                                            14,
                                                                                                                                                                                                            a[3]
                                                                                                                                                                                                          ]);
                                                                                                                                                                                                })
                                                                                                                                                                                            ],
                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                              /* tuple */[
                                                                                                                                                                                                "uint32_array - _BYTES_PER_ELEMENT",
                                                                                                                                                                                                (function () {
                                                                                                                                                                                                    return /* Eq */Block.__(0, [
                                                                                                                                                                                                              4,
                                                                                                                                                                                                              Uint32Array.BYTES_PER_ELEMENT
                                                                                                                                                                                                            ]);
                                                                                                                                                                                                  })
                                                                                                                                                                                              ],
                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                /* tuple */[
                                                                                                                                                                                                  "uint32_array - make",
                                                                                                                                                                                                  (function () {
                                                                                                                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                                                                                                                12,
                                                                                                                                                                                                                new Uint32Array(/* array */[
                                                                                                                                                                                                                      1,
                                                                                                                                                                                                                      2,
                                                                                                                                                                                                                      3
                                                                                                                                                                                                                    ]).byteLength
                                                                                                                                                                                                              ]);
                                                                                                                                                                                                    })
                                                                                                                                                                                                ],
                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                  /* tuple */[
                                                                                                                                                                                                    "uint32_array - fromBuffer",
                                                                                                                                                                                                    (function () {
                                                                                                                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                                                                                                                  32,
                                                                                                                                                                                                                  new Uint32Array(new ArrayBuffer(32)).byteLength
                                                                                                                                                                                                                ]);
                                                                                                                                                                                                      })
                                                                                                                                                                                                  ],
                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                    /* tuple */[
                                                                                                                                                                                                      "uint32_array - fromBufferOffset",
                                                                                                                                                                                                      (function () {
                                                                                                                                                                                                          var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                                                                                                                    24,
                                                                                                                                                                                                                    new Uint32Array(buffer, 8).byteLength
                                                                                                                                                                                                                  ]);
                                                                                                                                                                                                        })
                                                                                                                                                                                                    ],
                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                      /* tuple */[
                                                                                                                                                                                                        "uint32_array - fromBufferRange",
                                                                                                                                                                                                        (function () {
                                                                                                                                                                                                            var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                                                                                                                      8,
                                                                                                                                                                                                                      new Uint32Array(buffer, 8, 2).byteLength
                                                                                                                                                                                                                    ]);
                                                                                                                                                                                                          })
                                                                                                                                                                                                      ],
                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                        /* tuple */[
                                                                                                                                                                                                          "uint32_array - fromLength",
                                                                                                                                                                                                          (function () {
                                                                                                                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                                                                                                                        12,
                                                                                                                                                                                                                        new Uint32Array(3).byteLength
                                                                                                                                                                                                                      ]);
                                                                                                                                                                                                            })
                                                                                                                                                                                                        ],
                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                          /* tuple */[
                                                                                                                                                                                                            "uint32_array - unsafe_set - typed_array sanity check",
                                                                                                                                                                                                            (function () {
                                                                                                                                                                                                                var a = new Uint32Array(/* array */[
                                                                                                                                                                                                                      1,
                                                                                                                                                                                                                      2,
                                                                                                                                                                                                                      3,
                                                                                                                                                                                                                      4,
                                                                                                                                                                                                                      5
                                                                                                                                                                                                                    ]);
                                                                                                                                                                                                                a[3] = 14;
                                                                                                                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                                                                                                                          14,
                                                                                                                                                                                                                          a[3]
                                                                                                                                                                                                                        ]);
                                                                                                                                                                                                              })
                                                                                                                                                                                                          ],
                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                            /* tuple */[
                                                                                                                                                                                                              "float32_array - _BYTES_PER_ELEMENT",
                                                                                                                                                                                                              (function () {
                                                                                                                                                                                                                  return /* Eq */Block.__(0, [
                                                                                                                                                                                                                            4,
                                                                                                                                                                                                                            Float32Array.BYTES_PER_ELEMENT
                                                                                                                                                                                                                          ]);
                                                                                                                                                                                                                })
                                                                                                                                                                                                            ],
                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                              /* tuple */[
                                                                                                                                                                                                                "float32_array - make",
                                                                                                                                                                                                                (function () {
                                                                                                                                                                                                                    return /* Eq */Block.__(0, [
                                                                                                                                                                                                                              12,
                                                                                                                                                                                                                              new Float32Array(/* array */[
                                                                                                                                                                                                                                    1,
                                                                                                                                                                                                                                    2,
                                                                                                                                                                                                                                    3
                                                                                                                                                                                                                                  ]).byteLength
                                                                                                                                                                                                                            ]);
                                                                                                                                                                                                                  })
                                                                                                                                                                                                              ],
                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                /* tuple */[
                                                                                                                                                                                                                  "float32_array - fromBuffer",
                                                                                                                                                                                                                  (function () {
                                                                                                                                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                32,
                                                                                                                                                                                                                                new Float32Array(new ArrayBuffer(32)).byteLength
                                                                                                                                                                                                                              ]);
                                                                                                                                                                                                                    })
                                                                                                                                                                                                                ],
                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                  /* tuple */[
                                                                                                                                                                                                                    "float32_array - fromBufferOffset",
                                                                                                                                                                                                                    (function () {
                                                                                                                                                                                                                        var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                  24,
                                                                                                                                                                                                                                  new Float32Array(buffer, 8).byteLength
                                                                                                                                                                                                                                ]);
                                                                                                                                                                                                                      })
                                                                                                                                                                                                                  ],
                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                    /* tuple */[
                                                                                                                                                                                                                      "float32_array - fromBufferRange",
                                                                                                                                                                                                                      (function () {
                                                                                                                                                                                                                          var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                    8,
                                                                                                                                                                                                                                    new Float32Array(buffer, 8, 2).byteLength
                                                                                                                                                                                                                                  ]);
                                                                                                                                                                                                                        })
                                                                                                                                                                                                                    ],
                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                      /* tuple */[
                                                                                                                                                                                                                        "float32_array - fromLength",
                                                                                                                                                                                                                        (function () {
                                                                                                                                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                      12,
                                                                                                                                                                                                                                      new Float32Array(3).byteLength
                                                                                                                                                                                                                                    ]);
                                                                                                                                                                                                                          })
                                                                                                                                                                                                                      ],
                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                        /* tuple */[
                                                                                                                                                                                                                          "float32_array - unsafe_set - typed_array sanity check",
                                                                                                                                                                                                                          (function () {
                                                                                                                                                                                                                              var a = new Float32Array(/* array */[
                                                                                                                                                                                                                                    1,
                                                                                                                                                                                                                                    2,
                                                                                                                                                                                                                                    3,
                                                                                                                                                                                                                                    4,
                                                                                                                                                                                                                                    5
                                                                                                                                                                                                                                  ]);
                                                                                                                                                                                                                              a[3] = 14;
                                                                                                                                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                        14,
                                                                                                                                                                                                                                        a[3]
                                                                                                                                                                                                                                      ]);
                                                                                                                                                                                                                            })
                                                                                                                                                                                                                        ],
                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                          /* tuple */[
                                                                                                                                                                                                                            "float64_array - _BYTES_PER_ELEMENT",
                                                                                                                                                                                                                            (function () {
                                                                                                                                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                          8,
                                                                                                                                                                                                                                          Float64Array.BYTES_PER_ELEMENT
                                                                                                                                                                                                                                        ]);
                                                                                                                                                                                                                              })
                                                                                                                                                                                                                          ],
                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                            /* tuple */[
                                                                                                                                                                                                                              "float64_array - make",
                                                                                                                                                                                                                              (function () {
                                                                                                                                                                                                                                  return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                            24,
                                                                                                                                                                                                                                            new Float64Array(/* array */[
                                                                                                                                                                                                                                                  1,
                                                                                                                                                                                                                                                  2,
                                                                                                                                                                                                                                                  3
                                                                                                                                                                                                                                                ]).byteLength
                                                                                                                                                                                                                                          ]);
                                                                                                                                                                                                                                })
                                                                                                                                                                                                                            ],
                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                              /* tuple */[
                                                                                                                                                                                                                                "float64_array - fromBuffer",
                                                                                                                                                                                                                                (function () {
                                                                                                                                                                                                                                    return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                              32,
                                                                                                                                                                                                                                              new Float64Array(new ArrayBuffer(32)).byteLength
                                                                                                                                                                                                                                            ]);
                                                                                                                                                                                                                                  })
                                                                                                                                                                                                                              ],
                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                /* tuple */[
                                                                                                                                                                                                                                  "float64_array - fromBufferOffset",
                                                                                                                                                                                                                                  (function () {
                                                                                                                                                                                                                                      var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                24,
                                                                                                                                                                                                                                                new Float64Array(buffer, 8).byteLength
                                                                                                                                                                                                                                              ]);
                                                                                                                                                                                                                                    })
                                                                                                                                                                                                                                ],
                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                  /* tuple */[
                                                                                                                                                                                                                                    "float64_array - fromBufferRange",
                                                                                                                                                                                                                                    (function () {
                                                                                                                                                                                                                                        var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                  16,
                                                                                                                                                                                                                                                  new Float64Array(buffer, 8, 2).byteLength
                                                                                                                                                                                                                                                ]);
                                                                                                                                                                                                                                      })
                                                                                                                                                                                                                                  ],
                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                    /* tuple */[
                                                                                                                                                                                                                                      "float64_array - fromLength",
                                                                                                                                                                                                                                      (function () {
                                                                                                                                                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                    24,
                                                                                                                                                                                                                                                    new Float64Array(3).byteLength
                                                                                                                                                                                                                                                  ]);
                                                                                                                                                                                                                                        })
                                                                                                                                                                                                                                    ],
                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                      /* tuple */[
                                                                                                                                                                                                                                        "float64_array - unsafe_set - typed_array sanity check",
                                                                                                                                                                                                                                        (function () {
                                                                                                                                                                                                                                            var a = new Float64Array(/* array */[
                                                                                                                                                                                                                                                  1,
                                                                                                                                                                                                                                                  2,
                                                                                                                                                                                                                                                  3,
                                                                                                                                                                                                                                                  4,
                                                                                                                                                                                                                                                  5
                                                                                                                                                                                                                                                ]);
                                                                                                                                                                                                                                            a[3] = 14;
                                                                                                                                                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                      14,
                                                                                                                                                                                                                                                      a[3]
                                                                                                                                                                                                                                                    ]);
                                                                                                                                                                                                                                          })
                                                                                                                                                                                                                                      ],
                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                        /* tuple */[
                                                                                                                                                                                                                                          "DataView - make, byteLength",
                                                                                                                                                                                                                                          (function () {
                                                                                                                                                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                        32,
                                                                                                                                                                                                                                                        new DataView(new ArrayBuffer(32)).byteLength
                                                                                                                                                                                                                                                      ]);
                                                                                                                                                                                                                                            })
                                                                                                                                                                                                                                        ],
                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                          /* tuple */[
                                                                                                                                                                                                                                            "DataView - fromBuffer",
                                                                                                                                                                                                                                            (function () {
                                                                                                                                                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                          32,
                                                                                                                                                                                                                                                          new DataView(new ArrayBuffer(32)).byteLength
                                                                                                                                                                                                                                                        ]);
                                                                                                                                                                                                                                              })
                                                                                                                                                                                                                                          ],
                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                            /* tuple */[
                                                                                                                                                                                                                                              "DataView - fromBufferOffset",
                                                                                                                                                                                                                                              (function () {
                                                                                                                                                                                                                                                  var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                                                  return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                            24,
                                                                                                                                                                                                                                                            new DataView(buffer, 8).byteLength
                                                                                                                                                                                                                                                          ]);
                                                                                                                                                                                                                                                })
                                                                                                                                                                                                                                            ],
                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                              /* tuple */[
                                                                                                                                                                                                                                                "DataView - fromBufferRange",
                                                                                                                                                                                                                                                (function () {
                                                                                                                                                                                                                                                    var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                                                    return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                              4,
                                                                                                                                                                                                                                                              new DataView(buffer, 8, 4).byteLength
                                                                                                                                                                                                                                                            ]);
                                                                                                                                                                                                                                                  })
                                                                                                                                                                                                                                              ],
                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                /* tuple */[
                                                                                                                                                                                                                                                  "DataView - buffer",
                                                                                                                                                                                                                                                  (function () {
                                                                                                                                                                                                                                                      var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                buffer,
                                                                                                                                                                                                                                                                new DataView(buffer).buffer
                                                                                                                                                                                                                                                              ]);
                                                                                                                                                                                                                                                    })
                                                                                                                                                                                                                                                ],
                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                  /* tuple */[
                                                                                                                                                                                                                                                    "DataView - byteOffset",
                                                                                                                                                                                                                                                    (function () {
                                                                                                                                                                                                                                                        var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                  8,
                                                                                                                                                                                                                                                                  new DataView(buffer, 8).byteOffset
                                                                                                                                                                                                                                                                ]);
                                                                                                                                                                                                                                                      })
                                                                                                                                                                                                                                                  ],
                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                    /* tuple */[
                                                                                                                                                                                                                                                      "DataView - setInt8, getInt8",
                                                                                                                                                                                                                                                      (function () {
                                                                                                                                                                                                                                                          var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                          var view = new DataView(buffer);
                                                                                                                                                                                                                                                          view.setInt8(0, 1);
                                                                                                                                                                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                    1,
                                                                                                                                                                                                                                                                    view.getInt8(0)
                                                                                                                                                                                                                                                                  ]);
                                                                                                                                                                                                                                                        })
                                                                                                                                                                                                                                                    ],
                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                      /* tuple */[
                                                                                                                                                                                                                                                        "DataView - setUint8, getUint8",
                                                                                                                                                                                                                                                        (function () {
                                                                                                                                                                                                                                                            var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                            var view = new DataView(buffer);
                                                                                                                                                                                                                                                            view.setUint8(0, 128);
                                                                                                                                                                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                      128,
                                                                                                                                                                                                                                                                      view.getUint8(0)
                                                                                                                                                                                                                                                                    ]);
                                                                                                                                                                                                                                                          })
                                                                                                                                                                                                                                                      ],
                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                        /* tuple */[
                                                                                                                                                                                                                                                          "DataView - setInt16, getInt16",
                                                                                                                                                                                                                                                          (function () {
                                                                                                                                                                                                                                                              var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                              var view = new DataView(buffer);
                                                                                                                                                                                                                                                              view.setInt16(0, 257);
                                                                                                                                                                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                        257,
                                                                                                                                                                                                                                                                        view.getInt16(0)
                                                                                                                                                                                                                                                                      ]);
                                                                                                                                                                                                                                                            })
                                                                                                                                                                                                                                                        ],
                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                          /* tuple */[
                                                                                                                                                                                                                                                            "DataView - getInt16LittleEndian",
                                                                                                                                                                                                                                                            (function () {
                                                                                                                                                                                                                                                                var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                var view = new DataView(buffer);
                                                                                                                                                                                                                                                                view.setInt16(0, 25000, 1);
                                                                                                                                                                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                          25000,
                                                                                                                                                                                                                                                                          view.getInt16(0, 1)
                                                                                                                                                                                                                                                                        ]);
                                                                                                                                                                                                                                                              })
                                                                                                                                                                                                                                                          ],
                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                            /* tuple */[
                                                                                                                                                                                                                                                              "DataView - setInt16LittleEndian",
                                                                                                                                                                                                                                                              (function () {
                                                                                                                                                                                                                                                                  var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                  var view = new DataView(buffer);
                                                                                                                                                                                                                                                                  view.setInt16(0, 25000, 1);
                                                                                                                                                                                                                                                                  return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                            -22431,
                                                                                                                                                                                                                                                                            view.getInt16(0)
                                                                                                                                                                                                                                                                          ]);
                                                                                                                                                                                                                                                                })
                                                                                                                                                                                                                                                            ],
                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                              /* tuple */[
                                                                                                                                                                                                                                                                "DataView - setUint16, getUint16",
                                                                                                                                                                                                                                                                (function () {
                                                                                                                                                                                                                                                                    var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                    var view = new DataView(buffer);
                                                                                                                                                                                                                                                                    view.setUint16(0, 32768);
                                                                                                                                                                                                                                                                    return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                              32768,
                                                                                                                                                                                                                                                                              view.getUint16(0)
                                                                                                                                                                                                                                                                            ]);
                                                                                                                                                                                                                                                                  })
                                                                                                                                                                                                                                                              ],
                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                /* tuple */[
                                                                                                                                                                                                                                                                  "DataView - getUint16LittleEndian",
                                                                                                                                                                                                                                                                  (function () {
                                                                                                                                                                                                                                                                      var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                      var view = new DataView(buffer);
                                                                                                                                                                                                                                                                      view.setUint16(0, 32768, 1);
                                                                                                                                                                                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                                32768,
                                                                                                                                                                                                                                                                                view.getUint16(0, 1)
                                                                                                                                                                                                                                                                              ]);
                                                                                                                                                                                                                                                                    })
                                                                                                                                                                                                                                                                ],
                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                  /* tuple */[
                                                                                                                                                                                                                                                                    "DataView - setUint16LittleEndian",
                                                                                                                                                                                                                                                                    (function () {
                                                                                                                                                                                                                                                                        var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                        var view = new DataView(buffer);
                                                                                                                                                                                                                                                                        view.setUint16(0, 32768, 1);
                                                                                                                                                                                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                                  128,
                                                                                                                                                                                                                                                                                  view.getUint16(0)
                                                                                                                                                                                                                                                                                ]);
                                                                                                                                                                                                                                                                      })
                                                                                                                                                                                                                                                                  ],
                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                    /* tuple */[
                                                                                                                                                                                                                                                                      "DataView - setInt32, getInt32",
                                                                                                                                                                                                                                                                      (function () {
                                                                                                                                                                                                                                                                          var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                          var view = new DataView(buffer);
                                                                                                                                                                                                                                                                          view.setInt32(0, 65537);
                                                                                                                                                                                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                                    65537,
                                                                                                                                                                                                                                                                                    view.getInt32(0)
                                                                                                                                                                                                                                                                                  ]);
                                                                                                                                                                                                                                                                        })
                                                                                                                                                                                                                                                                    ],
                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                      /* tuple */[
                                                                                                                                                                                                                                                                        "DataView - getInt32LittleEndian",
                                                                                                                                                                                                                                                                        (function () {
                                                                                                                                                                                                                                                                            var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                            var view = new DataView(buffer);
                                                                                                                                                                                                                                                                            view.setInt32(0, 65537, 1);
                                                                                                                                                                                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                                      65537,
                                                                                                                                                                                                                                                                                      view.getInt32(0, 1)
                                                                                                                                                                                                                                                                                    ]);
                                                                                                                                                                                                                                                                          })
                                                                                                                                                                                                                                                                      ],
                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                        /* tuple */[
                                                                                                                                                                                                                                                                          "DataView - setInt32LittleEndian",
                                                                                                                                                                                                                                                                          (function () {
                                                                                                                                                                                                                                                                              var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                              var view = new DataView(buffer);
                                                                                                                                                                                                                                                                              view.setInt32(0, 65537, 1);
                                                                                                                                                                                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                                        16777472,
                                                                                                                                                                                                                                                                                        view.getInt32(0)
                                                                                                                                                                                                                                                                                      ]);
                                                                                                                                                                                                                                                                            })
                                                                                                                                                                                                                                                                        ],
                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                          /* tuple */[
                                                                                                                                                                                                                                                                            "DataView - setUint32, getUint32",
                                                                                                                                                                                                                                                                            (function () {
                                                                                                                                                                                                                                                                                var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                view.setUint32(0, 65537);
                                                                                                                                                                                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                                          65537,
                                                                                                                                                                                                                                                                                          view.getUint32(0)
                                                                                                                                                                                                                                                                                        ]);
                                                                                                                                                                                                                                                                              })
                                                                                                                                                                                                                                                                          ],
                                                                                                                                                                                                                                                                          /* :: */[
                                                                                                                                                                                                                                                                            /* tuple */[
                                                                                                                                                                                                                                                                              "DataView - getUint32LittleEndian",
                                                                                                                                                                                                                                                                              (function () {
                                                                                                                                                                                                                                                                                  var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                  var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                  view.setUint32(0, 65537, 1);
                                                                                                                                                                                                                                                                                  return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                                            65537,
                                                                                                                                                                                                                                                                                            view.getUint32(0, 1)
                                                                                                                                                                                                                                                                                          ]);
                                                                                                                                                                                                                                                                                })
                                                                                                                                                                                                                                                                            ],
                                                                                                                                                                                                                                                                            /* :: */[
                                                                                                                                                                                                                                                                              /* tuple */[
                                                                                                                                                                                                                                                                                "DataView - setUint32LittleEndian",
                                                                                                                                                                                                                                                                                (function () {
                                                                                                                                                                                                                                                                                    var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                    var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                    view.setUint32(0, 65537, 1);
                                                                                                                                                                                                                                                                                    return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                                              16777472,
                                                                                                                                                                                                                                                                                              view.getUint32(0)
                                                                                                                                                                                                                                                                                            ]);
                                                                                                                                                                                                                                                                                  })
                                                                                                                                                                                                                                                                              ],
                                                                                                                                                                                                                                                                              /* :: */[
                                                                                                                                                                                                                                                                                /* tuple */[
                                                                                                                                                                                                                                                                                  "DataView - setFloat32, getFloat32",
                                                                                                                                                                                                                                                                                  (function () {
                                                                                                                                                                                                                                                                                      var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                      var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                      view.setFloat32(0, 65537.0);
                                                                                                                                                                                                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                                                65537.0,
                                                                                                                                                                                                                                                                                                view.getFloat32(0)
                                                                                                                                                                                                                                                                                              ]);
                                                                                                                                                                                                                                                                                    })
                                                                                                                                                                                                                                                                                ],
                                                                                                                                                                                                                                                                                /* :: */[
                                                                                                                                                                                                                                                                                  /* tuple */[
                                                                                                                                                                                                                                                                                    "DataView - getFloat32LittleEndian",
                                                                                                                                                                                                                                                                                    (function () {
                                                                                                                                                                                                                                                                                        var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                        var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                        view.setFloat32(0, 65537.0, 1);
                                                                                                                                                                                                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                                                  65537.0,
                                                                                                                                                                                                                                                                                                  view.getFloat32(0, 1)
                                                                                                                                                                                                                                                                                                ]);
                                                                                                                                                                                                                                                                                      })
                                                                                                                                                                                                                                                                                  ],
                                                                                                                                                                                                                                                                                  /* :: */[
                                                                                                                                                                                                                                                                                    /* tuple */[
                                                                                                                                                                                                                                                                                      "DataView - setFloat32LittleEndian",
                                                                                                                                                                                                                                                                                      (function () {
                                                                                                                                                                                                                                                                                          var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                          var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                          view.setFloat32(0, 1.0, 1);
                                                                                                                                                                                                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                                                    4.600602988224807e-41,
                                                                                                                                                                                                                                                                                                    view.getFloat32(0)
                                                                                                                                                                                                                                                                                                  ]);
                                                                                                                                                                                                                                                                                        })
                                                                                                                                                                                                                                                                                    ],
                                                                                                                                                                                                                                                                                    /* :: */[
                                                                                                                                                                                                                                                                                      /* tuple */[
                                                                                                                                                                                                                                                                                        "DataView - setFloat64, getFloat64",
                                                                                                                                                                                                                                                                                        (function () {
                                                                                                                                                                                                                                                                                            var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                            var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                            view.setFloat64(0, 1e200);
                                                                                                                                                                                                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                                                      1e200,
                                                                                                                                                                                                                                                                                                      view.getFloat64(0)
                                                                                                                                                                                                                                                                                                    ]);
                                                                                                                                                                                                                                                                                          })
                                                                                                                                                                                                                                                                                      ],
                                                                                                                                                                                                                                                                                      /* :: */[
                                                                                                                                                                                                                                                                                        /* tuple */[
                                                                                                                                                                                                                                                                                          "DataView - getFloat64LittleEndian",
                                                                                                                                                                                                                                                                                          (function () {
                                                                                                                                                                                                                                                                                              var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                              var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                              view.setFloat64(0, 1e200, 1);
                                                                                                                                                                                                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                                                        1e200,
                                                                                                                                                                                                                                                                                                        view.getFloat64(0, 1)
                                                                                                                                                                                                                                                                                                      ]);
                                                                                                                                                                                                                                                                                            })
                                                                                                                                                                                                                                                                                        ],
                                                                                                                                                                                                                                                                                        /* :: */[
                                                                                                                                                                                                                                                                                          /* tuple */[
                                                                                                                                                                                                                                                                                            "DataView - setFloat64LittleEndian",
                                                                                                                                                                                                                                                                                            (function () {
                                                                                                                                                                                                                                                                                                var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                                var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                                view.setFloat64(0, 1.0, 1);
                                                                                                                                                                                                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                                                                                                                                                                                                          3.03865e-319,
                                                                                                                                                                                                                                                                                                          view.getFloat64(0)
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
                                                                                                                                                                    ]
                                                                                                                                                                  ]
                                                                                                                                                                ]
                                                                                                                                                              ]
                                                                                                                                                            ]
                                                                                                                                                          ]
                                                                                                                                                        ]
                                                                                                                                                      ]
                                                                                                                                                    ]
                                                                                                                                                  ]
                                                                                                                                                ]
                                                                                                                                              ]
                                                                                                                                            ]
                                                                                                                                          ]
                                                                                                                                        ]
                                                                                                                                      ]
                                                                                                                                    ]
                                                                                                                                  ]
                                                                                                                                ]
                                                                                                                              ]
                                                                                                                            ]
                                                                                                                          ]
                                                                                                                        ]
                                                                                                                      ]
                                                                                                                    ]
                                                                                                                  ]
                                                                                                                ]
                                                                                                              ]
                                                                                                            ]
                                                                                                          ]
                                                                                                        ]
                                                                                                      ]
                                                                                                    ]
                                                                                                  ]
                                                                                                ]
                                                                                              ]
                                                                                            ]
                                                                                          ]
                                                                                        ]
                                                                                      ]
                                                                                    ]
                                                                                  ]
                                                                                ]
                                                                              ]
                                                                            ]
                                                                          ]
                                                                        ]
                                                                      ]
                                                                    ]
                                                                  ]
                                                                ]
                                                              ]
                                                            ]
                                                          ]
                                                        ]
                                                      ]
                                                    ]
                                                  ]
                                                ]
                                              ]
                                            ]
                                          ]
                                        ]
                                      ]
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
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

Mt.from_pair_suites("js_typed_array_test.ml", suites);

exports.mkI8 = mkI8;
exports.via = via;
exports.viaInt8 = viaInt8;
exports.x = x;
exports.suites = suites;
/* x Not a pure module */
