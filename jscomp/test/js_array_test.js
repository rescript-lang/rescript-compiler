'use strict';

var Mt = require("./mt.js");
var Js_vector = require("../../lib/js/js_vector.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites_0 = [
  "File \"js_array_test.ml\", line 3, characters 4-11",
  (function (param) {
      var x = [
        1,
        2,
        3,
        4,
        5
      ];
      return {
              TAG: /* Eq */0,
              _0: [
                2,
                4
              ],
              _1: (Js_vector.filterInPlace((function (x) {
                        return x % 2 === 0;
                      }), x), x)
            };
    })
];

var suites_1 = {
  hd: [
    "File \"js_array_test.ml\", line 11, characters 4-11",
    (function (param) {
        var x = [
          1,
          2,
          3,
          4,
          5
        ];
        return {
                TAG: /* Eq */0,
                _0: true,
                _1: (Js_vector.filterInPlace((function (x) {
                          return x > 10;
                        }), x), x.length === 0)
              };
      })
  ],
  tl: {
    hd: [
      "isArray_array",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: true,
                  _1: Array.isArray([])
                };
        })
    ],
    tl: {
      hd: [
        "isArray_int",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: false,
                    _1: Array.isArray(34)
                  };
          })
      ],
      tl: {
        hd: [
          "length",
          (function (param) {
              return {
                      TAG: /* Eq */0,
                      _0: 3,
                      _1: [
                        1,
                        2,
                        3
                      ].length
                    };
            })
        ],
        tl: {
          hd: [
            "copyWithin",
            (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: [
                          1,
                          2,
                          3,
                          1,
                          2
                        ],
                        _1: [
                            1,
                            2,
                            3,
                            4,
                            5
                          ].copyWithin(-2)
                      };
              })
          ],
          tl: {
            hd: [
              "copyWithinFrom",
              (function (param) {
                  return {
                          TAG: /* Eq */0,
                          _0: [
                            4,
                            5,
                            3,
                            4,
                            5
                          ],
                          _1: [
                              1,
                              2,
                              3,
                              4,
                              5
                            ].copyWithin(0, 3)
                        };
                })
            ],
            tl: {
              hd: [
                "copyWithinFromRange",
                (function (param) {
                    return {
                            TAG: /* Eq */0,
                            _0: [
                              4,
                              2,
                              3,
                              4,
                              5
                            ],
                            _1: [
                                1,
                                2,
                                3,
                                4,
                                5
                              ].copyWithin(0, 3, 4)
                          };
                  })
              ],
              tl: {
                hd: [
                  "fillInPlace",
                  (function (param) {
                      return {
                              TAG: /* Eq */0,
                              _0: [
                                4,
                                4,
                                4
                              ],
                              _1: [
                                  1,
                                  2,
                                  3
                                ].fill(4)
                            };
                    })
                ],
                tl: {
                  hd: [
                    "fillFromInPlace",
                    (function (param) {
                        return {
                                TAG: /* Eq */0,
                                _0: [
                                  1,
                                  4,
                                  4
                                ],
                                _1: [
                                    1,
                                    2,
                                    3
                                  ].fill(4, 1)
                              };
                      })
                  ],
                  tl: {
                    hd: [
                      "fillRangeInPlace",
                      (function (param) {
                          return {
                                  TAG: /* Eq */0,
                                  _0: [
                                    1,
                                    4,
                                    3
                                  ],
                                  _1: [
                                      1,
                                      2,
                                      3
                                    ].fill(4, 1, 2)
                                };
                        })
                    ],
                    tl: {
                      hd: [
                        "pop",
                        (function (param) {
                            return {
                                    TAG: /* Eq */0,
                                    _0: 3,
                                    _1: Caml_option.undefined_to_opt([
                                            1,
                                            2,
                                            3
                                          ].pop())
                                  };
                          })
                      ],
                      tl: {
                        hd: [
                          "pop - empty array",
                          (function (param) {
                              return {
                                      TAG: /* Eq */0,
                                      _0: undefined,
                                      _1: Caml_option.undefined_to_opt([].pop())
                                    };
                            })
                        ],
                        tl: {
                          hd: [
                            "push",
                            (function (param) {
                                return {
                                        TAG: /* Eq */0,
                                        _0: 4,
                                        _1: [
                                            1,
                                            2,
                                            3
                                          ].push(4)
                                      };
                              })
                          ],
                          tl: {
                            hd: [
                              "pushMany",
                              (function (param) {
                                  return {
                                          TAG: /* Eq */0,
                                          _0: 5,
                                          _1: [
                                              1,
                                              2,
                                              3
                                            ].push(4, 5)
                                        };
                                })
                            ],
                            tl: {
                              hd: [
                                "reverseInPlace",
                                (function (param) {
                                    return {
                                            TAG: /* Eq */0,
                                            _0: [
                                              3,
                                              2,
                                              1
                                            ],
                                            _1: [
                                                1,
                                                2,
                                                3
                                              ].reverse()
                                          };
                                  })
                              ],
                              tl: {
                                hd: [
                                  "shift",
                                  (function (param) {
                                      return {
                                              TAG: /* Eq */0,
                                              _0: 1,
                                              _1: Caml_option.undefined_to_opt([
                                                      1,
                                                      2,
                                                      3
                                                    ].shift())
                                            };
                                    })
                                ],
                                tl: {
                                  hd: [
                                    "shift - empty array",
                                    (function (param) {
                                        return {
                                                TAG: /* Eq */0,
                                                _0: undefined,
                                                _1: Caml_option.undefined_to_opt([].shift())
                                              };
                                      })
                                  ],
                                  tl: {
                                    hd: [
                                      "sortInPlace",
                                      (function (param) {
                                          return {
                                                  TAG: /* Eq */0,
                                                  _0: [
                                                    1,
                                                    2,
                                                    3
                                                  ],
                                                  _1: [
                                                      3,
                                                      1,
                                                      2
                                                    ].sort()
                                                };
                                        })
                                    ],
                                    tl: {
                                      hd: [
                                        "sortInPlaceWith",
                                        (function (param) {
                                            return {
                                                    TAG: /* Eq */0,
                                                    _0: [
                                                      3,
                                                      2,
                                                      1
                                                    ],
                                                    _1: [
                                                        3,
                                                        1,
                                                        2
                                                      ].sort(function (a, b) {
                                                          return b - a | 0;
                                                        })
                                                  };
                                          })
                                      ],
                                      tl: {
                                        hd: [
                                          "spliceInPlace",
                                          (function (param) {
                                              var arr = [
                                                1,
                                                2,
                                                3,
                                                4
                                              ];
                                              var removed = arr.splice(2, 0, 5);
                                              return {
                                                      TAG: /* Eq */0,
                                                      _0: [
                                                        [
                                                          1,
                                                          2,
                                                          5,
                                                          3,
                                                          4
                                                        ],
                                                        []
                                                      ],
                                                      _1: [
                                                        arr,
                                                        removed
                                                      ]
                                                    };
                                            })
                                        ],
                                        tl: {
                                          hd: [
                                            "removeFromInPlace",
                                            (function (param) {
                                                var arr = [
                                                  1,
                                                  2,
                                                  3,
                                                  4
                                                ];
                                                var removed = arr.splice(2);
                                                return {
                                                        TAG: /* Eq */0,
                                                        _0: [
                                                          [
                                                            1,
                                                            2
                                                          ],
                                                          [
                                                            3,
                                                            4
                                                          ]
                                                        ],
                                                        _1: [
                                                          arr,
                                                          removed
                                                        ]
                                                      };
                                              })
                                          ],
                                          tl: {
                                            hd: [
                                              "removeCountInPlace",
                                              (function (param) {
                                                  var arr = [
                                                    1,
                                                    2,
                                                    3,
                                                    4
                                                  ];
                                                  var removed = arr.splice(2, 1);
                                                  return {
                                                          TAG: /* Eq */0,
                                                          _0: [
                                                            [
                                                              1,
                                                              2,
                                                              4
                                                            ],
                                                            [3]
                                                          ],
                                                          _1: [
                                                            arr,
                                                            removed
                                                          ]
                                                        };
                                                })
                                            ],
                                            tl: {
                                              hd: [
                                                "unshift",
                                                (function (param) {
                                                    return {
                                                            TAG: /* Eq */0,
                                                            _0: 4,
                                                            _1: [
                                                                1,
                                                                2,
                                                                3
                                                              ].unshift(4)
                                                          };
                                                  })
                                              ],
                                              tl: {
                                                hd: [
                                                  "unshiftMany",
                                                  (function (param) {
                                                      return {
                                                              TAG: /* Eq */0,
                                                              _0: 5,
                                                              _1: [
                                                                  1,
                                                                  2,
                                                                  3
                                                                ].unshift(4, 5)
                                                            };
                                                    })
                                                ],
                                                tl: {
                                                  hd: [
                                                    "append",
                                                    (function (param) {
                                                        return {
                                                                TAG: /* Eq */0,
                                                                _0: [
                                                                  1,
                                                                  2,
                                                                  3,
                                                                  4
                                                                ],
                                                                _1: [
                                                                    1,
                                                                    2,
                                                                    3
                                                                  ].concat([4])
                                                              };
                                                      })
                                                  ],
                                                  tl: {
                                                    hd: [
                                                      "concat",
                                                      (function (param) {
                                                          return {
                                                                  TAG: /* Eq */0,
                                                                  _0: [
                                                                    1,
                                                                    2,
                                                                    3,
                                                                    4,
                                                                    5
                                                                  ],
                                                                  _1: [
                                                                      1,
                                                                      2,
                                                                      3
                                                                    ].concat([
                                                                        4,
                                                                        5
                                                                      ])
                                                                };
                                                        })
                                                    ],
                                                    tl: {
                                                      hd: [
                                                        "concatMany",
                                                        (function (param) {
                                                            return {
                                                                    TAG: /* Eq */0,
                                                                    _0: [
                                                                      1,
                                                                      2,
                                                                      3,
                                                                      4,
                                                                      5,
                                                                      6,
                                                                      7
                                                                    ],
                                                                    _1: [
                                                                        1,
                                                                        2,
                                                                        3
                                                                      ].concat([
                                                                          4,
                                                                          5
                                                                        ], [
                                                                          6,
                                                                          7
                                                                        ])
                                                                  };
                                                          })
                                                      ],
                                                      tl: {
                                                        hd: [
                                                          "includes",
                                                          (function (param) {
                                                              return {
                                                                      TAG: /* Eq */0,
                                                                      _0: true,
                                                                      _1: [
                                                                          1,
                                                                          2,
                                                                          3
                                                                        ].includes(3)
                                                                    };
                                                            })
                                                        ],
                                                        tl: {
                                                          hd: [
                                                            "indexOf",
                                                            (function (param) {
                                                                return {
                                                                        TAG: /* Eq */0,
                                                                        _0: 1,
                                                                        _1: [
                                                                            1,
                                                                            2,
                                                                            3
                                                                          ].indexOf(2)
                                                                      };
                                                              })
                                                          ],
                                                          tl: {
                                                            hd: [
                                                              "indexOfFrom",
                                                              (function (param) {
                                                                  return {
                                                                          TAG: /* Eq */0,
                                                                          _0: 3,
                                                                          _1: [
                                                                              1,
                                                                              2,
                                                                              3,
                                                                              2
                                                                            ].indexOf(2, 2)
                                                                        };
                                                                })
                                                            ],
                                                            tl: {
                                                              hd: [
                                                                "join",
                                                                (function (param) {
                                                                    return {
                                                                            TAG: /* Eq */0,
                                                                            _0: "1,2,3",
                                                                            _1: [
                                                                                1,
                                                                                2,
                                                                                3
                                                                              ].join()
                                                                          };
                                                                  })
                                                              ],
                                                              tl: {
                                                                hd: [
                                                                  "joinWith",
                                                                  (function (param) {
                                                                      return {
                                                                              TAG: /* Eq */0,
                                                                              _0: "1;2;3",
                                                                              _1: [
                                                                                  1,
                                                                                  2,
                                                                                  3
                                                                                ].join(";")
                                                                            };
                                                                    })
                                                                ],
                                                                tl: {
                                                                  hd: [
                                                                    "lastIndexOf",
                                                                    (function (param) {
                                                                        return {
                                                                                TAG: /* Eq */0,
                                                                                _0: 1,
                                                                                _1: [
                                                                                    1,
                                                                                    2,
                                                                                    3
                                                                                  ].lastIndexOf(2)
                                                                              };
                                                                      })
                                                                  ],
                                                                  tl: {
                                                                    hd: [
                                                                      "lastIndexOfFrom",
                                                                      (function (param) {
                                                                          return {
                                                                                  TAG: /* Eq */0,
                                                                                  _0: 1,
                                                                                  _1: [
                                                                                      1,
                                                                                      2,
                                                                                      3,
                                                                                      2
                                                                                    ].lastIndexOf(2, 2)
                                                                                };
                                                                        })
                                                                    ],
                                                                    tl: {
                                                                      hd: [
                                                                        "slice",
                                                                        (function (param) {
                                                                            return {
                                                                                    TAG: /* Eq */0,
                                                                                    _0: [
                                                                                      2,
                                                                                      3
                                                                                    ],
                                                                                    _1: [
                                                                                        1,
                                                                                        2,
                                                                                        3,
                                                                                        4,
                                                                                        5
                                                                                      ].slice(1, 3)
                                                                                  };
                                                                          })
                                                                      ],
                                                                      tl: {
                                                                        hd: [
                                                                          "copy",
                                                                          (function (param) {
                                                                              return {
                                                                                      TAG: /* Eq */0,
                                                                                      _0: [
                                                                                        1,
                                                                                        2,
                                                                                        3,
                                                                                        4,
                                                                                        5
                                                                                      ],
                                                                                      _1: [
                                                                                          1,
                                                                                          2,
                                                                                          3,
                                                                                          4,
                                                                                          5
                                                                                        ].slice()
                                                                                    };
                                                                            })
                                                                        ],
                                                                        tl: {
                                                                          hd: [
                                                                            "sliceFrom",
                                                                            (function (param) {
                                                                                return {
                                                                                        TAG: /* Eq */0,
                                                                                        _0: [
                                                                                          3,
                                                                                          4,
                                                                                          5
                                                                                        ],
                                                                                        _1: [
                                                                                            1,
                                                                                            2,
                                                                                            3,
                                                                                            4,
                                                                                            5
                                                                                          ].slice(2)
                                                                                      };
                                                                              })
                                                                          ],
                                                                          tl: {
                                                                            hd: [
                                                                              "toString",
                                                                              (function (param) {
                                                                                  return {
                                                                                          TAG: /* Eq */0,
                                                                                          _0: "1,2,3",
                                                                                          _1: [
                                                                                              1,
                                                                                              2,
                                                                                              3
                                                                                            ].toString()
                                                                                        };
                                                                                })
                                                                            ],
                                                                            tl: {
                                                                              hd: [
                                                                                "toLocaleString",
                                                                                (function (param) {
                                                                                    return {
                                                                                            TAG: /* Eq */0,
                                                                                            _0: "1,2,3",
                                                                                            _1: [
                                                                                                1,
                                                                                                2,
                                                                                                3
                                                                                              ].toLocaleString()
                                                                                          };
                                                                                  })
                                                                              ],
                                                                              tl: {
                                                                                hd: [
                                                                                  "every",
                                                                                  (function (param) {
                                                                                      return {
                                                                                              TAG: /* Eq */0,
                                                                                              _0: true,
                                                                                              _1: [
                                                                                                  1,
                                                                                                  2,
                                                                                                  3
                                                                                                ].every(function (n) {
                                                                                                    return n > 0;
                                                                                                  })
                                                                                            };
                                                                                    })
                                                                                ],
                                                                                tl: {
                                                                                  hd: [
                                                                                    "everyi",
                                                                                    (function (param) {
                                                                                        return {
                                                                                                TAG: /* Eq */0,
                                                                                                _0: false,
                                                                                                _1: [
                                                                                                    1,
                                                                                                    2,
                                                                                                    3
                                                                                                  ].every(function (param, i) {
                                                                                                      return i > 0;
                                                                                                    })
                                                                                              };
                                                                                      })
                                                                                  ],
                                                                                  tl: {
                                                                                    hd: [
                                                                                      "filter",
                                                                                      (function (param) {
                                                                                          return {
                                                                                                  TAG: /* Eq */0,
                                                                                                  _0: [
                                                                                                    2,
                                                                                                    4
                                                                                                  ],
                                                                                                  _1: [
                                                                                                      1,
                                                                                                      2,
                                                                                                      3,
                                                                                                      4
                                                                                                    ].filter(function (n) {
                                                                                                        return n % 2 === 0;
                                                                                                      })
                                                                                                };
                                                                                        })
                                                                                    ],
                                                                                    tl: {
                                                                                      hd: [
                                                                                        "filteri",
                                                                                        (function (param) {
                                                                                            return {
                                                                                                    TAG: /* Eq */0,
                                                                                                    _0: [
                                                                                                      1,
                                                                                                      3
                                                                                                    ],
                                                                                                    _1: [
                                                                                                        1,
                                                                                                        2,
                                                                                                        3,
                                                                                                        4
                                                                                                      ].filter(function (param, i) {
                                                                                                          return i % 2 === 0;
                                                                                                        })
                                                                                                  };
                                                                                          })
                                                                                      ],
                                                                                      tl: {
                                                                                        hd: [
                                                                                          "find",
                                                                                          (function (param) {
                                                                                              return {
                                                                                                      TAG: /* Eq */0,
                                                                                                      _0: 2,
                                                                                                      _1: Caml_option.undefined_to_opt([
                                                                                                              1,
                                                                                                              2,
                                                                                                              3,
                                                                                                              4
                                                                                                            ].find(function (n) {
                                                                                                                return n % 2 === 0;
                                                                                                              }))
                                                                                                    };
                                                                                            })
                                                                                        ],
                                                                                        tl: {
                                                                                          hd: [
                                                                                            "find - no match",
                                                                                            (function (param) {
                                                                                                return {
                                                                                                        TAG: /* Eq */0,
                                                                                                        _0: undefined,
                                                                                                        _1: Caml_option.undefined_to_opt([
                                                                                                                1,
                                                                                                                2,
                                                                                                                3,
                                                                                                                4
                                                                                                              ].find(function (n) {
                                                                                                                  return n % 2 === 5;
                                                                                                                }))
                                                                                                      };
                                                                                              })
                                                                                          ],
                                                                                          tl: {
                                                                                            hd: [
                                                                                              "findi",
                                                                                              (function (param) {
                                                                                                  return {
                                                                                                          TAG: /* Eq */0,
                                                                                                          _0: 1,
                                                                                                          _1: Caml_option.undefined_to_opt([
                                                                                                                  1,
                                                                                                                  2,
                                                                                                                  3,
                                                                                                                  4
                                                                                                                ].find(function (param, i) {
                                                                                                                    return i % 2 === 0;
                                                                                                                  }))
                                                                                                        };
                                                                                                })
                                                                                            ],
                                                                                            tl: {
                                                                                              hd: [
                                                                                                "findi - no match",
                                                                                                (function (param) {
                                                                                                    return {
                                                                                                            TAG: /* Eq */0,
                                                                                                            _0: undefined,
                                                                                                            _1: Caml_option.undefined_to_opt([
                                                                                                                    1,
                                                                                                                    2,
                                                                                                                    3,
                                                                                                                    4
                                                                                                                  ].find(function (param, i) {
                                                                                                                      return i % 2 === 5;
                                                                                                                    }))
                                                                                                          };
                                                                                                  })
                                                                                              ],
                                                                                              tl: {
                                                                                                hd: [
                                                                                                  "findIndex",
                                                                                                  (function (param) {
                                                                                                      return {
                                                                                                              TAG: /* Eq */0,
                                                                                                              _0: 1,
                                                                                                              _1: [
                                                                                                                  1,
                                                                                                                  2,
                                                                                                                  3,
                                                                                                                  4
                                                                                                                ].findIndex(function (n) {
                                                                                                                    return n % 2 === 0;
                                                                                                                  })
                                                                                                            };
                                                                                                    })
                                                                                                ],
                                                                                                tl: {
                                                                                                  hd: [
                                                                                                    "findIndexi",
                                                                                                    (function (param) {
                                                                                                        return {
                                                                                                                TAG: /* Eq */0,
                                                                                                                _0: 0,
                                                                                                                _1: [
                                                                                                                    1,
                                                                                                                    2,
                                                                                                                    3,
                                                                                                                    4
                                                                                                                  ].findIndex(function (param, i) {
                                                                                                                      return i % 2 === 0;
                                                                                                                    })
                                                                                                              };
                                                                                                      })
                                                                                                  ],
                                                                                                  tl: {
                                                                                                    hd: [
                                                                                                      "forEach",
                                                                                                      (function (param) {
                                                                                                          var sum = {
                                                                                                            contents: 0
                                                                                                          };
                                                                                                          [
                                                                                                              1,
                                                                                                              2,
                                                                                                              3
                                                                                                            ].forEach(function (n) {
                                                                                                                sum.contents = sum.contents + n | 0;
                                                                                                                
                                                                                                              });
                                                                                                          return {
                                                                                                                  TAG: /* Eq */0,
                                                                                                                  _0: 6,
                                                                                                                  _1: sum.contents
                                                                                                                };
                                                                                                        })
                                                                                                    ],
                                                                                                    tl: {
                                                                                                      hd: [
                                                                                                        "forEachi",
                                                                                                        (function (param) {
                                                                                                            var sum = {
                                                                                                              contents: 0
                                                                                                            };
                                                                                                            [
                                                                                                                1,
                                                                                                                2,
                                                                                                                3
                                                                                                              ].forEach(function (param, i) {
                                                                                                                  sum.contents = sum.contents + i | 0;
                                                                                                                  
                                                                                                                });
                                                                                                            return {
                                                                                                                    TAG: /* Eq */0,
                                                                                                                    _0: 3,
                                                                                                                    _1: sum.contents
                                                                                                                  };
                                                                                                          })
                                                                                                      ],
                                                                                                      tl: {
                                                                                                        hd: [
                                                                                                          "map",
                                                                                                          (function (param) {
                                                                                                              return {
                                                                                                                      TAG: /* Eq */0,
                                                                                                                      _0: [
                                                                                                                        2,
                                                                                                                        4,
                                                                                                                        6,
                                                                                                                        8
                                                                                                                      ],
                                                                                                                      _1: [
                                                                                                                          1,
                                                                                                                          2,
                                                                                                                          3,
                                                                                                                          4
                                                                                                                        ].map(function (n) {
                                                                                                                            return (n << 1);
                                                                                                                          })
                                                                                                                    };
                                                                                                            })
                                                                                                        ],
                                                                                                        tl: {
                                                                                                          hd: [
                                                                                                            "map",
                                                                                                            (function (param) {
                                                                                                                return {
                                                                                                                        TAG: /* Eq */0,
                                                                                                                        _0: [
                                                                                                                          0,
                                                                                                                          2,
                                                                                                                          4,
                                                                                                                          6
                                                                                                                        ],
                                                                                                                        _1: [
                                                                                                                            1,
                                                                                                                            2,
                                                                                                                            3,
                                                                                                                            4
                                                                                                                          ].map(function (param, i) {
                                                                                                                              return (i << 1);
                                                                                                                            })
                                                                                                                      };
                                                                                                              })
                                                                                                          ],
                                                                                                          tl: {
                                                                                                            hd: [
                                                                                                              "reduce",
                                                                                                              (function (param) {
                                                                                                                  return {
                                                                                                                          TAG: /* Eq */0,
                                                                                                                          _0: -10,
                                                                                                                          _1: [
                                                                                                                              1,
                                                                                                                              2,
                                                                                                                              3,
                                                                                                                              4
                                                                                                                            ].reduce((function (acc, n) {
                                                                                                                                  return acc - n | 0;
                                                                                                                                }), 0)
                                                                                                                        };
                                                                                                                })
                                                                                                            ],
                                                                                                            tl: {
                                                                                                              hd: [
                                                                                                                "reducei",
                                                                                                                (function (param) {
                                                                                                                    return {
                                                                                                                            TAG: /* Eq */0,
                                                                                                                            _0: -6,
                                                                                                                            _1: [
                                                                                                                                1,
                                                                                                                                2,
                                                                                                                                3,
                                                                                                                                4
                                                                                                                              ].reduce((function (acc, param, i) {
                                                                                                                                    return acc - i | 0;
                                                                                                                                  }), 0)
                                                                                                                          };
                                                                                                                  })
                                                                                                              ],
                                                                                                              tl: {
                                                                                                                hd: [
                                                                                                                  "reduceRight",
                                                                                                                  (function (param) {
                                                                                                                      return {
                                                                                                                              TAG: /* Eq */0,
                                                                                                                              _0: -10,
                                                                                                                              _1: [
                                                                                                                                  1,
                                                                                                                                  2,
                                                                                                                                  3,
                                                                                                                                  4
                                                                                                                                ].reduceRight((function (acc, n) {
                                                                                                                                      return acc - n | 0;
                                                                                                                                    }), 0)
                                                                                                                            };
                                                                                                                    })
                                                                                                                ],
                                                                                                                tl: {
                                                                                                                  hd: [
                                                                                                                    "reduceRighti",
                                                                                                                    (function (param) {
                                                                                                                        return {
                                                                                                                                TAG: /* Eq */0,
                                                                                                                                _0: -6,
                                                                                                                                _1: [
                                                                                                                                    1,
                                                                                                                                    2,
                                                                                                                                    3,
                                                                                                                                    4
                                                                                                                                  ].reduceRight((function (acc, param, i) {
                                                                                                                                        return acc - i | 0;
                                                                                                                                      }), 0)
                                                                                                                              };
                                                                                                                      })
                                                                                                                  ],
                                                                                                                  tl: {
                                                                                                                    hd: [
                                                                                                                      "some",
                                                                                                                      (function (param) {
                                                                                                                          return {
                                                                                                                                  TAG: /* Eq */0,
                                                                                                                                  _0: false,
                                                                                                                                  _1: [
                                                                                                                                      1,
                                                                                                                                      2,
                                                                                                                                      3,
                                                                                                                                      4
                                                                                                                                    ].some(function (n) {
                                                                                                                                        return n <= 0;
                                                                                                                                      })
                                                                                                                                };
                                                                                                                        })
                                                                                                                    ],
                                                                                                                    tl: {
                                                                                                                      hd: [
                                                                                                                        "somei",
                                                                                                                        (function (param) {
                                                                                                                            return {
                                                                                                                                    TAG: /* Eq */0,
                                                                                                                                    _0: true,
                                                                                                                                    _1: [
                                                                                                                                        1,
                                                                                                                                        2,
                                                                                                                                        3,
                                                                                                                                        4
                                                                                                                                      ].some(function (param, i) {
                                                                                                                                          return i <= 0;
                                                                                                                                        })
                                                                                                                                  };
                                                                                                                          })
                                                                                                                      ],
                                                                                                                      tl: /* [] */0
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

var suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Js_array_test", suites);

exports.suites = suites;
/*  Not a pure module */
