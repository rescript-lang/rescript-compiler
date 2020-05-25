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

var suites_1 = /* :: */{
  _0: [
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
  _1: /* :: */{
    _0: [
      "isArray_array",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: true,
                  _1: Array.isArray([])
                };
        })
    ],
    _1: /* :: */{
      _0: [
        "isArray_int",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: false,
                    _1: Array.isArray(34)
                  };
          })
      ],
      _1: /* :: */{
        _0: [
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
        _1: /* :: */{
          _0: [
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
          _1: /* :: */{
            _0: [
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
            _1: /* :: */{
              _0: [
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
              _1: /* :: */{
                _0: [
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
                _1: /* :: */{
                  _0: [
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
                  _1: /* :: */{
                    _0: [
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
                    _1: /* :: */{
                      _0: [
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
                      _1: /* :: */{
                        _0: [
                          "pop - empty array",
                          (function (param) {
                              return {
                                      TAG: /* Eq */0,
                                      _0: undefined,
                                      _1: Caml_option.undefined_to_opt([].pop())
                                    };
                            })
                        ],
                        _1: /* :: */{
                          _0: [
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
                          _1: /* :: */{
                            _0: [
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
                            _1: /* :: */{
                              _0: [
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
                              _1: /* :: */{
                                _0: [
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
                                _1: /* :: */{
                                  _0: [
                                    "shift - empty array",
                                    (function (param) {
                                        return {
                                                TAG: /* Eq */0,
                                                _0: undefined,
                                                _1: Caml_option.undefined_to_opt([].shift())
                                              };
                                      })
                                  ],
                                  _1: /* :: */{
                                    _0: [
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
                                    _1: /* :: */{
                                      _0: [
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
                                      _1: /* :: */{
                                        _0: [
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
                                        _1: /* :: */{
                                          _0: [
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
                                          _1: /* :: */{
                                            _0: [
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
                                            _1: /* :: */{
                                              _0: [
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
                                              _1: /* :: */{
                                                _0: [
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
                                                _1: /* :: */{
                                                  _0: [
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
                                                  _1: /* :: */{
                                                    _0: [
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
                                                    _1: /* :: */{
                                                      _0: [
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
                                                      _1: /* :: */{
                                                        _0: [
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
                                                        _1: /* :: */{
                                                          _0: [
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
                                                          _1: /* :: */{
                                                            _0: [
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
                                                            _1: /* :: */{
                                                              _0: [
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
                                                              _1: /* :: */{
                                                                _0: [
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
                                                                _1: /* :: */{
                                                                  _0: [
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
                                                                  _1: /* :: */{
                                                                    _0: [
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
                                                                    _1: /* :: */{
                                                                      _0: [
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
                                                                      _1: /* :: */{
                                                                        _0: [
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
                                                                        _1: /* :: */{
                                                                          _0: [
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
                                                                          _1: /* :: */{
                                                                            _0: [
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
                                                                            _1: /* :: */{
                                                                              _0: [
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
                                                                              _1: /* :: */{
                                                                                _0: [
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
                                                                                _1: /* :: */{
                                                                                  _0: [
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
                                                                                  _1: /* :: */{
                                                                                    _0: [
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
                                                                                    _1: /* :: */{
                                                                                      _0: [
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
                                                                                      _1: /* :: */{
                                                                                        _0: [
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
                                                                                        _1: /* :: */{
                                                                                          _0: [
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
                                                                                          _1: /* :: */{
                                                                                            _0: [
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
                                                                                            _1: /* :: */{
                                                                                              _0: [
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
                                                                                              _1: /* :: */{
                                                                                                _0: [
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
                                                                                                _1: /* :: */{
                                                                                                  _0: [
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
                                                                                                  _1: /* :: */{
                                                                                                    _0: [
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
                                                                                                    _1: /* :: */{
                                                                                                      _0: [
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
                                                                                                      _1: /* :: */{
                                                                                                        _0: [
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
                                                                                                        _1: /* :: */{
                                                                                                          _0: [
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
                                                                                                          _1: /* :: */{
                                                                                                            _0: [
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
                                                                                                            _1: /* :: */{
                                                                                                              _0: [
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
                                                                                                              _1: /* :: */{
                                                                                                                _0: [
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
                                                                                                                _1: /* :: */{
                                                                                                                  _0: [
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
                                                                                                                  _1: /* :: */{
                                                                                                                    _0: [
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
                                                                                                                    _1: /* :: */{
                                                                                                                      _0: [
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
                                                                                                                      _1: /* [] */0
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

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("Js_array_test", suites);

exports.suites = suites;
/*  Not a pure module */
