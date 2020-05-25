'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_vector = require("../../lib/js/js_vector.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites_0 = /* tuple */[
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
              tag: /* Eq */0,
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
  _0: /* tuple */[
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
                tag: /* Eq */0,
                _0: true,
                _1: (Js_vector.filterInPlace((function (x) {
                          return x > 10;
                        }), x), x.length === 0)
              };
      })
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "isArray_array",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: true,
                  _1: Array.isArray([])
                };
        })
    ],
    _1: /* :: */{
      _0: /* tuple */[
        "isArray_int",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: false,
                    _1: Array.isArray(34)
                  };
          })
      ],
      _1: /* :: */{
        _0: /* tuple */[
          "length",
          (function (param) {
              return {
                      tag: /* Eq */0,
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
          _0: /* tuple */[
            "copyWithin",
            (function (param) {
                return {
                        tag: /* Eq */0,
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
            _0: /* tuple */[
              "copyWithinFrom",
              (function (param) {
                  return {
                          tag: /* Eq */0,
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
              _0: /* tuple */[
                "copyWithinFromRange",
                (function (param) {
                    return {
                            tag: /* Eq */0,
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
                _0: /* tuple */[
                  "fillInPlace",
                  (function (param) {
                      return {
                              tag: /* Eq */0,
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
                  _0: /* tuple */[
                    "fillFromInPlace",
                    (function (param) {
                        return {
                                tag: /* Eq */0,
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
                    _0: /* tuple */[
                      "fillRangeInPlace",
                      (function (param) {
                          return {
                                  tag: /* Eq */0,
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
                      _0: /* tuple */[
                        "pop",
                        (function (param) {
                            return {
                                    tag: /* Eq */0,
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
                        _0: /* tuple */[
                          "pop - empty array",
                          (function (param) {
                              return {
                                      tag: /* Eq */0,
                                      _0: undefined,
                                      _1: Caml_option.undefined_to_opt([].pop())
                                    };
                            })
                        ],
                        _1: /* :: */{
                          _0: /* tuple */[
                            "push",
                            (function (param) {
                                return {
                                        tag: /* Eq */0,
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
                            _0: /* tuple */[
                              "pushMany",
                              (function (param) {
                                  return {
                                          tag: /* Eq */0,
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
                              _0: /* tuple */[
                                "reverseInPlace",
                                (function (param) {
                                    return {
                                            tag: /* Eq */0,
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
                                _0: /* tuple */[
                                  "shift",
                                  (function (param) {
                                      return {
                                              tag: /* Eq */0,
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
                                  _0: /* tuple */[
                                    "shift - empty array",
                                    (function (param) {
                                        return {
                                                tag: /* Eq */0,
                                                _0: undefined,
                                                _1: Caml_option.undefined_to_opt([].shift())
                                              };
                                      })
                                  ],
                                  _1: /* :: */{
                                    _0: /* tuple */[
                                      "sortInPlace",
                                      (function (param) {
                                          return {
                                                  tag: /* Eq */0,
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
                                      _0: /* tuple */[
                                        "sortInPlaceWith",
                                        (function (param) {
                                            return {
                                                    tag: /* Eq */0,
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
                                        _0: /* tuple */[
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
                                                      tag: /* Eq */0,
                                                      _0: /* tuple */[
                                                        [
                                                          1,
                                                          2,
                                                          5,
                                                          3,
                                                          4
                                                        ],
                                                        []
                                                      ],
                                                      _1: /* tuple */[
                                                        arr,
                                                        removed
                                                      ]
                                                    };
                                            })
                                        ],
                                        _1: /* :: */{
                                          _0: /* tuple */[
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
                                                        tag: /* Eq */0,
                                                        _0: /* tuple */[
                                                          [
                                                            1,
                                                            2
                                                          ],
                                                          [
                                                            3,
                                                            4
                                                          ]
                                                        ],
                                                        _1: /* tuple */[
                                                          arr,
                                                          removed
                                                        ]
                                                      };
                                              })
                                          ],
                                          _1: /* :: */{
                                            _0: /* tuple */[
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
                                                          tag: /* Eq */0,
                                                          _0: /* tuple */[
                                                            [
                                                              1,
                                                              2,
                                                              4
                                                            ],
                                                            [3]
                                                          ],
                                                          _1: /* tuple */[
                                                            arr,
                                                            removed
                                                          ]
                                                        };
                                                })
                                            ],
                                            _1: /* :: */{
                                              _0: /* tuple */[
                                                "unshift",
                                                (function (param) {
                                                    return {
                                                            tag: /* Eq */0,
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
                                                _0: /* tuple */[
                                                  "unshiftMany",
                                                  (function (param) {
                                                      return {
                                                              tag: /* Eq */0,
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
                                                  _0: /* tuple */[
                                                    "append",
                                                    (function (param) {
                                                        return {
                                                                tag: /* Eq */0,
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
                                                    _0: /* tuple */[
                                                      "concat",
                                                      (function (param) {
                                                          return {
                                                                  tag: /* Eq */0,
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
                                                      _0: /* tuple */[
                                                        "concatMany",
                                                        (function (param) {
                                                            return {
                                                                    tag: /* Eq */0,
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
                                                        _0: /* tuple */[
                                                          "includes",
                                                          (function (param) {
                                                              return {
                                                                      tag: /* Eq */0,
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
                                                          _0: /* tuple */[
                                                            "indexOf",
                                                            (function (param) {
                                                                return {
                                                                        tag: /* Eq */0,
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
                                                            _0: /* tuple */[
                                                              "indexOfFrom",
                                                              (function (param) {
                                                                  return {
                                                                          tag: /* Eq */0,
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
                                                              _0: /* tuple */[
                                                                "join",
                                                                (function (param) {
                                                                    return {
                                                                            tag: /* Eq */0,
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
                                                                _0: /* tuple */[
                                                                  "joinWith",
                                                                  (function (param) {
                                                                      return {
                                                                              tag: /* Eq */0,
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
                                                                  _0: /* tuple */[
                                                                    "lastIndexOf",
                                                                    (function (param) {
                                                                        return {
                                                                                tag: /* Eq */0,
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
                                                                    _0: /* tuple */[
                                                                      "lastIndexOfFrom",
                                                                      (function (param) {
                                                                          return {
                                                                                  tag: /* Eq */0,
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
                                                                      _0: /* tuple */[
                                                                        "slice",
                                                                        (function (param) {
                                                                            return {
                                                                                    tag: /* Eq */0,
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
                                                                        _0: /* tuple */[
                                                                          "copy",
                                                                          (function (param) {
                                                                              return {
                                                                                      tag: /* Eq */0,
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
                                                                          _0: /* tuple */[
                                                                            "sliceFrom",
                                                                            (function (param) {
                                                                                return {
                                                                                        tag: /* Eq */0,
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
                                                                            _0: /* tuple */[
                                                                              "toString",
                                                                              (function (param) {
                                                                                  return {
                                                                                          tag: /* Eq */0,
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
                                                                              _0: /* tuple */[
                                                                                "toLocaleString",
                                                                                (function (param) {
                                                                                    return {
                                                                                            tag: /* Eq */0,
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
                                                                                _0: /* tuple */[
                                                                                  "every",
                                                                                  (function (param) {
                                                                                      return {
                                                                                              tag: /* Eq */0,
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
                                                                                  _0: /* tuple */[
                                                                                    "everyi",
                                                                                    (function (param) {
                                                                                        return {
                                                                                                tag: /* Eq */0,
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
                                                                                    _0: /* tuple */[
                                                                                      "filter",
                                                                                      (function (param) {
                                                                                          return {
                                                                                                  tag: /* Eq */0,
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
                                                                                      _0: /* tuple */[
                                                                                        "filteri",
                                                                                        (function (param) {
                                                                                            return {
                                                                                                    tag: /* Eq */0,
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
                                                                                        _0: /* tuple */[
                                                                                          "find",
                                                                                          (function (param) {
                                                                                              return {
                                                                                                      tag: /* Eq */0,
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
                                                                                          _0: /* tuple */[
                                                                                            "find - no match",
                                                                                            (function (param) {
                                                                                                return {
                                                                                                        tag: /* Eq */0,
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
                                                                                            _0: /* tuple */[
                                                                                              "findi",
                                                                                              (function (param) {
                                                                                                  return {
                                                                                                          tag: /* Eq */0,
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
                                                                                              _0: /* tuple */[
                                                                                                "findi - no match",
                                                                                                (function (param) {
                                                                                                    return {
                                                                                                            tag: /* Eq */0,
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
                                                                                                _0: /* tuple */[
                                                                                                  "findIndex",
                                                                                                  (function (param) {
                                                                                                      return {
                                                                                                              tag: /* Eq */0,
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
                                                                                                  _0: /* tuple */[
                                                                                                    "findIndexi",
                                                                                                    (function (param) {
                                                                                                        return {
                                                                                                                tag: /* Eq */0,
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
                                                                                                    _0: /* tuple */[
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
                                                                                                                  tag: /* Eq */0,
                                                                                                                  _0: 6,
                                                                                                                  _1: sum.contents
                                                                                                                };
                                                                                                        })
                                                                                                    ],
                                                                                                    _1: /* :: */{
                                                                                                      _0: /* tuple */[
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
                                                                                                                    tag: /* Eq */0,
                                                                                                                    _0: 3,
                                                                                                                    _1: sum.contents
                                                                                                                  };
                                                                                                          })
                                                                                                      ],
                                                                                                      _1: /* :: */{
                                                                                                        _0: /* tuple */[
                                                                                                          "map",
                                                                                                          (function (param) {
                                                                                                              return {
                                                                                                                      tag: /* Eq */0,
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
                                                                                                          _0: /* tuple */[
                                                                                                            "map",
                                                                                                            (function (param) {
                                                                                                                return {
                                                                                                                        tag: /* Eq */0,
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
                                                                                                            _0: /* tuple */[
                                                                                                              "reduce",
                                                                                                              (function (param) {
                                                                                                                  return {
                                                                                                                          tag: /* Eq */0,
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
                                                                                                              _0: /* tuple */[
                                                                                                                "reducei",
                                                                                                                (function (param) {
                                                                                                                    return {
                                                                                                                            tag: /* Eq */0,
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
                                                                                                                _0: /* tuple */[
                                                                                                                  "reduceRight",
                                                                                                                  (function (param) {
                                                                                                                      return {
                                                                                                                              tag: /* Eq */0,
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
                                                                                                                  _0: /* tuple */[
                                                                                                                    "reduceRighti",
                                                                                                                    (function (param) {
                                                                                                                        return {
                                                                                                                                tag: /* Eq */0,
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
                                                                                                                    _0: /* tuple */[
                                                                                                                      "some",
                                                                                                                      (function (param) {
                                                                                                                          return {
                                                                                                                                  tag: /* Eq */0,
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
                                                                                                                      _0: /* tuple */[
                                                                                                                        "somei",
                                                                                                                        (function (param) {
                                                                                                                            return {
                                                                                                                                    tag: /* Eq */0,
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
