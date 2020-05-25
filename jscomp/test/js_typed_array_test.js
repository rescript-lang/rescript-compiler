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

function viaInt8(f, arr) {
  return function (param, param$1) {
    return Array.from(Curry._1(param, new Int8Array(param$1)));
  };
}

var x = new Int8Array([
      1,
      2,
      3
    ]);

var suites_0 = /* tuple */[
  "array_buffer - make",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: 5,
              _1: new ArrayBuffer(5).byteLength
            };
    })
];

var suites_1 = /* :: */{
  _0: /* tuple */[
    "array_buffer - byteLength",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: 5,
                _1: new ArrayBuffer(5).byteLength
              };
      })
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "array_buffer - slice",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: 2,
                  _1: new ArrayBuffer(5).slice(2, 4).byteLength
                };
        })
    ],
    _1: /* :: */{
      _0: /* tuple */[
        "array_buffer - sliceFrom",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: 3,
                    _1: new ArrayBuffer(5).slice(2).byteLength
                  };
          })
      ],
      _1: /* :: */{
        _0: /* tuple */[
          "typed_array - unsafe_get",
          (function (param) {
              return {
                      tag: /* Eq */0,
                      _0: 4,
                      _1: new Int8Array([
                              1,
                              2,
                              3,
                              4,
                              5
                            ])[3]
                    };
            })
        ],
        _1: /* :: */{
          _0: /* tuple */[
            "typed_array - unsafe_set",
            (function (param) {
                var a = new Int8Array([
                      1,
                      2,
                      3,
                      4,
                      5
                    ]);
                a[3] = 14;
                return {
                        tag: /* Eq */0,
                        _0: 14,
                        _1: a[3]
                      };
              })
          ],
          _1: /* :: */{
            _0: /* tuple */[
              "typed_array - buffer",
              (function (param) {
                  return {
                          tag: /* Eq */0,
                          _0: new Int8Array([
                                3,
                                4,
                                5
                              ]),
                          _1: new Int8Array(new Int8Array([
                                    1,
                                    2,
                                    3,
                                    4,
                                    5
                                  ]).buffer, 2)
                        };
                })
            ],
            _1: /* :: */{
              _0: /* tuple */[
                "typed_array - byteLength",
                (function (param) {
                    return {
                            tag: /* Eq */0,
                            _0: 10,
                            _1: new Int16Array([
                                  1,
                                  2,
                                  3,
                                  4,
                                  5
                                ]).byteLength
                          };
                  })
              ],
              _1: /* :: */{
                _0: /* tuple */[
                  "typed_array - byteOffset",
                  (function (param) {
                      return {
                              tag: /* Eq */0,
                              _0: 0,
                              _1: new Int8Array([
                                    1,
                                    2,
                                    3,
                                    4,
                                    5
                                  ]).byteOffset
                            };
                    })
                ],
                _1: /* :: */{
                  _0: /* tuple */[
                    "typed_array - setArray",
                    (function (param) {
                        var f = function (a) {
                          a.set([
                                9,
                                8,
                                7
                              ]);
                          return a;
                        };
                        return {
                                tag: /* Eq */0,
                                _0: new Int8Array([
                                      9,
                                      8,
                                      7,
                                      4,
                                      5
                                    ]),
                                _1: f(new Int8Array([
                                          1,
                                          2,
                                          3,
                                          4,
                                          5
                                        ]))
                              };
                      })
                  ],
                  _1: /* :: */{
                    _0: /* tuple */[
                      "typed_array - setArrayOffset",
                      (function (param) {
                          var f = function (a) {
                            a.set([
                                  9,
                                  8,
                                  7
                                ], 2);
                            return a;
                          };
                          return {
                                  tag: /* Eq */0,
                                  _0: new Int8Array([
                                        1,
                                        2,
                                        9,
                                        8,
                                        7
                                      ]),
                                  _1: f(new Int8Array([
                                            1,
                                            2,
                                            3,
                                            4,
                                            5
                                          ]))
                                };
                        })
                    ],
                    _1: /* :: */{
                      _0: /* tuple */[
                        "typed_array - length",
                        (function (param) {
                            return {
                                    tag: /* Eq */0,
                                    _0: 5,
                                    _1: new Int8Array([
                                          1,
                                          2,
                                          3,
                                          4,
                                          5
                                        ]).length
                                  };
                          })
                      ],
                      _1: /* :: */{
                        _0: /* tuple */[
                          "typed_array - copyWithin",
                          (function (param) {
                              return {
                                      tag: /* Eq */0,
                                      _0: new Int8Array([
                                            1,
                                            2,
                                            3,
                                            1,
                                            2
                                          ]),
                                      _1: new Int8Array([
                                              1,
                                              2,
                                              3,
                                              4,
                                              5
                                            ]).copyWithin(-2)
                                    };
                            })
                        ],
                        _1: /* :: */{
                          _0: /* tuple */[
                            "typed_array - copyWithinFrom",
                            (function (param) {
                                return {
                                        tag: /* Eq */0,
                                        _0: new Int8Array([
                                              4,
                                              5,
                                              3,
                                              4,
                                              5
                                            ]),
                                        _1: new Int8Array([
                                                1,
                                                2,
                                                3,
                                                4,
                                                5
                                              ]).copyWithin(0, 3)
                                      };
                              })
                          ],
                          _1: /* :: */{
                            _0: /* tuple */[
                              "typed_array - copyWithinFromRange",
                              (function (param) {
                                  return {
                                          tag: /* Eq */0,
                                          _0: new Int8Array([
                                                4,
                                                2,
                                                3,
                                                4,
                                                5
                                              ]),
                                          _1: new Int8Array([
                                                  1,
                                                  2,
                                                  3,
                                                  4,
                                                  5
                                                ]).copyWithin(0, 3, 4)
                                        };
                                })
                            ],
                            _1: /* :: */{
                              _0: /* tuple */[
                                "typed_array - fillInPlace",
                                (function (param) {
                                    return {
                                            tag: /* Eq */0,
                                            _0: new Int8Array([
                                                  4,
                                                  4,
                                                  4
                                                ]),
                                            _1: new Int8Array([
                                                    1,
                                                    2,
                                                    3
                                                  ]).fill(4)
                                          };
                                  })
                              ],
                              _1: /* :: */{
                                _0: /* tuple */[
                                  "typed_array - fillFromInPlace",
                                  (function (param) {
                                      return {
                                              tag: /* Eq */0,
                                              _0: new Int8Array([
                                                    1,
                                                    4,
                                                    4
                                                  ]),
                                              _1: new Int8Array([
                                                      1,
                                                      2,
                                                      3
                                                    ]).fill(4, 1)
                                            };
                                    })
                                ],
                                _1: /* :: */{
                                  _0: /* tuple */[
                                    "typed_array - fillRangeInPlace",
                                    (function (param) {
                                        return {
                                                tag: /* Eq */0,
                                                _0: new Int8Array([
                                                      1,
                                                      4,
                                                      3
                                                    ]),
                                                _1: new Int8Array([
                                                        1,
                                                        2,
                                                        3
                                                      ]).fill(4, 1, 2)
                                              };
                                      })
                                  ],
                                  _1: /* :: */{
                                    _0: /* tuple */[
                                      "typed_array - reverseInPlace",
                                      (function (param) {
                                          return {
                                                  tag: /* Eq */0,
                                                  _0: new Int8Array([
                                                        3,
                                                        2,
                                                        1
                                                      ]),
                                                  _1: new Int8Array([
                                                          1,
                                                          2,
                                                          3
                                                        ]).reverse()
                                                };
                                        })
                                    ],
                                    _1: /* :: */{
                                      _0: /* tuple */[
                                        "typed_array - sortInPlace",
                                        (function (param) {
                                            return {
                                                    tag: /* Eq */0,
                                                    _0: new Int8Array([
                                                          1,
                                                          2,
                                                          3
                                                        ]),
                                                    _1: new Int8Array([
                                                            3,
                                                            1,
                                                            2
                                                          ]).sort()
                                                  };
                                          })
                                      ],
                                      _1: /* :: */{
                                        _0: /* tuple */[
                                          "typed_array - sortInPlaceWith",
                                          (function (param) {
                                              return {
                                                      tag: /* Eq */0,
                                                      _0: new Int8Array([
                                                            3,
                                                            2,
                                                            1
                                                          ]),
                                                      _1: new Int8Array([
                                                              3,
                                                              1,
                                                              2
                                                            ]).sort(function (a, b) {
                                                            return b - a | 0;
                                                          })
                                                    };
                                            })
                                        ],
                                        _1: /* :: */{
                                          _0: /* tuple */[
                                            "typed_array - includes",
                                            (function (param) {
                                                return {
                                                        tag: /* Eq */0,
                                                        _0: true,
                                                        _1: new Int8Array([
                                                                1,
                                                                2,
                                                                3
                                                              ]).includes(3)
                                                      };
                                              })
                                          ],
                                          _1: /* :: */{
                                            _0: /* tuple */[
                                              "typed_array - indexOf",
                                              (function (param) {
                                                  return {
                                                          tag: /* Eq */0,
                                                          _0: 1,
                                                          _1: new Int8Array([
                                                                  1,
                                                                  2,
                                                                  3
                                                                ]).indexOf(2)
                                                        };
                                                })
                                            ],
                                            _1: /* :: */{
                                              _0: /* tuple */[
                                                "typed_array - indexOfFrom",
                                                (function (param) {
                                                    return {
                                                            tag: /* Eq */0,
                                                            _0: 3,
                                                            _1: new Int8Array([
                                                                    1,
                                                                    2,
                                                                    3,
                                                                    2
                                                                  ]).indexOf(2, 2)
                                                          };
                                                  })
                                              ],
                                              _1: /* :: */{
                                                _0: /* tuple */[
                                                  "typed_array - join",
                                                  (function (param) {
                                                      return {
                                                              tag: /* Eq */0,
                                                              _0: "1,2,3",
                                                              _1: new Int8Array([
                                                                      1,
                                                                      2,
                                                                      3
                                                                    ]).join()
                                                            };
                                                    })
                                                ],
                                                _1: /* :: */{
                                                  _0: /* tuple */[
                                                    "typed_array - joinWith",
                                                    (function (param) {
                                                        return {
                                                                tag: /* Eq */0,
                                                                _0: "1;2;3",
                                                                _1: new Int8Array([
                                                                        1,
                                                                        2,
                                                                        3
                                                                      ]).join(";")
                                                              };
                                                      })
                                                  ],
                                                  _1: /* :: */{
                                                    _0: /* tuple */[
                                                      "typed_array - lastIndexOf",
                                                      (function (param) {
                                                          return {
                                                                  tag: /* Eq */0,
                                                                  _0: 1,
                                                                  _1: new Int8Array([
                                                                          1,
                                                                          2,
                                                                          3
                                                                        ]).lastIndexOf(2)
                                                                };
                                                        })
                                                    ],
                                                    _1: /* :: */{
                                                      _0: /* tuple */[
                                                        "typed_array - lastIndexOfFrom",
                                                        (function (param) {
                                                            return {
                                                                    tag: /* Eq */0,
                                                                    _0: 1,
                                                                    _1: new Int8Array([
                                                                            1,
                                                                            2,
                                                                            3,
                                                                            2
                                                                          ]).lastIndexOf(2, 2)
                                                                  };
                                                          })
                                                      ],
                                                      _1: /* :: */{
                                                        _0: /* tuple */[
                                                          "typed_array - slice",
                                                          (function (param) {
                                                              return {
                                                                      tag: /* Eq */0,
                                                                      _0: new Int8Array([
                                                                            2,
                                                                            3
                                                                          ]),
                                                                      _1: new Int8Array([
                                                                              1,
                                                                              2,
                                                                              3,
                                                                              4,
                                                                              5
                                                                            ]).slice(1, 3)
                                                                    };
                                                            })
                                                        ],
                                                        _1: /* :: */{
                                                          _0: /* tuple */[
                                                            "typed_array - copy",
                                                            (function (param) {
                                                                return {
                                                                        tag: /* Eq */0,
                                                                        _0: new Int8Array([
                                                                              1,
                                                                              2,
                                                                              3,
                                                                              4,
                                                                              5
                                                                            ]),
                                                                        _1: new Int8Array([
                                                                                1,
                                                                                2,
                                                                                3,
                                                                                4,
                                                                                5
                                                                              ]).slice()
                                                                      };
                                                              })
                                                          ],
                                                          _1: /* :: */{
                                                            _0: /* tuple */[
                                                              "typed_array - sliceFrom",
                                                              (function (param) {
                                                                  return {
                                                                          tag: /* Eq */0,
                                                                          _0: new Int8Array([
                                                                                3,
                                                                                4,
                                                                                5
                                                                              ]),
                                                                          _1: new Int8Array([
                                                                                  1,
                                                                                  2,
                                                                                  3,
                                                                                  4,
                                                                                  5
                                                                                ]).slice(2)
                                                                        };
                                                                })
                                                            ],
                                                            _1: /* :: */{
                                                              _0: /* tuple */[
                                                                "typed_array - subarray",
                                                                (function (param) {
                                                                    return {
                                                                            tag: /* Eq */0,
                                                                            _0: new Int8Array([
                                                                                  2,
                                                                                  3
                                                                                ]),
                                                                            _1: new Int8Array([
                                                                                    1,
                                                                                    2,
                                                                                    3,
                                                                                    4,
                                                                                    5
                                                                                  ]).subarray(1, 3)
                                                                          };
                                                                  })
                                                              ],
                                                              _1: /* :: */{
                                                                _0: /* tuple */[
                                                                  "typed_array - subarrayFrom",
                                                                  (function (param) {
                                                                      return {
                                                                              tag: /* Eq */0,
                                                                              _0: new Int8Array([
                                                                                    3,
                                                                                    4,
                                                                                    5
                                                                                  ]),
                                                                              _1: new Int8Array([
                                                                                      1,
                                                                                      2,
                                                                                      3,
                                                                                      4,
                                                                                      5
                                                                                    ]).subarray(2)
                                                                            };
                                                                    })
                                                                ],
                                                                _1: /* :: */{
                                                                  _0: /* tuple */[
                                                                    "typed_array - toString",
                                                                    (function (param) {
                                                                        return {
                                                                                tag: /* Eq */0,
                                                                                _0: "1,2,3",
                                                                                _1: new Int8Array([
                                                                                        1,
                                                                                        2,
                                                                                        3
                                                                                      ]).toString()
                                                                              };
                                                                      })
                                                                  ],
                                                                  _1: /* :: */{
                                                                    _0: /* tuple */[
                                                                      "typed_array - toLocaleString",
                                                                      (function (param) {
                                                                          return {
                                                                                  tag: /* Eq */0,
                                                                                  _0: "1,2,3",
                                                                                  _1: new Int8Array([
                                                                                          1,
                                                                                          2,
                                                                                          3
                                                                                        ]).toLocaleString()
                                                                                };
                                                                        })
                                                                    ],
                                                                    _1: /* :: */{
                                                                      _0: /* tuple */[
                                                                        "typed_array - every",
                                                                        (function (param) {
                                                                            return {
                                                                                    tag: /* Eq */0,
                                                                                    _0: true,
                                                                                    _1: new Int8Array([
                                                                                            1,
                                                                                            2,
                                                                                            3
                                                                                          ]).every(function (n) {
                                                                                          return n > 0;
                                                                                        })
                                                                                  };
                                                                          })
                                                                      ],
                                                                      _1: /* :: */{
                                                                        _0: /* tuple */[
                                                                          "typed_array - everyi",
                                                                          (function (param) {
                                                                              return {
                                                                                      tag: /* Eq */0,
                                                                                      _0: false,
                                                                                      _1: new Int8Array([
                                                                                              1,
                                                                                              2,
                                                                                              3
                                                                                            ]).every(function (param, i) {
                                                                                            return i > 0;
                                                                                          })
                                                                                    };
                                                                            })
                                                                        ],
                                                                        _1: /* :: */{
                                                                          _0: /* tuple */[
                                                                            "typed_array - filter",
                                                                            (function (param) {
                                                                                return {
                                                                                        tag: /* Eq */0,
                                                                                        _0: new Int8Array([
                                                                                              2,
                                                                                              4
                                                                                            ]),
                                                                                        _1: new Int8Array([
                                                                                                1,
                                                                                                2,
                                                                                                3,
                                                                                                4
                                                                                              ]).filter(function (n) {
                                                                                              return n % 2 === 0;
                                                                                            })
                                                                                      };
                                                                              })
                                                                          ],
                                                                          _1: /* :: */{
                                                                            _0: /* tuple */[
                                                                              "typed_array - filteri",
                                                                              (function (param) {
                                                                                  return {
                                                                                          tag: /* Eq */0,
                                                                                          _0: new Int8Array([
                                                                                                1,
                                                                                                3
                                                                                              ]),
                                                                                          _1: new Int8Array([
                                                                                                  1,
                                                                                                  2,
                                                                                                  3,
                                                                                                  4
                                                                                                ]).filter(function (param, i) {
                                                                                                return i % 2 === 0;
                                                                                              })
                                                                                        };
                                                                                })
                                                                            ],
                                                                            _1: /* :: */{
                                                                              _0: /* tuple */[
                                                                                "typed_array - find",
                                                                                (function (param) {
                                                                                    return {
                                                                                            tag: /* Eq */0,
                                                                                            _0: 2,
                                                                                            _1: new Int8Array([
                                                                                                    1,
                                                                                                    2,
                                                                                                    3,
                                                                                                    4
                                                                                                  ]).find(function (n) {
                                                                                                  return n % 2 === 0;
                                                                                                })
                                                                                          };
                                                                                  })
                                                                              ],
                                                                              _1: /* :: */{
                                                                                _0: /* tuple */[
                                                                                  "typed_array - findi",
                                                                                  (function (param) {
                                                                                      return {
                                                                                              tag: /* Eq */0,
                                                                                              _0: 1,
                                                                                              _1: new Int8Array([
                                                                                                      1,
                                                                                                      2,
                                                                                                      3,
                                                                                                      4
                                                                                                    ]).find(function (param, i) {
                                                                                                    return i % 2 === 0;
                                                                                                  })
                                                                                            };
                                                                                    })
                                                                                ],
                                                                                _1: /* :: */{
                                                                                  _0: /* tuple */[
                                                                                    "typed_array - findIndex",
                                                                                    (function (param) {
                                                                                        return {
                                                                                                tag: /* Eq */0,
                                                                                                _0: 1,
                                                                                                _1: new Int8Array([
                                                                                                        1,
                                                                                                        2,
                                                                                                        3,
                                                                                                        4
                                                                                                      ]).findIndex(function (n) {
                                                                                                      return n % 2 === 0;
                                                                                                    })
                                                                                              };
                                                                                      })
                                                                                  ],
                                                                                  _1: /* :: */{
                                                                                    _0: /* tuple */[
                                                                                      "typed_array - findIndexi",
                                                                                      (function (param) {
                                                                                          return {
                                                                                                  tag: /* Eq */0,
                                                                                                  _0: 0,
                                                                                                  _1: new Int8Array([
                                                                                                          1,
                                                                                                          2,
                                                                                                          3,
                                                                                                          4
                                                                                                        ]).findIndex(function (param, i) {
                                                                                                        return i % 2 === 0;
                                                                                                      })
                                                                                                };
                                                                                        })
                                                                                    ],
                                                                                    _1: /* :: */{
                                                                                      _0: /* tuple */[
                                                                                        "typed_array - forEach",
                                                                                        (function (param) {
                                                                                            var sum = {
                                                                                              contents: 0
                                                                                            };
                                                                                            new Int8Array([
                                                                                                    1,
                                                                                                    2,
                                                                                                    3
                                                                                                  ]).forEach(function (n) {
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
                                                                                          "typed_array - forEachi",
                                                                                          (function (param) {
                                                                                              var sum = {
                                                                                                contents: 0
                                                                                              };
                                                                                              new Int8Array([
                                                                                                      1,
                                                                                                      2,
                                                                                                      3
                                                                                                    ]).forEach(function (param, i) {
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
                                                                                            "typed_array - map",
                                                                                            (function (param) {
                                                                                                return {
                                                                                                        tag: /* Eq */0,
                                                                                                        _0: new Int8Array([
                                                                                                              2,
                                                                                                              4,
                                                                                                              6,
                                                                                                              8
                                                                                                            ]),
                                                                                                        _1: new Int8Array([
                                                                                                                1,
                                                                                                                2,
                                                                                                                3,
                                                                                                                4
                                                                                                              ]).map(function (n) {
                                                                                                              return (n << 1);
                                                                                                            })
                                                                                                      };
                                                                                              })
                                                                                          ],
                                                                                          _1: /* :: */{
                                                                                            _0: /* tuple */[
                                                                                              "typed_array - map",
                                                                                              (function (param) {
                                                                                                  return {
                                                                                                          tag: /* Eq */0,
                                                                                                          _0: new Int8Array([
                                                                                                                0,
                                                                                                                2,
                                                                                                                4,
                                                                                                                6
                                                                                                              ]),
                                                                                                          _1: new Int8Array([
                                                                                                                  1,
                                                                                                                  2,
                                                                                                                  3,
                                                                                                                  4
                                                                                                                ]).map(function (param, i) {
                                                                                                                return (i << 1);
                                                                                                              })
                                                                                                        };
                                                                                                })
                                                                                            ],
                                                                                            _1: /* :: */{
                                                                                              _0: /* tuple */[
                                                                                                "typed_array - reduce",
                                                                                                (function (param) {
                                                                                                    return {
                                                                                                            tag: /* Eq */0,
                                                                                                            _0: -10,
                                                                                                            _1: new Int8Array([
                                                                                                                    1,
                                                                                                                    2,
                                                                                                                    3,
                                                                                                                    4
                                                                                                                  ]).reduce((function (acc, n) {
                                                                                                                    return acc - n | 0;
                                                                                                                  }), 0)
                                                                                                          };
                                                                                                  })
                                                                                              ],
                                                                                              _1: /* :: */{
                                                                                                _0: /* tuple */[
                                                                                                  "typed_array - reducei",
                                                                                                  (function (param) {
                                                                                                      return {
                                                                                                              tag: /* Eq */0,
                                                                                                              _0: -6,
                                                                                                              _1: new Int8Array([
                                                                                                                      1,
                                                                                                                      2,
                                                                                                                      3,
                                                                                                                      4
                                                                                                                    ]).reduce((function (acc, param, i) {
                                                                                                                      return acc - i | 0;
                                                                                                                    }), 0)
                                                                                                            };
                                                                                                    })
                                                                                                ],
                                                                                                _1: /* :: */{
                                                                                                  _0: /* tuple */[
                                                                                                    "typed_array - reduceRight",
                                                                                                    (function (param) {
                                                                                                        return {
                                                                                                                tag: /* Eq */0,
                                                                                                                _0: -10,
                                                                                                                _1: new Int8Array([
                                                                                                                        1,
                                                                                                                        2,
                                                                                                                        3,
                                                                                                                        4
                                                                                                                      ]).reduceRight((function (acc, n) {
                                                                                                                        return acc - n | 0;
                                                                                                                      }), 0)
                                                                                                              };
                                                                                                      })
                                                                                                  ],
                                                                                                  _1: /* :: */{
                                                                                                    _0: /* tuple */[
                                                                                                      "typed_array - reduceRighti",
                                                                                                      (function (param) {
                                                                                                          return {
                                                                                                                  tag: /* Eq */0,
                                                                                                                  _0: -6,
                                                                                                                  _1: new Int8Array([
                                                                                                                          1,
                                                                                                                          2,
                                                                                                                          3,
                                                                                                                          4
                                                                                                                        ]).reduceRight((function (acc, param, i) {
                                                                                                                          return acc - i | 0;
                                                                                                                        }), 0)
                                                                                                                };
                                                                                                        })
                                                                                                    ],
                                                                                                    _1: /* :: */{
                                                                                                      _0: /* tuple */[
                                                                                                        "typed_array - some",
                                                                                                        (function (param) {
                                                                                                            return {
                                                                                                                    tag: /* Eq */0,
                                                                                                                    _0: false,
                                                                                                                    _1: new Int8Array([
                                                                                                                            1,
                                                                                                                            2,
                                                                                                                            3,
                                                                                                                            4
                                                                                                                          ]).some(function (n) {
                                                                                                                          return n <= 0;
                                                                                                                        })
                                                                                                                  };
                                                                                                          })
                                                                                                      ],
                                                                                                      _1: /* :: */{
                                                                                                        _0: /* tuple */[
                                                                                                          "typed_array - somei",
                                                                                                          (function (param) {
                                                                                                              return {
                                                                                                                      tag: /* Eq */0,
                                                                                                                      _0: true,
                                                                                                                      _1: new Int8Array([
                                                                                                                              1,
                                                                                                                              2,
                                                                                                                              3,
                                                                                                                              4
                                                                                                                            ]).some(function (param, i) {
                                                                                                                            return i <= 0;
                                                                                                                          })
                                                                                                                    };
                                                                                                            })
                                                                                                        ],
                                                                                                        _1: /* :: */{
                                                                                                          _0: /* tuple */[
                                                                                                            "int8_array - _BYTES_PER_ELEMENT",
                                                                                                            (function (param) {
                                                                                                                return {
                                                                                                                        tag: /* Eq */0,
                                                                                                                        _0: 1,
                                                                                                                        _1: Int8Array.BYTES_PER_ELEMENT
                                                                                                                      };
                                                                                                              })
                                                                                                          ],
                                                                                                          _1: /* :: */{
                                                                                                            _0: /* tuple */[
                                                                                                              "int8_array - make",
                                                                                                              (function (param) {
                                                                                                                  return {
                                                                                                                          tag: /* Eq */0,
                                                                                                                          _0: 3,
                                                                                                                          _1: new Int8Array([
                                                                                                                                1,
                                                                                                                                2,
                                                                                                                                3
                                                                                                                              ]).byteLength
                                                                                                                        };
                                                                                                                })
                                                                                                            ],
                                                                                                            _1: /* :: */{
                                                                                                              _0: /* tuple */[
                                                                                                                "int8_array - fromBuffer",
                                                                                                                (function (param) {
                                                                                                                    return {
                                                                                                                            tag: /* Eq */0,
                                                                                                                            _0: 32,
                                                                                                                            _1: new Int8Array(new ArrayBuffer(32)).byteLength
                                                                                                                          };
                                                                                                                  })
                                                                                                              ],
                                                                                                              _1: /* :: */{
                                                                                                                _0: /* tuple */[
                                                                                                                  "int8_array - fromBufferOffset",
                                                                                                                  (function (param) {
                                                                                                                      var buffer = new ArrayBuffer(32);
                                                                                                                      return {
                                                                                                                              tag: /* Eq */0,
                                                                                                                              _0: 24,
                                                                                                                              _1: new Int8Array(buffer, 8).byteLength
                                                                                                                            };
                                                                                                                    })
                                                                                                                ],
                                                                                                                _1: /* :: */{
                                                                                                                  _0: /* tuple */[
                                                                                                                    "int8_array - fromBufferRange",
                                                                                                                    (function (param) {
                                                                                                                        var buffer = new ArrayBuffer(32);
                                                                                                                        return {
                                                                                                                                tag: /* Eq */0,
                                                                                                                                _0: 2,
                                                                                                                                _1: new Int8Array(buffer, 8, 2).byteLength
                                                                                                                              };
                                                                                                                      })
                                                                                                                  ],
                                                                                                                  _1: /* :: */{
                                                                                                                    _0: /* tuple */[
                                                                                                                      "int8_array - fromLength",
                                                                                                                      (function (param) {
                                                                                                                          return {
                                                                                                                                  tag: /* Eq */0,
                                                                                                                                  _0: 3,
                                                                                                                                  _1: new Int8Array(3).byteLength
                                                                                                                                };
                                                                                                                        })
                                                                                                                    ],
                                                                                                                    _1: /* :: */{
                                                                                                                      _0: /* tuple */[
                                                                                                                        "int8_array - unsafe_set - typed_array sanity check",
                                                                                                                        (function (param) {
                                                                                                                            var a = new Int8Array([
                                                                                                                                  1,
                                                                                                                                  2,
                                                                                                                                  3,
                                                                                                                                  4,
                                                                                                                                  5
                                                                                                                                ]);
                                                                                                                            a[3] = 14;
                                                                                                                            return {
                                                                                                                                    tag: /* Eq */0,
                                                                                                                                    _0: 14,
                                                                                                                                    _1: a[3]
                                                                                                                                  };
                                                                                                                          })
                                                                                                                      ],
                                                                                                                      _1: /* :: */{
                                                                                                                        _0: /* tuple */[
                                                                                                                          "uint8_array - _BYTES_PER_ELEMENT",
                                                                                                                          (function (param) {
                                                                                                                              return {
                                                                                                                                      tag: /* Eq */0,
                                                                                                                                      _0: 1,
                                                                                                                                      _1: Uint8Array.BYTES_PER_ELEMENT
                                                                                                                                    };
                                                                                                                            })
                                                                                                                        ],
                                                                                                                        _1: /* :: */{
                                                                                                                          _0: /* tuple */[
                                                                                                                            "uint8_array - make",
                                                                                                                            (function (param) {
                                                                                                                                return {
                                                                                                                                        tag: /* Eq */0,
                                                                                                                                        _0: 3,
                                                                                                                                        _1: new Uint8Array([
                                                                                                                                              1,
                                                                                                                                              2,
                                                                                                                                              3
                                                                                                                                            ]).byteLength
                                                                                                                                      };
                                                                                                                              })
                                                                                                                          ],
                                                                                                                          _1: /* :: */{
                                                                                                                            _0: /* tuple */[
                                                                                                                              "uint8_array - fromBuffer",
                                                                                                                              (function (param) {
                                                                                                                                  return {
                                                                                                                                          tag: /* Eq */0,
                                                                                                                                          _0: 32,
                                                                                                                                          _1: new Uint8Array(new ArrayBuffer(32)).byteLength
                                                                                                                                        };
                                                                                                                                })
                                                                                                                            ],
                                                                                                                            _1: /* :: */{
                                                                                                                              _0: /* tuple */[
                                                                                                                                "uint8_array - fromBufferOffset",
                                                                                                                                (function (param) {
                                                                                                                                    var buffer = new ArrayBuffer(32);
                                                                                                                                    return {
                                                                                                                                            tag: /* Eq */0,
                                                                                                                                            _0: 24,
                                                                                                                                            _1: new Uint8Array(buffer, 8).byteLength
                                                                                                                                          };
                                                                                                                                  })
                                                                                                                              ],
                                                                                                                              _1: /* :: */{
                                                                                                                                _0: /* tuple */[
                                                                                                                                  "uint8_array - fromBufferRange",
                                                                                                                                  (function (param) {
                                                                                                                                      var buffer = new ArrayBuffer(32);
                                                                                                                                      return {
                                                                                                                                              tag: /* Eq */0,
                                                                                                                                              _0: 2,
                                                                                                                                              _1: new Uint8Array(buffer, 8, 2).byteLength
                                                                                                                                            };
                                                                                                                                    })
                                                                                                                                ],
                                                                                                                                _1: /* :: */{
                                                                                                                                  _0: /* tuple */[
                                                                                                                                    "uint8_array - fromLength",
                                                                                                                                    (function (param) {
                                                                                                                                        return {
                                                                                                                                                tag: /* Eq */0,
                                                                                                                                                _0: 3,
                                                                                                                                                _1: new Uint8Array(3).byteLength
                                                                                                                                              };
                                                                                                                                      })
                                                                                                                                  ],
                                                                                                                                  _1: /* :: */{
                                                                                                                                    _0: /* tuple */[
                                                                                                                                      "uint8_array - unsafe_set - typed_array sanity check",
                                                                                                                                      (function (param) {
                                                                                                                                          var a = new Uint8Array([
                                                                                                                                                1,
                                                                                                                                                2,
                                                                                                                                                3,
                                                                                                                                                4,
                                                                                                                                                5
                                                                                                                                              ]);
                                                                                                                                          a[3] = 14;
                                                                                                                                          return {
                                                                                                                                                  tag: /* Eq */0,
                                                                                                                                                  _0: 14,
                                                                                                                                                  _1: a[3]
                                                                                                                                                };
                                                                                                                                        })
                                                                                                                                    ],
                                                                                                                                    _1: /* :: */{
                                                                                                                                      _0: /* tuple */[
                                                                                                                                        "uint8clamped_array - _BYTES_PER_ELEMENT",
                                                                                                                                        (function (param) {
                                                                                                                                            return {
                                                                                                                                                    tag: /* Eq */0,
                                                                                                                                                    _0: 1,
                                                                                                                                                    _1: Uint8ClampedArray.BYTES_PER_ELEMENT
                                                                                                                                                  };
                                                                                                                                          })
                                                                                                                                      ],
                                                                                                                                      _1: /* :: */{
                                                                                                                                        _0: /* tuple */[
                                                                                                                                          "uint8clamped_array - make",
                                                                                                                                          (function (param) {
                                                                                                                                              return {
                                                                                                                                                      tag: /* Eq */0,
                                                                                                                                                      _0: 3,
                                                                                                                                                      _1: new Uint8ClampedArray([
                                                                                                                                                            1,
                                                                                                                                                            2,
                                                                                                                                                            3
                                                                                                                                                          ]).byteLength
                                                                                                                                                    };
                                                                                                                                            })
                                                                                                                                        ],
                                                                                                                                        _1: /* :: */{
                                                                                                                                          _0: /* tuple */[
                                                                                                                                            "uint8clamped_array - fromBuffer",
                                                                                                                                            (function (param) {
                                                                                                                                                return {
                                                                                                                                                        tag: /* Eq */0,
                                                                                                                                                        _0: 32,
                                                                                                                                                        _1: new Uint8ClampedArray(new ArrayBuffer(32)).byteLength
                                                                                                                                                      };
                                                                                                                                              })
                                                                                                                                          ],
                                                                                                                                          _1: /* :: */{
                                                                                                                                            _0: /* tuple */[
                                                                                                                                              "uint8clamped_array - fromBufferOffset",
                                                                                                                                              (function (param) {
                                                                                                                                                  var buffer = new ArrayBuffer(32);
                                                                                                                                                  return {
                                                                                                                                                          tag: /* Eq */0,
                                                                                                                                                          _0: 24,
                                                                                                                                                          _1: new Uint8ClampedArray(buffer, 8).byteLength
                                                                                                                                                        };
                                                                                                                                                })
                                                                                                                                            ],
                                                                                                                                            _1: /* :: */{
                                                                                                                                              _0: /* tuple */[
                                                                                                                                                "uint8clamped_array - fromBufferRange",
                                                                                                                                                (function (param) {
                                                                                                                                                    var buffer = new ArrayBuffer(32);
                                                                                                                                                    return {
                                                                                                                                                            tag: /* Eq */0,
                                                                                                                                                            _0: 2,
                                                                                                                                                            _1: new Uint8ClampedArray(buffer, 8, 2).byteLength
                                                                                                                                                          };
                                                                                                                                                  })
                                                                                                                                              ],
                                                                                                                                              _1: /* :: */{
                                                                                                                                                _0: /* tuple */[
                                                                                                                                                  "uint8clamped_array - fromLength",
                                                                                                                                                  (function (param) {
                                                                                                                                                      return {
                                                                                                                                                              tag: /* Eq */0,
                                                                                                                                                              _0: 3,
                                                                                                                                                              _1: new Uint8ClampedArray(3).byteLength
                                                                                                                                                            };
                                                                                                                                                    })
                                                                                                                                                ],
                                                                                                                                                _1: /* :: */{
                                                                                                                                                  _0: /* tuple */[
                                                                                                                                                    "uint8clamped_array - unsafe_set - typed_array sanity check",
                                                                                                                                                    (function (param) {
                                                                                                                                                        var a = new Uint8ClampedArray([
                                                                                                                                                              1,
                                                                                                                                                              2,
                                                                                                                                                              3,
                                                                                                                                                              4,
                                                                                                                                                              5
                                                                                                                                                            ]);
                                                                                                                                                        a[3] = 14;
                                                                                                                                                        return {
                                                                                                                                                                tag: /* Eq */0,
                                                                                                                                                                _0: 14,
                                                                                                                                                                _1: a[3]
                                                                                                                                                              };
                                                                                                                                                      })
                                                                                                                                                  ],
                                                                                                                                                  _1: /* :: */{
                                                                                                                                                    _0: /* tuple */[
                                                                                                                                                      "int16_array - _BYTES_PER_ELEMENT",
                                                                                                                                                      (function (param) {
                                                                                                                                                          return {
                                                                                                                                                                  tag: /* Eq */0,
                                                                                                                                                                  _0: 2,
                                                                                                                                                                  _1: Int16Array.BYTES_PER_ELEMENT
                                                                                                                                                                };
                                                                                                                                                        })
                                                                                                                                                    ],
                                                                                                                                                    _1: /* :: */{
                                                                                                                                                      _0: /* tuple */[
                                                                                                                                                        "int16_array - make",
                                                                                                                                                        (function (param) {
                                                                                                                                                            return {
                                                                                                                                                                    tag: /* Eq */0,
                                                                                                                                                                    _0: 6,
                                                                                                                                                                    _1: new Int16Array([
                                                                                                                                                                          1,
                                                                                                                                                                          2,
                                                                                                                                                                          3
                                                                                                                                                                        ]).byteLength
                                                                                                                                                                  };
                                                                                                                                                          })
                                                                                                                                                      ],
                                                                                                                                                      _1: /* :: */{
                                                                                                                                                        _0: /* tuple */[
                                                                                                                                                          "int16_array - fromBuffer",
                                                                                                                                                          (function (param) {
                                                                                                                                                              return {
                                                                                                                                                                      tag: /* Eq */0,
                                                                                                                                                                      _0: 32,
                                                                                                                                                                      _1: new Int16Array(new ArrayBuffer(32)).byteLength
                                                                                                                                                                    };
                                                                                                                                                            })
                                                                                                                                                        ],
                                                                                                                                                        _1: /* :: */{
                                                                                                                                                          _0: /* tuple */[
                                                                                                                                                            "int16_array - fromBufferOffset",
                                                                                                                                                            (function (param) {
                                                                                                                                                                var buffer = new ArrayBuffer(32);
                                                                                                                                                                return {
                                                                                                                                                                        tag: /* Eq */0,
                                                                                                                                                                        _0: 24,
                                                                                                                                                                        _1: new Int16Array(buffer, 8).byteLength
                                                                                                                                                                      };
                                                                                                                                                              })
                                                                                                                                                          ],
                                                                                                                                                          _1: /* :: */{
                                                                                                                                                            _0: /* tuple */[
                                                                                                                                                              "int16_array - fromBufferRange",
                                                                                                                                                              (function (param) {
                                                                                                                                                                  var buffer = new ArrayBuffer(32);
                                                                                                                                                                  return {
                                                                                                                                                                          tag: /* Eq */0,
                                                                                                                                                                          _0: 4,
                                                                                                                                                                          _1: new Int16Array(buffer, 8, 2).byteLength
                                                                                                                                                                        };
                                                                                                                                                                })
                                                                                                                                                            ],
                                                                                                                                                            _1: /* :: */{
                                                                                                                                                              _0: /* tuple */[
                                                                                                                                                                "int16_array - fromLength",
                                                                                                                                                                (function (param) {
                                                                                                                                                                    return {
                                                                                                                                                                            tag: /* Eq */0,
                                                                                                                                                                            _0: 6,
                                                                                                                                                                            _1: new Int16Array(3).byteLength
                                                                                                                                                                          };
                                                                                                                                                                  })
                                                                                                                                                              ],
                                                                                                                                                              _1: /* :: */{
                                                                                                                                                                _0: /* tuple */[
                                                                                                                                                                  "int16_array - unsafe_set - typed_array sanity check",
                                                                                                                                                                  (function (param) {
                                                                                                                                                                      var a = new Int16Array([
                                                                                                                                                                            1,
                                                                                                                                                                            2,
                                                                                                                                                                            3,
                                                                                                                                                                            4,
                                                                                                                                                                            5
                                                                                                                                                                          ]);
                                                                                                                                                                      a[3] = 14;
                                                                                                                                                                      return {
                                                                                                                                                                              tag: /* Eq */0,
                                                                                                                                                                              _0: 14,
                                                                                                                                                                              _1: a[3]
                                                                                                                                                                            };
                                                                                                                                                                    })
                                                                                                                                                                ],
                                                                                                                                                                _1: /* :: */{
                                                                                                                                                                  _0: /* tuple */[
                                                                                                                                                                    "uint16_array - _BYTES_PER_ELEMENT",
                                                                                                                                                                    (function (param) {
                                                                                                                                                                        return {
                                                                                                                                                                                tag: /* Eq */0,
                                                                                                                                                                                _0: 2,
                                                                                                                                                                                _1: Uint16Array.BYTES_PER_ELEMENT
                                                                                                                                                                              };
                                                                                                                                                                      })
                                                                                                                                                                  ],
                                                                                                                                                                  _1: /* :: */{
                                                                                                                                                                    _0: /* tuple */[
                                                                                                                                                                      "uint16_array - make",
                                                                                                                                                                      (function (param) {
                                                                                                                                                                          return {
                                                                                                                                                                                  tag: /* Eq */0,
                                                                                                                                                                                  _0: 6,
                                                                                                                                                                                  _1: new Uint16Array([
                                                                                                                                                                                        1,
                                                                                                                                                                                        2,
                                                                                                                                                                                        3
                                                                                                                                                                                      ]).byteLength
                                                                                                                                                                                };
                                                                                                                                                                        })
                                                                                                                                                                    ],
                                                                                                                                                                    _1: /* :: */{
                                                                                                                                                                      _0: /* tuple */[
                                                                                                                                                                        "uint16_array - fromBuffer",
                                                                                                                                                                        (function (param) {
                                                                                                                                                                            return {
                                                                                                                                                                                    tag: /* Eq */0,
                                                                                                                                                                                    _0: 32,
                                                                                                                                                                                    _1: new Uint16Array(new ArrayBuffer(32)).byteLength
                                                                                                                                                                                  };
                                                                                                                                                                          })
                                                                                                                                                                      ],
                                                                                                                                                                      _1: /* :: */{
                                                                                                                                                                        _0: /* tuple */[
                                                                                                                                                                          "uint16_array - fromBufferOffset",
                                                                                                                                                                          (function (param) {
                                                                                                                                                                              var buffer = new ArrayBuffer(32);
                                                                                                                                                                              return {
                                                                                                                                                                                      tag: /* Eq */0,
                                                                                                                                                                                      _0: 24,
                                                                                                                                                                                      _1: new Uint16Array(buffer, 8).byteLength
                                                                                                                                                                                    };
                                                                                                                                                                            })
                                                                                                                                                                        ],
                                                                                                                                                                        _1: /* :: */{
                                                                                                                                                                          _0: /* tuple */[
                                                                                                                                                                            "uint16_array - fromBufferRange",
                                                                                                                                                                            (function (param) {
                                                                                                                                                                                var buffer = new ArrayBuffer(32);
                                                                                                                                                                                return {
                                                                                                                                                                                        tag: /* Eq */0,
                                                                                                                                                                                        _0: 4,
                                                                                                                                                                                        _1: new Uint16Array(buffer, 8, 2).byteLength
                                                                                                                                                                                      };
                                                                                                                                                                              })
                                                                                                                                                                          ],
                                                                                                                                                                          _1: /* :: */{
                                                                                                                                                                            _0: /* tuple */[
                                                                                                                                                                              "uint16_array - fromLength",
                                                                                                                                                                              (function (param) {
                                                                                                                                                                                  return {
                                                                                                                                                                                          tag: /* Eq */0,
                                                                                                                                                                                          _0: 6,
                                                                                                                                                                                          _1: new Uint16Array(3).byteLength
                                                                                                                                                                                        };
                                                                                                                                                                                })
                                                                                                                                                                            ],
                                                                                                                                                                            _1: /* :: */{
                                                                                                                                                                              _0: /* tuple */[
                                                                                                                                                                                "uint16_array - unsafe_set - typed_array sanity check",
                                                                                                                                                                                (function (param) {
                                                                                                                                                                                    var a = new Uint16Array([
                                                                                                                                                                                          1,
                                                                                                                                                                                          2,
                                                                                                                                                                                          3,
                                                                                                                                                                                          4,
                                                                                                                                                                                          5
                                                                                                                                                                                        ]);
                                                                                                                                                                                    a[3] = 14;
                                                                                                                                                                                    return {
                                                                                                                                                                                            tag: /* Eq */0,
                                                                                                                                                                                            _0: 14,
                                                                                                                                                                                            _1: a[3]
                                                                                                                                                                                          };
                                                                                                                                                                                  })
                                                                                                                                                                              ],
                                                                                                                                                                              _1: /* :: */{
                                                                                                                                                                                _0: /* tuple */[
                                                                                                                                                                                  "int32_array - _BYTES_PER_ELEMENT",
                                                                                                                                                                                  (function (param) {
                                                                                                                                                                                      return {
                                                                                                                                                                                              tag: /* Eq */0,
                                                                                                                                                                                              _0: 4,
                                                                                                                                                                                              _1: Int32Array.BYTES_PER_ELEMENT
                                                                                                                                                                                            };
                                                                                                                                                                                    })
                                                                                                                                                                                ],
                                                                                                                                                                                _1: /* :: */{
                                                                                                                                                                                  _0: /* tuple */[
                                                                                                                                                                                    "int32_array - make",
                                                                                                                                                                                    (function (param) {
                                                                                                                                                                                        return {
                                                                                                                                                                                                tag: /* Eq */0,
                                                                                                                                                                                                _0: 12,
                                                                                                                                                                                                _1: new Int32Array($$Array.map((function (prim) {
                                                                                                                                                                                                            return prim;
                                                                                                                                                                                                          }), [
                                                                                                                                                                                                          1,
                                                                                                                                                                                                          2,
                                                                                                                                                                                                          3
                                                                                                                                                                                                        ])).byteLength
                                                                                                                                                                                              };
                                                                                                                                                                                      })
                                                                                                                                                                                  ],
                                                                                                                                                                                  _1: /* :: */{
                                                                                                                                                                                    _0: /* tuple */[
                                                                                                                                                                                      "int32_array - fromBuffer",
                                                                                                                                                                                      (function (param) {
                                                                                                                                                                                          return {
                                                                                                                                                                                                  tag: /* Eq */0,
                                                                                                                                                                                                  _0: 32,
                                                                                                                                                                                                  _1: new Int32Array(new ArrayBuffer(32)).byteLength
                                                                                                                                                                                                };
                                                                                                                                                                                        })
                                                                                                                                                                                    ],
                                                                                                                                                                                    _1: /* :: */{
                                                                                                                                                                                      _0: /* tuple */[
                                                                                                                                                                                        "int32_array - fromBufferOffset",
                                                                                                                                                                                        (function (param) {
                                                                                                                                                                                            var buffer = new ArrayBuffer(32);
                                                                                                                                                                                            return {
                                                                                                                                                                                                    tag: /* Eq */0,
                                                                                                                                                                                                    _0: 24,
                                                                                                                                                                                                    _1: new Int32Array(buffer, 8).byteLength
                                                                                                                                                                                                  };
                                                                                                                                                                                          })
                                                                                                                                                                                      ],
                                                                                                                                                                                      _1: /* :: */{
                                                                                                                                                                                        _0: /* tuple */[
                                                                                                                                                                                          "int32_array - fromBufferRange",
                                                                                                                                                                                          (function (param) {
                                                                                                                                                                                              var buffer = new ArrayBuffer(32);
                                                                                                                                                                                              return {
                                                                                                                                                                                                      tag: /* Eq */0,
                                                                                                                                                                                                      _0: 8,
                                                                                                                                                                                                      _1: new Int32Array(buffer, 8, 2).byteLength
                                                                                                                                                                                                    };
                                                                                                                                                                                            })
                                                                                                                                                                                        ],
                                                                                                                                                                                        _1: /* :: */{
                                                                                                                                                                                          _0: /* tuple */[
                                                                                                                                                                                            "int32_array - fromLength",
                                                                                                                                                                                            (function (param) {
                                                                                                                                                                                                return {
                                                                                                                                                                                                        tag: /* Eq */0,
                                                                                                                                                                                                        _0: 12,
                                                                                                                                                                                                        _1: new Int32Array(3).byteLength
                                                                                                                                                                                                      };
                                                                                                                                                                                              })
                                                                                                                                                                                          ],
                                                                                                                                                                                          _1: /* :: */{
                                                                                                                                                                                            _0: /* tuple */[
                                                                                                                                                                                              "int32_array - unsafe_set - typed_array sanity check",
                                                                                                                                                                                              (function (param) {
                                                                                                                                                                                                  var a = new Int32Array($$Array.map((function (prim) {
                                                                                                                                                                                                              return prim;
                                                                                                                                                                                                            }), [
                                                                                                                                                                                                            1,
                                                                                                                                                                                                            2,
                                                                                                                                                                                                            3,
                                                                                                                                                                                                            4,
                                                                                                                                                                                                            5
                                                                                                                                                                                                          ]));
                                                                                                                                                                                                  a[3] = 14;
                                                                                                                                                                                                  return {
                                                                                                                                                                                                          tag: /* Eq */0,
                                                                                                                                                                                                          _0: 14,
                                                                                                                                                                                                          _1: a[3]
                                                                                                                                                                                                        };
                                                                                                                                                                                                })
                                                                                                                                                                                            ],
                                                                                                                                                                                            _1: /* :: */{
                                                                                                                                                                                              _0: /* tuple */[
                                                                                                                                                                                                "uint32_array - _BYTES_PER_ELEMENT",
                                                                                                                                                                                                (function (param) {
                                                                                                                                                                                                    return {
                                                                                                                                                                                                            tag: /* Eq */0,
                                                                                                                                                                                                            _0: 4,
                                                                                                                                                                                                            _1: Uint32Array.BYTES_PER_ELEMENT
                                                                                                                                                                                                          };
                                                                                                                                                                                                  })
                                                                                                                                                                                              ],
                                                                                                                                                                                              _1: /* :: */{
                                                                                                                                                                                                _0: /* tuple */[
                                                                                                                                                                                                  "uint32_array - make",
                                                                                                                                                                                                  (function (param) {
                                                                                                                                                                                                      return {
                                                                                                                                                                                                              tag: /* Eq */0,
                                                                                                                                                                                                              _0: 12,
                                                                                                                                                                                                              _1: new Uint32Array([
                                                                                                                                                                                                                    1,
                                                                                                                                                                                                                    2,
                                                                                                                                                                                                                    3
                                                                                                                                                                                                                  ]).byteLength
                                                                                                                                                                                                            };
                                                                                                                                                                                                    })
                                                                                                                                                                                                ],
                                                                                                                                                                                                _1: /* :: */{
                                                                                                                                                                                                  _0: /* tuple */[
                                                                                                                                                                                                    "uint32_array - fromBuffer",
                                                                                                                                                                                                    (function (param) {
                                                                                                                                                                                                        return {
                                                                                                                                                                                                                tag: /* Eq */0,
                                                                                                                                                                                                                _0: 32,
                                                                                                                                                                                                                _1: new Uint32Array(new ArrayBuffer(32)).byteLength
                                                                                                                                                                                                              };
                                                                                                                                                                                                      })
                                                                                                                                                                                                  ],
                                                                                                                                                                                                  _1: /* :: */{
                                                                                                                                                                                                    _0: /* tuple */[
                                                                                                                                                                                                      "uint32_array - fromBufferOffset",
                                                                                                                                                                                                      (function (param) {
                                                                                                                                                                                                          var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                          return {
                                                                                                                                                                                                                  tag: /* Eq */0,
                                                                                                                                                                                                                  _0: 24,
                                                                                                                                                                                                                  _1: new Uint32Array(buffer, 8).byteLength
                                                                                                                                                                                                                };
                                                                                                                                                                                                        })
                                                                                                                                                                                                    ],
                                                                                                                                                                                                    _1: /* :: */{
                                                                                                                                                                                                      _0: /* tuple */[
                                                                                                                                                                                                        "uint32_array - fromBufferRange",
                                                                                                                                                                                                        (function (param) {
                                                                                                                                                                                                            var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                            return {
                                                                                                                                                                                                                    tag: /* Eq */0,
                                                                                                                                                                                                                    _0: 8,
                                                                                                                                                                                                                    _1: new Uint32Array(buffer, 8, 2).byteLength
                                                                                                                                                                                                                  };
                                                                                                                                                                                                          })
                                                                                                                                                                                                      ],
                                                                                                                                                                                                      _1: /* :: */{
                                                                                                                                                                                                        _0: /* tuple */[
                                                                                                                                                                                                          "uint32_array - fromLength",
                                                                                                                                                                                                          (function (param) {
                                                                                                                                                                                                              return {
                                                                                                                                                                                                                      tag: /* Eq */0,
                                                                                                                                                                                                                      _0: 12,
                                                                                                                                                                                                                      _1: new Uint32Array(3).byteLength
                                                                                                                                                                                                                    };
                                                                                                                                                                                                            })
                                                                                                                                                                                                        ],
                                                                                                                                                                                                        _1: /* :: */{
                                                                                                                                                                                                          _0: /* tuple */[
                                                                                                                                                                                                            "uint32_array - unsafe_set - typed_array sanity check",
                                                                                                                                                                                                            (function (param) {
                                                                                                                                                                                                                var a = new Uint32Array([
                                                                                                                                                                                                                      1,
                                                                                                                                                                                                                      2,
                                                                                                                                                                                                                      3,
                                                                                                                                                                                                                      4,
                                                                                                                                                                                                                      5
                                                                                                                                                                                                                    ]);
                                                                                                                                                                                                                a[3] = 14;
                                                                                                                                                                                                                return {
                                                                                                                                                                                                                        tag: /* Eq */0,
                                                                                                                                                                                                                        _0: 14,
                                                                                                                                                                                                                        _1: a[3]
                                                                                                                                                                                                                      };
                                                                                                                                                                                                              })
                                                                                                                                                                                                          ],
                                                                                                                                                                                                          _1: /* :: */{
                                                                                                                                                                                                            _0: /* tuple */[
                                                                                                                                                                                                              "float32_array - _BYTES_PER_ELEMENT",
                                                                                                                                                                                                              (function (param) {
                                                                                                                                                                                                                  return {
                                                                                                                                                                                                                          tag: /* Eq */0,
                                                                                                                                                                                                                          _0: 4,
                                                                                                                                                                                                                          _1: Float32Array.BYTES_PER_ELEMENT
                                                                                                                                                                                                                        };
                                                                                                                                                                                                                })
                                                                                                                                                                                                            ],
                                                                                                                                                                                                            _1: /* :: */{
                                                                                                                                                                                                              _0: /* tuple */[
                                                                                                                                                                                                                "float32_array - make",
                                                                                                                                                                                                                (function (param) {
                                                                                                                                                                                                                    return {
                                                                                                                                                                                                                            tag: /* Eq */0,
                                                                                                                                                                                                                            _0: 12,
                                                                                                                                                                                                                            _1: new Float32Array([
                                                                                                                                                                                                                                  1,
                                                                                                                                                                                                                                  2,
                                                                                                                                                                                                                                  3
                                                                                                                                                                                                                                ]).byteLength
                                                                                                                                                                                                                          };
                                                                                                                                                                                                                  })
                                                                                                                                                                                                              ],
                                                                                                                                                                                                              _1: /* :: */{
                                                                                                                                                                                                                _0: /* tuple */[
                                                                                                                                                                                                                  "float32_array - fromBuffer",
                                                                                                                                                                                                                  (function (param) {
                                                                                                                                                                                                                      return {
                                                                                                                                                                                                                              tag: /* Eq */0,
                                                                                                                                                                                                                              _0: 32,
                                                                                                                                                                                                                              _1: new Float32Array(new ArrayBuffer(32)).byteLength
                                                                                                                                                                                                                            };
                                                                                                                                                                                                                    })
                                                                                                                                                                                                                ],
                                                                                                                                                                                                                _1: /* :: */{
                                                                                                                                                                                                                  _0: /* tuple */[
                                                                                                                                                                                                                    "float32_array - fromBufferOffset",
                                                                                                                                                                                                                    (function (param) {
                                                                                                                                                                                                                        var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                        return {
                                                                                                                                                                                                                                tag: /* Eq */0,
                                                                                                                                                                                                                                _0: 24,
                                                                                                                                                                                                                                _1: new Float32Array(buffer, 8).byteLength
                                                                                                                                                                                                                              };
                                                                                                                                                                                                                      })
                                                                                                                                                                                                                  ],
                                                                                                                                                                                                                  _1: /* :: */{
                                                                                                                                                                                                                    _0: /* tuple */[
                                                                                                                                                                                                                      "float32_array - fromBufferRange",
                                                                                                                                                                                                                      (function (param) {
                                                                                                                                                                                                                          var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                          return {
                                                                                                                                                                                                                                  tag: /* Eq */0,
                                                                                                                                                                                                                                  _0: 8,
                                                                                                                                                                                                                                  _1: new Float32Array(buffer, 8, 2).byteLength
                                                                                                                                                                                                                                };
                                                                                                                                                                                                                        })
                                                                                                                                                                                                                    ],
                                                                                                                                                                                                                    _1: /* :: */{
                                                                                                                                                                                                                      _0: /* tuple */[
                                                                                                                                                                                                                        "float32_array - fromLength",
                                                                                                                                                                                                                        (function (param) {
                                                                                                                                                                                                                            return {
                                                                                                                                                                                                                                    tag: /* Eq */0,
                                                                                                                                                                                                                                    _0: 12,
                                                                                                                                                                                                                                    _1: new Float32Array(3).byteLength
                                                                                                                                                                                                                                  };
                                                                                                                                                                                                                          })
                                                                                                                                                                                                                      ],
                                                                                                                                                                                                                      _1: /* :: */{
                                                                                                                                                                                                                        _0: /* tuple */[
                                                                                                                                                                                                                          "float32_array - unsafe_set - typed_array sanity check",
                                                                                                                                                                                                                          (function (param) {
                                                                                                                                                                                                                              var a = new Float32Array([
                                                                                                                                                                                                                                    1,
                                                                                                                                                                                                                                    2,
                                                                                                                                                                                                                                    3,
                                                                                                                                                                                                                                    4,
                                                                                                                                                                                                                                    5
                                                                                                                                                                                                                                  ]);
                                                                                                                                                                                                                              a[3] = 14;
                                                                                                                                                                                                                              return {
                                                                                                                                                                                                                                      tag: /* Eq */0,
                                                                                                                                                                                                                                      _0: 14,
                                                                                                                                                                                                                                      _1: a[3]
                                                                                                                                                                                                                                    };
                                                                                                                                                                                                                            })
                                                                                                                                                                                                                        ],
                                                                                                                                                                                                                        _1: /* :: */{
                                                                                                                                                                                                                          _0: /* tuple */[
                                                                                                                                                                                                                            "float64_array - _BYTES_PER_ELEMENT",
                                                                                                                                                                                                                            (function (param) {
                                                                                                                                                                                                                                return {
                                                                                                                                                                                                                                        tag: /* Eq */0,
                                                                                                                                                                                                                                        _0: 8,
                                                                                                                                                                                                                                        _1: Float64Array.BYTES_PER_ELEMENT
                                                                                                                                                                                                                                      };
                                                                                                                                                                                                                              })
                                                                                                                                                                                                                          ],
                                                                                                                                                                                                                          _1: /* :: */{
                                                                                                                                                                                                                            _0: /* tuple */[
                                                                                                                                                                                                                              "float64_array - make",
                                                                                                                                                                                                                              (function (param) {
                                                                                                                                                                                                                                  return {
                                                                                                                                                                                                                                          tag: /* Eq */0,
                                                                                                                                                                                                                                          _0: 24,
                                                                                                                                                                                                                                          _1: new Float64Array([
                                                                                                                                                                                                                                                1,
                                                                                                                                                                                                                                                2,
                                                                                                                                                                                                                                                3
                                                                                                                                                                                                                                              ]).byteLength
                                                                                                                                                                                                                                        };
                                                                                                                                                                                                                                })
                                                                                                                                                                                                                            ],
                                                                                                                                                                                                                            _1: /* :: */{
                                                                                                                                                                                                                              _0: /* tuple */[
                                                                                                                                                                                                                                "float64_array - fromBuffer",
                                                                                                                                                                                                                                (function (param) {
                                                                                                                                                                                                                                    return {
                                                                                                                                                                                                                                            tag: /* Eq */0,
                                                                                                                                                                                                                                            _0: 32,
                                                                                                                                                                                                                                            _1: new Float64Array(new ArrayBuffer(32)).byteLength
                                                                                                                                                                                                                                          };
                                                                                                                                                                                                                                  })
                                                                                                                                                                                                                              ],
                                                                                                                                                                                                                              _1: /* :: */{
                                                                                                                                                                                                                                _0: /* tuple */[
                                                                                                                                                                                                                                  "float64_array - fromBufferOffset",
                                                                                                                                                                                                                                  (function (param) {
                                                                                                                                                                                                                                      var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                                      return {
                                                                                                                                                                                                                                              tag: /* Eq */0,
                                                                                                                                                                                                                                              _0: 24,
                                                                                                                                                                                                                                              _1: new Float64Array(buffer, 8).byteLength
                                                                                                                                                                                                                                            };
                                                                                                                                                                                                                                    })
                                                                                                                                                                                                                                ],
                                                                                                                                                                                                                                _1: /* :: */{
                                                                                                                                                                                                                                  _0: /* tuple */[
                                                                                                                                                                                                                                    "float64_array - fromBufferRange",
                                                                                                                                                                                                                                    (function (param) {
                                                                                                                                                                                                                                        var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                                        return {
                                                                                                                                                                                                                                                tag: /* Eq */0,
                                                                                                                                                                                                                                                _0: 16,
                                                                                                                                                                                                                                                _1: new Float64Array(buffer, 8, 2).byteLength
                                                                                                                                                                                                                                              };
                                                                                                                                                                                                                                      })
                                                                                                                                                                                                                                  ],
                                                                                                                                                                                                                                  _1: /* :: */{
                                                                                                                                                                                                                                    _0: /* tuple */[
                                                                                                                                                                                                                                      "float64_array - fromLength",
                                                                                                                                                                                                                                      (function (param) {
                                                                                                                                                                                                                                          return {
                                                                                                                                                                                                                                                  tag: /* Eq */0,
                                                                                                                                                                                                                                                  _0: 24,
                                                                                                                                                                                                                                                  _1: new Float64Array(3).byteLength
                                                                                                                                                                                                                                                };
                                                                                                                                                                                                                                        })
                                                                                                                                                                                                                                    ],
                                                                                                                                                                                                                                    _1: /* :: */{
                                                                                                                                                                                                                                      _0: /* tuple */[
                                                                                                                                                                                                                                        "float64_array - unsafe_set - typed_array sanity check",
                                                                                                                                                                                                                                        (function (param) {
                                                                                                                                                                                                                                            var a = new Float64Array([
                                                                                                                                                                                                                                                  1,
                                                                                                                                                                                                                                                  2,
                                                                                                                                                                                                                                                  3,
                                                                                                                                                                                                                                                  4,
                                                                                                                                                                                                                                                  5
                                                                                                                                                                                                                                                ]);
                                                                                                                                                                                                                                            a[3] = 14;
                                                                                                                                                                                                                                            return {
                                                                                                                                                                                                                                                    tag: /* Eq */0,
                                                                                                                                                                                                                                                    _0: 14,
                                                                                                                                                                                                                                                    _1: a[3]
                                                                                                                                                                                                                                                  };
                                                                                                                                                                                                                                          })
                                                                                                                                                                                                                                      ],
                                                                                                                                                                                                                                      _1: /* :: */{
                                                                                                                                                                                                                                        _0: /* tuple */[
                                                                                                                                                                                                                                          "DataView - make, byteLength",
                                                                                                                                                                                                                                          (function (param) {
                                                                                                                                                                                                                                              return {
                                                                                                                                                                                                                                                      tag: /* Eq */0,
                                                                                                                                                                                                                                                      _0: 32,
                                                                                                                                                                                                                                                      _1: new DataView(new ArrayBuffer(32)).byteLength
                                                                                                                                                                                                                                                    };
                                                                                                                                                                                                                                            })
                                                                                                                                                                                                                                        ],
                                                                                                                                                                                                                                        _1: /* :: */{
                                                                                                                                                                                                                                          _0: /* tuple */[
                                                                                                                                                                                                                                            "DataView - fromBuffer",
                                                                                                                                                                                                                                            (function (param) {
                                                                                                                                                                                                                                                return {
                                                                                                                                                                                                                                                        tag: /* Eq */0,
                                                                                                                                                                                                                                                        _0: 32,
                                                                                                                                                                                                                                                        _1: new DataView(new ArrayBuffer(32)).byteLength
                                                                                                                                                                                                                                                      };
                                                                                                                                                                                                                                              })
                                                                                                                                                                                                                                          ],
                                                                                                                                                                                                                                          _1: /* :: */{
                                                                                                                                                                                                                                            _0: /* tuple */[
                                                                                                                                                                                                                                              "DataView - fromBufferOffset",
                                                                                                                                                                                                                                              (function (param) {
                                                                                                                                                                                                                                                  var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                                                  return {
                                                                                                                                                                                                                                                          tag: /* Eq */0,
                                                                                                                                                                                                                                                          _0: 24,
                                                                                                                                                                                                                                                          _1: new DataView(buffer, 8).byteLength
                                                                                                                                                                                                                                                        };
                                                                                                                                                                                                                                                })
                                                                                                                                                                                                                                            ],
                                                                                                                                                                                                                                            _1: /* :: */{
                                                                                                                                                                                                                                              _0: /* tuple */[
                                                                                                                                                                                                                                                "DataView - fromBufferRange",
                                                                                                                                                                                                                                                (function (param) {
                                                                                                                                                                                                                                                    var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                                                    return {
                                                                                                                                                                                                                                                            tag: /* Eq */0,
                                                                                                                                                                                                                                                            _0: 4,
                                                                                                                                                                                                                                                            _1: new DataView(buffer, 8, 4).byteLength
                                                                                                                                                                                                                                                          };
                                                                                                                                                                                                                                                  })
                                                                                                                                                                                                                                              ],
                                                                                                                                                                                                                                              _1: /* :: */{
                                                                                                                                                                                                                                                _0: /* tuple */[
                                                                                                                                                                                                                                                  "DataView - buffer",
                                                                                                                                                                                                                                                  (function (param) {
                                                                                                                                                                                                                                                      var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                                                      return {
                                                                                                                                                                                                                                                              tag: /* Eq */0,
                                                                                                                                                                                                                                                              _0: buffer,
                                                                                                                                                                                                                                                              _1: new DataView(buffer).buffer
                                                                                                                                                                                                                                                            };
                                                                                                                                                                                                                                                    })
                                                                                                                                                                                                                                                ],
                                                                                                                                                                                                                                                _1: /* :: */{
                                                                                                                                                                                                                                                  _0: /* tuple */[
                                                                                                                                                                                                                                                    "DataView - byteOffset",
                                                                                                                                                                                                                                                    (function (param) {
                                                                                                                                                                                                                                                        var buffer = new ArrayBuffer(32);
                                                                                                                                                                                                                                                        return {
                                                                                                                                                                                                                                                                tag: /* Eq */0,
                                                                                                                                                                                                                                                                _0: 8,
                                                                                                                                                                                                                                                                _1: new DataView(buffer, 8).byteOffset
                                                                                                                                                                                                                                                              };
                                                                                                                                                                                                                                                      })
                                                                                                                                                                                                                                                  ],
                                                                                                                                                                                                                                                  _1: /* :: */{
                                                                                                                                                                                                                                                    _0: /* tuple */[
                                                                                                                                                                                                                                                      "DataView - setInt8, getInt8",
                                                                                                                                                                                                                                                      (function (param) {
                                                                                                                                                                                                                                                          var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                          var view = new DataView(buffer);
                                                                                                                                                                                                                                                          view.setInt8(0, 1);
                                                                                                                                                                                                                                                          return {
                                                                                                                                                                                                                                                                  tag: /* Eq */0,
                                                                                                                                                                                                                                                                  _0: 1,
                                                                                                                                                                                                                                                                  _1: view.getInt8(0)
                                                                                                                                                                                                                                                                };
                                                                                                                                                                                                                                                        })
                                                                                                                                                                                                                                                    ],
                                                                                                                                                                                                                                                    _1: /* :: */{
                                                                                                                                                                                                                                                      _0: /* tuple */[
                                                                                                                                                                                                                                                        "DataView - setUint8, getUint8",
                                                                                                                                                                                                                                                        (function (param) {
                                                                                                                                                                                                                                                            var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                            var view = new DataView(buffer);
                                                                                                                                                                                                                                                            view.setUint8(0, 128);
                                                                                                                                                                                                                                                            return {
                                                                                                                                                                                                                                                                    tag: /* Eq */0,
                                                                                                                                                                                                                                                                    _0: 128,
                                                                                                                                                                                                                                                                    _1: view.getUint8(0)
                                                                                                                                                                                                                                                                  };
                                                                                                                                                                                                                                                          })
                                                                                                                                                                                                                                                      ],
                                                                                                                                                                                                                                                      _1: /* :: */{
                                                                                                                                                                                                                                                        _0: /* tuple */[
                                                                                                                                                                                                                                                          "DataView - setInt16, getInt16",
                                                                                                                                                                                                                                                          (function (param) {
                                                                                                                                                                                                                                                              var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                              var view = new DataView(buffer);
                                                                                                                                                                                                                                                              view.setInt16(0, 257);
                                                                                                                                                                                                                                                              return {
                                                                                                                                                                                                                                                                      tag: /* Eq */0,
                                                                                                                                                                                                                                                                      _0: 257,
                                                                                                                                                                                                                                                                      _1: view.getInt16(0)
                                                                                                                                                                                                                                                                    };
                                                                                                                                                                                                                                                            })
                                                                                                                                                                                                                                                        ],
                                                                                                                                                                                                                                                        _1: /* :: */{
                                                                                                                                                                                                                                                          _0: /* tuple */[
                                                                                                                                                                                                                                                            "DataView - getInt16LittleEndian",
                                                                                                                                                                                                                                                            (function (param) {
                                                                                                                                                                                                                                                                var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                var view = new DataView(buffer);
                                                                                                                                                                                                                                                                view.setInt16(0, 25000, 1);
                                                                                                                                                                                                                                                                return {
                                                                                                                                                                                                                                                                        tag: /* Eq */0,
                                                                                                                                                                                                                                                                        _0: 25000,
                                                                                                                                                                                                                                                                        _1: view.getInt16(0, 1)
                                                                                                                                                                                                                                                                      };
                                                                                                                                                                                                                                                              })
                                                                                                                                                                                                                                                          ],
                                                                                                                                                                                                                                                          _1: /* :: */{
                                                                                                                                                                                                                                                            _0: /* tuple */[
                                                                                                                                                                                                                                                              "DataView - setInt16LittleEndian",
                                                                                                                                                                                                                                                              (function (param) {
                                                                                                                                                                                                                                                                  var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                  var view = new DataView(buffer);
                                                                                                                                                                                                                                                                  view.setInt16(0, 25000, 1);
                                                                                                                                                                                                                                                                  return {
                                                                                                                                                                                                                                                                          tag: /* Eq */0,
                                                                                                                                                                                                                                                                          _0: -22431,
                                                                                                                                                                                                                                                                          _1: view.getInt16(0)
                                                                                                                                                                                                                                                                        };
                                                                                                                                                                                                                                                                })
                                                                                                                                                                                                                                                            ],
                                                                                                                                                                                                                                                            _1: /* :: */{
                                                                                                                                                                                                                                                              _0: /* tuple */[
                                                                                                                                                                                                                                                                "DataView - setUint16, getUint16",
                                                                                                                                                                                                                                                                (function (param) {
                                                                                                                                                                                                                                                                    var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                    var view = new DataView(buffer);
                                                                                                                                                                                                                                                                    view.setUint16(0, 32768);
                                                                                                                                                                                                                                                                    return {
                                                                                                                                                                                                                                                                            tag: /* Eq */0,
                                                                                                                                                                                                                                                                            _0: 32768,
                                                                                                                                                                                                                                                                            _1: view.getUint16(0)
                                                                                                                                                                                                                                                                          };
                                                                                                                                                                                                                                                                  })
                                                                                                                                                                                                                                                              ],
                                                                                                                                                                                                                                                              _1: /* :: */{
                                                                                                                                                                                                                                                                _0: /* tuple */[
                                                                                                                                                                                                                                                                  "DataView - getUint16LittleEndian",
                                                                                                                                                                                                                                                                  (function (param) {
                                                                                                                                                                                                                                                                      var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                      var view = new DataView(buffer);
                                                                                                                                                                                                                                                                      view.setUint16(0, 32768, 1);
                                                                                                                                                                                                                                                                      return {
                                                                                                                                                                                                                                                                              tag: /* Eq */0,
                                                                                                                                                                                                                                                                              _0: 32768,
                                                                                                                                                                                                                                                                              _1: view.getUint16(0, 1)
                                                                                                                                                                                                                                                                            };
                                                                                                                                                                                                                                                                    })
                                                                                                                                                                                                                                                                ],
                                                                                                                                                                                                                                                                _1: /* :: */{
                                                                                                                                                                                                                                                                  _0: /* tuple */[
                                                                                                                                                                                                                                                                    "DataView - setUint16LittleEndian",
                                                                                                                                                                                                                                                                    (function (param) {
                                                                                                                                                                                                                                                                        var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                        var view = new DataView(buffer);
                                                                                                                                                                                                                                                                        view.setUint16(0, 32768, 1);
                                                                                                                                                                                                                                                                        return {
                                                                                                                                                                                                                                                                                tag: /* Eq */0,
                                                                                                                                                                                                                                                                                _0: 128,
                                                                                                                                                                                                                                                                                _1: view.getUint16(0)
                                                                                                                                                                                                                                                                              };
                                                                                                                                                                                                                                                                      })
                                                                                                                                                                                                                                                                  ],
                                                                                                                                                                                                                                                                  _1: /* :: */{
                                                                                                                                                                                                                                                                    _0: /* tuple */[
                                                                                                                                                                                                                                                                      "DataView - setInt32, getInt32",
                                                                                                                                                                                                                                                                      (function (param) {
                                                                                                                                                                                                                                                                          var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                          var view = new DataView(buffer);
                                                                                                                                                                                                                                                                          view.setInt32(0, 65537);
                                                                                                                                                                                                                                                                          return {
                                                                                                                                                                                                                                                                                  tag: /* Eq */0,
                                                                                                                                                                                                                                                                                  _0: 65537,
                                                                                                                                                                                                                                                                                  _1: view.getInt32(0)
                                                                                                                                                                                                                                                                                };
                                                                                                                                                                                                                                                                        })
                                                                                                                                                                                                                                                                    ],
                                                                                                                                                                                                                                                                    _1: /* :: */{
                                                                                                                                                                                                                                                                      _0: /* tuple */[
                                                                                                                                                                                                                                                                        "DataView - getInt32LittleEndian",
                                                                                                                                                                                                                                                                        (function (param) {
                                                                                                                                                                                                                                                                            var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                            var view = new DataView(buffer);
                                                                                                                                                                                                                                                                            view.setInt32(0, 65537, 1);
                                                                                                                                                                                                                                                                            return {
                                                                                                                                                                                                                                                                                    tag: /* Eq */0,
                                                                                                                                                                                                                                                                                    _0: 65537,
                                                                                                                                                                                                                                                                                    _1: view.getInt32(0, 1)
                                                                                                                                                                                                                                                                                  };
                                                                                                                                                                                                                                                                          })
                                                                                                                                                                                                                                                                      ],
                                                                                                                                                                                                                                                                      _1: /* :: */{
                                                                                                                                                                                                                                                                        _0: /* tuple */[
                                                                                                                                                                                                                                                                          "DataView - setInt32LittleEndian",
                                                                                                                                                                                                                                                                          (function (param) {
                                                                                                                                                                                                                                                                              var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                              var view = new DataView(buffer);
                                                                                                                                                                                                                                                                              view.setInt32(0, 65537, 1);
                                                                                                                                                                                                                                                                              return {
                                                                                                                                                                                                                                                                                      tag: /* Eq */0,
                                                                                                                                                                                                                                                                                      _0: 16777472,
                                                                                                                                                                                                                                                                                      _1: view.getInt32(0)
                                                                                                                                                                                                                                                                                    };
                                                                                                                                                                                                                                                                            })
                                                                                                                                                                                                                                                                        ],
                                                                                                                                                                                                                                                                        _1: /* :: */{
                                                                                                                                                                                                                                                                          _0: /* tuple */[
                                                                                                                                                                                                                                                                            "DataView - setUint32, getUint32",
                                                                                                                                                                                                                                                                            (function (param) {
                                                                                                                                                                                                                                                                                var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                view.setUint32(0, 65537);
                                                                                                                                                                                                                                                                                return {
                                                                                                                                                                                                                                                                                        tag: /* Eq */0,
                                                                                                                                                                                                                                                                                        _0: 65537,
                                                                                                                                                                                                                                                                                        _1: view.getUint32(0)
                                                                                                                                                                                                                                                                                      };
                                                                                                                                                                                                                                                                              })
                                                                                                                                                                                                                                                                          ],
                                                                                                                                                                                                                                                                          _1: /* :: */{
                                                                                                                                                                                                                                                                            _0: /* tuple */[
                                                                                                                                                                                                                                                                              "DataView - getUint32LittleEndian",
                                                                                                                                                                                                                                                                              (function (param) {
                                                                                                                                                                                                                                                                                  var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                  var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                  view.setUint32(0, 65537, 1);
                                                                                                                                                                                                                                                                                  return {
                                                                                                                                                                                                                                                                                          tag: /* Eq */0,
                                                                                                                                                                                                                                                                                          _0: 65537,
                                                                                                                                                                                                                                                                                          _1: view.getUint32(0, 1)
                                                                                                                                                                                                                                                                                        };
                                                                                                                                                                                                                                                                                })
                                                                                                                                                                                                                                                                            ],
                                                                                                                                                                                                                                                                            _1: /* :: */{
                                                                                                                                                                                                                                                                              _0: /* tuple */[
                                                                                                                                                                                                                                                                                "DataView - setUint32LittleEndian",
                                                                                                                                                                                                                                                                                (function (param) {
                                                                                                                                                                                                                                                                                    var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                    var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                    view.setUint32(0, 65537, 1);
                                                                                                                                                                                                                                                                                    return {
                                                                                                                                                                                                                                                                                            tag: /* Eq */0,
                                                                                                                                                                                                                                                                                            _0: 16777472,
                                                                                                                                                                                                                                                                                            _1: view.getUint32(0)
                                                                                                                                                                                                                                                                                          };
                                                                                                                                                                                                                                                                                  })
                                                                                                                                                                                                                                                                              ],
                                                                                                                                                                                                                                                                              _1: /* :: */{
                                                                                                                                                                                                                                                                                _0: /* tuple */[
                                                                                                                                                                                                                                                                                  "DataView - setFloat32, getFloat32",
                                                                                                                                                                                                                                                                                  (function (param) {
                                                                                                                                                                                                                                                                                      var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                      var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                      view.setFloat32(0, 65537.0);
                                                                                                                                                                                                                                                                                      return {
                                                                                                                                                                                                                                                                                              tag: /* Eq */0,
                                                                                                                                                                                                                                                                                              _0: 65537.0,
                                                                                                                                                                                                                                                                                              _1: view.getFloat32(0)
                                                                                                                                                                                                                                                                                            };
                                                                                                                                                                                                                                                                                    })
                                                                                                                                                                                                                                                                                ],
                                                                                                                                                                                                                                                                                _1: /* :: */{
                                                                                                                                                                                                                                                                                  _0: /* tuple */[
                                                                                                                                                                                                                                                                                    "DataView - getFloat32LittleEndian",
                                                                                                                                                                                                                                                                                    (function (param) {
                                                                                                                                                                                                                                                                                        var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                        var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                        view.setFloat32(0, 65537.0, 1);
                                                                                                                                                                                                                                                                                        return {
                                                                                                                                                                                                                                                                                                tag: /* Eq */0,
                                                                                                                                                                                                                                                                                                _0: 65537.0,
                                                                                                                                                                                                                                                                                                _1: view.getFloat32(0, 1)
                                                                                                                                                                                                                                                                                              };
                                                                                                                                                                                                                                                                                      })
                                                                                                                                                                                                                                                                                  ],
                                                                                                                                                                                                                                                                                  _1: /* :: */{
                                                                                                                                                                                                                                                                                    _0: /* tuple */[
                                                                                                                                                                                                                                                                                      "DataView - setFloat32LittleEndian",
                                                                                                                                                                                                                                                                                      (function (param) {
                                                                                                                                                                                                                                                                                          var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                          var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                          view.setFloat32(0, 1.0, 1);
                                                                                                                                                                                                                                                                                          return {
                                                                                                                                                                                                                                                                                                  tag: /* Eq */0,
                                                                                                                                                                                                                                                                                                  _0: 4.600602988224807e-41,
                                                                                                                                                                                                                                                                                                  _1: view.getFloat32(0)
                                                                                                                                                                                                                                                                                                };
                                                                                                                                                                                                                                                                                        })
                                                                                                                                                                                                                                                                                    ],
                                                                                                                                                                                                                                                                                    _1: /* :: */{
                                                                                                                                                                                                                                                                                      _0: /* tuple */[
                                                                                                                                                                                                                                                                                        "DataView - setFloat64, getFloat64",
                                                                                                                                                                                                                                                                                        (function (param) {
                                                                                                                                                                                                                                                                                            var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                            var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                            view.setFloat64(0, 1e200);
                                                                                                                                                                                                                                                                                            return {
                                                                                                                                                                                                                                                                                                    tag: /* Eq */0,
                                                                                                                                                                                                                                                                                                    _0: 1e200,
                                                                                                                                                                                                                                                                                                    _1: view.getFloat64(0)
                                                                                                                                                                                                                                                                                                  };
                                                                                                                                                                                                                                                                                          })
                                                                                                                                                                                                                                                                                      ],
                                                                                                                                                                                                                                                                                      _1: /* :: */{
                                                                                                                                                                                                                                                                                        _0: /* tuple */[
                                                                                                                                                                                                                                                                                          "DataView - getFloat64LittleEndian",
                                                                                                                                                                                                                                                                                          (function (param) {
                                                                                                                                                                                                                                                                                              var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                              var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                              view.setFloat64(0, 1e200, 1);
                                                                                                                                                                                                                                                                                              return {
                                                                                                                                                                                                                                                                                                      tag: /* Eq */0,
                                                                                                                                                                                                                                                                                                      _0: 1e200,
                                                                                                                                                                                                                                                                                                      _1: view.getFloat64(0, 1)
                                                                                                                                                                                                                                                                                                    };
                                                                                                                                                                                                                                                                                            })
                                                                                                                                                                                                                                                                                        ],
                                                                                                                                                                                                                                                                                        _1: /* :: */{
                                                                                                                                                                                                                                                                                          _0: /* tuple */[
                                                                                                                                                                                                                                                                                            "DataView - setFloat64LittleEndian",
                                                                                                                                                                                                                                                                                            (function (param) {
                                                                                                                                                                                                                                                                                                var buffer = new ArrayBuffer(8);
                                                                                                                                                                                                                                                                                                var view = new DataView(buffer);
                                                                                                                                                                                                                                                                                                view.setFloat64(0, 1.0, 1);
                                                                                                                                                                                                                                                                                                return {
                                                                                                                                                                                                                                                                                                        tag: /* Eq */0,
                                                                                                                                                                                                                                                                                                        _0: 3.03865e-319,
                                                                                                                                                                                                                                                                                                        _1: view.getFloat64(0)
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

Mt.from_pair_suites("Js_typed_array_test", suites);

exports.mkI8 = mkI8;
exports.via = via;
exports.viaInt8 = viaInt8;
exports.x = x;
exports.suites = suites;
/* x Not a pure module */
