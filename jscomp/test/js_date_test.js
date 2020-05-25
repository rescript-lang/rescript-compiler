'use strict';

var Mt = require("./mt.js");
var Caml_obj = require("../../lib/js/caml_obj.js");

function date(param) {
  return new Date("1976-03-08T12:34:56.789+01:23");
}

var suites_0 = /* tuple */[
  "valueOf",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: 195131516789,
              _1: new Date("1976-03-08T12:34:56.789+01:23").valueOf()
            };
    })
];

var suites_1 = /* :: */{
  _0: /* tuple */[
    "make",
    (function (param) {
        return {
                tag: /* Ok */4,
                _0: new Date().getTime() > 1487223505382
              };
      })
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "parseAsFloat",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: Date.parse("1976-03-08T12:34:56.789+01:23"),
                  _1: 195131516789
                };
        })
    ],
    _1: /* :: */{
      _0: /* tuple */[
        "parseAsFloat_invalid",
        (function (param) {
            return {
                    tag: /* Ok */4,
                    _0: isNaN(Date.parse("gibberish"))
                  };
          })
      ],
      _1: /* :: */{
        _0: /* tuple */[
          "fromFloat",
          (function (param) {
              return {
                      tag: /* Eq */0,
                      _0: "1976-03-08T11:11:56.789Z",
                      _1: new Date(195131516789).toISOString()
                    };
            })
        ],
        _1: /* :: */{
          _0: /* tuple */[
            "fromString_valid",
            (function (param) {
                return {
                        tag: /* Eq */0,
                        _0: 195131516789,
                        _1: new Date("1976-03-08T12:34:56.789+01:23").getTime()
                      };
              })
          ],
          _1: /* :: */{
            _0: /* tuple */[
              "fromString_invalid",
              (function (param) {
                  return {
                          tag: /* Ok */4,
                          _0: isNaN(new Date("gibberish").getTime())
                        };
                })
            ],
            _1: /* :: */{
              _0: /* tuple */[
                "makeWithYM",
                (function (param) {
                    var d = new Date(1984, 4);
                    return {
                            tag: /* Eq */0,
                            _0: /* tuple */[
                              1984,
                              4
                            ],
                            _1: /* tuple */[
                              d.getFullYear(),
                              d.getMonth()
                            ]
                          };
                  })
              ],
              _1: /* :: */{
                _0: /* tuple */[
                  "makeWithYMD",
                  (function (param) {
                      var d = new Date(1984, 4, 6);
                      return {
                              tag: /* Eq */0,
                              _0: /* tuple */[
                                1984,
                                4,
                                6
                              ],
                              _1: /* tuple */[
                                d.getFullYear(),
                                d.getMonth(),
                                d.getDate()
                              ]
                            };
                    })
                ],
                _1: /* :: */{
                  _0: /* tuple */[
                    "makeWithYMDH",
                    (function (param) {
                        var d = new Date(1984, 4, 6, 3);
                        return {
                                tag: /* Eq */0,
                                _0: /* tuple */[
                                  1984,
                                  4,
                                  6,
                                  3
                                ],
                                _1: /* tuple */[
                                  d.getFullYear(),
                                  d.getMonth(),
                                  d.getDate(),
                                  d.getHours()
                                ]
                              };
                      })
                  ],
                  _1: /* :: */{
                    _0: /* tuple */[
                      "makeWithYMDHM",
                      (function (param) {
                          var d = new Date(1984, 4, 6, 3, 59);
                          return {
                                  tag: /* Eq */0,
                                  _0: /* tuple */[
                                    1984,
                                    4,
                                    6,
                                    3,
                                    59
                                  ],
                                  _1: /* tuple */[
                                    d.getFullYear(),
                                    d.getMonth(),
                                    d.getDate(),
                                    d.getHours(),
                                    d.getMinutes()
                                  ]
                                };
                        })
                    ],
                    _1: /* :: */{
                      _0: /* tuple */[
                        "makeWithYMDHMS",
                        (function (param) {
                            var d = new Date(1984, 4, 6, 3, 59, 27);
                            return {
                                    tag: /* Eq */0,
                                    _0: /* tuple */[
                                      1984,
                                      4,
                                      6,
                                      3,
                                      59,
                                      27
                                    ],
                                    _1: /* tuple */[
                                      d.getFullYear(),
                                      d.getMonth(),
                                      d.getDate(),
                                      d.getHours(),
                                      d.getMinutes(),
                                      d.getSeconds()
                                    ]
                                  };
                          })
                      ],
                      _1: /* :: */{
                        _0: /* tuple */[
                          "utcWithYM",
                          (function (param) {
                              var d = Date.UTC(1984, 4);
                              var d$1 = new Date(d);
                              return {
                                      tag: /* Eq */0,
                                      _0: /* tuple */[
                                        1984,
                                        4
                                      ],
                                      _1: /* tuple */[
                                        d$1.getUTCFullYear(),
                                        d$1.getUTCMonth()
                                      ]
                                    };
                            })
                        ],
                        _1: /* :: */{
                          _0: /* tuple */[
                            "utcWithYMD",
                            (function (param) {
                                var d = Date.UTC(1984, 4, 6);
                                var d$1 = new Date(d);
                                return {
                                        tag: /* Eq */0,
                                        _0: /* tuple */[
                                          1984,
                                          4,
                                          6
                                        ],
                                        _1: /* tuple */[
                                          d$1.getUTCFullYear(),
                                          d$1.getUTCMonth(),
                                          d$1.getUTCDate()
                                        ]
                                      };
                              })
                          ],
                          _1: /* :: */{
                            _0: /* tuple */[
                              "utcWithYMDH",
                              (function (param) {
                                  var d = Date.UTC(1984, 4, 6, 3);
                                  var d$1 = new Date(d);
                                  return {
                                          tag: /* Eq */0,
                                          _0: /* tuple */[
                                            1984,
                                            4,
                                            6,
                                            3
                                          ],
                                          _1: /* tuple */[
                                            d$1.getUTCFullYear(),
                                            d$1.getUTCMonth(),
                                            d$1.getUTCDate(),
                                            d$1.getUTCHours()
                                          ]
                                        };
                                })
                            ],
                            _1: /* :: */{
                              _0: /* tuple */[
                                "utcWithYMDHM",
                                (function (param) {
                                    var d = Date.UTC(1984, 4, 6, 3, 59);
                                    var d$1 = new Date(d);
                                    return {
                                            tag: /* Eq */0,
                                            _0: /* tuple */[
                                              1984,
                                              4,
                                              6,
                                              3,
                                              59
                                            ],
                                            _1: /* tuple */[
                                              d$1.getUTCFullYear(),
                                              d$1.getUTCMonth(),
                                              d$1.getUTCDate(),
                                              d$1.getUTCHours(),
                                              d$1.getUTCMinutes()
                                            ]
                                          };
                                  })
                              ],
                              _1: /* :: */{
                                _0: /* tuple */[
                                  "utcWithYMDHMS",
                                  (function (param) {
                                      var d = Date.UTC(1984, 4, 6, 3, 59, 27);
                                      var d$1 = new Date(d);
                                      return {
                                              tag: /* Eq */0,
                                              _0: /* tuple */[
                                                1984,
                                                4,
                                                6,
                                                3,
                                                59,
                                                27
                                              ],
                                              _1: /* tuple */[
                                                d$1.getUTCFullYear(),
                                                d$1.getUTCMonth(),
                                                d$1.getUTCDate(),
                                                d$1.getUTCHours(),
                                                d$1.getUTCMinutes(),
                                                d$1.getUTCSeconds()
                                              ]
                                            };
                                    })
                                ],
                                _1: /* :: */{
                                  _0: /* tuple */[
                                    "getFullYear",
                                    (function (param) {
                                        return {
                                                tag: /* Eq */0,
                                                _0: 1976,
                                                _1: new Date("1976-03-08T12:34:56.789+01:23").getFullYear()
                                              };
                                      })
                                  ],
                                  _1: /* :: */{
                                    _0: /* tuple */[
                                      "getMilliseconds",
                                      (function (param) {
                                          return {
                                                  tag: /* Eq */0,
                                                  _0: 789,
                                                  _1: new Date("1976-03-08T12:34:56.789+01:23").getMilliseconds()
                                                };
                                        })
                                    ],
                                    _1: /* :: */{
                                      _0: /* tuple */[
                                        "getSeconds",
                                        (function (param) {
                                            return {
                                                    tag: /* Eq */0,
                                                    _0: 56,
                                                    _1: new Date("1976-03-08T12:34:56.789+01:23").getSeconds()
                                                  };
                                          })
                                      ],
                                      _1: /* :: */{
                                        _0: /* tuple */[
                                          "getTime",
                                          (function (param) {
                                              return {
                                                      tag: /* Eq */0,
                                                      _0: 195131516789,
                                                      _1: new Date("1976-03-08T12:34:56.789+01:23").getTime()
                                                    };
                                            })
                                        ],
                                        _1: /* :: */{
                                          _0: /* tuple */[
                                            "getUTCDate",
                                            (function (param) {
                                                return {
                                                        tag: /* Eq */0,
                                                        _0: 8,
                                                        _1: new Date("1976-03-08T12:34:56.789+01:23").getUTCDate()
                                                      };
                                              })
                                          ],
                                          _1: /* :: */{
                                            _0: /* tuple */[
                                              "getUTCDay",
                                              (function (param) {
                                                  return {
                                                          tag: /* Eq */0,
                                                          _0: 1,
                                                          _1: new Date("1976-03-08T12:34:56.789+01:23").getUTCDay()
                                                        };
                                                })
                                            ],
                                            _1: /* :: */{
                                              _0: /* tuple */[
                                                "getUTCFUllYear",
                                                (function (param) {
                                                    return {
                                                            tag: /* Eq */0,
                                                            _0: 1976,
                                                            _1: new Date("1976-03-08T12:34:56.789+01:23").getUTCFullYear()
                                                          };
                                                  })
                                              ],
                                              _1: /* :: */{
                                                _0: /* tuple */[
                                                  "getUTCHours",
                                                  (function (param) {
                                                      return {
                                                              tag: /* Eq */0,
                                                              _0: 11,
                                                              _1: new Date("1976-03-08T12:34:56.789+01:23").getUTCHours()
                                                            };
                                                    })
                                                ],
                                                _1: /* :: */{
                                                  _0: /* tuple */[
                                                    "getUTCMilliseconds",
                                                    (function (param) {
                                                        return {
                                                                tag: /* Eq */0,
                                                                _0: 789,
                                                                _1: new Date("1976-03-08T12:34:56.789+01:23").getUTCMilliseconds()
                                                              };
                                                      })
                                                  ],
                                                  _1: /* :: */{
                                                    _0: /* tuple */[
                                                      "getUTCMinutes",
                                                      (function (param) {
                                                          return {
                                                                  tag: /* Eq */0,
                                                                  _0: 11,
                                                                  _1: new Date("1976-03-08T12:34:56.789+01:23").getUTCMinutes()
                                                                };
                                                        })
                                                    ],
                                                    _1: /* :: */{
                                                      _0: /* tuple */[
                                                        "getUTCMonth",
                                                        (function (param) {
                                                            return {
                                                                    tag: /* Eq */0,
                                                                    _0: 2,
                                                                    _1: new Date("1976-03-08T12:34:56.789+01:23").getUTCMonth()
                                                                  };
                                                          })
                                                      ],
                                                      _1: /* :: */{
                                                        _0: /* tuple */[
                                                          "getUTCSeconds",
                                                          (function (param) {
                                                              return {
                                                                      tag: /* Eq */0,
                                                                      _0: 56,
                                                                      _1: new Date("1976-03-08T12:34:56.789+01:23").getUTCSeconds()
                                                                    };
                                                            })
                                                        ],
                                                        _1: /* :: */{
                                                          _0: /* tuple */[
                                                            "getYear",
                                                            (function (param) {
                                                                return {
                                                                        tag: /* Eq */0,
                                                                        _0: 1976,
                                                                        _1: new Date("1976-03-08T12:34:56.789+01:23").getFullYear()
                                                                      };
                                                              })
                                                          ],
                                                          _1: /* :: */{
                                                            _0: /* tuple */[
                                                              "setDate",
                                                              (function (param) {
                                                                  var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                  d.setDate(12);
                                                                  return {
                                                                          tag: /* Eq */0,
                                                                          _0: 12,
                                                                          _1: d.getDate()
                                                                        };
                                                                })
                                                            ],
                                                            _1: /* :: */{
                                                              _0: /* tuple */[
                                                                "setFullYear",
                                                                (function (param) {
                                                                    var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                    d.setFullYear(1986);
                                                                    return {
                                                                            tag: /* Eq */0,
                                                                            _0: 1986,
                                                                            _1: d.getFullYear()
                                                                          };
                                                                  })
                                                              ],
                                                              _1: /* :: */{
                                                                _0: /* tuple */[
                                                                  "setFullYearM",
                                                                  (function (param) {
                                                                      var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                      d.setFullYear(1986, 7);
                                                                      return {
                                                                              tag: /* Eq */0,
                                                                              _0: /* tuple */[
                                                                                1986,
                                                                                7
                                                                              ],
                                                                              _1: /* tuple */[
                                                                                d.getFullYear(),
                                                                                d.getMonth()
                                                                              ]
                                                                            };
                                                                    })
                                                                ],
                                                                _1: /* :: */{
                                                                  _0: /* tuple */[
                                                                    "setFullYearMD",
                                                                    (function (param) {
                                                                        var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                        d.setFullYear(1986, 7, 23);
                                                                        return {
                                                                                tag: /* Eq */0,
                                                                                _0: /* tuple */[
                                                                                  1986,
                                                                                  7,
                                                                                  23
                                                                                ],
                                                                                _1: /* tuple */[
                                                                                  d.getFullYear(),
                                                                                  d.getMonth(),
                                                                                  d.getDate()
                                                                                ]
                                                                              };
                                                                      })
                                                                  ],
                                                                  _1: /* :: */{
                                                                    _0: /* tuple */[
                                                                      "setHours",
                                                                      (function (param) {
                                                                          var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                          d.setHours(22);
                                                                          return {
                                                                                  tag: /* Eq */0,
                                                                                  _0: 22,
                                                                                  _1: d.getHours()
                                                                                };
                                                                        })
                                                                    ],
                                                                    _1: /* :: */{
                                                                      _0: /* tuple */[
                                                                        "setHoursM",
                                                                        (function (param) {
                                                                            var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                            d.setHours(22, 48);
                                                                            return {
                                                                                    tag: /* Eq */0,
                                                                                    _0: /* tuple */[
                                                                                      22,
                                                                                      48
                                                                                    ],
                                                                                    _1: /* tuple */[
                                                                                      d.getHours(),
                                                                                      d.getMinutes()
                                                                                    ]
                                                                                  };
                                                                          })
                                                                      ],
                                                                      _1: /* :: */{
                                                                        _0: /* tuple */[
                                                                          "setHoursMS",
                                                                          (function (param) {
                                                                              var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                              d.setHours(22, 48, 54);
                                                                              return {
                                                                                      tag: /* Eq */0,
                                                                                      _0: /* tuple */[
                                                                                        22,
                                                                                        48,
                                                                                        54
                                                                                      ],
                                                                                      _1: /* tuple */[
                                                                                        d.getHours(),
                                                                                        d.getMinutes(),
                                                                                        d.getSeconds()
                                                                                      ]
                                                                                    };
                                                                            })
                                                                        ],
                                                                        _1: /* :: */{
                                                                          _0: /* tuple */[
                                                                            "setMilliseconds",
                                                                            (function (param) {
                                                                                var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                d.setMilliseconds(543);
                                                                                return {
                                                                                        tag: /* Eq */0,
                                                                                        _0: 543,
                                                                                        _1: d.getMilliseconds()
                                                                                      };
                                                                              })
                                                                          ],
                                                                          _1: /* :: */{
                                                                            _0: /* tuple */[
                                                                              "setMinutes",
                                                                              (function (param) {
                                                                                  var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                  d.setMinutes(18);
                                                                                  return {
                                                                                          tag: /* Eq */0,
                                                                                          _0: 18,
                                                                                          _1: d.getMinutes()
                                                                                        };
                                                                                })
                                                                            ],
                                                                            _1: /* :: */{
                                                                              _0: /* tuple */[
                                                                                "setMinutesS",
                                                                                (function (param) {
                                                                                    var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                    d.setMinutes(18, 42);
                                                                                    return {
                                                                                            tag: /* Eq */0,
                                                                                            _0: /* tuple */[
                                                                                              18,
                                                                                              42
                                                                                            ],
                                                                                            _1: /* tuple */[
                                                                                              d.getMinutes(),
                                                                                              d.getSeconds()
                                                                                            ]
                                                                                          };
                                                                                  })
                                                                              ],
                                                                              _1: /* :: */{
                                                                                _0: /* tuple */[
                                                                                  "setMinutesSMs",
                                                                                  (function (param) {
                                                                                      var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                      d.setMinutes(18, 42, 311);
                                                                                      return {
                                                                                              tag: /* Eq */0,
                                                                                              _0: /* tuple */[
                                                                                                18,
                                                                                                42,
                                                                                                311
                                                                                              ],
                                                                                              _1: /* tuple */[
                                                                                                d.getMinutes(),
                                                                                                d.getSeconds(),
                                                                                                d.getMilliseconds()
                                                                                              ]
                                                                                            };
                                                                                    })
                                                                                ],
                                                                                _1: /* :: */{
                                                                                  _0: /* tuple */[
                                                                                    "setMonth",
                                                                                    (function (param) {
                                                                                        var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                        d.setMonth(10);
                                                                                        return {
                                                                                                tag: /* Eq */0,
                                                                                                _0: 10,
                                                                                                _1: d.getMonth()
                                                                                              };
                                                                                      })
                                                                                  ],
                                                                                  _1: /* :: */{
                                                                                    _0: /* tuple */[
                                                                                      "setMonthD",
                                                                                      (function (param) {
                                                                                          var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                          d.setMonth(10, 14);
                                                                                          return {
                                                                                                  tag: /* Eq */0,
                                                                                                  _0: /* tuple */[
                                                                                                    10,
                                                                                                    14
                                                                                                  ],
                                                                                                  _1: /* tuple */[
                                                                                                    d.getMonth(),
                                                                                                    d.getDate()
                                                                                                  ]
                                                                                                };
                                                                                        })
                                                                                    ],
                                                                                    _1: /* :: */{
                                                                                      _0: /* tuple */[
                                                                                        "setSeconds",
                                                                                        (function (param) {
                                                                                            var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                            d.setSeconds(36);
                                                                                            return {
                                                                                                    tag: /* Eq */0,
                                                                                                    _0: 36,
                                                                                                    _1: d.getSeconds()
                                                                                                  };
                                                                                          })
                                                                                      ],
                                                                                      _1: /* :: */{
                                                                                        _0: /* tuple */[
                                                                                          "setSecondsMs",
                                                                                          (function (param) {
                                                                                              var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                              d.setSeconds(36, 420);
                                                                                              return {
                                                                                                      tag: /* Eq */0,
                                                                                                      _0: /* tuple */[
                                                                                                        36,
                                                                                                        420
                                                                                                      ],
                                                                                                      _1: /* tuple */[
                                                                                                        d.getSeconds(),
                                                                                                        d.getMilliseconds()
                                                                                                      ]
                                                                                                    };
                                                                                            })
                                                                                        ],
                                                                                        _1: /* :: */{
                                                                                          _0: /* tuple */[
                                                                                            "setUTCDate",
                                                                                            (function (param) {
                                                                                                var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                d.setUTCDate(12);
                                                                                                return {
                                                                                                        tag: /* Eq */0,
                                                                                                        _0: 12,
                                                                                                        _1: d.getUTCDate()
                                                                                                      };
                                                                                              })
                                                                                          ],
                                                                                          _1: /* :: */{
                                                                                            _0: /* tuple */[
                                                                                              "setUTCFullYear",
                                                                                              (function (param) {
                                                                                                  var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                  d.setUTCFullYear(1986);
                                                                                                  return {
                                                                                                          tag: /* Eq */0,
                                                                                                          _0: 1986,
                                                                                                          _1: d.getUTCFullYear()
                                                                                                        };
                                                                                                })
                                                                                            ],
                                                                                            _1: /* :: */{
                                                                                              _0: /* tuple */[
                                                                                                "setUTCFullYearM",
                                                                                                (function (param) {
                                                                                                    var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                    d.setUTCFullYear(1986, 7);
                                                                                                    return {
                                                                                                            tag: /* Eq */0,
                                                                                                            _0: /* tuple */[
                                                                                                              1986,
                                                                                                              7
                                                                                                            ],
                                                                                                            _1: /* tuple */[
                                                                                                              d.getUTCFullYear(),
                                                                                                              d.getUTCMonth()
                                                                                                            ]
                                                                                                          };
                                                                                                  })
                                                                                              ],
                                                                                              _1: /* :: */{
                                                                                                _0: /* tuple */[
                                                                                                  "setUTCFullYearMD",
                                                                                                  (function (param) {
                                                                                                      var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                      d.setUTCFullYear(1986, 7, 23);
                                                                                                      return {
                                                                                                              tag: /* Eq */0,
                                                                                                              _0: /* tuple */[
                                                                                                                1986,
                                                                                                                7,
                                                                                                                23
                                                                                                              ],
                                                                                                              _1: /* tuple */[
                                                                                                                d.getUTCFullYear(),
                                                                                                                d.getUTCMonth(),
                                                                                                                d.getUTCDate()
                                                                                                              ]
                                                                                                            };
                                                                                                    })
                                                                                                ],
                                                                                                _1: /* :: */{
                                                                                                  _0: /* tuple */[
                                                                                                    "setUTCHours",
                                                                                                    (function (param) {
                                                                                                        var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                        d.setUTCHours(22);
                                                                                                        return {
                                                                                                                tag: /* Eq */0,
                                                                                                                _0: 22,
                                                                                                                _1: d.getUTCHours()
                                                                                                              };
                                                                                                      })
                                                                                                  ],
                                                                                                  _1: /* :: */{
                                                                                                    _0: /* tuple */[
                                                                                                      "setUTCHoursM",
                                                                                                      (function (param) {
                                                                                                          var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                          d.setUTCHours(22, 48);
                                                                                                          return {
                                                                                                                  tag: /* Eq */0,
                                                                                                                  _0: /* tuple */[
                                                                                                                    22,
                                                                                                                    48
                                                                                                                  ],
                                                                                                                  _1: /* tuple */[
                                                                                                                    d.getUTCHours(),
                                                                                                                    d.getUTCMinutes()
                                                                                                                  ]
                                                                                                                };
                                                                                                        })
                                                                                                    ],
                                                                                                    _1: /* :: */{
                                                                                                      _0: /* tuple */[
                                                                                                        "setUTCHoursMS",
                                                                                                        (function (param) {
                                                                                                            var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                            d.setUTCHours(22, 48, 54);
                                                                                                            return {
                                                                                                                    tag: /* Eq */0,
                                                                                                                    _0: /* tuple */[
                                                                                                                      22,
                                                                                                                      48,
                                                                                                                      54
                                                                                                                    ],
                                                                                                                    _1: /* tuple */[
                                                                                                                      d.getUTCHours(),
                                                                                                                      d.getUTCMinutes(),
                                                                                                                      d.getUTCSeconds()
                                                                                                                    ]
                                                                                                                  };
                                                                                                          })
                                                                                                      ],
                                                                                                      _1: /* :: */{
                                                                                                        _0: /* tuple */[
                                                                                                          "setUTCMilliseconds",
                                                                                                          (function (param) {
                                                                                                              var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                              d.setUTCMilliseconds(543);
                                                                                                              return {
                                                                                                                      tag: /* Eq */0,
                                                                                                                      _0: 543,
                                                                                                                      _1: d.getUTCMilliseconds()
                                                                                                                    };
                                                                                                            })
                                                                                                        ],
                                                                                                        _1: /* :: */{
                                                                                                          _0: /* tuple */[
                                                                                                            "setUTCMinutes",
                                                                                                            (function (param) {
                                                                                                                var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                d.setUTCMinutes(18);
                                                                                                                return {
                                                                                                                        tag: /* Eq */0,
                                                                                                                        _0: 18,
                                                                                                                        _1: d.getUTCMinutes()
                                                                                                                      };
                                                                                                              })
                                                                                                          ],
                                                                                                          _1: /* :: */{
                                                                                                            _0: /* tuple */[
                                                                                                              "setUTCMinutesS",
                                                                                                              (function (param) {
                                                                                                                  var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                  d.setUTCMinutes(18, 42);
                                                                                                                  return {
                                                                                                                          tag: /* Eq */0,
                                                                                                                          _0: /* tuple */[
                                                                                                                            18,
                                                                                                                            42
                                                                                                                          ],
                                                                                                                          _1: /* tuple */[
                                                                                                                            d.getUTCMinutes(),
                                                                                                                            d.getUTCSeconds()
                                                                                                                          ]
                                                                                                                        };
                                                                                                                })
                                                                                                            ],
                                                                                                            _1: /* :: */{
                                                                                                              _0: /* tuple */[
                                                                                                                "setUTCMinutesSMs",
                                                                                                                (function (param) {
                                                                                                                    var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                    d.setUTCMinutes(18, 42, 311);
                                                                                                                    return {
                                                                                                                            tag: /* Eq */0,
                                                                                                                            _0: /* tuple */[
                                                                                                                              18,
                                                                                                                              42,
                                                                                                                              311
                                                                                                                            ],
                                                                                                                            _1: /* tuple */[
                                                                                                                              d.getUTCMinutes(),
                                                                                                                              d.getUTCSeconds(),
                                                                                                                              d.getUTCMilliseconds()
                                                                                                                            ]
                                                                                                                          };
                                                                                                                  })
                                                                                                              ],
                                                                                                              _1: /* :: */{
                                                                                                                _0: /* tuple */[
                                                                                                                  "setUTCMonth",
                                                                                                                  (function (param) {
                                                                                                                      var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                      d.setUTCMonth(10);
                                                                                                                      return {
                                                                                                                              tag: /* Eq */0,
                                                                                                                              _0: 10,
                                                                                                                              _1: d.getUTCMonth()
                                                                                                                            };
                                                                                                                    })
                                                                                                                ],
                                                                                                                _1: /* :: */{
                                                                                                                  _0: /* tuple */[
                                                                                                                    "setUTCMonthD",
                                                                                                                    (function (param) {
                                                                                                                        var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                        d.setUTCMonth(10, 14);
                                                                                                                        return {
                                                                                                                                tag: /* Eq */0,
                                                                                                                                _0: /* tuple */[
                                                                                                                                  10,
                                                                                                                                  14
                                                                                                                                ],
                                                                                                                                _1: /* tuple */[
                                                                                                                                  d.getUTCMonth(),
                                                                                                                                  d.getUTCDate()
                                                                                                                                ]
                                                                                                                              };
                                                                                                                      })
                                                                                                                  ],
                                                                                                                  _1: /* :: */{
                                                                                                                    _0: /* tuple */[
                                                                                                                      "setUTCSeconds",
                                                                                                                      (function (param) {
                                                                                                                          var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                          d.setUTCSeconds(36);
                                                                                                                          return {
                                                                                                                                  tag: /* Eq */0,
                                                                                                                                  _0: 36,
                                                                                                                                  _1: d.getUTCSeconds()
                                                                                                                                };
                                                                                                                        })
                                                                                                                    ],
                                                                                                                    _1: /* :: */{
                                                                                                                      _0: /* tuple */[
                                                                                                                        "setUTCSecondsMs",
                                                                                                                        (function (param) {
                                                                                                                            var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                            d.setUTCSeconds(36, 420);
                                                                                                                            return {
                                                                                                                                    tag: /* Eq */0,
                                                                                                                                    _0: /* tuple */[
                                                                                                                                      36,
                                                                                                                                      420
                                                                                                                                    ],
                                                                                                                                    _1: /* tuple */[
                                                                                                                                      d.getUTCSeconds(),
                                                                                                                                      d.getUTCMilliseconds()
                                                                                                                                    ]
                                                                                                                                  };
                                                                                                                          })
                                                                                                                      ],
                                                                                                                      _1: /* :: */{
                                                                                                                        _0: /* tuple */[
                                                                                                                          "toDateString",
                                                                                                                          (function (param) {
                                                                                                                              return {
                                                                                                                                      tag: /* Eq */0,
                                                                                                                                      _0: "Mon Mar 08 1976",
                                                                                                                                      _1: new Date("1976-03-08T12:34:56.789+01:23").toDateString()
                                                                                                                                    };
                                                                                                                            })
                                                                                                                        ],
                                                                                                                        _1: /* :: */{
                                                                                                                          _0: /* tuple */[
                                                                                                                            "toGMTString",
                                                                                                                            (function (param) {
                                                                                                                                return {
                                                                                                                                        tag: /* Eq */0,
                                                                                                                                        _0: "Mon, 08 Mar 1976 11:11:56 GMT",
                                                                                                                                        _1: new Date("1976-03-08T12:34:56.789+01:23").toUTCString()
                                                                                                                                      };
                                                                                                                              })
                                                                                                                          ],
                                                                                                                          _1: /* :: */{
                                                                                                                            _0: /* tuple */[
                                                                                                                              "toISOString",
                                                                                                                              (function (param) {
                                                                                                                                  return {
                                                                                                                                          tag: /* Eq */0,
                                                                                                                                          _0: "1976-03-08T11:11:56.789Z",
                                                                                                                                          _1: new Date("1976-03-08T12:34:56.789+01:23").toISOString()
                                                                                                                                        };
                                                                                                                                })
                                                                                                                            ],
                                                                                                                            _1: /* :: */{
                                                                                                                              _0: /* tuple */[
                                                                                                                                "toJSON",
                                                                                                                                (function (param) {
                                                                                                                                    return {
                                                                                                                                            tag: /* Eq */0,
                                                                                                                                            _0: "1976-03-08T11:11:56.789Z",
                                                                                                                                            _1: new Date("1976-03-08T12:34:56.789+01:23").toJSON()
                                                                                                                                          };
                                                                                                                                  })
                                                                                                                              ],
                                                                                                                              _1: /* :: */{
                                                                                                                                _0: /* tuple */[
                                                                                                                                  "toJSONUnsafe",
                                                                                                                                  (function (param) {
                                                                                                                                      return {
                                                                                                                                              tag: /* Eq */0,
                                                                                                                                              _0: "1976-03-08T11:11:56.789Z",
                                                                                                                                              _1: new Date("1976-03-08T12:34:56.789+01:23").toJSON()
                                                                                                                                            };
                                                                                                                                    })
                                                                                                                                ],
                                                                                                                                _1: /* :: */{
                                                                                                                                  _0: /* tuple */[
                                                                                                                                    "toUTCString",
                                                                                                                                    (function (param) {
                                                                                                                                        return {
                                                                                                                                                tag: /* Eq */0,
                                                                                                                                                _0: "Mon, 08 Mar 1976 11:11:56 GMT",
                                                                                                                                                _1: new Date("1976-03-08T12:34:56.789+01:23").toUTCString()
                                                                                                                                              };
                                                                                                                                      })
                                                                                                                                  ],
                                                                                                                                  _1: /* :: */{
                                                                                                                                    _0: /* tuple */[
                                                                                                                                      "eq",
                                                                                                                                      (function (param) {
                                                                                                                                          var a = new Date("2013-03-01T01:10:00");
                                                                                                                                          var b = new Date("2013-03-01T01:10:00");
                                                                                                                                          var c = new Date("2013-03-01T01:10:01");
                                                                                                                                          return {
                                                                                                                                                  tag: /* Ok */4,
                                                                                                                                                  _0: Caml_obj.caml_equal(a, b) && Caml_obj.caml_notequal(b, c) && Caml_obj.caml_greaterthan(c, b)
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
};

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("Js_date_test", suites);

var N;

exports.N = N;
exports.date = date;
exports.suites = suites;
/*  Not a pure module */
