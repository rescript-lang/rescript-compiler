'use strict';

var Mt = require("./mt.js");
var Caml_obj = require("../../lib/js/caml_obj.js");

function date(param) {
  return new Date("1976-03-08T12:34:56.789+01:23");
}

var suites_0 = [
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
  _0: [
    "make",
    (function (param) {
        return {
                tag: /* Ok */4,
                _0: new Date().getTime() > 1487223505382
              };
      })
  ],
  _1: /* :: */{
    _0: [
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
      _0: [
        "parseAsFloat_invalid",
        (function (param) {
            return {
                    tag: /* Ok */4,
                    _0: isNaN(Date.parse("gibberish"))
                  };
          })
      ],
      _1: /* :: */{
        _0: [
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
          _0: [
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
            _0: [
              "fromString_invalid",
              (function (param) {
                  return {
                          tag: /* Ok */4,
                          _0: isNaN(new Date("gibberish").getTime())
                        };
                })
            ],
            _1: /* :: */{
              _0: [
                "makeWithYM",
                (function (param) {
                    var d = new Date(1984, 4);
                    return {
                            tag: /* Eq */0,
                            _0: [
                              1984,
                              4
                            ],
                            _1: [
                              d.getFullYear(),
                              d.getMonth()
                            ]
                          };
                  })
              ],
              _1: /* :: */{
                _0: [
                  "makeWithYMD",
                  (function (param) {
                      var d = new Date(1984, 4, 6);
                      return {
                              tag: /* Eq */0,
                              _0: [
                                1984,
                                4,
                                6
                              ],
                              _1: [
                                d.getFullYear(),
                                d.getMonth(),
                                d.getDate()
                              ]
                            };
                    })
                ],
                _1: /* :: */{
                  _0: [
                    "makeWithYMDH",
                    (function (param) {
                        var d = new Date(1984, 4, 6, 3);
                        return {
                                tag: /* Eq */0,
                                _0: [
                                  1984,
                                  4,
                                  6,
                                  3
                                ],
                                _1: [
                                  d.getFullYear(),
                                  d.getMonth(),
                                  d.getDate(),
                                  d.getHours()
                                ]
                              };
                      })
                  ],
                  _1: /* :: */{
                    _0: [
                      "makeWithYMDHM",
                      (function (param) {
                          var d = new Date(1984, 4, 6, 3, 59);
                          return {
                                  tag: /* Eq */0,
                                  _0: [
                                    1984,
                                    4,
                                    6,
                                    3,
                                    59
                                  ],
                                  _1: [
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
                      _0: [
                        "makeWithYMDHMS",
                        (function (param) {
                            var d = new Date(1984, 4, 6, 3, 59, 27);
                            return {
                                    tag: /* Eq */0,
                                    _0: [
                                      1984,
                                      4,
                                      6,
                                      3,
                                      59,
                                      27
                                    ],
                                    _1: [
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
                        _0: [
                          "utcWithYM",
                          (function (param) {
                              var d = Date.UTC(1984, 4);
                              var d$1 = new Date(d);
                              return {
                                      tag: /* Eq */0,
                                      _0: [
                                        1984,
                                        4
                                      ],
                                      _1: [
                                        d$1.getUTCFullYear(),
                                        d$1.getUTCMonth()
                                      ]
                                    };
                            })
                        ],
                        _1: /* :: */{
                          _0: [
                            "utcWithYMD",
                            (function (param) {
                                var d = Date.UTC(1984, 4, 6);
                                var d$1 = new Date(d);
                                return {
                                        tag: /* Eq */0,
                                        _0: [
                                          1984,
                                          4,
                                          6
                                        ],
                                        _1: [
                                          d$1.getUTCFullYear(),
                                          d$1.getUTCMonth(),
                                          d$1.getUTCDate()
                                        ]
                                      };
                              })
                          ],
                          _1: /* :: */{
                            _0: [
                              "utcWithYMDH",
                              (function (param) {
                                  var d = Date.UTC(1984, 4, 6, 3);
                                  var d$1 = new Date(d);
                                  return {
                                          tag: /* Eq */0,
                                          _0: [
                                            1984,
                                            4,
                                            6,
                                            3
                                          ],
                                          _1: [
                                            d$1.getUTCFullYear(),
                                            d$1.getUTCMonth(),
                                            d$1.getUTCDate(),
                                            d$1.getUTCHours()
                                          ]
                                        };
                                })
                            ],
                            _1: /* :: */{
                              _0: [
                                "utcWithYMDHM",
                                (function (param) {
                                    var d = Date.UTC(1984, 4, 6, 3, 59);
                                    var d$1 = new Date(d);
                                    return {
                                            tag: /* Eq */0,
                                            _0: [
                                              1984,
                                              4,
                                              6,
                                              3,
                                              59
                                            ],
                                            _1: [
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
                                _0: [
                                  "utcWithYMDHMS",
                                  (function (param) {
                                      var d = Date.UTC(1984, 4, 6, 3, 59, 27);
                                      var d$1 = new Date(d);
                                      return {
                                              tag: /* Eq */0,
                                              _0: [
                                                1984,
                                                4,
                                                6,
                                                3,
                                                59,
                                                27
                                              ],
                                              _1: [
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
                                  _0: [
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
                                    _0: [
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
                                      _0: [
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
                                        _0: [
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
                                          _0: [
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
                                            _0: [
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
                                              _0: [
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
                                                _0: [
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
                                                  _0: [
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
                                                    _0: [
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
                                                      _0: [
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
                                                        _0: [
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
                                                          _0: [
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
                                                            _0: [
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
                                                              _0: [
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
                                                                _0: [
                                                                  "setFullYearM",
                                                                  (function (param) {
                                                                      var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                      d.setFullYear(1986, 7);
                                                                      return {
                                                                              tag: /* Eq */0,
                                                                              _0: [
                                                                                1986,
                                                                                7
                                                                              ],
                                                                              _1: [
                                                                                d.getFullYear(),
                                                                                d.getMonth()
                                                                              ]
                                                                            };
                                                                    })
                                                                ],
                                                                _1: /* :: */{
                                                                  _0: [
                                                                    "setFullYearMD",
                                                                    (function (param) {
                                                                        var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                        d.setFullYear(1986, 7, 23);
                                                                        return {
                                                                                tag: /* Eq */0,
                                                                                _0: [
                                                                                  1986,
                                                                                  7,
                                                                                  23
                                                                                ],
                                                                                _1: [
                                                                                  d.getFullYear(),
                                                                                  d.getMonth(),
                                                                                  d.getDate()
                                                                                ]
                                                                              };
                                                                      })
                                                                  ],
                                                                  _1: /* :: */{
                                                                    _0: [
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
                                                                      _0: [
                                                                        "setHoursM",
                                                                        (function (param) {
                                                                            var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                            d.setHours(22, 48);
                                                                            return {
                                                                                    tag: /* Eq */0,
                                                                                    _0: [
                                                                                      22,
                                                                                      48
                                                                                    ],
                                                                                    _1: [
                                                                                      d.getHours(),
                                                                                      d.getMinutes()
                                                                                    ]
                                                                                  };
                                                                          })
                                                                      ],
                                                                      _1: /* :: */{
                                                                        _0: [
                                                                          "setHoursMS",
                                                                          (function (param) {
                                                                              var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                              d.setHours(22, 48, 54);
                                                                              return {
                                                                                      tag: /* Eq */0,
                                                                                      _0: [
                                                                                        22,
                                                                                        48,
                                                                                        54
                                                                                      ],
                                                                                      _1: [
                                                                                        d.getHours(),
                                                                                        d.getMinutes(),
                                                                                        d.getSeconds()
                                                                                      ]
                                                                                    };
                                                                            })
                                                                        ],
                                                                        _1: /* :: */{
                                                                          _0: [
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
                                                                            _0: [
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
                                                                              _0: [
                                                                                "setMinutesS",
                                                                                (function (param) {
                                                                                    var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                    d.setMinutes(18, 42);
                                                                                    return {
                                                                                            tag: /* Eq */0,
                                                                                            _0: [
                                                                                              18,
                                                                                              42
                                                                                            ],
                                                                                            _1: [
                                                                                              d.getMinutes(),
                                                                                              d.getSeconds()
                                                                                            ]
                                                                                          };
                                                                                  })
                                                                              ],
                                                                              _1: /* :: */{
                                                                                _0: [
                                                                                  "setMinutesSMs",
                                                                                  (function (param) {
                                                                                      var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                      d.setMinutes(18, 42, 311);
                                                                                      return {
                                                                                              tag: /* Eq */0,
                                                                                              _0: [
                                                                                                18,
                                                                                                42,
                                                                                                311
                                                                                              ],
                                                                                              _1: [
                                                                                                d.getMinutes(),
                                                                                                d.getSeconds(),
                                                                                                d.getMilliseconds()
                                                                                              ]
                                                                                            };
                                                                                    })
                                                                                ],
                                                                                _1: /* :: */{
                                                                                  _0: [
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
                                                                                    _0: [
                                                                                      "setMonthD",
                                                                                      (function (param) {
                                                                                          var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                          d.setMonth(10, 14);
                                                                                          return {
                                                                                                  tag: /* Eq */0,
                                                                                                  _0: [
                                                                                                    10,
                                                                                                    14
                                                                                                  ],
                                                                                                  _1: [
                                                                                                    d.getMonth(),
                                                                                                    d.getDate()
                                                                                                  ]
                                                                                                };
                                                                                        })
                                                                                    ],
                                                                                    _1: /* :: */{
                                                                                      _0: [
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
                                                                                        _0: [
                                                                                          "setSecondsMs",
                                                                                          (function (param) {
                                                                                              var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                              d.setSeconds(36, 420);
                                                                                              return {
                                                                                                      tag: /* Eq */0,
                                                                                                      _0: [
                                                                                                        36,
                                                                                                        420
                                                                                                      ],
                                                                                                      _1: [
                                                                                                        d.getSeconds(),
                                                                                                        d.getMilliseconds()
                                                                                                      ]
                                                                                                    };
                                                                                            })
                                                                                        ],
                                                                                        _1: /* :: */{
                                                                                          _0: [
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
                                                                                            _0: [
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
                                                                                              _0: [
                                                                                                "setUTCFullYearM",
                                                                                                (function (param) {
                                                                                                    var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                    d.setUTCFullYear(1986, 7);
                                                                                                    return {
                                                                                                            tag: /* Eq */0,
                                                                                                            _0: [
                                                                                                              1986,
                                                                                                              7
                                                                                                            ],
                                                                                                            _1: [
                                                                                                              d.getUTCFullYear(),
                                                                                                              d.getUTCMonth()
                                                                                                            ]
                                                                                                          };
                                                                                                  })
                                                                                              ],
                                                                                              _1: /* :: */{
                                                                                                _0: [
                                                                                                  "setUTCFullYearMD",
                                                                                                  (function (param) {
                                                                                                      var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                      d.setUTCFullYear(1986, 7, 23);
                                                                                                      return {
                                                                                                              tag: /* Eq */0,
                                                                                                              _0: [
                                                                                                                1986,
                                                                                                                7,
                                                                                                                23
                                                                                                              ],
                                                                                                              _1: [
                                                                                                                d.getUTCFullYear(),
                                                                                                                d.getUTCMonth(),
                                                                                                                d.getUTCDate()
                                                                                                              ]
                                                                                                            };
                                                                                                    })
                                                                                                ],
                                                                                                _1: /* :: */{
                                                                                                  _0: [
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
                                                                                                    _0: [
                                                                                                      "setUTCHoursM",
                                                                                                      (function (param) {
                                                                                                          var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                          d.setUTCHours(22, 48);
                                                                                                          return {
                                                                                                                  tag: /* Eq */0,
                                                                                                                  _0: [
                                                                                                                    22,
                                                                                                                    48
                                                                                                                  ],
                                                                                                                  _1: [
                                                                                                                    d.getUTCHours(),
                                                                                                                    d.getUTCMinutes()
                                                                                                                  ]
                                                                                                                };
                                                                                                        })
                                                                                                    ],
                                                                                                    _1: /* :: */{
                                                                                                      _0: [
                                                                                                        "setUTCHoursMS",
                                                                                                        (function (param) {
                                                                                                            var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                            d.setUTCHours(22, 48, 54);
                                                                                                            return {
                                                                                                                    tag: /* Eq */0,
                                                                                                                    _0: [
                                                                                                                      22,
                                                                                                                      48,
                                                                                                                      54
                                                                                                                    ],
                                                                                                                    _1: [
                                                                                                                      d.getUTCHours(),
                                                                                                                      d.getUTCMinutes(),
                                                                                                                      d.getUTCSeconds()
                                                                                                                    ]
                                                                                                                  };
                                                                                                          })
                                                                                                      ],
                                                                                                      _1: /* :: */{
                                                                                                        _0: [
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
                                                                                                          _0: [
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
                                                                                                            _0: [
                                                                                                              "setUTCMinutesS",
                                                                                                              (function (param) {
                                                                                                                  var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                  d.setUTCMinutes(18, 42);
                                                                                                                  return {
                                                                                                                          tag: /* Eq */0,
                                                                                                                          _0: [
                                                                                                                            18,
                                                                                                                            42
                                                                                                                          ],
                                                                                                                          _1: [
                                                                                                                            d.getUTCMinutes(),
                                                                                                                            d.getUTCSeconds()
                                                                                                                          ]
                                                                                                                        };
                                                                                                                })
                                                                                                            ],
                                                                                                            _1: /* :: */{
                                                                                                              _0: [
                                                                                                                "setUTCMinutesSMs",
                                                                                                                (function (param) {
                                                                                                                    var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                    d.setUTCMinutes(18, 42, 311);
                                                                                                                    return {
                                                                                                                            tag: /* Eq */0,
                                                                                                                            _0: [
                                                                                                                              18,
                                                                                                                              42,
                                                                                                                              311
                                                                                                                            ],
                                                                                                                            _1: [
                                                                                                                              d.getUTCMinutes(),
                                                                                                                              d.getUTCSeconds(),
                                                                                                                              d.getUTCMilliseconds()
                                                                                                                            ]
                                                                                                                          };
                                                                                                                  })
                                                                                                              ],
                                                                                                              _1: /* :: */{
                                                                                                                _0: [
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
                                                                                                                  _0: [
                                                                                                                    "setUTCMonthD",
                                                                                                                    (function (param) {
                                                                                                                        var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                        d.setUTCMonth(10, 14);
                                                                                                                        return {
                                                                                                                                tag: /* Eq */0,
                                                                                                                                _0: [
                                                                                                                                  10,
                                                                                                                                  14
                                                                                                                                ],
                                                                                                                                _1: [
                                                                                                                                  d.getUTCMonth(),
                                                                                                                                  d.getUTCDate()
                                                                                                                                ]
                                                                                                                              };
                                                                                                                      })
                                                                                                                  ],
                                                                                                                  _1: /* :: */{
                                                                                                                    _0: [
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
                                                                                                                      _0: [
                                                                                                                        "setUTCSecondsMs",
                                                                                                                        (function (param) {
                                                                                                                            var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                            d.setUTCSeconds(36, 420);
                                                                                                                            return {
                                                                                                                                    tag: /* Eq */0,
                                                                                                                                    _0: [
                                                                                                                                      36,
                                                                                                                                      420
                                                                                                                                    ],
                                                                                                                                    _1: [
                                                                                                                                      d.getUTCSeconds(),
                                                                                                                                      d.getUTCMilliseconds()
                                                                                                                                    ]
                                                                                                                                  };
                                                                                                                          })
                                                                                                                      ],
                                                                                                                      _1: /* :: */{
                                                                                                                        _0: [
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
                                                                                                                          _0: [
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
                                                                                                                            _0: [
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
                                                                                                                              _0: [
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
                                                                                                                                _0: [
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
                                                                                                                                  _0: [
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
                                                                                                                                    _0: [
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
