'use strict';

var Mt = require("./mt.js");
var Caml_obj = require("../../lib/js/caml_obj.js");

function date(param) {
  return new Date("1976-03-08T12:34:56.789+01:23");
}

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "valueOf",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: 195131516789,
                Arg1: new Date("1976-03-08T12:34:56.789+01:23").valueOf()
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "make",
      (function (param) {
          return /* constructor */{
                  tag: "Ok",
                  Arg0: new Date().getTime() > 1487223505382
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "parseAsFloat",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: Date.parse("1976-03-08T12:34:56.789+01:23"),
                    Arg1: 195131516789
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "parseAsFloat_invalid",
          (function (param) {
              return /* constructor */{
                      tag: "Ok",
                      Arg0: isNaN(Date.parse("gibberish"))
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "fromFloat",
            (function (param) {
                return /* constructor */{
                        tag: "Eq",
                        Arg0: "1976-03-08T11:11:56.789Z",
                        Arg1: new Date(195131516789).toISOString()
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "fromString_valid",
              (function (param) {
                  return /* constructor */{
                          tag: "Eq",
                          Arg0: 195131516789,
                          Arg1: new Date("1976-03-08T12:34:56.789+01:23").getTime()
                        };
                })
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "fromString_invalid",
                (function (param) {
                    return /* constructor */{
                            tag: "Ok",
                            Arg0: isNaN(new Date("gibberish").getTime())
                          };
                  })
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "makeWithYM",
                  (function (param) {
                      var d = new Date(1984, 4);
                      return /* constructor */{
                              tag: "Eq",
                              Arg0: /* tuple */[
                                1984,
                                4
                              ],
                              Arg1: /* tuple */[
                                d.getFullYear(),
                                d.getMonth()
                              ]
                            };
                    })
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "makeWithYMD",
                    (function (param) {
                        var d = new Date(1984, 4, 6);
                        return /* constructor */{
                                tag: "Eq",
                                Arg0: /* tuple */[
                                  1984,
                                  4,
                                  6
                                ],
                                Arg1: /* tuple */[
                                  d.getFullYear(),
                                  d.getMonth(),
                                  d.getDate()
                                ]
                              };
                      })
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "makeWithYMDH",
                      (function (param) {
                          var d = new Date(1984, 4, 6, 3);
                          return /* constructor */{
                                  tag: "Eq",
                                  Arg0: /* tuple */[
                                    1984,
                                    4,
                                    6,
                                    3
                                  ],
                                  Arg1: /* tuple */[
                                    d.getFullYear(),
                                    d.getMonth(),
                                    d.getDate(),
                                    d.getHours()
                                  ]
                                };
                        })
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "makeWithYMDHM",
                        (function (param) {
                            var d = new Date(1984, 4, 6, 3, 59);
                            return /* constructor */{
                                    tag: "Eq",
                                    Arg0: /* tuple */[
                                      1984,
                                      4,
                                      6,
                                      3,
                                      59
                                    ],
                                    Arg1: /* tuple */[
                                      d.getFullYear(),
                                      d.getMonth(),
                                      d.getDate(),
                                      d.getHours(),
                                      d.getMinutes()
                                    ]
                                  };
                          })
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          "makeWithYMDHMS",
                          (function (param) {
                              var d = new Date(1984, 4, 6, 3, 59, 27);
                              return /* constructor */{
                                      tag: "Eq",
                                      Arg0: /* tuple */[
                                        1984,
                                        4,
                                        6,
                                        3,
                                        59,
                                        27
                                      ],
                                      Arg1: /* tuple */[
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
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            "utcWithYM",
                            (function (param) {
                                var d = Date.UTC(1984, 4);
                                var d$1 = new Date(d);
                                return /* constructor */{
                                        tag: "Eq",
                                        Arg0: /* tuple */[
                                          1984,
                                          4
                                        ],
                                        Arg1: /* tuple */[
                                          d$1.getUTCFullYear(),
                                          d$1.getUTCMonth()
                                        ]
                                      };
                              })
                          ],
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: /* tuple */[
                              "utcWithYMD",
                              (function (param) {
                                  var d = Date.UTC(1984, 4, 6);
                                  var d$1 = new Date(d);
                                  return /* constructor */{
                                          tag: "Eq",
                                          Arg0: /* tuple */[
                                            1984,
                                            4,
                                            6
                                          ],
                                          Arg1: /* tuple */[
                                            d$1.getUTCFullYear(),
                                            d$1.getUTCMonth(),
                                            d$1.getUTCDate()
                                          ]
                                        };
                                })
                            ],
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: /* tuple */[
                                "utcWithYMDH",
                                (function (param) {
                                    var d = Date.UTC(1984, 4, 6, 3);
                                    var d$1 = new Date(d);
                                    return /* constructor */{
                                            tag: "Eq",
                                            Arg0: /* tuple */[
                                              1984,
                                              4,
                                              6,
                                              3
                                            ],
                                            Arg1: /* tuple */[
                                              d$1.getUTCFullYear(),
                                              d$1.getUTCMonth(),
                                              d$1.getUTCDate(),
                                              d$1.getUTCHours()
                                            ]
                                          };
                                  })
                              ],
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* tuple */[
                                  "utcWithYMDHM",
                                  (function (param) {
                                      var d = Date.UTC(1984, 4, 6, 3, 59);
                                      var d$1 = new Date(d);
                                      return /* constructor */{
                                              tag: "Eq",
                                              Arg0: /* tuple */[
                                                1984,
                                                4,
                                                6,
                                                3,
                                                59
                                              ],
                                              Arg1: /* tuple */[
                                                d$1.getUTCFullYear(),
                                                d$1.getUTCMonth(),
                                                d$1.getUTCDate(),
                                                d$1.getUTCHours(),
                                                d$1.getUTCMinutes()
                                              ]
                                            };
                                    })
                                ],
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: /* tuple */[
                                    "utcWithYMDHMS",
                                    (function (param) {
                                        var d = Date.UTC(1984, 4, 6, 3, 59, 27);
                                        var d$1 = new Date(d);
                                        return /* constructor */{
                                                tag: "Eq",
                                                Arg0: /* tuple */[
                                                  1984,
                                                  4,
                                                  6,
                                                  3,
                                                  59,
                                                  27
                                                ],
                                                Arg1: /* tuple */[
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
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: /* tuple */[
                                      "getFullYear",
                                      (function (param) {
                                          return /* constructor */{
                                                  tag: "Eq",
                                                  Arg0: 1976,
                                                  Arg1: new Date("1976-03-08T12:34:56.789+01:23").getFullYear()
                                                };
                                        })
                                    ],
                                    Arg1: /* constructor */{
                                      tag: "::",
                                      Arg0: /* tuple */[
                                        "getMilliseconds",
                                        (function (param) {
                                            return /* constructor */{
                                                    tag: "Eq",
                                                    Arg0: 789,
                                                    Arg1: new Date("1976-03-08T12:34:56.789+01:23").getMilliseconds()
                                                  };
                                          })
                                      ],
                                      Arg1: /* constructor */{
                                        tag: "::",
                                        Arg0: /* tuple */[
                                          "getSeconds",
                                          (function (param) {
                                              return /* constructor */{
                                                      tag: "Eq",
                                                      Arg0: 56,
                                                      Arg1: new Date("1976-03-08T12:34:56.789+01:23").getSeconds()
                                                    };
                                            })
                                        ],
                                        Arg1: /* constructor */{
                                          tag: "::",
                                          Arg0: /* tuple */[
                                            "getTime",
                                            (function (param) {
                                                return /* constructor */{
                                                        tag: "Eq",
                                                        Arg0: 195131516789,
                                                        Arg1: new Date("1976-03-08T12:34:56.789+01:23").getTime()
                                                      };
                                              })
                                          ],
                                          Arg1: /* constructor */{
                                            tag: "::",
                                            Arg0: /* tuple */[
                                              "getUTCDate",
                                              (function (param) {
                                                  return /* constructor */{
                                                          tag: "Eq",
                                                          Arg0: 8,
                                                          Arg1: new Date("1976-03-08T12:34:56.789+01:23").getUTCDate()
                                                        };
                                                })
                                            ],
                                            Arg1: /* constructor */{
                                              tag: "::",
                                              Arg0: /* tuple */[
                                                "getUTCDay",
                                                (function (param) {
                                                    return /* constructor */{
                                                            tag: "Eq",
                                                            Arg0: 1,
                                                            Arg1: new Date("1976-03-08T12:34:56.789+01:23").getUTCDay()
                                                          };
                                                  })
                                              ],
                                              Arg1: /* constructor */{
                                                tag: "::",
                                                Arg0: /* tuple */[
                                                  "getUTCFUllYear",
                                                  (function (param) {
                                                      return /* constructor */{
                                                              tag: "Eq",
                                                              Arg0: 1976,
                                                              Arg1: new Date("1976-03-08T12:34:56.789+01:23").getUTCFullYear()
                                                            };
                                                    })
                                                ],
                                                Arg1: /* constructor */{
                                                  tag: "::",
                                                  Arg0: /* tuple */[
                                                    "getUTCHours",
                                                    (function (param) {
                                                        return /* constructor */{
                                                                tag: "Eq",
                                                                Arg0: 11,
                                                                Arg1: new Date("1976-03-08T12:34:56.789+01:23").getUTCHours()
                                                              };
                                                      })
                                                  ],
                                                  Arg1: /* constructor */{
                                                    tag: "::",
                                                    Arg0: /* tuple */[
                                                      "getUTCMilliseconds",
                                                      (function (param) {
                                                          return /* constructor */{
                                                                  tag: "Eq",
                                                                  Arg0: 789,
                                                                  Arg1: new Date("1976-03-08T12:34:56.789+01:23").getUTCMilliseconds()
                                                                };
                                                        })
                                                    ],
                                                    Arg1: /* constructor */{
                                                      tag: "::",
                                                      Arg0: /* tuple */[
                                                        "getUTCMinutes",
                                                        (function (param) {
                                                            return /* constructor */{
                                                                    tag: "Eq",
                                                                    Arg0: 11,
                                                                    Arg1: new Date("1976-03-08T12:34:56.789+01:23").getUTCMinutes()
                                                                  };
                                                          })
                                                      ],
                                                      Arg1: /* constructor */{
                                                        tag: "::",
                                                        Arg0: /* tuple */[
                                                          "getUTCMonth",
                                                          (function (param) {
                                                              return /* constructor */{
                                                                      tag: "Eq",
                                                                      Arg0: 2,
                                                                      Arg1: new Date("1976-03-08T12:34:56.789+01:23").getUTCMonth()
                                                                    };
                                                            })
                                                        ],
                                                        Arg1: /* constructor */{
                                                          tag: "::",
                                                          Arg0: /* tuple */[
                                                            "getUTCSeconds",
                                                            (function (param) {
                                                                return /* constructor */{
                                                                        tag: "Eq",
                                                                        Arg0: 56,
                                                                        Arg1: new Date("1976-03-08T12:34:56.789+01:23").getUTCSeconds()
                                                                      };
                                                              })
                                                          ],
                                                          Arg1: /* constructor */{
                                                            tag: "::",
                                                            Arg0: /* tuple */[
                                                              "getYear",
                                                              (function (param) {
                                                                  return /* constructor */{
                                                                          tag: "Eq",
                                                                          Arg0: 1976,
                                                                          Arg1: new Date("1976-03-08T12:34:56.789+01:23").getFullYear()
                                                                        };
                                                                })
                                                            ],
                                                            Arg1: /* constructor */{
                                                              tag: "::",
                                                              Arg0: /* tuple */[
                                                                "setDate",
                                                                (function (param) {
                                                                    var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                    d.setDate(12);
                                                                    return /* constructor */{
                                                                            tag: "Eq",
                                                                            Arg0: 12,
                                                                            Arg1: d.getDate()
                                                                          };
                                                                  })
                                                              ],
                                                              Arg1: /* constructor */{
                                                                tag: "::",
                                                                Arg0: /* tuple */[
                                                                  "setFullYear",
                                                                  (function (param) {
                                                                      var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                      d.setFullYear(1986);
                                                                      return /* constructor */{
                                                                              tag: "Eq",
                                                                              Arg0: 1986,
                                                                              Arg1: d.getFullYear()
                                                                            };
                                                                    })
                                                                ],
                                                                Arg1: /* constructor */{
                                                                  tag: "::",
                                                                  Arg0: /* tuple */[
                                                                    "setFullYearM",
                                                                    (function (param) {
                                                                        var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                        d.setFullYear(1986, 7);
                                                                        return /* constructor */{
                                                                                tag: "Eq",
                                                                                Arg0: /* tuple */[
                                                                                  1986,
                                                                                  7
                                                                                ],
                                                                                Arg1: /* tuple */[
                                                                                  d.getFullYear(),
                                                                                  d.getMonth()
                                                                                ]
                                                                              };
                                                                      })
                                                                  ],
                                                                  Arg1: /* constructor */{
                                                                    tag: "::",
                                                                    Arg0: /* tuple */[
                                                                      "setFullYearMD",
                                                                      (function (param) {
                                                                          var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                          d.setFullYear(1986, 7, 23);
                                                                          return /* constructor */{
                                                                                  tag: "Eq",
                                                                                  Arg0: /* tuple */[
                                                                                    1986,
                                                                                    7,
                                                                                    23
                                                                                  ],
                                                                                  Arg1: /* tuple */[
                                                                                    d.getFullYear(),
                                                                                    d.getMonth(),
                                                                                    d.getDate()
                                                                                  ]
                                                                                };
                                                                        })
                                                                    ],
                                                                    Arg1: /* constructor */{
                                                                      tag: "::",
                                                                      Arg0: /* tuple */[
                                                                        "setHours",
                                                                        (function (param) {
                                                                            var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                            d.setHours(22);
                                                                            return /* constructor */{
                                                                                    tag: "Eq",
                                                                                    Arg0: 22,
                                                                                    Arg1: d.getHours()
                                                                                  };
                                                                          })
                                                                      ],
                                                                      Arg1: /* constructor */{
                                                                        tag: "::",
                                                                        Arg0: /* tuple */[
                                                                          "setHoursM",
                                                                          (function (param) {
                                                                              var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                              d.setHours(22, 48);
                                                                              return /* constructor */{
                                                                                      tag: "Eq",
                                                                                      Arg0: /* tuple */[
                                                                                        22,
                                                                                        48
                                                                                      ],
                                                                                      Arg1: /* tuple */[
                                                                                        d.getHours(),
                                                                                        d.getMinutes()
                                                                                      ]
                                                                                    };
                                                                            })
                                                                        ],
                                                                        Arg1: /* constructor */{
                                                                          tag: "::",
                                                                          Arg0: /* tuple */[
                                                                            "setHoursMS",
                                                                            (function (param) {
                                                                                var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                d.setHours(22, 48, 54);
                                                                                return /* constructor */{
                                                                                        tag: "Eq",
                                                                                        Arg0: /* tuple */[
                                                                                          22,
                                                                                          48,
                                                                                          54
                                                                                        ],
                                                                                        Arg1: /* tuple */[
                                                                                          d.getHours(),
                                                                                          d.getMinutes(),
                                                                                          d.getSeconds()
                                                                                        ]
                                                                                      };
                                                                              })
                                                                          ],
                                                                          Arg1: /* constructor */{
                                                                            tag: "::",
                                                                            Arg0: /* tuple */[
                                                                              "setMilliseconds",
                                                                              (function (param) {
                                                                                  var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                  d.setMilliseconds(543);
                                                                                  return /* constructor */{
                                                                                          tag: "Eq",
                                                                                          Arg0: 543,
                                                                                          Arg1: d.getMilliseconds()
                                                                                        };
                                                                                })
                                                                            ],
                                                                            Arg1: /* constructor */{
                                                                              tag: "::",
                                                                              Arg0: /* tuple */[
                                                                                "setMinutes",
                                                                                (function (param) {
                                                                                    var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                    d.setMinutes(18);
                                                                                    return /* constructor */{
                                                                                            tag: "Eq",
                                                                                            Arg0: 18,
                                                                                            Arg1: d.getMinutes()
                                                                                          };
                                                                                  })
                                                                              ],
                                                                              Arg1: /* constructor */{
                                                                                tag: "::",
                                                                                Arg0: /* tuple */[
                                                                                  "setMinutesS",
                                                                                  (function (param) {
                                                                                      var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                      d.setMinutes(18, 42);
                                                                                      return /* constructor */{
                                                                                              tag: "Eq",
                                                                                              Arg0: /* tuple */[
                                                                                                18,
                                                                                                42
                                                                                              ],
                                                                                              Arg1: /* tuple */[
                                                                                                d.getMinutes(),
                                                                                                d.getSeconds()
                                                                                              ]
                                                                                            };
                                                                                    })
                                                                                ],
                                                                                Arg1: /* constructor */{
                                                                                  tag: "::",
                                                                                  Arg0: /* tuple */[
                                                                                    "setMinutesSMs",
                                                                                    (function (param) {
                                                                                        var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                        d.setMinutes(18, 42, 311);
                                                                                        return /* constructor */{
                                                                                                tag: "Eq",
                                                                                                Arg0: /* tuple */[
                                                                                                  18,
                                                                                                  42,
                                                                                                  311
                                                                                                ],
                                                                                                Arg1: /* tuple */[
                                                                                                  d.getMinutes(),
                                                                                                  d.getSeconds(),
                                                                                                  d.getMilliseconds()
                                                                                                ]
                                                                                              };
                                                                                      })
                                                                                  ],
                                                                                  Arg1: /* constructor */{
                                                                                    tag: "::",
                                                                                    Arg0: /* tuple */[
                                                                                      "setMonth",
                                                                                      (function (param) {
                                                                                          var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                          d.setMonth(10);
                                                                                          return /* constructor */{
                                                                                                  tag: "Eq",
                                                                                                  Arg0: 10,
                                                                                                  Arg1: d.getMonth()
                                                                                                };
                                                                                        })
                                                                                    ],
                                                                                    Arg1: /* constructor */{
                                                                                      tag: "::",
                                                                                      Arg0: /* tuple */[
                                                                                        "setMonthD",
                                                                                        (function (param) {
                                                                                            var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                            d.setMonth(10, 14);
                                                                                            return /* constructor */{
                                                                                                    tag: "Eq",
                                                                                                    Arg0: /* tuple */[
                                                                                                      10,
                                                                                                      14
                                                                                                    ],
                                                                                                    Arg1: /* tuple */[
                                                                                                      d.getMonth(),
                                                                                                      d.getDate()
                                                                                                    ]
                                                                                                  };
                                                                                          })
                                                                                      ],
                                                                                      Arg1: /* constructor */{
                                                                                        tag: "::",
                                                                                        Arg0: /* tuple */[
                                                                                          "setSeconds",
                                                                                          (function (param) {
                                                                                              var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                              d.setSeconds(36);
                                                                                              return /* constructor */{
                                                                                                      tag: "Eq",
                                                                                                      Arg0: 36,
                                                                                                      Arg1: d.getSeconds()
                                                                                                    };
                                                                                            })
                                                                                        ],
                                                                                        Arg1: /* constructor */{
                                                                                          tag: "::",
                                                                                          Arg0: /* tuple */[
                                                                                            "setSecondsMs",
                                                                                            (function (param) {
                                                                                                var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                d.setSeconds(36, 420);
                                                                                                return /* constructor */{
                                                                                                        tag: "Eq",
                                                                                                        Arg0: /* tuple */[
                                                                                                          36,
                                                                                                          420
                                                                                                        ],
                                                                                                        Arg1: /* tuple */[
                                                                                                          d.getSeconds(),
                                                                                                          d.getMilliseconds()
                                                                                                        ]
                                                                                                      };
                                                                                              })
                                                                                          ],
                                                                                          Arg1: /* constructor */{
                                                                                            tag: "::",
                                                                                            Arg0: /* tuple */[
                                                                                              "setUTCDate",
                                                                                              (function (param) {
                                                                                                  var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                  d.setUTCDate(12);
                                                                                                  return /* constructor */{
                                                                                                          tag: "Eq",
                                                                                                          Arg0: 12,
                                                                                                          Arg1: d.getUTCDate()
                                                                                                        };
                                                                                                })
                                                                                            ],
                                                                                            Arg1: /* constructor */{
                                                                                              tag: "::",
                                                                                              Arg0: /* tuple */[
                                                                                                "setUTCFullYear",
                                                                                                (function (param) {
                                                                                                    var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                    d.setUTCFullYear(1986);
                                                                                                    return /* constructor */{
                                                                                                            tag: "Eq",
                                                                                                            Arg0: 1986,
                                                                                                            Arg1: d.getUTCFullYear()
                                                                                                          };
                                                                                                  })
                                                                                              ],
                                                                                              Arg1: /* constructor */{
                                                                                                tag: "::",
                                                                                                Arg0: /* tuple */[
                                                                                                  "setUTCFullYearM",
                                                                                                  (function (param) {
                                                                                                      var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                      d.setUTCFullYear(1986, 7);
                                                                                                      return /* constructor */{
                                                                                                              tag: "Eq",
                                                                                                              Arg0: /* tuple */[
                                                                                                                1986,
                                                                                                                7
                                                                                                              ],
                                                                                                              Arg1: /* tuple */[
                                                                                                                d.getUTCFullYear(),
                                                                                                                d.getUTCMonth()
                                                                                                              ]
                                                                                                            };
                                                                                                    })
                                                                                                ],
                                                                                                Arg1: /* constructor */{
                                                                                                  tag: "::",
                                                                                                  Arg0: /* tuple */[
                                                                                                    "setUTCFullYearMD",
                                                                                                    (function (param) {
                                                                                                        var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                        d.setUTCFullYear(1986, 7, 23);
                                                                                                        return /* constructor */{
                                                                                                                tag: "Eq",
                                                                                                                Arg0: /* tuple */[
                                                                                                                  1986,
                                                                                                                  7,
                                                                                                                  23
                                                                                                                ],
                                                                                                                Arg1: /* tuple */[
                                                                                                                  d.getUTCFullYear(),
                                                                                                                  d.getUTCMonth(),
                                                                                                                  d.getUTCDate()
                                                                                                                ]
                                                                                                              };
                                                                                                      })
                                                                                                  ],
                                                                                                  Arg1: /* constructor */{
                                                                                                    tag: "::",
                                                                                                    Arg0: /* tuple */[
                                                                                                      "setUTCHours",
                                                                                                      (function (param) {
                                                                                                          var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                          d.setUTCHours(22);
                                                                                                          return /* constructor */{
                                                                                                                  tag: "Eq",
                                                                                                                  Arg0: 22,
                                                                                                                  Arg1: d.getUTCHours()
                                                                                                                };
                                                                                                        })
                                                                                                    ],
                                                                                                    Arg1: /* constructor */{
                                                                                                      tag: "::",
                                                                                                      Arg0: /* tuple */[
                                                                                                        "setUTCHoursM",
                                                                                                        (function (param) {
                                                                                                            var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                            d.setUTCHours(22, 48);
                                                                                                            return /* constructor */{
                                                                                                                    tag: "Eq",
                                                                                                                    Arg0: /* tuple */[
                                                                                                                      22,
                                                                                                                      48
                                                                                                                    ],
                                                                                                                    Arg1: /* tuple */[
                                                                                                                      d.getUTCHours(),
                                                                                                                      d.getUTCMinutes()
                                                                                                                    ]
                                                                                                                  };
                                                                                                          })
                                                                                                      ],
                                                                                                      Arg1: /* constructor */{
                                                                                                        tag: "::",
                                                                                                        Arg0: /* tuple */[
                                                                                                          "setUTCHoursMS",
                                                                                                          (function (param) {
                                                                                                              var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                              d.setUTCHours(22, 48, 54);
                                                                                                              return /* constructor */{
                                                                                                                      tag: "Eq",
                                                                                                                      Arg0: /* tuple */[
                                                                                                                        22,
                                                                                                                        48,
                                                                                                                        54
                                                                                                                      ],
                                                                                                                      Arg1: /* tuple */[
                                                                                                                        d.getUTCHours(),
                                                                                                                        d.getUTCMinutes(),
                                                                                                                        d.getUTCSeconds()
                                                                                                                      ]
                                                                                                                    };
                                                                                                            })
                                                                                                        ],
                                                                                                        Arg1: /* constructor */{
                                                                                                          tag: "::",
                                                                                                          Arg0: /* tuple */[
                                                                                                            "setUTCMilliseconds",
                                                                                                            (function (param) {
                                                                                                                var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                d.setUTCMilliseconds(543);
                                                                                                                return /* constructor */{
                                                                                                                        tag: "Eq",
                                                                                                                        Arg0: 543,
                                                                                                                        Arg1: d.getUTCMilliseconds()
                                                                                                                      };
                                                                                                              })
                                                                                                          ],
                                                                                                          Arg1: /* constructor */{
                                                                                                            tag: "::",
                                                                                                            Arg0: /* tuple */[
                                                                                                              "setUTCMinutes",
                                                                                                              (function (param) {
                                                                                                                  var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                  d.setUTCMinutes(18);
                                                                                                                  return /* constructor */{
                                                                                                                          tag: "Eq",
                                                                                                                          Arg0: 18,
                                                                                                                          Arg1: d.getUTCMinutes()
                                                                                                                        };
                                                                                                                })
                                                                                                            ],
                                                                                                            Arg1: /* constructor */{
                                                                                                              tag: "::",
                                                                                                              Arg0: /* tuple */[
                                                                                                                "setUTCMinutesS",
                                                                                                                (function (param) {
                                                                                                                    var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                    d.setUTCMinutes(18, 42);
                                                                                                                    return /* constructor */{
                                                                                                                            tag: "Eq",
                                                                                                                            Arg0: /* tuple */[
                                                                                                                              18,
                                                                                                                              42
                                                                                                                            ],
                                                                                                                            Arg1: /* tuple */[
                                                                                                                              d.getUTCMinutes(),
                                                                                                                              d.getUTCSeconds()
                                                                                                                            ]
                                                                                                                          };
                                                                                                                  })
                                                                                                              ],
                                                                                                              Arg1: /* constructor */{
                                                                                                                tag: "::",
                                                                                                                Arg0: /* tuple */[
                                                                                                                  "setUTCMinutesSMs",
                                                                                                                  (function (param) {
                                                                                                                      var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                      d.setUTCMinutes(18, 42, 311);
                                                                                                                      return /* constructor */{
                                                                                                                              tag: "Eq",
                                                                                                                              Arg0: /* tuple */[
                                                                                                                                18,
                                                                                                                                42,
                                                                                                                                311
                                                                                                                              ],
                                                                                                                              Arg1: /* tuple */[
                                                                                                                                d.getUTCMinutes(),
                                                                                                                                d.getUTCSeconds(),
                                                                                                                                d.getUTCMilliseconds()
                                                                                                                              ]
                                                                                                                            };
                                                                                                                    })
                                                                                                                ],
                                                                                                                Arg1: /* constructor */{
                                                                                                                  tag: "::",
                                                                                                                  Arg0: /* tuple */[
                                                                                                                    "setUTCMonth",
                                                                                                                    (function (param) {
                                                                                                                        var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                        d.setUTCMonth(10);
                                                                                                                        return /* constructor */{
                                                                                                                                tag: "Eq",
                                                                                                                                Arg0: 10,
                                                                                                                                Arg1: d.getUTCMonth()
                                                                                                                              };
                                                                                                                      })
                                                                                                                  ],
                                                                                                                  Arg1: /* constructor */{
                                                                                                                    tag: "::",
                                                                                                                    Arg0: /* tuple */[
                                                                                                                      "setUTCMonthD",
                                                                                                                      (function (param) {
                                                                                                                          var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                          d.setUTCMonth(10, 14);
                                                                                                                          return /* constructor */{
                                                                                                                                  tag: "Eq",
                                                                                                                                  Arg0: /* tuple */[
                                                                                                                                    10,
                                                                                                                                    14
                                                                                                                                  ],
                                                                                                                                  Arg1: /* tuple */[
                                                                                                                                    d.getUTCMonth(),
                                                                                                                                    d.getUTCDate()
                                                                                                                                  ]
                                                                                                                                };
                                                                                                                        })
                                                                                                                    ],
                                                                                                                    Arg1: /* constructor */{
                                                                                                                      tag: "::",
                                                                                                                      Arg0: /* tuple */[
                                                                                                                        "setUTCSeconds",
                                                                                                                        (function (param) {
                                                                                                                            var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                            d.setUTCSeconds(36);
                                                                                                                            return /* constructor */{
                                                                                                                                    tag: "Eq",
                                                                                                                                    Arg0: 36,
                                                                                                                                    Arg1: d.getUTCSeconds()
                                                                                                                                  };
                                                                                                                          })
                                                                                                                      ],
                                                                                                                      Arg1: /* constructor */{
                                                                                                                        tag: "::",
                                                                                                                        Arg0: /* tuple */[
                                                                                                                          "setUTCSecondsMs",
                                                                                                                          (function (param) {
                                                                                                                              var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                              d.setUTCSeconds(36, 420);
                                                                                                                              return /* constructor */{
                                                                                                                                      tag: "Eq",
                                                                                                                                      Arg0: /* tuple */[
                                                                                                                                        36,
                                                                                                                                        420
                                                                                                                                      ],
                                                                                                                                      Arg1: /* tuple */[
                                                                                                                                        d.getUTCSeconds(),
                                                                                                                                        d.getUTCMilliseconds()
                                                                                                                                      ]
                                                                                                                                    };
                                                                                                                            })
                                                                                                                        ],
                                                                                                                        Arg1: /* constructor */{
                                                                                                                          tag: "::",
                                                                                                                          Arg0: /* tuple */[
                                                                                                                            "toDateString",
                                                                                                                            (function (param) {
                                                                                                                                return /* constructor */{
                                                                                                                                        tag: "Eq",
                                                                                                                                        Arg0: "Mon Mar 08 1976",
                                                                                                                                        Arg1: new Date("1976-03-08T12:34:56.789+01:23").toDateString()
                                                                                                                                      };
                                                                                                                              })
                                                                                                                          ],
                                                                                                                          Arg1: /* constructor */{
                                                                                                                            tag: "::",
                                                                                                                            Arg0: /* tuple */[
                                                                                                                              "toGMTString",
                                                                                                                              (function (param) {
                                                                                                                                  return /* constructor */{
                                                                                                                                          tag: "Eq",
                                                                                                                                          Arg0: "Mon, 08 Mar 1976 11:11:56 GMT",
                                                                                                                                          Arg1: new Date("1976-03-08T12:34:56.789+01:23").toUTCString()
                                                                                                                                        };
                                                                                                                                })
                                                                                                                            ],
                                                                                                                            Arg1: /* constructor */{
                                                                                                                              tag: "::",
                                                                                                                              Arg0: /* tuple */[
                                                                                                                                "toISOString",
                                                                                                                                (function (param) {
                                                                                                                                    return /* constructor */{
                                                                                                                                            tag: "Eq",
                                                                                                                                            Arg0: "1976-03-08T11:11:56.789Z",
                                                                                                                                            Arg1: new Date("1976-03-08T12:34:56.789+01:23").toISOString()
                                                                                                                                          };
                                                                                                                                  })
                                                                                                                              ],
                                                                                                                              Arg1: /* constructor */{
                                                                                                                                tag: "::",
                                                                                                                                Arg0: /* tuple */[
                                                                                                                                  "toJSON",
                                                                                                                                  (function (param) {
                                                                                                                                      return /* constructor */{
                                                                                                                                              tag: "Eq",
                                                                                                                                              Arg0: "1976-03-08T11:11:56.789Z",
                                                                                                                                              Arg1: new Date("1976-03-08T12:34:56.789+01:23").toJSON()
                                                                                                                                            };
                                                                                                                                    })
                                                                                                                                ],
                                                                                                                                Arg1: /* constructor */{
                                                                                                                                  tag: "::",
                                                                                                                                  Arg0: /* tuple */[
                                                                                                                                    "toJSONUnsafe",
                                                                                                                                    (function (param) {
                                                                                                                                        return /* constructor */{
                                                                                                                                                tag: "Eq",
                                                                                                                                                Arg0: "1976-03-08T11:11:56.789Z",
                                                                                                                                                Arg1: new Date("1976-03-08T12:34:56.789+01:23").toJSON()
                                                                                                                                              };
                                                                                                                                      })
                                                                                                                                  ],
                                                                                                                                  Arg1: /* constructor */{
                                                                                                                                    tag: "::",
                                                                                                                                    Arg0: /* tuple */[
                                                                                                                                      "toUTCString",
                                                                                                                                      (function (param) {
                                                                                                                                          return /* constructor */{
                                                                                                                                                  tag: "Eq",
                                                                                                                                                  Arg0: "Mon, 08 Mar 1976 11:11:56 GMT",
                                                                                                                                                  Arg1: new Date("1976-03-08T12:34:56.789+01:23").toUTCString()
                                                                                                                                                };
                                                                                                                                        })
                                                                                                                                    ],
                                                                                                                                    Arg1: /* constructor */{
                                                                                                                                      tag: "::",
                                                                                                                                      Arg0: /* tuple */[
                                                                                                                                        "eq",
                                                                                                                                        (function (param) {
                                                                                                                                            var a = new Date("2013-03-01T01:10:00");
                                                                                                                                            var b = new Date("2013-03-01T01:10:00");
                                                                                                                                            var c = new Date("2013-03-01T01:10:01");
                                                                                                                                            return /* constructor */{
                                                                                                                                                    tag: "Ok",
                                                                                                                                                    Arg0: Caml_obj.caml_equal(a, b) && Caml_obj.caml_notequal(b, c) && Caml_obj.caml_greaterthan(c, b)
                                                                                                                                                  };
                                                                                                                                          })
                                                                                                                                      ],
                                                                                                                                      Arg1: "[]"
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                }
                                                                                                                              }
                                                                                                                            }
                                                                                                                          }
                                                                                                                        }
                                                                                                                      }
                                                                                                                    }
                                                                                                                  }
                                                                                                                }
                                                                                                              }
                                                                                                            }
                                                                                                          }
                                                                                                        }
                                                                                                      }
                                                                                                    }
                                                                                                  }
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
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

Mt.from_pair_suites("Js_date_test", suites);

var N = 0;

exports.N = N;
exports.date = date;
exports.suites = suites;
/*  Not a pure module */
