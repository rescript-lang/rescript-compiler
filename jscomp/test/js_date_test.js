'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

function date() {
  return new Date("1976-03-08T12:34:56.789+01:23");
}

var suites_000 = /* tuple */[
  "valueOf",
  (function () {
      return /* Eq */Block.__(0, [
                195131516789,
                new Date("1976-03-08T12:34:56.789+01:23").valueOf()
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "make",
    (function () {
        return /* Ok */Block.__(4, [+(new Date().getTime() > 1487223505382)]);
      })
  ],
  /* :: */[
    /* tuple */[
      "fromFloat",
      (function () {
          return /* Eq */Block.__(0, [
                    "1976-03-08T11:11:56.789Z",
                    new Date(195131516789).toISOString()
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "fromString_valid",
        (function () {
            return /* Eq */Block.__(0, [
                      195131516789,
                      new Date("1976-03-08T12:34:56.789+01:23").getTime()
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "fromString_invalid",
          (function () {
              return /* Ok */Block.__(4, [+isNaN(new Date("gibberish").getTime())]);
            })
        ],
        /* :: */[
          /* tuple */[
            "makeWithYM",
            (function () {
                var d = new Date(1984, 4);
                return /* Eq */Block.__(0, [
                          /* tuple */[
                            1984,
                            4
                          ],
                          /* tuple */[
                            d.getFullYear(),
                            d.getMonth()
                          ]
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "makeWithYMD",
              (function () {
                  var d = new Date(1984, 4, 6);
                  return /* Eq */Block.__(0, [
                            /* tuple */[
                              1984,
                              4,
                              6
                            ],
                            /* tuple */[
                              d.getFullYear(),
                              d.getMonth(),
                              d.getDate()
                            ]
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "makeWithYMDH",
                (function () {
                    var d = new Date(1984, 4, 6, 3);
                    return /* Eq */Block.__(0, [
                              /* tuple */[
                                1984,
                                4,
                                6,
                                3
                              ],
                              /* tuple */[
                                d.getFullYear(),
                                d.getMonth(),
                                d.getDate(),
                                d.getHours()
                              ]
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "makeWithYMDHM",
                  (function () {
                      var d = new Date(1984, 4, 6, 3, 59);
                      return /* Eq */Block.__(0, [
                                /* tuple */[
                                  1984,
                                  4,
                                  6,
                                  3,
                                  59
                                ],
                                /* tuple */[
                                  d.getFullYear(),
                                  d.getMonth(),
                                  d.getDate(),
                                  d.getHours(),
                                  d.getMinutes()
                                ]
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "makeWithYMDHMS",
                    (function () {
                        var d = new Date(1984, 4, 6, 3, 59, 27);
                        return /* Eq */Block.__(0, [
                                  /* tuple */[
                                    1984,
                                    4,
                                    6,
                                    3,
                                    59,
                                    27
                                  ],
                                  /* tuple */[
                                    d.getFullYear(),
                                    d.getMonth(),
                                    d.getDate(),
                                    d.getHours(),
                                    d.getMinutes(),
                                    d.getSeconds()
                                  ]
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "utcWithYM",
                      (function () {
                          var d = Date.UTC(1984, 4);
                          var d$1 = new Date(d);
                          return /* Eq */Block.__(0, [
                                    /* tuple */[
                                      1984,
                                      4
                                    ],
                                    /* tuple */[
                                      d$1.getUTCFullYear(),
                                      d$1.getUTCMonth()
                                    ]
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "utcWithYMD",
                        (function () {
                            var d = Date.UTC(1984, 4, 6);
                            var d$1 = new Date(d);
                            return /* Eq */Block.__(0, [
                                      /* tuple */[
                                        1984,
                                        4,
                                        6
                                      ],
                                      /* tuple */[
                                        d$1.getUTCFullYear(),
                                        d$1.getUTCMonth(),
                                        d$1.getUTCDate()
                                      ]
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "utcWithYMDH",
                          (function () {
                              var d = Date.UTC(1984, 4, 6, 3);
                              var d$1 = new Date(d);
                              return /* Eq */Block.__(0, [
                                        /* tuple */[
                                          1984,
                                          4,
                                          6,
                                          3
                                        ],
                                        /* tuple */[
                                          d$1.getUTCFullYear(),
                                          d$1.getUTCMonth(),
                                          d$1.getUTCDate(),
                                          d$1.getUTCHours()
                                        ]
                                      ]);
                            })
                        ],
                        /* :: */[
                          /* tuple */[
                            "utcWithYMDHM",
                            (function () {
                                var d = Date.UTC(1984, 4, 6, 3, 59);
                                var d$1 = new Date(d);
                                return /* Eq */Block.__(0, [
                                          /* tuple */[
                                            1984,
                                            4,
                                            6,
                                            3,
                                            59
                                          ],
                                          /* tuple */[
                                            d$1.getUTCFullYear(),
                                            d$1.getUTCMonth(),
                                            d$1.getUTCDate(),
                                            d$1.getUTCHours(),
                                            d$1.getUTCMinutes()
                                          ]
                                        ]);
                              })
                          ],
                          /* :: */[
                            /* tuple */[
                              "utcWithYMDHMS",
                              (function () {
                                  var d = Date.UTC(1984, 4, 6, 3, 59, 27);
                                  var d$1 = new Date(d);
                                  return /* Eq */Block.__(0, [
                                            /* tuple */[
                                              1984,
                                              4,
                                              6,
                                              3,
                                              59,
                                              27
                                            ],
                                            /* tuple */[
                                              d$1.getUTCFullYear(),
                                              d$1.getUTCMonth(),
                                              d$1.getUTCDate(),
                                              d$1.getUTCHours(),
                                              d$1.getUTCMinutes(),
                                              d$1.getUTCSeconds()
                                            ]
                                          ]);
                                })
                            ],
                            /* :: */[
                              /* tuple */[
                                "getFullYear",
                                (function () {
                                    return /* Eq */Block.__(0, [
                                              1976,
                                              new Date("1976-03-08T12:34:56.789+01:23").getFullYear()
                                            ]);
                                  })
                              ],
                              /* :: */[
                                /* tuple */[
                                  "getMilliseconds",
                                  (function () {
                                      return /* Eq */Block.__(0, [
                                                789,
                                                new Date("1976-03-08T12:34:56.789+01:23").getMilliseconds()
                                              ]);
                                    })
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "getSeconds",
                                    (function () {
                                        return /* Eq */Block.__(0, [
                                                  56,
                                                  new Date("1976-03-08T12:34:56.789+01:23").getSeconds()
                                                ]);
                                      })
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "getTime",
                                      (function () {
                                          return /* Eq */Block.__(0, [
                                                    195131516789,
                                                    new Date("1976-03-08T12:34:56.789+01:23").getTime()
                                                  ]);
                                        })
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "getUTCDate",
                                        (function () {
                                            return /* Eq */Block.__(0, [
                                                      8,
                                                      new Date("1976-03-08T12:34:56.789+01:23").getUTCDate()
                                                    ]);
                                          })
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "getUTCDay",
                                          (function () {
                                              return /* Eq */Block.__(0, [
                                                        1,
                                                        new Date("1976-03-08T12:34:56.789+01:23").getUTCDay()
                                                      ]);
                                            })
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "getUTCFUllYear",
                                            (function () {
                                                return /* Eq */Block.__(0, [
                                                          1976,
                                                          new Date("1976-03-08T12:34:56.789+01:23").getUTCFullYear()
                                                        ]);
                                              })
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "getUTCHours",
                                              (function () {
                                                  return /* Eq */Block.__(0, [
                                                            11,
                                                            new Date("1976-03-08T12:34:56.789+01:23").getUTCHours()
                                                          ]);
                                                })
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "getUTCMilliseconds",
                                                (function () {
                                                    return /* Eq */Block.__(0, [
                                                              789,
                                                              new Date("1976-03-08T12:34:56.789+01:23").getUTCMilliseconds()
                                                            ]);
                                                  })
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "getUTCMinutes",
                                                  (function () {
                                                      return /* Eq */Block.__(0, [
                                                                11,
                                                                new Date("1976-03-08T12:34:56.789+01:23").getUTCMinutes()
                                                              ]);
                                                    })
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "getUTCMonth",
                                                    (function () {
                                                        return /* Eq */Block.__(0, [
                                                                  2,
                                                                  new Date("1976-03-08T12:34:56.789+01:23").getUTCMonth()
                                                                ]);
                                                      })
                                                  ],
                                                  /* :: */[
                                                    /* tuple */[
                                                      "getUTCSeconds",
                                                      (function () {
                                                          return /* Eq */Block.__(0, [
                                                                    56,
                                                                    new Date("1976-03-08T12:34:56.789+01:23").getUTCSeconds()
                                                                  ]);
                                                        })
                                                    ],
                                                    /* :: */[
                                                      /* tuple */[
                                                        "getYear",
                                                        (function () {
                                                            return /* Eq */Block.__(0, [
                                                                      76,
                                                                      new Date("1976-03-08T12:34:56.789+01:23").getYear()
                                                                    ]);
                                                          })
                                                      ],
                                                      /* :: */[
                                                        /* tuple */[
                                                          "setDate",
                                                          (function () {
                                                              var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                              d.setDate(12);
                                                              return /* Eq */Block.__(0, [
                                                                        12,
                                                                        d.getDate()
                                                                      ]);
                                                            })
                                                        ],
                                                        /* :: */[
                                                          /* tuple */[
                                                            "setFullYear",
                                                            (function () {
                                                                var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                d.setFullYear(1986);
                                                                return /* Eq */Block.__(0, [
                                                                          1986,
                                                                          d.getFullYear()
                                                                        ]);
                                                              })
                                                          ],
                                                          /* :: */[
                                                            /* tuple */[
                                                              "setFullYearM",
                                                              (function () {
                                                                  var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                  d.setFullYear(1986, 7);
                                                                  return /* Eq */Block.__(0, [
                                                                            /* tuple */[
                                                                              1986,
                                                                              7
                                                                            ],
                                                                            /* tuple */[
                                                                              d.getFullYear(),
                                                                              d.getMonth()
                                                                            ]
                                                                          ]);
                                                                })
                                                            ],
                                                            /* :: */[
                                                              /* tuple */[
                                                                "setFullYearMD",
                                                                (function () {
                                                                    var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                    d.setFullYear(1986, 7, 23);
                                                                    return /* Eq */Block.__(0, [
                                                                              /* tuple */[
                                                                                1986,
                                                                                7,
                                                                                23
                                                                              ],
                                                                              /* tuple */[
                                                                                d.getFullYear(),
                                                                                d.getMonth(),
                                                                                d.getDate()
                                                                              ]
                                                                            ]);
                                                                  })
                                                              ],
                                                              /* :: */[
                                                                /* tuple */[
                                                                  "setHours",
                                                                  (function () {
                                                                      var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                      d.setHours(22);
                                                                      return /* Eq */Block.__(0, [
                                                                                22,
                                                                                d.getHours()
                                                                              ]);
                                                                    })
                                                                ],
                                                                /* :: */[
                                                                  /* tuple */[
                                                                    "setHoursM",
                                                                    (function () {
                                                                        var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                        d.setHours(22, 48);
                                                                        return /* Eq */Block.__(0, [
                                                                                  /* tuple */[
                                                                                    22,
                                                                                    48
                                                                                  ],
                                                                                  /* tuple */[
                                                                                    d.getHours(),
                                                                                    d.getMinutes()
                                                                                  ]
                                                                                ]);
                                                                      })
                                                                  ],
                                                                  /* :: */[
                                                                    /* tuple */[
                                                                      "setHoursMS",
                                                                      (function () {
                                                                          var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                          d.setHours(22, 48, 54);
                                                                          return /* Eq */Block.__(0, [
                                                                                    /* tuple */[
                                                                                      22,
                                                                                      48,
                                                                                      54
                                                                                    ],
                                                                                    /* tuple */[
                                                                                      d.getHours(),
                                                                                      d.getMinutes(),
                                                                                      d.getSeconds()
                                                                                    ]
                                                                                  ]);
                                                                        })
                                                                    ],
                                                                    /* :: */[
                                                                      /* tuple */[
                                                                        "setMilliseconds",
                                                                        (function () {
                                                                            var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                            d.setMilliseconds(543);
                                                                            return /* Eq */Block.__(0, [
                                                                                      543,
                                                                                      d.getMilliseconds()
                                                                                    ]);
                                                                          })
                                                                      ],
                                                                      /* :: */[
                                                                        /* tuple */[
                                                                          "setMinutes",
                                                                          (function () {
                                                                              var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                              d.setMinutes(18);
                                                                              return /* Eq */Block.__(0, [
                                                                                        18,
                                                                                        d.getMinutes()
                                                                                      ]);
                                                                            })
                                                                        ],
                                                                        /* :: */[
                                                                          /* tuple */[
                                                                            "setMinutesS",
                                                                            (function () {
                                                                                var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                d.setMinutes(18, 42);
                                                                                return /* Eq */Block.__(0, [
                                                                                          /* tuple */[
                                                                                            18,
                                                                                            42
                                                                                          ],
                                                                                          /* tuple */[
                                                                                            d.getMinutes(),
                                                                                            d.getSeconds()
                                                                                          ]
                                                                                        ]);
                                                                              })
                                                                          ],
                                                                          /* :: */[
                                                                            /* tuple */[
                                                                              "setMinutesSMs",
                                                                              (function () {
                                                                                  var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                  d.setMinutes(18, 42, 311);
                                                                                  return /* Eq */Block.__(0, [
                                                                                            /* tuple */[
                                                                                              18,
                                                                                              42,
                                                                                              311
                                                                                            ],
                                                                                            /* tuple */[
                                                                                              d.getMinutes(),
                                                                                              d.getSeconds(),
                                                                                              d.getMilliseconds()
                                                                                            ]
                                                                                          ]);
                                                                                })
                                                                            ],
                                                                            /* :: */[
                                                                              /* tuple */[
                                                                                "setMonth",
                                                                                (function () {
                                                                                    var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                    d.setMonth(10);
                                                                                    return /* Eq */Block.__(0, [
                                                                                              10,
                                                                                              d.getMonth()
                                                                                            ]);
                                                                                  })
                                                                              ],
                                                                              /* :: */[
                                                                                /* tuple */[
                                                                                  "setMonthD",
                                                                                  (function () {
                                                                                      var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                      d.setMonth(10, 14);
                                                                                      return /* Eq */Block.__(0, [
                                                                                                /* tuple */[
                                                                                                  10,
                                                                                                  14
                                                                                                ],
                                                                                                /* tuple */[
                                                                                                  d.getMonth(),
                                                                                                  d.getDate()
                                                                                                ]
                                                                                              ]);
                                                                                    })
                                                                                ],
                                                                                /* :: */[
                                                                                  /* tuple */[
                                                                                    "setSeconds",
                                                                                    (function () {
                                                                                        var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                        d.setSeconds(36);
                                                                                        return /* Eq */Block.__(0, [
                                                                                                  36,
                                                                                                  d.getSeconds()
                                                                                                ]);
                                                                                      })
                                                                                  ],
                                                                                  /* :: */[
                                                                                    /* tuple */[
                                                                                      "setSecondsMs",
                                                                                      (function () {
                                                                                          var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                          d.setSeconds(36, 420);
                                                                                          return /* Eq */Block.__(0, [
                                                                                                    /* tuple */[
                                                                                                      36,
                                                                                                      420
                                                                                                    ],
                                                                                                    /* tuple */[
                                                                                                      d.getSeconds(),
                                                                                                      d.getMilliseconds()
                                                                                                    ]
                                                                                                  ]);
                                                                                        })
                                                                                    ],
                                                                                    /* :: */[
                                                                                      /* tuple */[
                                                                                        "setUTCDate",
                                                                                        (function () {
                                                                                            var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                            d.setUTCDate(12);
                                                                                            return /* Eq */Block.__(0, [
                                                                                                      12,
                                                                                                      d.getUTCDate()
                                                                                                    ]);
                                                                                          })
                                                                                      ],
                                                                                      /* :: */[
                                                                                        /* tuple */[
                                                                                          "setUTCFullYear",
                                                                                          (function () {
                                                                                              var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                              d.setUTCFullYear(1986);
                                                                                              return /* Eq */Block.__(0, [
                                                                                                        1986,
                                                                                                        d.getUTCFullYear()
                                                                                                      ]);
                                                                                            })
                                                                                        ],
                                                                                        /* :: */[
                                                                                          /* tuple */[
                                                                                            "setUTCFullYearM",
                                                                                            (function () {
                                                                                                var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                d.setUTCFullYear(1986, 7);
                                                                                                return /* Eq */Block.__(0, [
                                                                                                          /* tuple */[
                                                                                                            1986,
                                                                                                            7
                                                                                                          ],
                                                                                                          /* tuple */[
                                                                                                            d.getUTCFullYear(),
                                                                                                            d.getUTCMonth()
                                                                                                          ]
                                                                                                        ]);
                                                                                              })
                                                                                          ],
                                                                                          /* :: */[
                                                                                            /* tuple */[
                                                                                              "setUTCFullYearMD",
                                                                                              (function () {
                                                                                                  var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                  d.setUTCFullYear(1986, 7, 23);
                                                                                                  return /* Eq */Block.__(0, [
                                                                                                            /* tuple */[
                                                                                                              1986,
                                                                                                              7,
                                                                                                              23
                                                                                                            ],
                                                                                                            /* tuple */[
                                                                                                              d.getUTCFullYear(),
                                                                                                              d.getUTCMonth(),
                                                                                                              d.getUTCDate()
                                                                                                            ]
                                                                                                          ]);
                                                                                                })
                                                                                            ],
                                                                                            /* :: */[
                                                                                              /* tuple */[
                                                                                                "setUTCHours",
                                                                                                (function () {
                                                                                                    var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                    d.setUTCHours(22);
                                                                                                    return /* Eq */Block.__(0, [
                                                                                                              22,
                                                                                                              d.getUTCHours()
                                                                                                            ]);
                                                                                                  })
                                                                                              ],
                                                                                              /* :: */[
                                                                                                /* tuple */[
                                                                                                  "setUTCHoursM",
                                                                                                  (function () {
                                                                                                      var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                      d.setUTCHours(22, 48);
                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                /* tuple */[
                                                                                                                  22,
                                                                                                                  48
                                                                                                                ],
                                                                                                                /* tuple */[
                                                                                                                  d.getUTCHours(),
                                                                                                                  d.getUTCMinutes()
                                                                                                                ]
                                                                                                              ]);
                                                                                                    })
                                                                                                ],
                                                                                                /* :: */[
                                                                                                  /* tuple */[
                                                                                                    "setUTCHoursMS",
                                                                                                    (function () {
                                                                                                        var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                        d.setUTCHours(22, 48, 54);
                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                  /* tuple */[
                                                                                                                    22,
                                                                                                                    48,
                                                                                                                    54
                                                                                                                  ],
                                                                                                                  /* tuple */[
                                                                                                                    d.getUTCHours(),
                                                                                                                    d.getUTCMinutes(),
                                                                                                                    d.getUTCSeconds()
                                                                                                                  ]
                                                                                                                ]);
                                                                                                      })
                                                                                                  ],
                                                                                                  /* :: */[
                                                                                                    /* tuple */[
                                                                                                      "setUTCMilliseconds",
                                                                                                      (function () {
                                                                                                          var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                          d.setUTCMilliseconds(543);
                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                    543,
                                                                                                                    d.getUTCMilliseconds()
                                                                                                                  ]);
                                                                                                        })
                                                                                                    ],
                                                                                                    /* :: */[
                                                                                                      /* tuple */[
                                                                                                        "setUTCMinutes",
                                                                                                        (function () {
                                                                                                            var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                            d.setUTCMinutes(18);
                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                      18,
                                                                                                                      d.getUTCMinutes()
                                                                                                                    ]);
                                                                                                          })
                                                                                                      ],
                                                                                                      /* :: */[
                                                                                                        /* tuple */[
                                                                                                          "setUTCMinutesS",
                                                                                                          (function () {
                                                                                                              var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                              d.setUTCMinutes(18, 42);
                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                        /* tuple */[
                                                                                                                          18,
                                                                                                                          42
                                                                                                                        ],
                                                                                                                        /* tuple */[
                                                                                                                          d.getUTCMinutes(),
                                                                                                                          d.getUTCSeconds()
                                                                                                                        ]
                                                                                                                      ]);
                                                                                                            })
                                                                                                        ],
                                                                                                        /* :: */[
                                                                                                          /* tuple */[
                                                                                                            "setUTCMinutesSMs",
                                                                                                            (function () {
                                                                                                                var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                d.setUTCMinutes(18, 42, 311);
                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                          /* tuple */[
                                                                                                                            18,
                                                                                                                            42,
                                                                                                                            311
                                                                                                                          ],
                                                                                                                          /* tuple */[
                                                                                                                            d.getUTCMinutes(),
                                                                                                                            d.getUTCSeconds(),
                                                                                                                            d.getUTCMilliseconds()
                                                                                                                          ]
                                                                                                                        ]);
                                                                                                              })
                                                                                                          ],
                                                                                                          /* :: */[
                                                                                                            /* tuple */[
                                                                                                              "setUTCMonth",
                                                                                                              (function () {
                                                                                                                  var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                  d.setUTCMonth(10);
                                                                                                                  return /* Eq */Block.__(0, [
                                                                                                                            10,
                                                                                                                            d.getUTCMonth()
                                                                                                                          ]);
                                                                                                                })
                                                                                                            ],
                                                                                                            /* :: */[
                                                                                                              /* tuple */[
                                                                                                                "setUTCMonthD",
                                                                                                                (function () {
                                                                                                                    var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                    d.setUTCMonth(10, 14);
                                                                                                                    return /* Eq */Block.__(0, [
                                                                                                                              /* tuple */[
                                                                                                                                10,
                                                                                                                                14
                                                                                                                              ],
                                                                                                                              /* tuple */[
                                                                                                                                d.getUTCMonth(),
                                                                                                                                d.getUTCDate()
                                                                                                                              ]
                                                                                                                            ]);
                                                                                                                  })
                                                                                                              ],
                                                                                                              /* :: */[
                                                                                                                /* tuple */[
                                                                                                                  "setUTCSeconds",
                                                                                                                  (function () {
                                                                                                                      var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                      d.setUTCSeconds(36);
                                                                                                                      return /* Eq */Block.__(0, [
                                                                                                                                36,
                                                                                                                                d.getUTCSeconds()
                                                                                                                              ]);
                                                                                                                    })
                                                                                                                ],
                                                                                                                /* :: */[
                                                                                                                  /* tuple */[
                                                                                                                    "setUTCSecondsMs",
                                                                                                                    (function () {
                                                                                                                        var d = new Date("1976-03-08T12:34:56.789+01:23");
                                                                                                                        d.setUTCSeconds(36, 420);
                                                                                                                        return /* Eq */Block.__(0, [
                                                                                                                                  /* tuple */[
                                                                                                                                    36,
                                                                                                                                    420
                                                                                                                                  ],
                                                                                                                                  /* tuple */[
                                                                                                                                    d.getUTCSeconds(),
                                                                                                                                    d.getUTCMilliseconds()
                                                                                                                                  ]
                                                                                                                                ]);
                                                                                                                      })
                                                                                                                  ],
                                                                                                                  /* :: */[
                                                                                                                    /* tuple */[
                                                                                                                      "toDateString",
                                                                                                                      (function () {
                                                                                                                          return /* Eq */Block.__(0, [
                                                                                                                                    "Mon Mar 08 1976",
                                                                                                                                    new Date("1976-03-08T12:34:56.789+01:23").toDateString()
                                                                                                                                  ]);
                                                                                                                        })
                                                                                                                    ],
                                                                                                                    /* :: */[
                                                                                                                      /* tuple */[
                                                                                                                        "toGMTString",
                                                                                                                        (function () {
                                                                                                                            return /* Eq */Block.__(0, [
                                                                                                                                      "Mon, 08 Mar 1976 11:11:56 GMT",
                                                                                                                                      new Date("1976-03-08T12:34:56.789+01:23").toGMTString()
                                                                                                                                    ]);
                                                                                                                          })
                                                                                                                      ],
                                                                                                                      /* :: */[
                                                                                                                        /* tuple */[
                                                                                                                          "toISOString",
                                                                                                                          (function () {
                                                                                                                              return /* Eq */Block.__(0, [
                                                                                                                                        "1976-03-08T11:11:56.789Z",
                                                                                                                                        new Date("1976-03-08T12:34:56.789+01:23").toISOString()
                                                                                                                                      ]);
                                                                                                                            })
                                                                                                                        ],
                                                                                                                        /* :: */[
                                                                                                                          /* tuple */[
                                                                                                                            "toJSON",
                                                                                                                            (function () {
                                                                                                                                return /* Eq */Block.__(0, [
                                                                                                                                          "1976-03-08T11:11:56.789Z",
                                                                                                                                          new Date("1976-03-08T12:34:56.789+01:23").toJSON()
                                                                                                                                        ]);
                                                                                                                              })
                                                                                                                          ],
                                                                                                                          /* :: */[
                                                                                                                            /* tuple */[
                                                                                                                              "toUTCString",
                                                                                                                              (function () {
                                                                                                                                  return /* Eq */Block.__(0, [
                                                                                                                                            "Mon, 08 Mar 1976 11:11:56 GMT",
                                                                                                                                            new Date("1976-03-08T12:34:56.789+01:23").toUTCString()
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
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_date_test.ml", suites);

exports.date = date;
exports.suites = suites;
/*  Not a pure module */
