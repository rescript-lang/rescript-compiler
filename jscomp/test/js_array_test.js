'use strict';

var Mt    = require("./mt");
var Block = require("../../lib/js/block");

var suites_000 = /* tuple */[
  "length",
  function () {
    return /* Eq */Block.__(0, [
              3,
              /* int array */[
                1,
                2,
                3
              ].length
            ]);
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "pop",
    function () {
      return /* Eq */Block.__(0, [
                3,
                /* int array */[
                    1,
                    2,
                    3
                  ].pop()
              ]);
    }
  ],
  /* :: */[
    /* tuple */[
      "push",
      function () {
        return /* Eq */Block.__(0, [
                  4,
                  /* int array */[
                      1,
                      2,
                      3
                    ].push(4)
                ]);
      }
    ],
    /* :: */[
      /* tuple */[
        "pushMany",
        function () {
          return /* Eq */Block.__(0, [
                    5,
                    /* int array */[
                        1,
                        2,
                        3
                      ].push(4, 5)
                  ]);
        }
      ],
      /* :: */[
        /* tuple */[
          "reverseInPlace",
          function () {
            return /* Eq */Block.__(0, [
                      /* int array */[
                        3,
                        2,
                        1
                      ],
                      /* int array */[
                          1,
                          2,
                          3
                        ].reverse()
                    ]);
          }
        ],
        /* :: */[
          /* tuple */[
            "shift",
            function () {
              return /* Eq */Block.__(0, [
                        1,
                        /* int array */[
                            1,
                            2,
                            3
                          ].shift()
                      ]);
            }
          ],
          /* :: */[
            /* tuple */[
              "sortInPlace",
              function () {
                return /* Eq */Block.__(0, [
                          /* int array */[
                            1,
                            2,
                            3
                          ],
                          /* int array */[
                              3,
                              1,
                              2
                            ].sort()
                        ]);
              }
            ],
            /* :: */[
              /* tuple */[
                "sortInPlaceWith",
                function () {
                  return /* Eq */Block.__(0, [
                            /* int array */[
                              3,
                              2,
                              1
                            ],
                            /* int array */[
                                3,
                                1,
                                2
                              ].sort(function (a, b) {
                                  return b - a | 0;
                                })
                          ]);
                }
              ],
              /* :: */[
                /* tuple */[
                  "spliceInPlace",
                  function () {
                    var arr = /* int array */[
                      1,
                      2,
                      3,
                      4
                    ];
                    var removed = arr.splice(2, 0, 5);
                    return /* Eq */Block.__(0, [
                              /* tuple */[
                                /* array */[
                                  1,
                                  2,
                                  5,
                                  3,
                                  4
                                ],
                                /* int array */[]
                              ],
                              /* tuple */[
                                arr,
                                removed
                              ]
                            ]);
                  }
                ],
                /* :: */[
                  /* tuple */[
                    "removeFromInPlace",
                    function () {
                      var arr = /* int array */[
                        1,
                        2,
                        3,
                        4
                      ];
                      var removed = arr.splice(2);
                      return /* Eq */Block.__(0, [
                                /* tuple */[
                                  /* int array */[
                                    1,
                                    2
                                  ],
                                  /* int array */[
                                    3,
                                    4
                                  ]
                                ],
                                /* tuple */[
                                  arr,
                                  removed
                                ]
                              ]);
                    }
                  ],
                  /* :: */[
                    /* tuple */[
                      "removeCountInPlace",
                      function () {
                        var arr = /* int array */[
                          1,
                          2,
                          3,
                          4
                        ];
                        var removed = arr.splice(2, 1);
                        return /* Eq */Block.__(0, [
                                  /* tuple */[
                                    /* int array */[
                                      1,
                                      2,
                                      4
                                    ],
                                    /* int array */[3]
                                  ],
                                  /* tuple */[
                                    arr,
                                    removed
                                  ]
                                ]);
                      }
                    ],
                    /* :: */[
                      /* tuple */[
                        "unshift",
                        function () {
                          return /* Eq */Block.__(0, [
                                    4,
                                    /* int array */[
                                        1,
                                        2,
                                        3
                                      ].unshift(4)
                                  ]);
                        }
                      ],
                      /* :: */[
                        /* tuple */[
                          "unshiftMany",
                          function () {
                            return /* Eq */Block.__(0, [
                                      5,
                                      /* int array */[
                                          1,
                                          2,
                                          3
                                        ].unshift(4, 5)
                                    ]);
                          }
                        ],
                        /* :: */[
                          /* tuple */[
                            "append",
                            function () {
                              return /* Eq */Block.__(0, [
                                        /* int array */[
                                          1,
                                          2,
                                          3,
                                          4
                                        ],
                                        /* int array */[
                                            1,
                                            2,
                                            3
                                          ].concat(4)
                                      ]);
                            }
                          ],
                          /* :: */[
                            /* tuple */[
                              "concat",
                              function () {
                                return /* Eq */Block.__(0, [
                                          /* array */[
                                            1,
                                            2,
                                            3,
                                            4,
                                            5
                                          ],
                                          /* int array */[
                                              1,
                                              2,
                                              3
                                            ].concat(/* int array */[
                                                4,
                                                5
                                              ])
                                        ]);
                              }
                            ],
                            /* :: */[
                              /* tuple */[
                                "concatMany",
                                function () {
                                  return /* Eq */Block.__(0, [
                                            /* array */[
                                              1,
                                              2,
                                              3,
                                              4,
                                              5,
                                              6,
                                              7
                                            ],
                                            /* int array */[
                                                1,
                                                2,
                                                3
                                              ].concat(/* int array */[
                                                  4,
                                                  5
                                                ], /* int array */[
                                                  6,
                                                  7
                                                ])
                                          ]);
                                }
                              ],
                              /* :: */[
                                /* tuple */[
                                  "indexOf",
                                  function () {
                                    return /* Eq */Block.__(0, [
                                              1,
                                              /* int array */[
                                                  1,
                                                  2,
                                                  3
                                                ].indexOf(2)
                                            ]);
                                  }
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "indexOfFrom",
                                    function () {
                                      return /* Eq */Block.__(0, [
                                                3,
                                                /* int array */[
                                                    1,
                                                    2,
                                                    3,
                                                    2
                                                  ].indexOf(2, 2)
                                              ]);
                                    }
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "join",
                                      function () {
                                        return /* Eq */Block.__(0, [
                                                  "1,2,3",
                                                  /* int array */[
                                                      1,
                                                      2,
                                                      3
                                                    ].join()
                                                ]);
                                      }
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "joinWith",
                                        function () {
                                          return /* Eq */Block.__(0, [
                                                    "1;2;3",
                                                    /* int array */[
                                                        1,
                                                        2,
                                                        3
                                                      ].join(";")
                                                  ]);
                                        }
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "lastIndexOf",
                                          function () {
                                            return /* Eq */Block.__(0, [
                                                      1,
                                                      /* int array */[
                                                          1,
                                                          2,
                                                          3
                                                        ].lastIndexOf(2)
                                                    ]);
                                          }
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "lastIndexOfFrom",
                                            function () {
                                              return /* Eq */Block.__(0, [
                                                        1,
                                                        /* int array */[
                                                            1,
                                                            2,
                                                            3,
                                                            2
                                                          ].lastIndexOf(2, 2)
                                                      ]);
                                            }
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "slice",
                                              function () {
                                                return /* Eq */Block.__(0, [
                                                          /* int array */[
                                                            2,
                                                            3
                                                          ],
                                                          /* array */[
                                                              1,
                                                              2,
                                                              3,
                                                              4,
                                                              5
                                                            ].slice(1, 3)
                                                        ]);
                                              }
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "copy",
                                                function () {
                                                  return /* Eq */Block.__(0, [
                                                            /* array */[
                                                              1,
                                                              2,
                                                              3,
                                                              4,
                                                              5
                                                            ],
                                                            /* array */[
                                                                1,
                                                                2,
                                                                3,
                                                                4,
                                                                5
                                                              ].slice()
                                                          ]);
                                                }
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "sliceFrom",
                                                  function () {
                                                    return /* Eq */Block.__(0, [
                                                              /* int array */[
                                                                3,
                                                                4,
                                                                5
                                                              ],
                                                              /* array */[
                                                                  1,
                                                                  2,
                                                                  3,
                                                                  4,
                                                                  5
                                                                ].slice(2)
                                                            ]);
                                                  }
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "toString",
                                                    function () {
                                                      return /* Eq */Block.__(0, [
                                                                "1,2,3",
                                                                /* int array */[
                                                                    1,
                                                                    2,
                                                                    3
                                                                  ].toString()
                                                              ]);
                                                    }
                                                  ],
                                                  /* :: */[
                                                    /* tuple */[
                                                      "toLocaleString",
                                                      function () {
                                                        return /* Eq */Block.__(0, [
                                                                  "1,2,3",
                                                                  /* int array */[
                                                                      1,
                                                                      2,
                                                                      3
                                                                    ].toLocaleString()
                                                                ]);
                                                      }
                                                    ],
                                                    /* :: */[
                                                      /* tuple */[
                                                        "every",
                                                        function () {
                                                          return /* Eq */Block.__(0, [
                                                                    true,
                                                                    /* int array */[
                                                                        1,
                                                                        2,
                                                                        3
                                                                      ].every(function (n) {
                                                                          var b = +(n > 0);
                                                                          if (b) {
                                                                            return true;
                                                                          }
                                                                          else {
                                                                            return false;
                                                                          }
                                                                        })
                                                                  ]);
                                                        }
                                                      ],
                                                      /* :: */[
                                                        /* tuple */[
                                                          "everyi",
                                                          function () {
                                                            return /* Eq */Block.__(0, [
                                                                      false,
                                                                      /* int array */[
                                                                          1,
                                                                          2,
                                                                          3
                                                                        ].every(function (_, i) {
                                                                            var b = +(i > 0);
                                                                            if (b) {
                                                                              return true;
                                                                            }
                                                                            else {
                                                                              return false;
                                                                            }
                                                                          })
                                                                    ]);
                                                          }
                                                        ],
                                                        /* :: */[
                                                          /* tuple */[
                                                            "filter",
                                                            function () {
                                                              return /* Eq */Block.__(0, [
                                                                        /* int array */[
                                                                          2,
                                                                          4
                                                                        ],
                                                                        /* int array */[
                                                                            1,
                                                                            2,
                                                                            3,
                                                                            4
                                                                          ].filter(function (n) {
                                                                              return +(n % 2 === 0);
                                                                            })
                                                                      ]);
                                                            }
                                                          ],
                                                          /* :: */[
                                                            /* tuple */[
                                                              "filteri",
                                                              function () {
                                                                return /* Eq */Block.__(0, [
                                                                          /* int array */[
                                                                            1,
                                                                            3
                                                                          ],
                                                                          /* int array */[
                                                                              1,
                                                                              2,
                                                                              3,
                                                                              4
                                                                            ].filter(function (_, i) {
                                                                                var b = +(i % 2 === 0);
                                                                                if (b) {
                                                                                  return true;
                                                                                }
                                                                                else {
                                                                                  return false;
                                                                                }
                                                                              })
                                                                        ]);
                                                              }
                                                            ],
                                                            /* :: */[
                                                              /* tuple */[
                                                                "forEach",
                                                                function () {
                                                                  var sum = [0];
                                                                  /* int array */[
                                                                      1,
                                                                      2,
                                                                      3
                                                                    ].forEach(function (n) {
                                                                        sum[0] = sum[0] + n | 0;
                                                                        return /* () */0;
                                                                      });
                                                                  return /* Eq */Block.__(0, [
                                                                            6,
                                                                            sum[0]
                                                                          ]);
                                                                }
                                                              ],
                                                              /* :: */[
                                                                /* tuple */[
                                                                  "forEachi",
                                                                  function () {
                                                                    var sum = [0];
                                                                    /* int array */[
                                                                        1,
                                                                        2,
                                                                        3
                                                                      ].forEach(function (_, i) {
                                                                          sum[0] = sum[0] + i | 0;
                                                                          return /* () */0;
                                                                        });
                                                                    return /* Eq */Block.__(0, [
                                                                              3,
                                                                              sum[0]
                                                                            ]);
                                                                  }
                                                                ],
                                                                /* :: */[
                                                                  /* tuple */[
                                                                    "map",
                                                                    function () {
                                                                      return /* Eq */Block.__(0, [
                                                                                /* int array */[
                                                                                  2,
                                                                                  4,
                                                                                  6,
                                                                                  8
                                                                                ],
                                                                                /* int array */[
                                                                                    1,
                                                                                    2,
                                                                                    3,
                                                                                    4
                                                                                  ].map(function (n) {
                                                                                      return (n << 1);
                                                                                    })
                                                                              ]);
                                                                    }
                                                                  ],
                                                                  /* :: */[
                                                                    /* tuple */[
                                                                      "map",
                                                                      function () {
                                                                        return /* Eq */Block.__(0, [
                                                                                  /* int array */[
                                                                                    0,
                                                                                    2,
                                                                                    4,
                                                                                    6
                                                                                  ],
                                                                                  /* int array */[
                                                                                      1,
                                                                                      2,
                                                                                      3,
                                                                                      4
                                                                                    ].map(function (_, i) {
                                                                                        return (i << 1);
                                                                                      })
                                                                                ]);
                                                                      }
                                                                    ],
                                                                    /* :: */[
                                                                      /* tuple */[
                                                                        "reduce",
                                                                        function () {
                                                                          return /* Eq */Block.__(0, [
                                                                                    -10,
                                                                                    /* int array */[
                                                                                        1,
                                                                                        2,
                                                                                        3,
                                                                                        4
                                                                                      ].reduce(function (acc, n) {
                                                                                          return acc - n | 0;
                                                                                        }, 0)
                                                                                  ]);
                                                                        }
                                                                      ],
                                                                      /* :: */[
                                                                        /* tuple */[
                                                                          "reducei",
                                                                          function () {
                                                                            return /* Eq */Block.__(0, [
                                                                                      -6,
                                                                                      /* int array */[
                                                                                          1,
                                                                                          2,
                                                                                          3,
                                                                                          4
                                                                                        ].reduce(function (acc, _, i) {
                                                                                            return acc - i | 0;
                                                                                          }, 0)
                                                                                    ]);
                                                                          }
                                                                        ],
                                                                        /* :: */[
                                                                          /* tuple */[
                                                                            "reduceRight",
                                                                            function () {
                                                                              return /* Eq */Block.__(0, [
                                                                                        -10,
                                                                                        /* int array */[
                                                                                            1,
                                                                                            2,
                                                                                            3,
                                                                                            4
                                                                                          ].reduceRight(function (acc, n) {
                                                                                              return acc - n | 0;
                                                                                            }, 0)
                                                                                      ]);
                                                                            }
                                                                          ],
                                                                          /* :: */[
                                                                            /* tuple */[
                                                                              "reduceRighti",
                                                                              function () {
                                                                                return /* Eq */Block.__(0, [
                                                                                          -6,
                                                                                          /* int array */[
                                                                                              1,
                                                                                              2,
                                                                                              3,
                                                                                              4
                                                                                            ].reduceRight(function (acc, _, i) {
                                                                                                return acc - i | 0;
                                                                                              }, 0)
                                                                                        ]);
                                                                              }
                                                                            ],
                                                                            /* :: */[
                                                                              /* tuple */[
                                                                                "some",
                                                                                function () {
                                                                                  return /* Eq */Block.__(0, [
                                                                                            false,
                                                                                            /* int array */[
                                                                                                1,
                                                                                                2,
                                                                                                3,
                                                                                                4
                                                                                              ].some(function (n) {
                                                                                                  var b = +(n <= 0);
                                                                                                  if (b) {
                                                                                                    return true;
                                                                                                  }
                                                                                                  else {
                                                                                                    return false;
                                                                                                  }
                                                                                                })
                                                                                          ]);
                                                                                }
                                                                              ],
                                                                              /* :: */[
                                                                                /* tuple */[
                                                                                  "somei",
                                                                                  function () {
                                                                                    return /* Eq */Block.__(0, [
                                                                                              true,
                                                                                              /* int array */[
                                                                                                  1,
                                                                                                  2,
                                                                                                  3,
                                                                                                  4
                                                                                                ].some(function (_, i) {
                                                                                                    var b = +(i <= 0);
                                                                                                    if (b) {
                                                                                                      return true;
                                                                                                    }
                                                                                                    else {
                                                                                                      return false;
                                                                                                    }
                                                                                                  })
                                                                                            ]);
                                                                                  }
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
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_array_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
