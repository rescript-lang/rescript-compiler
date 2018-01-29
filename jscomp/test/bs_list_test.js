'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Bs_List = require("../../lib/js/bs_List.js");
var Bs_Array = require("../../lib/js/bs_Array.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

function b(loc, x) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Ok */Block.__(4, [x]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

function sum(xs) {
  var v = [0];
  Bs_List.forEach(xs, (function (x) {
          v[0] = v[0] + x | 0;
          return /* () */0;
        }));
  return v[0];
}

function sum2(xs, ys) {
  var v = [0];
  Bs_List.forEach2(xs, ys, (function (x, y) {
          v[0] = (v[0] + x | 0) + y | 0;
          return /* () */0;
        }));
  return v[0];
}

var u = Bs_List.makeBy(5, (function (i) {
        return Caml_int32.imul(i, i);
      }));

function f(i) {
  return eq("File \"bs_list_test.ml\", line 32, characters 7-14", Bs_List.getExn(u, i), Caml_int32.imul(i, i));
}

for(var i = 0; i <= 4; ++i){
  f(i);
}

eq("File \"bs_list_test.ml\", line 36, characters 5-12", Bs_List.map(u, (function (i) {
            return i + 1 | 0;
          })), /* :: */[
      1,
      /* :: */[
        2,
        /* :: */[
          5,
          /* :: */[
            10,
            /* :: */[
              17,
              /* [] */0
            ]
          ]
        ]
      ]
    ]);

eq("FLATTEN", Bs_List.concatMany(/* :: */[
          /* :: */[
            1,
            /* [] */0
          ],
          /* :: */[
            /* :: */[
              2,
              /* [] */0
            ],
            /* :: */[
              /* :: */[
                3,
                /* [] */0
              ],
              /* :: */[
                /* [] */0,
                /* :: */[
                  Bs_List.makeBy(4, (function (i) {
                          return i;
                        })),
                  /* [] */0
                ]
              ]
            ]
          ]
        ]), /* :: */[
      1,
      /* :: */[
        2,
        /* :: */[
          3,
          /* :: */[
            0,
            /* :: */[
              1,
              /* :: */[
                2,
                /* :: */[
                  3,
                  /* [] */0
                ]
              ]
            ]
          ]
        ]
      ]
    ]);

eq("FLATTEN", Bs_List.concatMany(/* [] */0), /* [] */0);

eq("FLATTEN", Bs_List.concatMany(/* :: */[
          /* [] */0,
          /* :: */[
            /* [] */0,
            /* :: */[
              /* :: */[
                2,
                /* [] */0
              ],
              /* :: */[
                /* :: */[
                  1,
                  /* [] */0
                ],
                /* :: */[
                  /* :: */[
                    2,
                    /* [] */0
                  ],
                  /* :: */[
                    /* [] */0,
                    /* [] */0
                  ]
                ]
              ]
            ]
          ]
        ]), /* :: */[
      2,
      /* :: */[
        1,
        /* :: */[
          2,
          /* [] */0
        ]
      ]
    ]);

eq("File \"bs_list_test.ml\", line 50, characters 5-12", Bs_List.toArray(Bs_List.concat(Bs_List.makeBy(100, (function (i) {
                    return i;
                  })), Bs_List.makeBy(100, (function (i) {
                    return i;
                  })))), Bs_Array.concat(Bs_Array.makeBy(100, (function (i) {
                return i;
              })), Bs_Array.makeBy(100, (function (i) {
                return i;
              }))));

eq("APPEND", Bs_List.concat(/* :: */[
          1,
          /* [] */0
        ], /* [] */0), /* :: */[
      1,
      /* [] */0
    ]);

eq("APPEND", Bs_List.concat(/* [] */0, /* :: */[
          1,
          /* [] */0
        ]), /* :: */[
      1,
      /* [] */0
    ]);

eq("ZIP", Bs_List.zip(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* :: */[
          3,
          /* :: */[
            4,
            /* [] */0
          ]
        ]), /* :: */[
      /* tuple */[
        1,
        3
      ],
      /* :: */[
        /* tuple */[
          2,
          4
        ],
        /* [] */0
      ]
    ]);

eq("ZIP", Bs_List.zip(/* [] */0, /* :: */[
          1,
          /* [] */0
        ]), /* [] */0);

eq("ZIP", Bs_List.zip(/* [] */0, /* [] */0), /* [] */0);

eq("ZIP", Bs_List.zip(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* [] */0), /* [] */0);

eq("ZIP", Bs_List.zip(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* :: */[
          2,
          /* :: */[
            3,
            /* :: */[
              4,
              /* [] */0
            ]
          ]
        ]), /* :: */[
      /* tuple */[
        1,
        2
      ],
      /* :: */[
        /* tuple */[
          2,
          3
        ],
        /* :: */[
          /* tuple */[
            3,
            4
          ],
          /* [] */0
        ]
      ]
    ]);

function mod2(x) {
  return +(x % 2 === 0);
}

eq("PARTITION", Bs_List.partition(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                2,
                /* :: */[
                  3,
                  /* :: */[
                    4,
                    /* [] */0
                  ]
                ]
              ]
            ]
          ]
        ], mod2), /* tuple */[
      /* :: */[
        2,
        /* :: */[
          2,
          /* :: */[
            4,
            /* [] */0
          ]
        ]
      ],
      /* :: */[
        1,
        /* :: */[
          3,
          /* :: */[
            3,
            /* [] */0
          ]
        ]
      ]
    ]);

eq("PARTITION", Bs_List.partition(/* :: */[
          2,
          /* :: */[
            2,
            /* :: */[
              2,
              /* :: */[
                4,
                /* [] */0
              ]
            ]
          ]
        ], mod2), /* tuple */[
      /* :: */[
        2,
        /* :: */[
          2,
          /* :: */[
            2,
            /* :: */[
              4,
              /* [] */0
            ]
          ]
        ]
      ],
      /* [] */0
    ]);

eq("PARTITION", Bs_List.partition(/* :: */[
          2,
          /* :: */[
            2,
            /* :: */[
              2,
              /* :: */[
                4,
                /* [] */0
              ]
            ]
          ]
        ], (function (x) {
            return 1 - mod2(x);
          })), /* tuple */[
      /* [] */0,
      /* :: */[
        2,
        /* :: */[
          2,
          /* :: */[
            2,
            /* :: */[
              4,
              /* [] */0
            ]
          ]
        ]
      ]
    ]);

eq("PARTITION", Bs_List.partition(/* [] */0, mod2), /* tuple */[
      /* [] */0,
      /* [] */0
    ]);

eq("UNZIP", Bs_List.unzip(/* [] */0), /* tuple */[
      /* [] */0,
      /* [] */0
    ]);

eq("UNZIP", Bs_List.unzip(/* :: */[
          /* tuple */[
            1,
            2
          ],
          /* [] */0
        ]), /* tuple */[
      /* :: */[
        1,
        /* [] */0
      ],
      /* :: */[
        2,
        /* [] */0
      ]
    ]);

eq("UNZIP", Bs_List.unzip(/* :: */[
          /* tuple */[
            1,
            2
          ],
          /* :: */[
            /* tuple */[
              3,
              4
            ],
            /* [] */0
          ]
        ]), /* tuple */[
      /* :: */[
        1,
        /* :: */[
          3,
          /* [] */0
        ]
      ],
      /* :: */[
        2,
        /* :: */[
          4,
          /* [] */0
        ]
      ]
    ]);

eq("FILTER", Bs_List.keepBy(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                4,
                /* [] */0
              ]
            ]
          ]
        ], mod2), /* :: */[
      2,
      /* :: */[
        4,
        /* [] */0
      ]
    ]);

eq("FILTER", Bs_List.keepBy(/* :: */[
          1,
          /* :: */[
            3,
            /* :: */[
              41,
              /* [] */0
            ]
          ]
        ], mod2), /* [] */0);

eq("FILTER", Bs_List.keepBy(/* [] */0, mod2), /* [] */0);

eq("FILTER", Bs_List.keepBy(/* :: */[
          2,
          /* :: */[
            2,
            /* :: */[
              2,
              /* :: */[
                4,
                /* :: */[
                  6,
                  /* [] */0
                ]
              ]
            ]
          ]
        ], mod2), /* :: */[
      2,
      /* :: */[
        2,
        /* :: */[
          2,
          /* :: */[
            4,
            /* :: */[
              6,
              /* [] */0
            ]
          ]
        ]
      ]
    ]);

function id(x) {
  return x;
}

eq("MAP", Bs_List.map(Bs_List.makeBy(5, id), (function (x) {
            return (x << 1);
          })), /* :: */[
      0,
      /* :: */[
        2,
        /* :: */[
          4,
          /* :: */[
            6,
            /* :: */[
              8,
              /* [] */0
            ]
          ]
        ]
      ]
    ]);

eq("MAP", Bs_List.map(/* [] */0, id), /* [] */0);

eq("MAP", Bs_List.map(/* :: */[
          1,
          /* [] */0
        ], (function (x) {
            return -x | 0;
          })), /* :: */[
      -1,
      /* [] */0
    ]);

function add(a, b) {
  return a + b | 0;
}

var length_10_id = Bs_List.makeBy(10, id);

var length_8_id = Bs_List.makeBy(8, id);

var d = Bs_List.makeBy(10, (function (x) {
        return (x << 1);
      }));

eq("MAP2", Bs_List.zipBy(length_10_id, length_10_id, add), d);

eq("MAP2", Bs_List.zipBy(/* [] */0, /* :: */[
          1,
          /* [] */0
        ], add), /* [] */0);

eq("MAP2", Bs_List.zipBy(/* :: */[
          1,
          /* [] */0
        ], /* [] */0, add), /* [] */0);

eq("MAP2", Bs_List.zipBy(/* [] */0, /* [] */0, add), /* [] */0);

eq("MAP2", Bs_List.zipBy(length_10_id, length_10_id, add), Bs_List.concat(Bs_List.map(length_8_id, (function (x) {
                return (x << 1);
              })), /* :: */[
          16,
          /* :: */[
            18,
            /* [] */0
          ]
        ]));

eq("MAP2", Bs_List.zipBy(length_10_id, length_8_id, add), Bs_List.mapWithIndex(length_8_id, (function (i, x) {
            return i + x | 0;
          })));

eq("MAP2", Bs_List.reverse(Bs_List.mapReverse2(length_10_id, length_10_id, add)), Bs_List.map(length_10_id, (function (x) {
            return (x << 1);
          })));

var xs = Bs_List.reverse(Bs_List.mapReverse2(length_8_id, length_10_id, add));

eq("File \"bs_list_test.ml\", line 127, characters 5-12", Bs_List.length(xs), 8);

eq("MAP2", xs, Bs_List.zipBy(length_10_id, length_8_id, add));

eq("TAKE", Bs_List.take(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], 2), /* Some */[/* :: */[
        1,
        /* :: */[
          2,
          /* [] */0
        ]
      ]]);

eq("TAKE", Bs_List.take(/* [] */0, 1), /* None */0);

eq("TAKE", Bs_List.take(/* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ], 3), /* None */0);

eq("TAKE", Bs_List.take(/* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ], 2), /* Some */[/* :: */[
        1,
        /* :: */[
          2,
          /* [] */0
        ]
      ]]);

eq("TAKE", Bs_List.take(length_10_id, 8), /* Some */[length_8_id]);

eq("TAKE", Bs_List.take(length_10_id, 0), /* Some */[/* [] */0]);

eq("TAKE", Bs_List.take(length_8_id, -2), /* None */0);

eq("DROP", Bs_List.drop(length_10_id, 10), /* Some */[/* [] */0]);

eq("DROP", Bs_List.drop(length_10_id, 8), /* Some */[/* :: */[
        8,
        /* :: */[
          9,
          /* [] */0
        ]
      ]]);

eq("DROP", Bs_List.drop(length_10_id, 0), /* Some */[length_10_id]);

eq("DROP", Bs_List.drop(length_8_id, -1), /* None */0);

var a = Bs_List.makeBy(5, id);

eq("SPLIT", Bs_List.splitAt(/* [] */0, 1), /* None */0);

eq("SPLIT", Bs_List.splitAt(a, 6), /* None */0);

eq("SPLIT", Bs_List.splitAt(a, 5), /* Some */[/* tuple */[
        a,
        /* [] */0
      ]]);

eq("SPLIT", Bs_List.splitAt(a, 4), /* Some */[/* tuple */[
        /* :: */[
          0,
          /* :: */[
            1,
            /* :: */[
              2,
              /* :: */[
                3,
                /* [] */0
              ]
            ]
          ]
        ],
        /* :: */[
          4,
          /* [] */0
        ]
      ]]);

eq("SPLIT", Bs_List.splitAt(a, 3), /* Some */[/* tuple */[
        /* :: */[
          0,
          /* :: */[
            1,
            /* :: */[
              2,
              /* [] */0
            ]
          ]
        ],
        /* :: */[
          3,
          /* :: */[
            4,
            /* [] */0
          ]
        ]
      ]]);

eq("SPLIT", Bs_List.splitAt(a, 2), /* Some */[/* tuple */[
        /* :: */[
          0,
          /* :: */[
            1,
            /* [] */0
          ]
        ],
        /* :: */[
          2,
          /* :: */[
            3,
            /* :: */[
              4,
              /* [] */0
            ]
          ]
        ]
      ]]);

eq("SPLIT", Bs_List.splitAt(a, 1), /* Some */[/* tuple */[
        /* :: */[
          0,
          /* [] */0
        ],
        /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                4,
                /* [] */0
              ]
            ]
          ]
        ]
      ]]);

eq("SPLIT", Bs_List.splitAt(a, 0), /* Some */[/* tuple */[
        /* [] */0,
        a
      ]]);

eq("SPLIT", Bs_List.splitAt(a, -1), /* None */0);

function succx(x) {
  return x + 1 | 0;
}

function eq$1(x, y) {
  return +(x === y);
}

eq("REMOVEASSOQ", Bs_List.removeAssocByReference(/* :: */[
          /* tuple */[
            1,
            "1"
          ],
          /* :: */[
            /* tuple */[
              2,
              "2"
            ],
            /* :: */[
              /* tuple */[
                3,
                "3"
              ],
              /* [] */0
            ]
          ]
        ], 3), /* :: */[
      /* tuple */[
        1,
        "1"
      ],
      /* :: */[
        /* tuple */[
          2,
          "2"
        ],
        /* [] */0
      ]
    ]);

eq("REMOVEASSOQ", Bs_List.removeAssocByReference(/* :: */[
          /* tuple */[
            1,
            "1"
          ],
          /* :: */[
            /* tuple */[
              2,
              "2"
            ],
            /* :: */[
              /* tuple */[
                3,
                "3"
              ],
              /* [] */0
            ]
          ]
        ], 1), /* :: */[
      /* tuple */[
        2,
        "2"
      ],
      /* :: */[
        /* tuple */[
          3,
          "3"
        ],
        /* [] */0
      ]
    ]);

eq("REMOVEASSOQ", Bs_List.removeAssocByReference(/* :: */[
          /* tuple */[
            1,
            "1"
          ],
          /* :: */[
            /* tuple */[
              2,
              "2"
            ],
            /* :: */[
              /* tuple */[
                3,
                "3"
              ],
              /* [] */0
            ]
          ]
        ], 2), /* :: */[
      /* tuple */[
        1,
        "1"
      ],
      /* :: */[
        /* tuple */[
          3,
          "3"
        ],
        /* [] */0
      ]
    ]);

eq("REMOVEASSOQ", Bs_List.removeAssocByReference(/* :: */[
          /* tuple */[
            1,
            "1"
          ],
          /* :: */[
            /* tuple */[
              2,
              "2"
            ],
            /* :: */[
              /* tuple */[
                3,
                "3"
              ],
              /* [] */0
            ]
          ]
        ], 0), /* :: */[
      /* tuple */[
        1,
        "1"
      ],
      /* :: */[
        /* tuple */[
          2,
          "2"
        ],
        /* :: */[
          /* tuple */[
            3,
            "3"
          ],
          /* [] */0
        ]
      ]
    ]);

eq("REMOVEASSOQ", Bs_List.removeAssoc(/* :: */[
          /* tuple */[
            1,
            "1"
          ],
          /* :: */[
            /* tuple */[
              2,
              "2"
            ],
            /* :: */[
              /* tuple */[
                3,
                "3"
              ],
              /* [] */0
            ]
          ]
        ], 3, eq$1), /* :: */[
      /* tuple */[
        1,
        "1"
      ],
      /* :: */[
        /* tuple */[
          2,
          "2"
        ],
        /* [] */0
      ]
    ]);

eq("REMOVEASSOQ", Bs_List.removeAssoc(/* :: */[
          /* tuple */[
            1,
            "1"
          ],
          /* :: */[
            /* tuple */[
              2,
              "2"
            ],
            /* :: */[
              /* tuple */[
                3,
                "3"
              ],
              /* [] */0
            ]
          ]
        ], 1, eq$1), /* :: */[
      /* tuple */[
        2,
        "2"
      ],
      /* :: */[
        /* tuple */[
          3,
          "3"
        ],
        /* [] */0
      ]
    ]);

eq("REMOVEASSOQ", Bs_List.removeAssoc(/* :: */[
          /* tuple */[
            1,
            "1"
          ],
          /* :: */[
            /* tuple */[
              2,
              "2"
            ],
            /* :: */[
              /* tuple */[
                3,
                "3"
              ],
              /* [] */0
            ]
          ]
        ], 2, eq$1), /* :: */[
      /* tuple */[
        1,
        "1"
      ],
      /* :: */[
        /* tuple */[
          3,
          "3"
        ],
        /* [] */0
      ]
    ]);

eq("REMOVEASSOQ", Bs_List.removeAssoc(/* :: */[
          /* tuple */[
            1,
            "1"
          ],
          /* :: */[
            /* tuple */[
              2,
              "2"
            ],
            /* :: */[
              /* tuple */[
                3,
                "3"
              ],
              /* [] */0
            ]
          ]
        ], 0, eq$1), /* :: */[
      /* tuple */[
        1,
        "1"
      ],
      /* :: */[
        /* tuple */[
          2,
          "2"
        ],
        /* :: */[
          /* tuple */[
            3,
            "3"
          ],
          /* [] */0
        ]
      ]
    ]);

eq("File \"bs_list_test.ml\", line 176, characters 5-12", /* tuple */[
      Bs_List.head(length_10_id),
      Bs_List.tail(length_10_id)
    ], /* tuple */[
      /* Some */[0],
      Bs_List.drop(length_10_id, 1)
    ]);

eq("File \"bs_list_test.ml\", line 177, characters 5-12", Bs_List.head(/* [] */0), /* None */0);

Bs_List.forEachWithIndex(length_10_id, (function (i, x) {
        return eq("File \"bs_list_test.ml\", line 179, characters 9-16", Bs_List.get(length_10_id, i), /* Some */[x]);
      }));

eq("File \"bs_list_test.ml\", line 180, characters 5-12", Bs_List.tail(/* [] */0), /* None */0);

eq("File \"bs_list_test.ml\", line 181, characters 5-12", Bs_List.drop(/* [] */0, 3), /* None */0);

eq("File \"bs_list_test.ml\", line 182, characters 5-12", Bs_List.mapWithIndex(/* [] */0, (function (i, x) {
            return i + x | 0;
          })), /* [] */0);

eq("File \"bs_list_test.ml\", line 183, characters 5-12", Bs_List.get(length_10_id, -1), /* None */0);

eq("File \"bs_list_test.ml\", line 184, characters 5-12", Bs_List.get(length_10_id, 12), /* None */0);

eq("File \"bs_list_test.ml\", line 185, characters 5-12", sum(/* [] */0), 0);

eq("File \"bs_list_test.ml\", line 186, characters 5-12", sum(length_10_id), 45);

eq("File \"bs_list_test.ml\", line 187, characters 5-12", Bs_List.makeBy(0, id), /* [] */0);

eq("File \"bs_list_test.ml\", line 188, characters 5-12", Bs_List.reverse(Bs_List.reverse(length_10_id)), length_10_id);

eq("File \"bs_list_test.ml\", line 189, characters 5-12", Bs_List.reverse(Bs_List.reverse(length_8_id)), length_8_id);

eq("File \"bs_list_test.ml\", line 190, characters 5-12", Bs_List.reverse(/* [] */0), /* [] */0);

eq("File \"bs_list_test.ml\", line 191, characters 5-12", Bs_List.reverse(Bs_List.mapReverse(length_10_id, succx)), Bs_List.map(length_10_id, succx));

eq("File \"bs_list_test.ml\", line 194, characters 5-12", Bs_List.reduce(length_10_id, 0, add), 45);

eq("File \"bs_list_test.ml\", line 196, characters 5-12", Bs_List.reduceReverse(length_10_id, 0, add), 45);

eq("File \"bs_list_test.ml\", line 200, characters 5-12", sum2(length_10_id, length_10_id), 90);

eq("File \"bs_list_test.ml\", line 201, characters 5-12", sum2(length_8_id, length_10_id), 56);

eq("File \"bs_list_test.ml\", line 202, characters 5-12", Bs_List.reduce2(length_10_id, length_8_id, 0, (function (acc, x, y) {
            return (acc + x | 0) + y | 0;
          })), 56);

eq("File \"bs_list_test.ml\", line 204, characters 5-12", Bs_List.reduceReverse2(length_10_id, length_8_id, 0, (function (acc, x, y) {
            return (acc + x | 0) + y | 0;
          })), 56);

eq("File \"bs_list_test.ml\", line 206, characters 5-12", Bs_List.reduceReverse2(length_10_id, length_10_id, 0, (function (acc, x, y) {
            return (acc + x | 0) + y | 0;
          })), 90);

eq("File \"bs_list_test.ml\", line 208, characters 5-12", Bs_List.every(/* :: */[
          2,
          /* :: */[
            4,
            /* :: */[
              6,
              /* [] */0
            ]
          ]
        ], mod2), /* true */1);

eq("File \"bs_list_test.ml\", line 209, characters 5-12", Bs_List.every(/* :: */[
          1,
          /* [] */0
        ], mod2), /* false */0);

eq("File \"bs_list_test.ml\", line 210, characters 5-12", Bs_List.every(/* [] */0, mod2), /* true */1);

eq("File \"bs_list_test.ml\", line 211, characters 5-12", Bs_List.some(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              5,
              /* [] */0
            ]
          ]
        ], mod2), /* true */1);

eq("File \"bs_list_test.ml\", line 212, characters 5-12", Bs_List.some(/* :: */[
          1,
          /* :: */[
            3,
            /* :: */[
              5,
              /* [] */0
            ]
          ]
        ], mod2), /* false */0);

eq("File \"bs_list_test.ml\", line 213, characters 5-12", Bs_List.some(/* [] */0, mod2), /* false */0);

eq("File \"bs_list_test.ml\", line 214, characters 5-12", Bs_List.every2(/* [] */0, /* :: */[
          1,
          /* [] */0
        ], (function (x, y) {
            return +(x > y);
          })), /* true */1);

eq("File \"bs_list_test.ml\", line 215, characters 5-12", Bs_List.every2(/* :: */[
          2,
          /* [] */0
        ], /* :: */[
          1,
          /* [] */0
        ], (function (x, y) {
            return +(x > y);
          })), /* true */1);

eq("File \"bs_list_test.ml\", line 216, characters 5-12", Bs_List.every2(/* :: */[
          2,
          /* :: */[
            3,
            /* [] */0
          ]
        ], /* :: */[
          1,
          /* :: */[
            4,
            /* [] */0
          ]
        ], (function (x, y) {
            return +(x > y);
          })), /* false */0);

eq("File \"bs_list_test.ml\", line 217, characters 5-12", Bs_List.some2(/* [] */0, /* :: */[
          1,
          /* [] */0
        ], (function (x, y) {
            return +(x > y);
          })), /* false */0);

eq("File \"bs_list_test.ml\", line 218, characters 5-12", Bs_List.some2(/* :: */[
          2,
          /* :: */[
            3,
            /* [] */0
          ]
        ], /* :: */[
          1,
          /* :: */[
            4,
            /* [] */0
          ]
        ], (function (x, y) {
            return +(x > y);
          })), /* true */1);

eq("File \"bs_list_test.ml\", line 219, characters 5-12", Bs_List.some2(/* :: */[
          0,
          /* :: */[
            3,
            /* [] */0
          ]
        ], /* :: */[
          1,
          /* :: */[
            4,
            /* [] */0
          ]
        ], (function (x, y) {
            return +(x > y);
          })), /* false */0);

eq("File \"bs_list_test.ml\", line 220, characters 5-12", Bs_List.has(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], "2", (function (x, s) {
            return +("" + x === s);
          })), /* true */1);

eq("File \"bs_list_test.ml\", line 221, characters 5-12", Bs_List.has(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], "0", (function (x, s) {
            return +("" + x === s);
          })), /* false */0);

function makeTest(n) {
  return eq("File \"bs_list_test.ml\", line 224, characters 5-12", Bs_List.make(n, 3), Bs_List.makeBy(n, (function () {
                    return 3;
                  })));
}

makeTest(0);

makeTest(1);

makeTest(2);

makeTest(3);

var u0 = Bs_List.makeBy(20, (function (x) {
        return x;
      }));

var u1 = Bs_List.keepMap(u0, (function (x) {
        if (x % 7) {
          return /* None */0;
        } else {
          return /* Some */[x + 1 | 0];
        }
      }));

eq("File \"bs_list_test.ml\", line 235, characters 5-12", u1, /* :: */[
      1,
      /* :: */[
        8,
        /* :: */[
          15,
          /* [] */0
        ]
      ]
    ]);

Mt.from_pair_suites("bs_list_test.ml", suites[0]);

var N = 0;

var A = 0;

var J = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.N = N;
exports.A = A;
exports.J = J;
exports.sum = sum;
exports.sum2 = sum2;
exports.mod2 = mod2;
exports.id = id;
exports.add = add;
exports.length_10_id = length_10_id;
exports.length_8_id = length_8_id;
exports.succx = succx;
exports.makeTest = makeTest;
/* u Not a pure module */
