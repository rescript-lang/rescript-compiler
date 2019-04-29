'use strict';

var Mt = require("./mt.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Belt_List = require("../../lib/js/belt_List.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

function $$throw(loc, x) {
  return Mt.throw_suites(test_id, suites, loc, x);
}

function sum(xs) {
  var v = /* record */[/* contents */0];
  Belt_List.forEach(xs, (function (x) {
          v[0] = v[0] + x | 0;
          return /* () */0;
        }));
  return v[0];
}

function sum2(xs, ys) {
  var v = /* record */[/* contents */0];
  Belt_List.forEach2(xs, ys, (function (x, y) {
          v[0] = (v[0] + x | 0) + y | 0;
          return /* () */0;
        }));
  return v[0];
}

var u = Belt_List.makeBy(5, (function (i) {
        return Caml_int32.imul(i, i);
      }));

function f(i) {
  return eq("File \"bs_list_test.ml\", line 26, characters 7-14", Belt_List.getExn(u, i), Caml_int32.imul(i, i));
}

for(var i = 0; i <= 4; ++i){
  f(i);
}

eq("File \"bs_list_test.ml\", line 30, characters 5-12", Belt_List.map(u, (function (i) {
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

eq("File \"bs_list_test.ml\", line 31, characters 5-12", Belt_List.getBy(/* :: */[
          1,
          /* :: */[
            4,
            /* :: */[
              3,
              /* :: */[
                2,
                /* [] */0
              ]
            ]
          ]
        ], (function (x) {
            return x % 2 === 0;
          })), 4);

eq("File \"bs_list_test.ml\", line 32, characters 5-12", Belt_List.getBy(/* :: */[
          1,
          /* :: */[
            4,
            /* :: */[
              3,
              /* :: */[
                2,
                /* [] */0
              ]
            ]
          ]
        ], (function (x) {
            return x % 5 === 0;
          })), undefined);

eq("FLATTEN", Belt_List.flatten(/* :: */[
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
                  Belt_List.makeBy(4, (function (i) {
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

eq("FLATTEN", Belt_List.flatten(/* [] */0), /* [] */0);

eq("FLATTEN", Belt_List.flatten(/* :: */[
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

eq("CONCATMANY", Belt_List.concatMany(/* array */[
          /* :: */[
            1,
            /* [] */0
          ],
          /* :: */[
            2,
            /* [] */0
          ],
          /* :: */[
            3,
            /* [] */0
          ],
          /* [] */0,
          Belt_List.makeBy(4, (function (i) {
                  return i;
                }))
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

eq("CONCATMANY", Belt_List.concatMany(/* array */[]), /* [] */0);

eq("CONCATMANY", Belt_List.concatMany(/* array */[
          /* [] */0,
          /* [] */0,
          /* :: */[
            2,
            /* [] */0
          ],
          /* :: */[
            1,
            /* [] */0
          ],
          /* :: */[
            2,
            /* [] */0
          ],
          /* [] */0
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

eq("CONCATMANY", Belt_List.concatMany(/* array */[
          /* [] */0,
          /* [] */0,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ],
          /* :: */[
            1,
            /* [] */0
          ],
          /* :: */[
            2,
            /* [] */0
          ],
          /* [] */0
        ]), /* :: */[
      2,
      /* :: */[
        3,
        /* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]
      ]
    ]);

eq("CONCATMANY", Belt_List.concatMany(/* array */[/* :: */[
            1,
            /* :: */[
              2,
              /* :: */[
                3,
                /* [] */0
              ]
            ]
          ]]), /* :: */[
      1,
      /* :: */[
        2,
        /* :: */[
          3,
          /* [] */0
        ]
      ]
    ]);

eq("File \"bs_list_test.ml\", line 57, characters 5-12", Belt_List.toArray(Belt_List.concat(Belt_List.makeBy(100, (function (i) {
                    return i;
                  })), Belt_List.makeBy(100, (function (i) {
                    return i;
                  })))), Belt_Array.concat(Belt_Array.makeBy(100, (function (i) {
                return i;
              })), Belt_Array.makeBy(100, (function (i) {
                return i;
              }))));

eq("APPEND", Belt_List.concat(/* :: */[
          1,
          /* [] */0
        ], /* [] */0), /* :: */[
      1,
      /* [] */0
    ]);

eq("APPEND", Belt_List.concat(/* [] */0, /* :: */[
          1,
          /* [] */0
        ]), /* :: */[
      1,
      /* [] */0
    ]);

eq("ZIP", Belt_List.zip(/* :: */[
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

eq("ZIP", Belt_List.zip(/* [] */0, /* :: */[
          1,
          /* [] */0
        ]), /* [] */0);

eq("ZIP", Belt_List.zip(/* [] */0, /* [] */0), /* [] */0);

eq("ZIP", Belt_List.zip(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* [] */0), /* [] */0);

eq("ZIP", Belt_List.zip(/* :: */[
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
  return x % 2 === 0;
}

function evenIndex(_x, i) {
  return i % 2 === 0;
}

eq("PARTITION", Belt_List.partition(/* :: */[
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

eq("PARTITION", Belt_List.partition(/* :: */[
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

eq("PARTITION", Belt_List.partition(/* :: */[
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
            return !mod2(x);
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

eq("PARTITION", Belt_List.partition(/* [] */0, mod2), /* tuple */[
      /* [] */0,
      /* [] */0
    ]);

eq("UNZIP", Belt_List.unzip(/* [] */0), /* tuple */[
      /* [] */0,
      /* [] */0
    ]);

eq("UNZIP", Belt_List.unzip(/* :: */[
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

eq("UNZIP", Belt_List.unzip(/* :: */[
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

eq("FILTER", Belt_List.keep(/* :: */[
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

eq("FILTER", Belt_List.keep(/* :: */[
          1,
          /* :: */[
            3,
            /* :: */[
              41,
              /* [] */0
            ]
          ]
        ], mod2), /* [] */0);

eq("FILTER", Belt_List.keep(/* [] */0, mod2), /* [] */0);

eq("FILTER", Belt_List.keep(/* :: */[
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

eq("FILTER2", Belt_List.keepWithIndex(/* [] */0, evenIndex), /* [] */0);

eq("FILTER2", Belt_List.keepWithIndex(/* :: */[
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
        ], evenIndex), /* :: */[
      1,
      /* :: */[
        3,
        /* [] */0
      ]
    ]);

eq("FILTER2", Belt_List.keepWithIndex(/* :: */[
          0,
          /* :: */[
            1,
            /* :: */[
              2,
              /* :: */[
                3,
                /* :: */[
                  4,
                  /* :: */[
                    5,
                    /* :: */[
                      6,
                      /* :: */[
                        7,
                        /* [] */0
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ], evenIndex), /* :: */[
      0,
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
    ]);

function id(x) {
  return x;
}

eq("MAP", Belt_List.map(Belt_List.makeBy(5, id), (function (x) {
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

eq("MAP", Belt_List.map(/* [] */0, id), /* [] */0);

eq("MAP", Belt_List.map(/* :: */[
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

var length_10_id = Belt_List.makeBy(10, id);

var length_8_id = Belt_List.makeBy(8, id);

var d = Belt_List.makeBy(10, (function (x) {
        return (x << 1);
      }));

eq("MAP2", Belt_List.zipBy(length_10_id, length_10_id, add), d);

eq("MAP2", Belt_List.zipBy(/* [] */0, /* :: */[
          1,
          /* [] */0
        ], add), /* [] */0);

eq("MAP2", Belt_List.zipBy(/* :: */[
          1,
          /* [] */0
        ], /* [] */0, add), /* [] */0);

eq("MAP2", Belt_List.zipBy(/* [] */0, /* [] */0, add), /* [] */0);

eq("MAP2", Belt_List.zipBy(length_10_id, length_10_id, add), Belt_List.concat(Belt_List.map(length_8_id, (function (x) {
                return (x << 1);
              })), /* :: */[
          16,
          /* :: */[
            18,
            /* [] */0
          ]
        ]));

eq("MAP2", Belt_List.zipBy(length_10_id, length_8_id, add), Belt_List.mapWithIndex(length_8_id, (function (i, x) {
            return i + x | 0;
          })));

eq("MAP2", Belt_List.reverse(Belt_List.mapReverse2(length_10_id, length_10_id, add)), Belt_List.map(length_10_id, (function (x) {
            return (x << 1);
          })));

var xs = Belt_List.reverse(Belt_List.mapReverse2(length_8_id, length_10_id, add));

eq("File \"bs_list_test.ml\", line 144, characters 5-12", Belt_List.length(xs), 8);

eq("MAP2", xs, Belt_List.zipBy(length_10_id, length_8_id, add));

eq("MAP2", Belt_List.mapReverse2(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ], (function (x, y) {
            return x + y | 0;
          })), /* :: */[
      4,
      /* :: */[
        2,
        /* [] */0
      ]
    ]);

eq("TAKE", Belt_List.take(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], 2), /* :: */[
      1,
      /* :: */[
        2,
        /* [] */0
      ]
    ]);

eq("TAKE", Belt_List.take(/* [] */0, 1), undefined);

eq("TAKE", Belt_List.take(/* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ], 3), undefined);

eq("TAKE", Belt_List.take(/* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ], 2), /* :: */[
      1,
      /* :: */[
        2,
        /* [] */0
      ]
    ]);

eq("TAKE", Belt_List.take(length_10_id, 8), length_8_id);

eq("TAKE", Belt_List.take(length_10_id, 0), /* [] */0);

eq("TAKE", Belt_List.take(length_8_id, -2), undefined);

eq("DROP", Belt_List.drop(length_10_id, 10), /* [] */0);

eq("DROP", Belt_List.drop(length_10_id, 8), /* :: */[
      8,
      /* :: */[
        9,
        /* [] */0
      ]
    ]);

eq("DROP", Belt_List.drop(length_10_id, 0), length_10_id);

eq("DROP", Belt_List.drop(length_8_id, -1), undefined);

var a = Belt_List.makeBy(5, id);

eq("SPLIT", Belt_List.splitAt(/* [] */0, 1), undefined);

eq("SPLIT", Belt_List.splitAt(a, 6), undefined);

eq("SPLIT", Belt_List.splitAt(a, 5), /* tuple */[
      a,
      /* [] */0
    ]);

eq("SPLIT", Belt_List.splitAt(a, 4), /* tuple */[
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
    ]);

eq("SPLIT", Belt_List.splitAt(a, 3), /* tuple */[
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
    ]);

eq("SPLIT", Belt_List.splitAt(a, 2), /* tuple */[
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
    ]);

eq("SPLIT", Belt_List.splitAt(a, 1), /* tuple */[
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
    ]);

eq("SPLIT", Belt_List.splitAt(a, 0), /* tuple */[
      /* [] */0,
      a
    ]);

eq("SPLIT", Belt_List.splitAt(a, -1), undefined);

function succx(x) {
  return x + 1 | 0;
}

function eqx(x, y) {
  return x === y;
}

b("File \"bs_list_test.ml\", line 182, characters 4-11", Belt_List.hasAssoc(/* :: */[
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
        ], 2, (function (prim, prim$1) {
            return prim === prim$1;
          })));

b("File \"bs_list_test.ml\", line 183, characters 4-11", !Belt_List.hasAssoc(/* :: */[
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
        ], 4, (function (prim, prim$1) {
            return prim === prim$1;
          })));

b("File \"bs_list_test.ml\", line 184, characters 4-11", Belt_List.hasAssoc(/* :: */[
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
        ], 4, (function (x, y) {
            return (x + 1 | 0) === y;
          })));

eq("REMOVEASSOQ", Belt_List.removeAssoc(/* :: */[
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
        ], 3, (function (prim, prim$1) {
            return prim === prim$1;
          })), /* :: */[
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

eq("REMOVEASSOQ", Belt_List.removeAssoc(/* :: */[
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
        ], 1, (function (prim, prim$1) {
            return prim === prim$1;
          })), /* :: */[
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

eq("REMOVEASSOQ", Belt_List.removeAssoc(/* :: */[
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
        ], 2, (function (prim, prim$1) {
            return prim === prim$1;
          })), /* :: */[
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

eq("REMOVEASSOQ", Belt_List.removeAssoc(/* :: */[
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
        ], 0, (function (prim, prim$1) {
            return prim === prim$1;
          })), /* :: */[
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

eq("REMOVEASSOQ", Belt_List.removeAssoc(/* :: */[
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
        ], 3, eqx), /* :: */[
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

eq("REMOVEASSOQ", Belt_List.removeAssoc(/* :: */[
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
        ], 1, eqx), /* :: */[
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

eq("REMOVEASSOQ", Belt_List.removeAssoc(/* :: */[
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
        ], 2, eqx), /* :: */[
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

eq("REMOVEASSOQ", Belt_List.removeAssoc(/* [] */0, 2, eqx), /* [] */0);

var ll = /* :: */[
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
];

var ll0 = Belt_List.removeAssoc(ll, 0, eqx);

b("File \"bs_list_test.ml\", line 196, characters 5-12", ll === ll0);

var ll1 = Belt_List.setAssoc(ll, 2, "22", (function (prim, prim$1) {
        return prim === prim$1;
      }));

eq("File \"bs_list_test.ml\", line 198, characters 5-12", ll1, /* :: */[
      /* tuple */[
        1,
        "1"
      ],
      /* :: */[
        /* tuple */[
          2,
          "22"
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

var ll2 = Belt_List.setAssoc(ll1, 22, "2", (function (prim, prim$1) {
        return prim === prim$1;
      }));

b("File \"bs_list_test.ml\", line 200, characters 4-11", Caml_obj.caml_equal(ll2, /* :: */[
          /* tuple */[
            22,
            "2"
          ],
          ll1
        ]));

b("File \"bs_list_test.ml\", line 201, characters 4-11", Belt_List.tailExn(ll2) === ll1);

b("File \"bs_list_test.ml\", line 202, characters 4-11", Caml_obj.caml_equal(Belt_List.setAssoc(/* :: */[
              /* tuple */[
                1,
                "a"
              ],
              /* :: */[
                /* tuple */[
                  2,
                  "b"
                ],
                /* :: */[
                  /* tuple */[
                    3,
                    "c"
                  ],
                  /* [] */0
                ]
              ]
            ], 2, "x", (function (prim, prim$1) {
                return prim === prim$1;
              })), /* :: */[
          /* tuple */[
            1,
            "a"
          ],
          /* :: */[
            /* tuple */[
              2,
              "x"
            ],
            /* :: */[
              /* tuple */[
                3,
                "c"
              ],
              /* [] */0
            ]
          ]
        ]));

b("File \"bs_list_test.ml\", line 204, characters 4-11", Caml_obj.caml_equal(Belt_List.setAssoc(/* :: */[
              /* tuple */[
                1,
                "a"
              ],
              /* :: */[
                /* tuple */[
                  3,
                  "c"
                ],
                /* [] */0
              ]
            ], 2, "2", (function (prim, prim$1) {
                return prim === prim$1;
              })), /* :: */[
          /* tuple */[
            2,
            "2"
          ],
          /* :: */[
            /* tuple */[
              1,
              "a"
            ],
            /* :: */[
              /* tuple */[
                3,
                "c"
              ],
              /* [] */0
            ]
          ]
        ]));

eq("File \"bs_list_test.ml\", line 206, characters 5-12", Belt_List.setAssoc(/* [] */0, 1, "1", (function (prim, prim$1) {
            return prim === prim$1;
          })), /* :: */[
      /* tuple */[
        1,
        "1"
      ],
      /* [] */0
    ]);

debugger;

eq("File \"bs_list_test.ml\", line 208, characters 5-12", Belt_List.setAssoc(/* :: */[
          /* tuple */[
            1,
            "2"
          ],
          /* [] */0
        ], 1, "1", (function (prim, prim$1) {
            return prim === prim$1;
          })), /* :: */[
      /* tuple */[
        1,
        "1"
      ],
      /* [] */0
    ]);

eq("File \"bs_list_test.ml\", line 210, characters 5-12", Belt_List.setAssoc(/* :: */[
          /* tuple */[
            0,
            "0"
          ],
          /* :: */[
            /* tuple */[
              1,
              "2"
            ],
            /* [] */0
          ]
        ], 1, "1", (function (prim, prim$1) {
            return prim === prim$1;
          })), /* :: */[
      /* tuple */[
        0,
        "0"
      ],
      /* :: */[
        /* tuple */[
          1,
          "1"
        ],
        /* [] */0
      ]
    ]);

b("File \"bs_list_test.ml\", line 211, characters 4-11", Caml_obj.caml_equal(Belt_List.getAssoc(/* :: */[
              /* tuple */[
                1,
                "a"
              ],
              /* :: */[
                /* tuple */[
                  2,
                  "b"
                ],
                /* :: */[
                  /* tuple */[
                    3,
                    "c"
                  ],
                  /* [] */0
                ]
              ]
            ], 2, (function (prim, prim$1) {
                return prim === prim$1;
              })), "b"));

b("File \"bs_list_test.ml\", line 212, characters 4-11", Belt_List.getAssoc(/* :: */[
          /* tuple */[
            1,
            "a"
          ],
          /* :: */[
            /* tuple */[
              2,
              "b"
            ],
            /* :: */[
              /* tuple */[
                3,
                "c"
              ],
              /* [] */0
            ]
          ]
        ], 4, (function (prim, prim$1) {
            return prim === prim$1;
          })) === undefined);

eq("File \"bs_list_test.ml\", line 216, characters 5-12", /* tuple */[
      Belt_List.head(length_10_id),
      Belt_List.tail(length_10_id)
    ], /* tuple */[
      0,
      Belt_List.drop(length_10_id, 1)
    ]);

eq("File \"bs_list_test.ml\", line 219, characters 5-12", Belt_List.head(/* [] */0), undefined);

$$throw("File \"bs_list_test.ml\", line 220, characters 8-15", (function (param) {
        return Belt_List.headExn(/* [] */0);
      }));

$$throw("File \"bs_list_test.ml\", line 221, characters 8-15", (function (param) {
        Belt_List.tailExn(/* [] */0);
        return /* () */0;
      }));

$$throw("File \"bs_list_test.ml\", line 222, characters 8-15", (function (param) {
        Belt_List.getExn(/* :: */[
              0,
              /* :: */[
                1,
                /* [] */0
              ]
            ], -1);
        return /* () */0;
      }));

$$throw("File \"bs_list_test.ml\", line 223, characters 8-15", (function (param) {
        Belt_List.getExn(/* :: */[
              0,
              /* :: */[
                1,
                /* [] */0
              ]
            ], 2);
        return /* () */0;
      }));

eq("File \"bs_list_test.ml\", line 224, characters 5-12", Belt_List.map(/* :: */[
          0,
          /* :: */[
            1,
            /* [] */0
          ]
        ], (function (i) {
            return Belt_List.getExn(/* :: */[
                        0,
                        /* :: */[
                          1,
                          /* [] */0
                        ]
                      ], i);
          })), /* :: */[
      0,
      /* :: */[
        1,
        /* [] */0
      ]
    ]);

eq("File \"bs_list_test.ml\", line 225, characters 5-12", Belt_List.headExn(/* :: */[
          1,
          /* [] */0
        ]), 1);

eq("File \"bs_list_test.ml\", line 226, characters 5-12", Belt_List.tailExn(/* :: */[
          1,
          /* [] */0
        ]), /* [] */0);

Belt_List.forEachWithIndex(length_10_id, (function (i, x) {
        return eq("File \"bs_list_test.ml\", line 228, characters 9-16", Belt_List.get(length_10_id, i), x);
      }));

eq("File \"bs_list_test.ml\", line 229, characters 5-12", Belt_List.tail(/* [] */0), undefined);

eq("File \"bs_list_test.ml\", line 230, characters 5-12", Belt_List.drop(/* [] */0, 3), undefined);

eq("File \"bs_list_test.ml\", line 231, characters 5-12", Belt_List.mapWithIndex(/* [] */0, (function (i, x) {
            return i + x | 0;
          })), /* [] */0);

eq("File \"bs_list_test.ml\", line 232, characters 5-12", Belt_List.get(length_10_id, -1), undefined);

eq("File \"bs_list_test.ml\", line 233, characters 5-12", Belt_List.get(length_10_id, 12), undefined);

eq("File \"bs_list_test.ml\", line 234, characters 5-12", sum(/* [] */0), 0);

eq("File \"bs_list_test.ml\", line 235, characters 5-12", sum(length_10_id), 45);

eq("File \"bs_list_test.ml\", line 236, characters 5-12", Belt_List.makeBy(0, id), /* [] */0);

eq("File \"bs_list_test.ml\", line 237, characters 5-12", Belt_List.reverse(Belt_List.reverse(length_10_id)), length_10_id);

eq("File \"bs_list_test.ml\", line 238, characters 5-12", Belt_List.reverse(Belt_List.reverse(length_8_id)), length_8_id);

eq("File \"bs_list_test.ml\", line 239, characters 5-12", Belt_List.reverse(/* [] */0), /* [] */0);

eq("File \"bs_list_test.ml\", line 240, characters 5-12", Belt_List.reverse(Belt_List.mapReverse(length_10_id, succx)), Belt_List.map(length_10_id, succx));

eq("File \"bs_list_test.ml\", line 243, characters 5-12", Belt_List.reduce(length_10_id, 0, add), 45);

eq("File \"bs_list_test.ml\", line 245, characters 5-12", Belt_List.reduceReverse(length_10_id, 0, add), 45);

eq("File \"bs_list_test.ml\", line 247, characters 5-12", Belt_List.reduceReverse(Belt_List.makeBy(10000, (function (i) {
                return i;
              })), 0, (function (prim, prim$1) {
            return prim + prim$1 | 0;
          })), 49995000);

eq("File \"bs_list_test.ml\", line 252, characters 5-12", sum2(length_10_id, length_10_id), 90);

eq("File \"bs_list_test.ml\", line 253, characters 5-12", sum2(length_8_id, length_10_id), 56);

eq("File \"bs_list_test.ml\", line 254, characters 5-12", sum2(length_10_id, length_8_id), 56);

eq("File \"bs_list_test.ml\", line 255, characters 5-12", Belt_List.reduce2(length_10_id, length_8_id, 0, (function (acc, x, y) {
            return (acc + x | 0) + y | 0;
          })), 56);

eq("File \"bs_list_test.ml\", line 257, characters 5-12", Belt_List.reduce2(/* :: */[
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
            4,
            /* :: */[
              6,
              /* [] */0
            ]
          ]
        ], 0, (function (a, b, c) {
            return (a + b | 0) + c | 0;
          })), 18);

eq("File \"bs_list_test.ml\", line 258, characters 5-12", Belt_List.reduceReverse2(length_10_id, length_8_id, 0, (function (acc, x, y) {
            return (acc + x | 0) + y | 0;
          })), 56);

eq("File \"bs_list_test.ml\", line 260, characters 5-12", Belt_List.reduceReverse2(length_10_id, length_10_id, 0, (function (acc, x, y) {
            return (acc + x | 0) + y | 0;
          })), 90);

eq("File \"bs_list_test.ml\", line 262, characters 5-12", Belt_List.reduceReverse2(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ], 0, (function (acc, x, y) {
            return (acc + x | 0) + y | 0;
          })), 6);

eq("File \"bs_list_test.ml\", line 263, characters 5-12", Belt_List.every(/* :: */[
          2,
          /* :: */[
            4,
            /* :: */[
              6,
              /* [] */0
            ]
          ]
        ], mod2), true);

eq("File \"bs_list_test.ml\", line 264, characters 5-12", Belt_List.every(/* :: */[
          1,
          /* [] */0
        ], mod2), false);

eq("File \"bs_list_test.ml\", line 265, characters 5-12", Belt_List.every(/* [] */0, mod2), true);

eq("File \"bs_list_test.ml\", line 266, characters 5-12", Belt_List.some(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              5,
              /* [] */0
            ]
          ]
        ], mod2), true);

eq("File \"bs_list_test.ml\", line 267, characters 5-12", Belt_List.some(/* :: */[
          1,
          /* :: */[
            3,
            /* :: */[
              5,
              /* [] */0
            ]
          ]
        ], mod2), false);

eq("File \"bs_list_test.ml\", line 268, characters 5-12", Belt_List.some(/* [] */0, mod2), false);

eq("File \"bs_list_test.ml\", line 269, characters 5-12", Belt_List.has(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], "2", (function (x, s) {
            return String(x) === s;
          })), true);

eq("File \"bs_list_test.ml\", line 270, characters 5-12", Belt_List.has(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], "0", (function (x, s) {
            return String(x) === s;
          })), false);

b("File \"bs_list_test.ml\", line 272, characters 4-11", Belt_List.reduceReverse(/* :: */[
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
        ], 0, (function (prim, prim$1) {
            return prim + prim$1 | 0;
          })) === 10);

b("File \"bs_list_test.ml\", line 273, characters 4-11", Belt_List.reduceReverse(/* :: */[
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
        ], 10, (function (prim, prim$1) {
            return prim - prim$1 | 0;
          })) === 0);

b("File \"bs_list_test.ml\", line 274, characters 4-11", Caml_obj.caml_equal(Belt_List.reduceReverse(/* :: */[
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
            ], /* [] */0, Belt_List.add), /* :: */[
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
        ]));

b("File \"bs_list_test.ml\", line 275, characters 4-11", Belt_List.reduce(/* :: */[
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
        ], 0, (function (prim, prim$1) {
            return prim + prim$1 | 0;
          })) === 10);

b("File \"bs_list_test.ml\", line 276, characters 4-11", Belt_List.reduce(/* :: */[
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
        ], 10, (function (prim, prim$1) {
            return prim - prim$1 | 0;
          })) === 0);

b("File \"bs_list_test.ml\", line 277, characters 4-11", Caml_obj.caml_equal(Belt_List.reduce(/* :: */[
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
            ], /* [] */0, Belt_List.add), /* :: */[
          4,
          /* :: */[
            3,
            /* :: */[
              2,
              /* :: */[
                1,
                /* [] */0
              ]
            ]
          ]
        ]));

b("File \"bs_list_test.ml\", line 278, characters 4-11", Belt_List.reduceWithIndex(/* :: */[
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
        ], 0, (function (acc, x, i) {
            return (acc + x | 0) + i | 0;
          })) === 16);

b("File \"bs_list_test.ml\", line 279, characters 4-11", Belt_List.reduceReverse2(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ], 0, (function (acc, x, y) {
            return (acc + x | 0) + y | 0;
          })) === 6);

var a$1 = Belt_List.makeBy(10000, (function (i) {
        return i;
      }));

b("File \"bs_list_test.ml\", line 282, characters 4-11", Belt_List.reduceReverse2(a$1, /* :: */[
          0,
          a$1
        ], 0, (function (acc, x, y) {
            return (acc + x | 0) + y | 0;
          })) === 99980001);

eq("File \"bs_list_test.ml\", line 288, characters 5-12", Belt_List.every2(/* [] */0, /* :: */[
          1,
          /* [] */0
        ], (function (x, y) {
            return x > y;
          })), true);

eq("File \"bs_list_test.ml\", line 289, characters 5-12", Belt_List.every2(/* :: */[
          2,
          /* :: */[
            3,
            /* [] */0
          ]
        ], /* :: */[
          1,
          /* [] */0
        ], (function (x, y) {
            return x > y;
          })), true);

eq("File \"bs_list_test.ml\", line 290, characters 5-12", Belt_List.every2(/* :: */[
          2,
          /* [] */0
        ], /* :: */[
          1,
          /* [] */0
        ], (function (x, y) {
            return x > y;
          })), true);

eq("File \"bs_list_test.ml\", line 291, characters 5-12", Belt_List.every2(/* :: */[
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
            return x > y;
          })), false);

eq("File \"bs_list_test.ml\", line 292, characters 5-12", Belt_List.every2(/* :: */[
          2,
          /* :: */[
            3,
            /* [] */0
          ]
        ], /* :: */[
          1,
          /* :: */[
            0,
            /* [] */0
          ]
        ], (function (x, y) {
            return x > y;
          })), true);

eq("File \"bs_list_test.ml\", line 293, characters 5-12", Belt_List.some2(/* [] */0, /* :: */[
          1,
          /* [] */0
        ], (function (x, y) {
            return x > y;
          })), false);

eq("File \"bs_list_test.ml\", line 294, characters 5-12", Belt_List.some2(/* :: */[
          2,
          /* :: */[
            3,
            /* [] */0
          ]
        ], /* :: */[
          1,
          /* [] */0
        ], (function (x, y) {
            return x > y;
          })), true);

eq("File \"bs_list_test.ml\", line 295, characters 5-12", Belt_List.some2(/* :: */[
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
            return x > y;
          })), true);

eq("File \"bs_list_test.ml\", line 296, characters 5-12", Belt_List.some2(/* :: */[
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
            return x > y;
          })), false);

eq("File \"bs_list_test.ml\", line 297, characters 5-12", Belt_List.some2(/* :: */[
          0,
          /* :: */[
            3,
            /* [] */0
          ]
        ], /* :: */[
          3,
          /* :: */[
            2,
            /* [] */0
          ]
        ], (function (x, y) {
            return x > y;
          })), true);

eq("File \"bs_list_test.ml\", line 298, characters 5-12", Belt_List.some2(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* :: */[
          -1,
          /* :: */[
            -2,
            /* [] */0
          ]
        ], (function (x, y) {
            return x === y;
          })), false);

function makeTest(n) {
  return eq("File \"bs_list_test.ml\", line 301, characters 5-12", Belt_List.make(n, 3), Belt_List.makeBy(n, (function (param) {
                    return 3;
                  })));
}

eq("File \"bs_list_test.ml\", line 304, characters 5-12", /* :: */[
      2,
      /* :: */[
        3,
        /* [] */0
      ]
    ], /* :: */[
      2,
      /* :: */[
        3,
        /* [] */0
      ]
    ]);

b("File \"bs_list_test.ml\", line 310, characters 4-11", Belt_List.cmp(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* :: */[
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
        ], Caml_primitive.caml_int_compare) > 0);

b("File \"bs_list_test.ml\", line 311, characters 4-11", Belt_List.cmp(/* :: */[
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
        ], /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], Caml_primitive.caml_int_compare) > 0);

b("File \"bs_list_test.ml\", line 312, characters 4-11", Belt_List.cmp(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* :: */[
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
        ], Caml_primitive.caml_int_compare) < 0);

b("File \"bs_list_test.ml\", line 313, characters 4-11", Belt_List.cmp(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* :: */[
          0,
          /* :: */[
            1,
            /* :: */[
              2,
              /* [] */0
            ]
          ]
        ], Caml_primitive.caml_int_compare) > 0);

b("File \"bs_list_test.ml\", line 314, characters 4-11", Belt_List.cmp(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], Caml_primitive.caml_int_compare) === 0);

b("File \"bs_list_test.ml\", line 315, characters 4-11", Belt_List.cmp(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              4,
              /* [] */0
            ]
          ]
        ], /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], Caml_primitive.caml_int_compare) > 0);

b("File \"bs_list_test.ml\", line 316, characters 4-11", Belt_List.cmpByLength(/* [] */0, /* [] */0) === 0);

b("File \"bs_list_test.ml\", line 317, characters 4-11", Belt_List.cmpByLength(/* :: */[
          1,
          /* [] */0
        ], /* [] */0) > 0);

b("File \"bs_list_test.ml\", line 318, characters 4-11", Belt_List.cmpByLength(/* [] */0, /* :: */[
          1,
          /* [] */0
        ]) < 0);

b("File \"bs_list_test.ml\", line 319, characters 4-11", Belt_List.cmpByLength(/* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ], /* :: */[
          1,
          /* [] */0
        ]) > 0);

b("File \"bs_list_test.ml\", line 320, characters 4-11", Belt_List.cmpByLength(/* :: */[
          1,
          /* [] */0
        ], /* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]) < 0);

b("File \"bs_list_test.ml\", line 321, characters 4-11", Belt_List.cmpByLength(/* :: */[
          1,
          /* :: */[
            3,
            /* [] */0
          ]
        ], /* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]) === 0);

makeTest(0);

makeTest(1);

makeTest(2);

makeTest(3);

function cmp(a, b) {
  return a - b | 0;
}

eq("SORT", Belt_List.sort(/* :: */[
          5,
          /* :: */[
            4,
            /* :: */[
              3,
              /* :: */[
                2,
                /* [] */0
              ]
            ]
          ]
        ], cmp), /* :: */[
      2,
      /* :: */[
        3,
        /* :: */[
          4,
          /* :: */[
            5,
            /* [] */0
          ]
        ]
      ]
    ]);

eq("SORT", Belt_List.sort(/* :: */[
          3,
          /* :: */[
            9,
            /* :: */[
              37,
              /* :: */[
                3,
                /* :: */[
                  1,
                  /* [] */0
                ]
              ]
            ]
          ]
        ], cmp), /* :: */[
      1,
      /* :: */[
        3,
        /* :: */[
          3,
          /* :: */[
            9,
            /* :: */[
              37,
              /* [] */0
            ]
          ]
        ]
      ]
    ]);

b("File \"bs_list_test.ml\", line 337, characters 4-11", !Belt_List.eq(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ], (function (x, y) {
            return x === y;
          })));

b("File \"bs_list_test.ml\", line 338, characters 4-11", Belt_List.eq(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], (function (x, y) {
            return x === y;
          })));

b("File \"bs_list_test.ml\", line 339, characters 4-11", !Belt_List.eq(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              4,
              /* [] */0
            ]
          ]
        ], (function (x, y) {
            return x === y;
          })));

b("File \"bs_list_test.ml\", line 340, characters 4-11", !Belt_List.eq(/* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ], /* :: */[
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
        ], (function (prim, prim$1) {
            return prim === prim$1;
          })));

var u0 = Belt_List.makeBy(20, (function (x) {
        return x;
      }));

var u1 = Belt_List.keepMap(u0, (function (x) {
        if (x % 7 === 0) {
          return x + 1 | 0;
        }
        
      }));

eq("File \"bs_list_test.ml\", line 344, characters 5-12", u1, /* :: */[
      1,
      /* :: */[
        8,
        /* :: */[
          15,
          /* [] */0
        ]
      ]
    ]);

b("File \"bs_list_test.ml\", line 345, characters 4-11", Caml_obj.caml_equal(Belt_List.keepMap(/* :: */[
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
            ], (function (x) {
                if (x % 2 === 0) {
                  return -x | 0;
                }
                
              })), /* :: */[
          -2,
          /* :: */[
            -4,
            /* [] */0
          ]
        ]));

b("File \"bs_list_test.ml\", line 349, characters 4-11", Belt_List.keepMap(/* :: */[
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
        ], (function (x) {
            if (x % 5 === 0) {
              return x;
            }
            
          })) === /* [] */0);

Mt.from_pair_suites("Bs_list_test", suites[0]);

var N = 0;

var A = 0;

var J = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.$$throw = $$throw;
exports.N = N;
exports.A = A;
exports.J = J;
exports.sum = sum;
exports.sum2 = sum2;
exports.mod2 = mod2;
exports.evenIndex = evenIndex;
exports.id = id;
exports.add = add;
exports.length_10_id = length_10_id;
exports.length_8_id = length_8_id;
exports.succx = succx;
exports.makeTest = makeTest;
/* u Not a pure module */
