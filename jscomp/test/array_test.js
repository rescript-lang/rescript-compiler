'use strict';

var Mt             = require("./mt.js");
var List           = require("../../lib/js/list.js");
var $$Array        = require("../../lib/js/array.js");
var Block          = require("../../lib/js/block.js");
var Caml_obj       = require("../../lib/js/caml_obj.js");
var Caml_array     = require("../../lib/js/caml_array.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");

function is_sorted(x) {
  var len = x.length;
  var _i = 0;
  while(true) {
    var i = _i;
    if (i >= (len - 1 | 0)) {
      return /* true */1;
    } else if (Caml_obj.caml_lessthan(Caml_array.caml_array_get(x, i), Caml_array.caml_array_get(x, i + 1 | 0))) {
      _i = i + 1 | 0;
      continue ;
      
    } else {
      return /* false */0;
    }
  };
}

var array_suites_000 = /* tuple */[
  "init",
  (function () {
      return /* Eq */Block.__(0, [
                $$Array.init(5, (function (x) {
                        return x;
                      })),
                /* array */[
                  0,
                  1,
                  2,
                  3,
                  4
                ]
              ]);
    })
];

var array_suites_001 = /* :: */[
  /* tuple */[
    "toList",
    (function () {
        var aux = function (xs) {
          return List.fold_left((function (acc, param) {
                        return /* :: */[
                                /* tuple */[
                                  $$Array.to_list(param[0]),
                                  param[1]
                                ],
                                acc
                              ];
                      }), /* [] */0, xs);
        };
        var match = List.split(aux(/* :: */[
                  /* tuple */[
                    /* int array */[],
                    /* [] */0
                  ],
                  /* [] */0
                ]));
        return /* Eq */Block.__(0, [
                  match[0],
                  match[1]
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "concat",
      (function () {
          return /* Eq */Block.__(0, [
                    /* array */[
                      0,
                      1,
                      2,
                      3,
                      4,
                      5
                    ],
                    Caml_array.caml_array_concat(/* :: */[
                          /* int array */[
                            0,
                            1,
                            2
                          ],
                          /* :: */[
                            /* int array */[
                              3,
                              4
                            ],
                            /* :: */[
                              /* int array */[],
                              /* :: */[
                                /* int array */[5],
                                /* [] */0
                              ]
                            ]
                          ]
                        ])
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "make",
        (function () {
            return /* Eq */Block.__(0, [
                      Caml_array.caml_make_vect(100, /* "a" */97),
                      $$Array.init(100, (function () {
                              return /* "a" */97;
                            }))
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "sub",
          (function () {
              return /* Eq */Block.__(0, [
                        $$Array.sub(/* array */[
                              0,
                              1,
                              2,
                              3,
                              4
                            ], 2, 2),
                        /* int array */[
                          2,
                          3
                        ]
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "blit",
            (function () {
                var u = /* int array */[
                  100,
                  0,
                  0
                ];
                var v = $$Array.init(3, (function (x) {
                        return (x << 1);
                      }));
                $$Array.blit(v, 1, u, 1, 2);
                return /* Eq */Block.__(0, [
                          /* tuple */[
                            /* int array */[
                              0,
                              2,
                              4
                            ],
                            /* int array */[
                              100,
                              2,
                              4
                            ]
                          ],
                          /* tuple */[
                            v,
                            u
                          ]
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "make",
              (function () {
                  return /* Eq */Block.__(0, [
                            Caml_array.caml_make_vect(2, 1),
                            /* int array */[
                              1,
                              1
                            ]
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "sort",
                (function () {
                    var u = /* int array */[
                      3,
                      0,
                      1
                    ];
                    $$Array.sort(Caml_primitive.caml_int_compare, u);
                    return /* Eq */Block.__(0, [
                              Caml_obj.caml_equal(/* int array */[
                                    0,
                                    1,
                                    3
                                  ], u),
                              /* true */1
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "sort_large",
                  (function () {
                      var v = $$Array.init(4, (function (i) {
                              return i % 17;
                            }));
                      $$Array.sort(Caml_primitive.caml_int_compare, v);
                      return /* Eq */Block.__(0, [
                                /* true */1,
                                is_sorted(v)
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
];

var array_suites = /* :: */[
  array_suites_000,
  array_suites_001
];

Mt.from_pair_suites("array_test.ml", /* array_suites */array_suites);

/*  Not a pure module */
