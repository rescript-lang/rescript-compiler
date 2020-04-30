'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function starts_with(xs, prefix, p) {
  var H = Caml_exceptions.create("H");
  var len1 = xs.length;
  var len2 = prefix.length;
  if (len2 > len1) {
    return false;
  }
  try {
    for(var i = 0; i < len2; ++i){
      if (!Curry._2(p, Caml_array.caml_array_get(xs, i), Caml_array.caml_array_get(prefix, i))) {
        throw {
              RE_EXN_ID: H,
              Error: new Error()
            };
      }
      
    }
    return true;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === H) {
      return false;
    }
    throw exn;
  }
}

function is_sorted(x) {
  var len = x.length;
  var _i = 0;
  while(true) {
    var i = _i;
    if (i >= (len - 1 | 0)) {
      return true;
    }
    if (!Caml_obj.caml_lessthan(Caml_array.caml_array_get(x, i), Caml_array.caml_array_get(x, i + 1 | 0))) {
      return false;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

var array_suites_000 = /* tuple */[
  "init",
  (function (param) {
      return /* Eq */Block.__(0, [
                $$Array.init(5, (function (x) {
                        return x;
                      })),
                [
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
    (function (param) {
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
                    [],
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
      (function (param) {
          return /* Eq */Block.__(0, [
                    [
                      0,
                      1,
                      2,
                      3,
                      4,
                      5
                    ],
                    Caml_array.caml_array_concat(/* :: */[
                          [
                            0,
                            1,
                            2
                          ],
                          /* :: */[
                            [
                              3,
                              4
                            ],
                            /* :: */[
                              [],
                              /* :: */[
                                [5],
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
        (function (param) {
            return /* Eq */Block.__(0, [
                      /* tuple */[
                        Caml_array.caml_make_vect(100, /* "a" */97),
                        Caml_array.caml_make_float_vect(100)
                      ],
                      /* tuple */[
                        $$Array.init(100, (function (param) {
                                return /* "a" */97;
                              })),
                        $$Array.init(100, (function (param) {
                                return 0;
                              }))
                      ]
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "sub",
          (function (param) {
              return /* Eq */Block.__(0, [
                        $$Array.sub([
                              0,
                              1,
                              2,
                              3,
                              4
                            ], 2, 2),
                        [
                          2,
                          3
                        ]
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "blit",
            (function (param) {
                var u = [
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
                            [
                              0,
                              2,
                              4
                            ],
                            [
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
              "File \"array_test.ml\", line 63, characters 2-9",
              (function (param) {
                  var a0 = $$Array.init(100, (function (i) {
                          return (i << 0);
                        }));
                  $$Array.blit(a0, 10, a0, 5, 20);
                  return /* Eq */Block.__(0, [
                            true,
                            starts_with(a0, [
                                  0,
                                  1,
                                  2,
                                  3,
                                  4,
                                  10,
                                  11,
                                  12,
                                  13,
                                  14,
                                  15,
                                  16,
                                  17,
                                  18,
                                  19,
                                  20,
                                  21,
                                  22,
                                  23,
                                  24,
                                  25,
                                  26,
                                  27,
                                  28
                                ], (function (prim, prim$1) {
                                    return prim === prim$1;
                                  }))
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "File \"array_test.ml\", line 72, characters 2-9",
                (function (param) {
                    var a0 = $$Array.init(100, (function (i) {
                            return (i << 0);
                          }));
                    $$Array.blit(a0, 5, a0, 10, 20);
                    return /* Eq */Block.__(0, [
                              true,
                              starts_with(a0, [
                                    0,
                                    1,
                                    2,
                                    3,
                                    4,
                                    5,
                                    6,
                                    7,
                                    8,
                                    9,
                                    5,
                                    6,
                                    7,
                                    8,
                                    9,
                                    10,
                                    11,
                                    12,
                                    13,
                                    14,
                                    15,
                                    16,
                                    17,
                                    18,
                                    19,
                                    20
                                  ], (function (prim, prim$1) {
                                      return prim === prim$1;
                                    }))
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "make",
                  (function (param) {
                      return /* Eq */Block.__(0, [
                                Caml_array.caml_make_vect(2, 1),
                                [
                                  1,
                                  1
                                ]
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "sort",
                    (function (param) {
                        var u = [
                          3,
                          0,
                          1
                        ];
                        $$Array.sort(Caml_primitive.caml_int_compare, u);
                        return /* Eq */Block.__(0, [
                                  Caml_obj.caml_equal([
                                        0,
                                        1,
                                        3
                                      ], u),
                                  true
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "sort_large",
                      (function (param) {
                          var v = $$Array.init(4, (function (i) {
                                  return i % 17;
                                }));
                          $$Array.sort(Caml_primitive.caml_int_compare, v);
                          return /* Eq */Block.__(0, [
                                    true,
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
    ]
  ]
];

var array_suites = /* :: */[
  array_suites_000,
  array_suites_001
];

Mt.from_pair_suites("Array_test", array_suites);

/*  Not a pure module */
