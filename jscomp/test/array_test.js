'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

function starts_with(xs, prefix, p) {
  var H = Caml_exceptions.create("H");
  var len1 = xs.length;
  var len2 = prefix.length;
  if (len2 > len1) {
    return false;
  } else {
    try {
      for(var i = 0 ,i_finish = len2 - 1 | 0; i <= i_finish; ++i){
        if (!Curry._2(p, Caml_array.caml_array_get(xs, i), Caml_array.caml_array_get(prefix, i))) {
          throw H;
        }
        
      }
      return true;
    }
    catch (exn){
      if (exn === H) {
        return false;
      } else {
        throw exn;
      }
    }
  }
}

function is_sorted(x) {
  var len = x.length;
  var _i = 0;
  while(true) {
    var i = _i;
    if (i >= (len - 1 | 0)) {
      return true;
    } else if (Caml_obj.caml_lessthan(Caml_array.caml_array_get(x, i), Caml_array.caml_array_get(x, i + 1 | 0))) {
      _i = i + 1 | 0;
      continue ;
    } else {
      return false;
    }
  };
}

var array_suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "init",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: $$Array.init(5, (function (x) {
                        return x;
                      })),
                Arg1: /* array */[
                  0,
                  1,
                  2,
                  3,
                  4
                ]
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "toList",
      (function (param) {
          var aux = function (xs) {
            return List.fold_left((function (acc, param) {
                          return /* constructor */{
                                  tag: "::",
                                  Arg0: /* tuple */[
                                    $$Array.to_list(param[0]),
                                    param[1]
                                  ],
                                  Arg1: acc
                                };
                        }), "[]", xs);
          };
          var match = List.split(aux(/* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      /* array */[],
                      "[]"
                    ],
                    Arg1: "[]"
                  }));
          return /* constructor */{
                  tag: "Eq",
                  Arg0: match[0],
                  Arg1: match[1]
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "concat",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: /* array */[
                      0,
                      1,
                      2,
                      3,
                      4,
                      5
                    ],
                    Arg1: Caml_array.caml_array_concat(/* constructor */{
                          tag: "::",
                          Arg0: /* array */[
                            0,
                            1,
                            2
                          ],
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: /* array */[
                              3,
                              4
                            ],
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: /* array */[],
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* array */[5],
                                Arg1: "[]"
                              }
                            }
                          }
                        })
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "make",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: /* tuple */[
                        Caml_array.caml_make_vect(100, /* "a" */97),
                        Caml_array.caml_make_float_vect(100)
                      ],
                      Arg1: /* tuple */[
                        $$Array.init(100, (function (param) {
                                return /* "a" */97;
                              })),
                        $$Array.init(100, (function (param) {
                                return 0;
                              }))
                      ]
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "sub",
            (function (param) {
                return /* constructor */{
                        tag: "Eq",
                        Arg0: $$Array.sub(/* array */[
                              0,
                              1,
                              2,
                              3,
                              4
                            ], 2, 2),
                        Arg1: /* array */[
                          2,
                          3
                        ]
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "blit",
              (function (param) {
                  var u = /* array */[
                    100,
                    0,
                    0
                  ];
                  var v = $$Array.init(3, (function (x) {
                          return (x << 1);
                        }));
                  $$Array.blit(v, 1, u, 1, 2);
                  return /* constructor */{
                          tag: "Eq",
                          Arg0: /* tuple */[
                            /* array */[
                              0,
                              2,
                              4
                            ],
                            /* array */[
                              100,
                              2,
                              4
                            ]
                          ],
                          Arg1: /* tuple */[
                            v,
                            u
                          ]
                        };
                })
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "File \"array_test.ml\", line 63, characters 2-9",
                (function (param) {
                    var a0 = $$Array.init(100, (function (i) {
                            return (i << 0);
                          }));
                    $$Array.blit(a0, 10, a0, 5, 20);
                    return /* constructor */{
                            tag: "Eq",
                            Arg0: true,
                            Arg1: starts_with(a0, /* array */[
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
                                ], Caml_obj.caml_equal)
                          };
                  })
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "File \"array_test.ml\", line 72, characters 2-9",
                  (function (param) {
                      var a0 = $$Array.init(100, (function (i) {
                              return (i << 0);
                            }));
                      $$Array.blit(a0, 5, a0, 10, 20);
                      return /* constructor */{
                              tag: "Eq",
                              Arg0: true,
                              Arg1: starts_with(a0, /* array */[
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
                                  ], Caml_obj.caml_equal)
                            };
                    })
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "make",
                    (function (param) {
                        return /* constructor */{
                                tag: "Eq",
                                Arg0: Caml_array.caml_make_vect(2, 1),
                                Arg1: /* array */[
                                  1,
                                  1
                                ]
                              };
                      })
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "sort",
                      (function (param) {
                          var u = /* array */[
                            3,
                            0,
                            1
                          ];
                          $$Array.sort(Caml_primitive.caml_int_compare, u);
                          return /* constructor */{
                                  tag: "Eq",
                                  Arg0: Caml_obj.caml_equal(/* array */[
                                        0,
                                        1,
                                        3
                                      ], u),
                                  Arg1: true
                                };
                        })
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "sort_large",
                        (function (param) {
                            var v = $$Array.init(4, (function (i) {
                                    return i % 17;
                                  }));
                            $$Array.sort(Caml_primitive.caml_int_compare, v);
                            return /* constructor */{
                                    tag: "Eq",
                                    Arg0: true,
                                    Arg1: is_sorted(v)
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
};

Mt.from_pair_suites("Array_test", array_suites);

/*  Not a pure module */
