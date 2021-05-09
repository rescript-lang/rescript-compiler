'use strict';

var Mt = require("./mt.js");
var Caml = require("../../lib/js/caml.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function starts_with(xs, prefix, p) {
  var H = /* @__PURE__ */Caml_exceptions.create("H");
  var len1 = xs.length;
  var len2 = prefix.length;
  if (len2 > len1) {
    return false;
  }
  try {
    for(var i = 0; i < len2; ++i){
      if (!Curry._2(p, Caml_array.get(xs, i), Caml_array.get(prefix, i))) {
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
    if (!Caml_obj.lessthan(Caml_array.get(x, i), Caml_array.get(x, i + 1 | 0))) {
      return false;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

var array_suites_0 = [
  "init",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: $$Array.init(5, (function (x) {
                      return x;
                    })),
              _1: [
                0,
                1,
                2,
                3,
                4
              ]
            };
    })
];

var array_suites_1 = {
  hd: [
    "toList",
    (function (param) {
        var aux = function (xs) {
          return List.fold_left((function (acc, param) {
                        return {
                                hd: [
                                  $$Array.to_list(param[0]),
                                  param[1]
                                ],
                                tl: acc
                              };
                      }), /* [] */0, xs);
        };
        var match = List.split(aux({
                  hd: [
                    [],
                    /* [] */0
                  ],
                  tl: /* [] */0
                }));
        return {
                TAG: /* Eq */0,
                _0: match[0],
                _1: match[1]
              };
      })
  ],
  tl: {
    hd: [
      "concat",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: [
                    0,
                    1,
                    2,
                    3,
                    4,
                    5
                  ],
                  _1: Caml_array.concat({
                        hd: [
                          0,
                          1,
                          2
                        ],
                        tl: {
                          hd: [
                            3,
                            4
                          ],
                          tl: {
                            hd: [],
                            tl: {
                              hd: [5],
                              tl: /* [] */0
                            }
                          }
                        }
                      })
                };
        })
    ],
    tl: {
      hd: [
        "make",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: [
                      Caml_array.make(100, /* 'a' */97),
                      Caml_array.make_float(100)
                    ],
                    _1: [
                      $$Array.init(100, (function (param) {
                              return /* 'a' */97;
                            })),
                      $$Array.init(100, (function (param) {
                              return 0;
                            }))
                    ]
                  };
          })
      ],
      tl: {
        hd: [
          "sub",
          (function (param) {
              return {
                      TAG: /* Eq */0,
                      _0: $$Array.sub([
                            0,
                            1,
                            2,
                            3,
                            4
                          ], 2, 2),
                      _1: [
                        2,
                        3
                      ]
                    };
            })
        ],
        tl: {
          hd: [
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
                return {
                        TAG: /* Eq */0,
                        _0: [
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
                        _1: [
                          v,
                          u
                        ]
                      };
              })
          ],
          tl: {
            hd: [
              "File \"array_test.ml\", line 63, characters 2-9",
              (function (param) {
                  var a0 = $$Array.init(100, (function (i) {
                          return (i << 0);
                        }));
                  $$Array.blit(a0, 10, a0, 5, 20);
                  return {
                          TAG: /* Eq */0,
                          _0: true,
                          _1: starts_with(a0, [
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
                              ], (function (prim0, prim1) {
                                  return prim0 === prim1;
                                }))
                        };
                })
            ],
            tl: {
              hd: [
                "File \"array_test.ml\", line 72, characters 2-9",
                (function (param) {
                    var a0 = $$Array.init(100, (function (i) {
                            return (i << 0);
                          }));
                    $$Array.blit(a0, 5, a0, 10, 20);
                    return {
                            TAG: /* Eq */0,
                            _0: true,
                            _1: starts_with(a0, [
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
                                ], (function (prim0, prim1) {
                                    return prim0 === prim1;
                                  }))
                          };
                  })
              ],
              tl: {
                hd: [
                  "make",
                  (function (param) {
                      return {
                              TAG: /* Eq */0,
                              _0: Caml_array.make(2, 1),
                              _1: [
                                1,
                                1
                              ]
                            };
                    })
                ],
                tl: {
                  hd: [
                    "sort",
                    (function (param) {
                        var u = [
                          3,
                          0,
                          1
                        ];
                        $$Array.sort(Caml.int_compare, u);
                        return {
                                TAG: /* Eq */0,
                                _0: Caml_obj.equal([
                                      0,
                                      1,
                                      3
                                    ], u),
                                _1: true
                              };
                      })
                  ],
                  tl: {
                    hd: [
                      "sort_large",
                      (function (param) {
                          var v = $$Array.init(4, (function (i) {
                                  return i % 17;
                                }));
                          $$Array.sort(Caml.int_compare, v);
                          return {
                                  TAG: /* Eq */0,
                                  _0: true,
                                  _1: is_sorted(v)
                                };
                        })
                    ],
                    tl: /* [] */0
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

var array_suites = {
  hd: array_suites_0,
  tl: array_suites_1
};

Mt.from_pair_suites("Array_test", array_suites);

/*  Not a pure module */
