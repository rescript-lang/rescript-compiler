'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var x = [];

Caml_obj.caml_update_dummy(x, /* :: */[
      1,
      x
    ]);

var a = [];

var b = [];

var c = [];

Caml_obj.caml_update_dummy(a, /* :: */[
      2,
      b
    ]);

Caml_obj.caml_update_dummy(b, /* :: */[
      3,
      c
    ]);

Caml_obj.caml_update_dummy(c, /* :: */[
      3,
      a
    ]);

var xx = [];

Caml_obj.caml_update_dummy(xx, /* :: */[
      1,
      xx
    ]);

function naive(n) {
  if (n === 0 || n === 1) {
    return 1;
  } else {
    return (n + naive(n - 1 | 0) | 0) + naive(n - 2 | 0) | 0;
  }
}

var four = [2];

var three = [3];

var h = Block.__(250, [fib]);

var v = [(function () {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "rec_value_test.ml",
              23,
              24
            ]
          ];
    })];

function fib(n) {
  if (n > 3 || n < 0) {
    return fib(n - 1 | 0) + fib(n - 2 | 0) | 0;
  } else {
    switch (n) {
      case 0 : 
          return four[0];
      case 1 : 
          return 1;
      case 2 : 
          return three[0];
      case 3 : 
          var tag = h.tag | 0;
          v[0] = tag === 250 ? fib : (
              tag === 246 ? CamlinternalLazy.force_lazy_block(h) : h
            );
          return 1;
      
    }
  }
}

var ys = [];

Caml_obj.caml_update_dummy(ys, /* :: */[
      1,
      ys
    ]);

var xs_000 = /* :: */[
  2,
  /* :: */[
    List.hd(ys),
    /* [] */0
  ]
];

function zs() {
  return List.hd(xs[0]);
}

var xs_000$1 = /* :: */[
  2,
  /* [] */0
];

var xs = /* tuple */[
  xs_000$1,
  zs
];

function fib2(n) {
  if (n === 0 || n === 1) {
    return 1;
  } else {
    return fib2(n - 1 | 0) + fib2(n - 2 | 0) | 0;
  }
}

var two = 2;

function fib3(n) {
  if (n === 0 || n === 1) {
    return 1;
  } else {
    return fib3(n - 1 | 0) + fib3(n - 2 | 0) | 0;
  }
}

function even(n) {
  if (n === 0) {
    return /* true */1;
  } else {
    var n$1 = n - 1 | 0;
    if (n$1 === 1) {
      return /* true */1;
    } else {
      return even(n$1 - 1 | 0);
    }
  }
}

function even2(_n) {
  while(true) {
    var n = _n;
    if (n === 0) {
      return /* true */1;
    } else {
      _n = n - 1 | 0;
      continue ;
    }
  };
}

function lazy_v() {
  var tag = lazy_v.tag | 0;
  if (tag !== 250) {
    if (tag === 246) {
      CamlinternalLazy.force_lazy_block(lazy_v);
    }
    
  }
  return /* () */0;
}

function sum(_acc, _n) {
  while(true) {
    var n = _n;
    var acc = _acc;
    if (n > 0) {
      _n = n - 1 | 0;
      _acc = acc + n | 0;
      continue ;
    } else {
      return acc;
    }
  };
}

var fake_v = /* :: */[
  1,
  /* :: */[
    2,
    /* [] */0
  ]
];

var fake_y = /* :: */[
  2,
  /* :: */[
    3,
    /* [] */0
  ]
];

var fake_z = /* :: */[
  1,
  fake_y
];

var fake_y2 = /* :: */[
  2,
  /* :: */[
    3,
    /* [] */0
  ]
];

var fake_z2_001 = /* :: */[
  sum(0, 10),
  fake_y2
];

var fake_z2 = /* :: */[
  1,
  fake_z2_001
];

var rec_variant_b = [];

var rec_variant_a = [];

Caml_obj.caml_update_dummy(rec_variant_b, /* B */Block.__(0, [
        "gho",
        (function () {
            return rec_variant_a;
          })
      ]));

Caml_obj.caml_update_dummy(rec_variant_a, /* A */Block.__(1, [
        3,
        (function () {
            return rec_variant_b;
          })
      ]));

var suites_000 = /* tuple */[
  "hd",
  (function () {
      return /* Eq */Block.__(0, [
                1,
                List.hd(List.tl(x))
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "mutual",
    (function () {
        var tmp;
        if (a) {
          var match = a[1];
          if (match) {
            tmp = match[0];
          } else {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  [
                    "rec_value_test.ml",
                    108,
                    2
                  ]
                ];
          }
        } else {
          throw [
                Caml_builtin_exceptions.assert_failure,
                [
                  "rec_value_test.ml",
                  108,
                  2
                ]
              ];
        }
        return /* Eq */Block.__(0, [
                  3,
                  tmp
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "rec_sum",
      (function () {
          return /* Eq */Block.__(0, [
                    55,
                    sum(0, 10)
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "File \"rec_value_test.ml\", line 111, characters 2-9",
        (function () {
            return /* Eq */Block.__(0, [
                      /* :: */[
                        1,
                        /* :: */[
                          2,
                          /* [] */0
                        ]
                      ],
                      fake_v
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "File \"rec_value_test.ml\", line 114, characters 2-9",
          (function () {
              return /* Eq */Block.__(0, [
                        /* :: */[
                          2,
                          /* :: */[
                            3,
                            /* [] */0
                          ]
                        ],
                        fake_y
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "File \"rec_value_test.ml\", line 117, characters 2-9",
            (function () {
                return /* Eq */Block.__(0, [
                          /* :: */[
                            1,
                            /* :: */[
                              2,
                              /* :: */[
                                3,
                                /* [] */0
                              ]
                            ]
                          ],
                          fake_z
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "File \"rec_value_test.ml\", line 120, characters 2-9",
              (function () {
                  return /* Eq */Block.__(0, [
                            /* :: */[
                              1,
                              /* :: */[
                                55,
                                /* :: */[
                                  2,
                                  /* :: */[
                                    3,
                                    /* [] */0
                                  ]
                                ]
                              ]
                            ],
                            fake_z2
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "File \"rec_value_test.ml\", line 123, characters 2-9",
                (function () {
                    return /* Eq */Block.__(0, [
                              /* :: */[
                                2,
                                /* :: */[
                                  3,
                                  /* [] */0
                                ]
                              ],
                              fake_y2
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "File \"rec_value_test.ml\", line 126, characters 2-9",
                  (function () {
                      return /* Eq */Block.__(0, [
                                3,
                                3
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "File \"rec_value_test.ml\", line 129, characters 2-9",
                    (function () {
                        if (rec_variant_b.tag) {
                          throw [
                                Caml_builtin_exceptions.assert_failure,
                                [
                                  "rec_value_test.ml",
                                  132,
                                  11
                                ]
                              ];
                        } else {
                          return /* Eq */Block.__(0, [
                                    Curry._1(rec_variant_b[1], /* () */0),
                                    rec_variant_a
                                  ]);
                        }
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "File \"rec_value_test.ml\", line 134, characters 2-9",
                      (function () {
                          if (rec_variant_a.tag) {
                            return /* Eq */Block.__(0, [
                                      Curry._1(rec_variant_a[1], /* () */0),
                                      rec_variant_b
                                    ]);
                          } else {
                            throw [
                                  Caml_builtin_exceptions.assert_failure,
                                  [
                                    "rec_value_test.ml",
                                    137,
                                    11
                                  ]
                                ];
                          }
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

var suites = /* :: */[
  suites_000,
  suites_001
];

function fake_minus(n) {
  console.log(n);
  return n + 1 | 0;
}

var fake_odd = fake_minus;

function fake_inline_minus(n) {
  return n + 1 | 0;
}

function fake_inline(n) {
  return n + 1 | 0;
}

Mt.from_pair_suites("rec_value_test.ml", suites);

var v$1 = 3;

var fake_inline_inlie2 = 4;

exports.x = x;
exports.a = a;
exports.b = b;
exports.c = c;
exports.xx = xx;
exports.naive = naive;
exports.fib = fib;
exports.xs = xs;
exports.fib2 = fib2;
exports.two = two;
exports.fib3 = fib3;
exports.even = even;
exports.even2 = even2;
exports.lazy_v = lazy_v;
exports.sum = sum;
exports.fake_v = fake_v;
exports.fake_y = fake_y;
exports.fake_z = fake_z;
exports.fake_z2 = fake_z2;
exports.fake_y2 = fake_y2;
exports.v = v$1;
exports.rec_variant_b = rec_variant_b;
exports.rec_variant_a = rec_variant_a;
exports.suites = suites;
exports.fake_odd = fake_odd;
exports.fake_minus = fake_minus;
exports.fake_inline = fake_inline;
exports.fake_inline_minus = fake_inline_minus;
exports.fake_inline_inlie2 = fake_inline_inlie2;
/* xs Not a pure module */
