'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var x = [];

Caml_obj.caml_update_dummy(x, /* constructor */{
      tag: "::",
      Arg0: 1,
      Arg1: x
    });

var a = [];

var b = [];

var c = [];

Caml_obj.caml_update_dummy(a, /* constructor */{
      tag: "::",
      Arg0: 2,
      Arg1: b
    });

Caml_obj.caml_update_dummy(b, /* constructor */{
      tag: "::",
      Arg0: 3,
      Arg1: c
    });

Caml_obj.caml_update_dummy(c, /* constructor */{
      tag: "::",
      Arg0: 3,
      Arg1: a
    });

var xx = [];

Caml_obj.caml_update_dummy(xx, /* constructor */{
      tag: "::",
      Arg0: 1,
      Arg1: xx
    });

function naive(n) {
  if (n === 0 || n === 1) {
    return 1;
  } else {
    return (n + naive(n - 1 | 0) | 0) + naive(n - 2 | 0) | 0;
  }
}

var four = /* record */[/* contents */2];

var three = /* record */[/* contents */3];

var h = Block.__(250, [fib]);

var v = /* record */[/* contents */(function (param) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            /* tuple */[
              "rec_value_test.ml",
              23,
              24
            ]
          ];
    })];

function fib(n) {
  switch (n) {
    case 0 :
        return four[0];
    case 1 :
        return 1;
    case 2 :
        return three[0];
    case 3 :
        v[0] = CamlinternalLazy.force(h);
        return 1;
    default:
      return fib(n - 1 | 0) + fib(n - 2 | 0) | 0;
  }
}

var ys = [];

Caml_obj.caml_update_dummy(ys, /* constructor */{
      tag: "::",
      Arg0: 1,
      Arg1: ys
    });

var xs_000 = /* constructor */{
  tag: "::",
  Arg0: 2,
  Arg1: /* constructor */{
    tag: "::",
    Arg0: List.hd(ys),
    Arg1: "[]"
  }
};

function zs(param) {
  return List.hd(xs[0]);
}

var xs_000$1 = /* constructor */{
  tag: "::",
  Arg0: 2,
  Arg1: "[]"
};

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
    return true;
  } else {
    var n$1 = n - 1 | 0;
    if (n$1 === 1) {
      return true;
    } else {
      return even(n$1 - 1 | 0);
    }
  }
}

function even2(_n) {
  while(true) {
    var n = _n;
    if (n === 0) {
      return true;
    } else {
      _n = n - 1 | 0;
      continue ;
    }
  };
}

function lazy_v(param) {
  CamlinternalLazy.force(lazy_v);
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

var fake_v = /* constructor */{
  tag: "::",
  Arg0: 1,
  Arg1: /* constructor */{
    tag: "::",
    Arg0: 2,
    Arg1: "[]"
  }
};

var fake_y = /* constructor */{
  tag: "::",
  Arg0: 2,
  Arg1: /* constructor */{
    tag: "::",
    Arg0: 3,
    Arg1: "[]"
  }
};

var fake_z = /* constructor */{
  tag: "::",
  Arg0: 1,
  Arg1: fake_y
};

var fake_y2 = /* constructor */{
  tag: "::",
  Arg0: 2,
  Arg1: /* constructor */{
    tag: "::",
    Arg0: 3,
    Arg1: "[]"
  }
};

var fake_z2 = /* constructor */{
  tag: "::",
  Arg0: 1,
  Arg1: /* constructor */{
    tag: "::",
    Arg0: sum(0, 10),
    Arg1: fake_y2
  }
};

var rec_variant_b = [];

var rec_variant_a = [];

Caml_obj.caml_update_dummy(rec_variant_b, /* constructor */{
      tag: "B",
      Arg0: "gho",
      Arg1: (function (param) {
          return rec_variant_a;
        })
    });

Caml_obj.caml_update_dummy(rec_variant_a, /* constructor */{
      tag: "A",
      Arg0: 3,
      Arg1: (function (param) {
          return rec_variant_b;
        })
    });

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "hd",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: 1,
                Arg1: List.hd(List.tl(x))
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "mutual",
      (function (param) {
          var tmp;
          var exit = 0;
          if (a !== "[]" && b !== "[]" && c !== "[]" && a !== "[]") {
            var match = a.Arg1;
            if (match !== "[]") {
              tmp = match.Arg0;
            } else {
              exit = 1;
            }
          } else {
            exit = 1;
          }
          if (exit === 1) {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "rec_value_test.ml",
                    108,
                    2
                  ]
                ];
          }
          return /* constructor */{
                  tag: "Eq",
                  Arg0: 3,
                  Arg1: tmp
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "rec_sum",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: 55,
                    Arg1: sum(0, 10)
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "File \"rec_value_test.ml\", line 111, characters 2-9",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: /* constructor */{
                        tag: "::",
                        Arg0: 1,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: 2,
                          Arg1: "[]"
                        }
                      },
                      Arg1: fake_v
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "File \"rec_value_test.ml\", line 114, characters 2-9",
            (function (param) {
                return /* constructor */{
                        tag: "Eq",
                        Arg0: /* constructor */{
                          tag: "::",
                          Arg0: 2,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: 3,
                            Arg1: "[]"
                          }
                        },
                        Arg1: fake_y
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "File \"rec_value_test.ml\", line 117, characters 2-9",
              (function (param) {
                  return /* constructor */{
                          tag: "Eq",
                          Arg0: /* constructor */{
                            tag: "::",
                            Arg0: 1,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: 2,
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: 3,
                                Arg1: "[]"
                              }
                            }
                          },
                          Arg1: fake_z
                        };
                })
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "File \"rec_value_test.ml\", line 120, characters 2-9",
                (function (param) {
                    return /* constructor */{
                            tag: "Eq",
                            Arg0: /* constructor */{
                              tag: "::",
                              Arg0: 1,
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: 55,
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: 2,
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: 3,
                                    Arg1: "[]"
                                  }
                                }
                              }
                            },
                            Arg1: fake_z2
                          };
                  })
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "File \"rec_value_test.ml\", line 123, characters 2-9",
                  (function (param) {
                      return /* constructor */{
                              tag: "Eq",
                              Arg0: /* constructor */{
                                tag: "::",
                                Arg0: 2,
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: 3,
                                  Arg1: "[]"
                                }
                              },
                              Arg1: fake_y2
                            };
                    })
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "File \"rec_value_test.ml\", line 126, characters 2-9",
                    (function (param) {
                        return /* constructor */{
                                tag: "Eq",
                                Arg0: 3,
                                Arg1: 3
                              };
                      })
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "File \"rec_value_test.ml\", line 129, characters 2-9",
                      (function (param) {
                          if (/* XXX */rec_variant_b.tag === "B") {
                            return /* constructor */{
                                    tag: "Eq",
                                    Arg0: Curry._1(rec_variant_b.Arg1, /* () */0),
                                    Arg1: rec_variant_a
                                  };
                          } else {
                            throw [
                                  Caml_builtin_exceptions.assert_failure,
                                  /* tuple */[
                                    "rec_value_test.ml",
                                    132,
                                    11
                                  ]
                                ];
                          }
                        })
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "File \"rec_value_test.ml\", line 134, characters 2-9",
                        (function (param) {
                            if (/* XXX */rec_variant_a.tag === "B") {
                              throw [
                                    Caml_builtin_exceptions.assert_failure,
                                    /* tuple */[
                                      "rec_value_test.ml",
                                      137,
                                      11
                                    ]
                                  ];
                            } else {
                              return /* constructor */{
                                      tag: "Eq",
                                      Arg0: Curry._1(rec_variant_a.Arg1, /* () */0),
                                      Arg1: rec_variant_b
                                    };
                            }
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

function fake_minus(n) {
  console.log(n);
  return n + 1 | 0;
}

var fake_odd = fake_minus;

function fake_inline_minus(n) {
  return n + 1 | 0;
}

var fake_inline = fake_inline_minus;

var fake_inline_inlie2 = fake_inline_minus(3);

Mt.from_pair_suites("Rec_value_test", suites);

var v$1 = 3;

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
