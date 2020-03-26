'use strict';

var Mt = require("./mt.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Belt_List = require("../../lib/js/belt_List.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, v) {
  return Mt.bool_suites(test_id, suites, loc, v);
}

function f0(x) {
  var match = x[1];
  if (match !== void 0 && match) {
    return 1;
  } else {
    return 2;
  }
}

function f1(u) {
  if (u) {
    return 0;
  } else {
    return 1;
  }
}

function f2(x, y, zOpt, param) {
  var z = zOpt !== void 0 ? zOpt : 3;
  console.log(x);
  if (y !== void 0) {
    return y + z | 0;
  } else {
    return 0;
  }
}

function f3(x) {
  if (x !== void 0) {
    return 1;
  } else {
    return 0;
  }
}

function f4(x) {
  if (x !== void 0) {
    return x + 1 | 0;
  } else {
    return 0;
  }
}

function f5(a) {
  return false;
}

function f6(a) {
  return true;
}

var f10 = Caml_option.some(Caml_option.some(Caml_option.some(Caml_option.some(void 0))));

var f11 = Caml_option.some(f10);

var randomized = {
  contents: false
};

function create(randomOpt, param) {
  var random = randomOpt !== void 0 ? randomOpt : randomized.contents;
  if (random) {
    return 2;
  } else {
    return 1;
  }
}

var ff = create(false, void 0);

function f13(xOpt, yOpt, param) {
  var x = xOpt !== void 0 ? xOpt : 3;
  var y = yOpt !== void 0 ? yOpt : 4;
  return x + y | 0;
}

var a = f13(2, void 0, void 0);

function f12(x) {
  return x;
}

var length_8_id = Belt_List.makeBy(8, (function (x) {
        return x;
      }));

var length_10_id = Belt_List.makeBy(10, (function (x) {
        return x;
      }));

function f13$1(param) {
  return Caml_obj.caml_equal(Belt_List.take(length_10_id, 8), /* :: */[
              1,
              /* :: */[
                2,
                /* :: */[
                  3,
                  /* [] */0
                ]
              ]
            ]);
}

b("File \"option_repr_test.ml\", line 94, characters 4-11", Caml_obj.caml_lessthan(void 0, null));

b("File \"option_repr_test.ml\", line 95, characters 4-11", !Caml_obj.caml_greaterthan(void 0, null));

b("File \"option_repr_test.ml\", line 96, characters 4-11", Caml_obj.caml_greaterthan(null, void 0));

b("File \"option_repr_test.ml\", line 97, characters 4-11", Caml_obj.caml_lessthan(void 0, Caml_option.some(void 0)));

b("File \"option_repr_test.ml\", line 98, characters 4-11", Caml_obj.caml_greaterthan(Caml_option.some(void 0), void 0));

console.log(6, void 0);

function ltx(a, b) {
  if (Caml_obj.caml_lessthan(a, b)) {
    return Caml_obj.caml_greaterthan(b, a);
  } else {
    return false;
  }
}

function gtx(a, b) {
  if (Caml_obj.caml_greaterthan(a, b)) {
    return Caml_obj.caml_lessthan(b, a);
  } else {
    return false;
  }
}

function eqx(a, b) {
  if (Caml_obj.caml_equal(a, b)) {
    return Caml_obj.caml_equal(b, a);
  } else {
    return false;
  }
}

function neqx(a, b) {
  if (Caml_obj.caml_notequal(a, b)) {
    return Caml_obj.caml_notequal(b, a);
  } else {
    return false;
  }
}

function all_true(xs) {
  return Belt_List.every(xs, (function (x) {
                return x;
              }));
}

var xs_000 = gtx(Caml_option.some(null), Caml_option.some(void 0));

var xs = /* :: */[
  xs_000,
  /* [] */0
];

b("File \"option_repr_test.ml\", line 121, characters 5-12", Belt_List.every(xs, (function (x) {
            return x;
          })));

var xs_000$1 = ltx(Caml_option.some(void 0), 3);

var xs_001 = /* :: */[
  ltx(Caml_option.some(void 0), Caml_option.some(Caml_option.some(void 0))),
  /* :: */[
    ltx(Caml_option.some(void 0), "3"),
    /* :: */[
      ltx(Caml_option.some(void 0), true),
      /* :: */[
        ltx(Caml_option.some(void 0), false),
        /* :: */[
          ltx(false, true),
          /* :: */[
            ltx(false, true),
            /* :: */[
              ltx(void 0, Caml_option.some(void 0)),
              /* :: */[
                ltx(void 0, null),
                /* :: */[
                  ltx(void 0, (function (x) {
                          return x;
                        })),
                  /* :: */[
                    ltx(null, 3),
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

var xs$1 = /* :: */[
  xs_000$1,
  xs_001
];

b("File \"option_repr_test.ml\", line 127, characters 5-12", Belt_List.every(xs$1, (function (x) {
            return x;
          })));

var xs_000$2 = eqx(void 0, void 0);

var xs_001$1 = /* :: */[
  neqx(void 0, null),
  /* :: */[
    eqx(Caml_option.some(void 0), Caml_option.some(void 0)),
    /* :: */[
      eqx(Caml_option.some(Caml_option.some(void 0)), Caml_option.some(Caml_option.some(void 0))),
      /* :: */[
        neqx(Caml_option.some(Caml_option.some(Caml_option.some(void 0))), Caml_option.some(Caml_option.some(void 0))),
        /* [] */0
      ]
    ]
  ]
];

var xs$2 = /* :: */[
  xs_000$2,
  xs_001$1
];

b("File \"option_repr_test.ml\", line 143, characters 5-12", Belt_List.every(xs$2, (function (x) {
            return x;
          })));

function v(x) {
  return x;
}

function v0(x) {
  return x;
}

var N0 = {
  v: v,
  v0: v0
};

Mt.from_pair_suites("Option_repr_test", suites.contents);

var f7;

var f8 = Caml_option.some(void 0);

var f9 = Caml_option.some(Caml_option.some(void 0));

var N = /* alias */0;

var none_arg;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.f0 = f0;
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.f5 = f5;
exports.f6 = f6;
exports.f7 = f7;
exports.f8 = f8;
exports.f9 = f9;
exports.f10 = f10;
exports.f11 = f11;
exports.randomized = randomized;
exports.create = create;
exports.ff = ff;
exports.a = a;
exports.f12 = f12;
exports.N = N;
exports.length_8_id = length_8_id;
exports.length_10_id = length_10_id;
exports.f13 = f13$1;
exports.none_arg = none_arg;
exports.ltx = ltx;
exports.gtx = gtx;
exports.eqx = eqx;
exports.neqx = neqx;
exports.all_true = all_true;
exports.N0 = N0;
/* ff Not a pure module */
