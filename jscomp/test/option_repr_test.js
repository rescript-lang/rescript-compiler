'use strict';

var Mt = require("./mt.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Belt_List = require("../../lib/js/belt_List.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
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
  if (match !== undefined && match) {
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

function f2(x, y, $staropt$star, param) {
  var z = $staropt$star !== undefined ? $staropt$star : 3;
  console.log(x);
  if (y !== undefined) {
    return y + z | 0;
  } else {
    return 0;
  }
}

function f3(x) {
  if (x !== undefined) {
    return 1;
  } else {
    return 0;
  }
}

function f4(x) {
  if (x !== undefined) {
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

var f10 = Caml_option.some(Caml_option.some(Caml_option.some(Caml_option.some(undefined))));

var f11 = Caml_option.some(f10);

var randomized = /* record */{
  contents: false
};

function create($staropt$star, param) {
  var random = $staropt$star !== undefined ? $staropt$star : randomized.contents;
  if (random) {
    return 2;
  } else {
    return 1;
  }
}

var ff = create(false, /* () */0);

function f13($staropt$star, $staropt$star$1, param) {
  var x = $staropt$star !== undefined ? $staropt$star : 3;
  var y = $staropt$star$1 !== undefined ? $staropt$star$1 : 4;
  return x + y | 0;
}

var a = f13(2, undefined, /* () */0);

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

b("File \"option_repr_test.ml\", line 94, characters 4-11", Caml_obj.caml_lessthan(undefined, null));

b("File \"option_repr_test.ml\", line 95, characters 4-11", !Caml_obj.caml_greaterthan(undefined, null));

b("File \"option_repr_test.ml\", line 96, characters 4-11", Caml_obj.caml_greaterthan(null, undefined));

b("File \"option_repr_test.ml\", line 97, characters 4-11", Caml_obj.caml_lessthan(undefined, Caml_option.some(undefined)));

b("File \"option_repr_test.ml\", line 98, characters 4-11", Caml_obj.caml_greaterthan(Caml_option.some(undefined), undefined));

console.log(6, undefined);

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

var xs_000 = gtx(Caml_option.some(null), Caml_option.some(undefined));

var xs = /* :: */[
  xs_000,
  /* [] */0
];

b("File \"option_repr_test.ml\", line 121, characters 5-12", Belt_List.every(xs, (function (x) {
            return x;
          })));

var xs_000$1 = ltx(Caml_option.some(undefined), 3);

var xs_001 = /* :: */[
  ltx(Caml_option.some(undefined), Caml_option.some(Caml_option.some(undefined))),
  /* :: */[
    ltx(Caml_option.some(undefined), "3"),
    /* :: */[
      ltx(Caml_option.some(undefined), true),
      /* :: */[
        ltx(Caml_option.some(undefined), false),
        /* :: */[
          ltx(false, true),
          /* :: */[
            ltx(false, true),
            /* :: */[
              ltx(undefined, Caml_option.some(undefined)),
              /* :: */[
                ltx(undefined, null),
                /* :: */[
                  ltx(undefined, (function (x) {
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

var xs_000$2 = eqx(undefined, undefined);

var xs_001$1 = /* :: */[
  neqx(undefined, null),
  /* :: */[
    eqx(Caml_option.some(undefined), Caml_option.some(undefined)),
    /* :: */[
      eqx(Caml_option.some(Caml_option.some(undefined)), Caml_option.some(Caml_option.some(undefined))),
      /* :: */[
        neqx(Caml_option.some(Caml_option.some(Caml_option.some(undefined))), Caml_option.some(Caml_option.some(undefined))),
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

Mt.from_pair_suites("Option_repr_test", suites.contents);

var f7 = undefined;

var f8 = Caml_option.some(undefined);

var f9 = Caml_option.some(Caml_option.some(undefined));

var N = 0;

var none_arg = undefined;

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
/* ff Not a pure module */
