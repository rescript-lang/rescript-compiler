'use strict';

var Mt              = require("./mt.js");
var Block           = require("../../lib/js/block.js");
var Curry           = require("../../lib/js/curry.js");
var Caml_obj        = require("../../lib/js/caml_obj.js");
var Caml_oo_curry   = require("../../lib/js/caml_oo_curry.js");
var CamlinternalOO  = require("../../lib/js/camlinternalOO.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var shared = ["repr"];

var shared$1 = [
  "leq",
  "value"
];

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

function comparable_001($$class) {
  CamlinternalOO.get_method_label($$class, "leq");
  return (function (_, self) {
      return CamlinternalOO.create_object_opt(self, $$class);
    });
}

var comparable = [
  0,
  comparable_001,
  0,
  0
];

function money_init($$class) {
  var ids = CamlinternalOO.new_methods_variables($$class, [
        "value",
        "leq"
      ], shared);
  var value = ids[0];
  var leq = ids[1];
  var repr = ids[2];
  var inh = CamlinternalOO.inherits($$class, 0, ["leq"], 0, comparable, 1);
  var obj_init = inh[0];
  CamlinternalOO.set_methods($$class, /* array */[
        value,
        (function (self$2) {
            return self$2[repr];
          }),
        leq,
        (function (self$2, p) {
            return +(self$2[repr] <= Caml_oo_curry.js1(834174833, 1, p));
          })
      ]);
  return (function (_, self, x) {
      var self$1 = CamlinternalOO.create_object_opt(self, $$class);
      Curry._1(obj_init, self$1);
      self$1[repr] = x;
      return CamlinternalOO.run_initializers_opt(self, self$1, $$class);
    });
}

var money = CamlinternalOO.make_class(shared$1, money_init);

function money2_init($$class) {
  var times = CamlinternalOO.get_method_label($$class, "times");
  var inh = CamlinternalOO.inherits($$class, shared, 0, shared$1, money, 1);
  var obj_init = inh[0];
  var repr = inh[1];
  CamlinternalOO.set_method($$class, times, (function (self$3, k) {
          var copy = Caml_exceptions.caml_set_oo_id(Caml_obj.caml_obj_dup(self$3));
          copy[repr] = k * self$3[repr];
          return copy;
        }));
  return (function (_, self, x) {
      var self$1 = CamlinternalOO.create_object_opt(self, $$class);
      Curry._2(obj_init, self$1, x);
      return CamlinternalOO.run_initializers_opt(self, self$1, $$class);
    });
}

var money2 = CamlinternalOO.make_class([
      "leq",
      "times",
      "value"
    ], money2_init);

function min(x, y) {
  if (Caml_oo_curry.js2(5393368, 2, x, y)) {
    return x;
  } else {
    return y;
  }
}

var tmp = min(Curry._2(money[0], 0, 1.0), Curry._2(money[0], 0, 3.0));

eq("File \"class8_test.ml\", line 34, characters 5-12", 1, Caml_oo_curry.js1(834174833, 3, tmp));

var tmp$1 = min(Curry._2(money2[0], 0, 5.0), Curry._2(money2[0], 0, 3));

eq("File \"class8_test.ml\", line 39, characters 5-12", 3, Caml_oo_curry.js1(834174833, 4, tmp$1));

Mt.from_pair_suites("class8_test.ml", suites[0]);

exports.suites     = suites;
exports.test_id    = test_id;
exports.eq         = eq;
exports.comparable = comparable;
exports.money      = money;
exports.money2     = money2;
exports.min        = min;
/* money Not a pure module */
