// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj        = require("../runtime/caml_obj");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Mt              = require("./mt");
var Block           = require("../runtime/block");
var Curry           = require("../runtime/curry");
var CamlinternalOO  = require("../stdlib/camlinternalOO");

var shared = [
  "leq",
  "value"
];

var shared$1 = ["repr"];

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      function () {
        return /* Eq */Block.__(0, [
                  x,
                  y
                ]);
      }
    ],
    suites[0]
  ];
  return /* () */0;
}

var comparable = {
  
};

Caml_obj.caml_update_dummy(comparable, [
      0,
      function ($$class) {
        CamlinternalOO.get_method_label($$class, "leq");
        return function (_, self) {
          return CamlinternalOO.create_object_opt(self, $$class);
        };
      },
      0,
      0
    ]);

function money_init($$class) {
  var ids = CamlinternalOO.new_methods_variables($$class, [
        "value",
        "leq"
      ], shared$1);
  var value = ids[0];
  var leq = ids[1];
  var repr = ids[2];
  var inh = CamlinternalOO.inherits($$class, 0, ["leq"], 0, comparable, 1);
  var obj_init = inh[0];
  CamlinternalOO.set_methods($$class, /* array */[
        value,
        function (self$neg2) {
          return self$neg2[repr];
        },
        leq,
        function (self$neg2, p) {
          return +(self$neg2[repr] <= (
                    p.tag === 248 ? Curry.js1(834174833, 1, p) : p.value
                  ));
        }
      ]);
  return function (_, self, x) {
    var self$1 = CamlinternalOO.create_object_opt(self, $$class);
    Curry._1(obj_init, self$1);
    self$1[repr] = x;
    return CamlinternalOO.run_initializers_opt(self, self$1, $$class);
  };
}

var money = CamlinternalOO.make_class(shared, money_init);

function money2_init($$class) {
  var times = CamlinternalOO.get_method_label($$class, "times");
  var inh = CamlinternalOO.inherits($$class, shared$1, 0, shared, money, 1);
  var obj_init = inh[0];
  var repr = inh[1];
  CamlinternalOO.set_method($$class, times, function (self$neg3, k) {
        var copy = Caml_exceptions.caml_set_oo_id(Caml_obj.caml_obj_dup(self$neg3));
        copy[repr] = k * self$neg3[repr];
        return copy;
      });
  return function (_, self, x) {
    var self$1 = CamlinternalOO.create_object_opt(self, $$class);
    Curry._2(obj_init, self$1, x);
    return CamlinternalOO.run_initializers_opt(self, self$1, $$class);
  };
}

var money2 = CamlinternalOO.make_class([
      "leq",
      "times",
      "value"
    ], money2_init);

function min(x, y) {
  if (x.tag === 248 ? Curry.js2(5393368, 2, x, y) : Curry._1(x.leq.bind(x), y)) {
    return x;
  }
  else {
    return y;
  }
}

var tmp = min(Curry._2(money[0], 0, 1.0), Curry._2(money[0], 0, 3.0));

eq('File "class8_test.ml", line 34, characters 5-12', 1, tmp.tag === 248 ? Curry.js1(834174833, 3, tmp) : tmp.value);

var tmp$1 = min(Curry._2(money2[0], 0, 5.0), Curry._2(money2[0], 0, 3));

eq('File "class8_test.ml", line 39, characters 5-12', 3, tmp$1.tag === 248 ? Curry.js1(834174833, 4, tmp$1) : tmp$1.value);

Mt.from_pair_suites("class8_test.ml", suites[0]);

exports.suites     = suites;
exports.test_id    = test_id;
exports.eq         = eq;
exports.comparable = comparable;
exports.money      = money;
exports.money2     = money2;
exports.min        = min;
/* money Not a pure module */
