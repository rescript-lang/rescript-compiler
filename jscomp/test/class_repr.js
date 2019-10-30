'use strict';

var Oo = require("../../lib/js/oo.js");
var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_oo_curry = require("../../lib/js/caml_oo_curry.js");
var CamlinternalOO = require("../../lib/js/camlinternalOO.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var shared = /* array */["get_x"];

var shared$1 = /* array */[
  "incr",
  "get_money"
];

function x0_init($$class) {
  var v = CamlinternalOO.new_variable($$class, "");
  var x = CamlinternalOO.new_variable($$class, "x");
  return (function (env, self, v$1) {
      var self$1 = CamlinternalOO.create_object_opt(self, $$class);
      self$1[v] = v$1;
      self$1[x] = v$1 + 2 | 0;
      return self$1;
    });
}

var x0 = CamlinternalOO.make_class(0, x0_init);

function x_init($$class) {
  var v = CamlinternalOO.new_variable($$class, "");
  var ids = CamlinternalOO.new_methods_variables($$class, shared, /* array */["x"]);
  var get_x = ids[0];
  var x = ids[1];
  CamlinternalOO.set_method($$class, get_x, (function (self$2) {
          return self$2[x];
        }));
  return (function (env, self, v$1) {
      var self$1 = CamlinternalOO.create_object_opt(self, $$class);
      self$1[v] = v$1;
      self$1[x] = v$1;
      return self$1;
    });
}

var x = CamlinternalOO.make_class(shared, x_init);

var v = Curry._2(x[0], 0, 3);

var u = Oo.copy(v);

if (Caml_oo_curry.js1(291546447, 1, v) !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "class_repr.ml",
          30,
          9
        ]
      ];
}

if (Caml_oo_curry.js1(291546447, 2, u) !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "class_repr.ml",
          32,
          9
        ]
      ];
}

function xx_init($$class) {
  var x = CamlinternalOO.new_variable($$class, "");
  var ids = CamlinternalOO.new_methods_variables($$class, shared$1, /* array */["money"]);
  var incr = ids[0];
  var get_money = ids[1];
  var money = ids[2];
  CamlinternalOO.set_methods($$class, /* array */[
        get_money,
        (function (self$3) {
            return self$3[money];
          }),
        incr,
        (function (self$3) {
            var copy = Caml_exceptions.caml_set_oo_id(Caml_obj.caml_obj_dup(self$3));
            copy[money] = 2 * self$3[x] + Curry._1(self$3[0][get_money], self$3);
            return copy;
          })
      ]);
  return (function (env, self, x$1) {
      var self$1 = CamlinternalOO.create_object_opt(self, $$class);
      self$1[x] = x$1;
      self$1[money] = x$1;
      return self$1;
    });
}

var xx = CamlinternalOO.make_class(shared$1, xx_init);

var v1 = Curry._2(xx[0], 0, 3);

var v2 = Caml_oo_curry.js1(-977586732, 3, v1);

if (Caml_oo_curry.js1(-804710761, 4, v1) !== 3) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "class_repr.ml",
          44,
          9
        ]
      ];
}

console.log(/* tuple */[
      Caml_oo_curry.js1(-804710761, 5, v1),
      Caml_oo_curry.js1(-804710761, 6, v2)
    ]);

if (Caml_oo_curry.js1(-804710761, 7, v2) !== 9) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "class_repr.ml",
          46,
          9
        ]
      ];
}

exports.x0 = x0;
exports.x = x;
exports.v = v;
exports.u = u;
exports.xx = xx;
exports.v1 = v1;
exports.v2 = v2;
/* x0 Not a pure module */
