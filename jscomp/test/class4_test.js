'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_oo_curry = require("../../lib/js/caml_oo_curry.js");
var CamlinternalOO = require("../../lib/js/camlinternalOO.js");

var shared = [
  "move",
  "get_x"
];

var shared$1 = [
  "bump",
  "get_x"
];

var shared$2 = ["x"];

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

function restricted_point_init($$class) {
  var ids = CamlinternalOO.new_methods_variables($$class, [
        "move",
        "get_x",
        "bump"
      ], shared$2);
  var move = ids[0];
  var get_x = ids[1];
  var bump = ids[2];
  var x = ids[3];
  CamlinternalOO.set_methods($$class, /* array */[
        get_x,
        (function (self$1) {
            return self$1[x];
          }),
        move,
        (function (self$1, d) {
            self$1[x] = self$1[x] + d | 0;
            return /* () */0;
          }),
        bump,
        (function (self$1) {
            return Curry._2(self$1[0][move], self$1, 1);
          })
      ]);
  return (function (_, self, x_init) {
      var self$1 = CamlinternalOO.create_object_opt(self, $$class);
      self$1[x] = x_init;
      return self$1;
    });
}

var restricted_point = CamlinternalOO.make_class(shared$1, restricted_point_init);

function restricted_point$prime_init($$class) {
  var inh = CamlinternalOO.inherits($$class, 0, 0, shared$1, restricted_point, 1);
  var obj_init = inh[0];
  return (function (_, self, x) {
      return Curry._2(obj_init, self, x);
    });
}

var restricted_point$prime = CamlinternalOO.make_class(shared$1, restricted_point$prime_init);

function restricted_point2$prime_init($$class) {
  var inh = CamlinternalOO.inherits($$class, 0, 0, shared$1, restricted_point, 1);
  var obj_init = inh[0];
  return (function (_, self, x) {
      return Curry._2(obj_init, self, x);
    });
}

var restricted_point2$prime = CamlinternalOO.make_class(shared$1, restricted_point2$prime_init);

var Point = /* module */[/* restricted_point' */restricted_point];

function abstract_point_001($$class) {
  var x_init = CamlinternalOO.new_variable($$class, "");
  var ids = CamlinternalOO.get_method_labels($$class, [
        "move",
        "get_x",
        "get_offset"
      ]);
  var get_x = ids[1];
  var get_offset = ids[2];
  CamlinternalOO.set_method($$class, get_offset, (function (self$5) {
          return Curry._1(self$5[0][get_x], self$5) - self$5[x_init] | 0;
        }));
  return (function (_, self, x_init$1) {
      var self$1 = CamlinternalOO.create_object_opt(self, $$class);
      self$1[x_init] = x_init$1;
      return self$1;
    });
}

var abstract_point = [
  0,
  abstract_point_001,
  0,
  0
];

function point_init($$class) {
  var ids = CamlinternalOO.new_methods_variables($$class, shared, shared$2);
  var move = ids[0];
  var get_x = ids[1];
  var x = ids[2];
  var inh = CamlinternalOO.inherits($$class, 0, shared, ["get_offset"], abstract_point, 1);
  var obj_init = inh[0];
  CamlinternalOO.set_methods($$class, /* array */[
        get_x,
        (function (self$6) {
            return self$6[x];
          }),
        move,
        (function (self$6, d) {
            self$6[x] = self$6[x] + d | 0;
            return /* () */0;
          })
      ]);
  return (function (_, self, x_init) {
      var self$1 = CamlinternalOO.create_object_opt(self, $$class);
      Curry._2(obj_init, self$1, x_init);
      self$1[x] = x_init;
      return CamlinternalOO.run_initializers_opt(self, self$1, $$class);
    });
}

var point = CamlinternalOO.make_class([
      "move",
      "get_offset",
      "get_x"
    ], point_init);

function colored_point_init($$class) {
  var ids = CamlinternalOO.new_methods_variables($$class, ["color"], ["c"]);
  var color = ids[0];
  var c = ids[1];
  var inh = CamlinternalOO.inherits($$class, shared$2, 0, [
        "get_offset",
        "get_x",
        "move"
      ], point, 1);
  var obj_init = inh[0];
  CamlinternalOO.set_method($$class, color, (function (self$7) {
          return self$7[c];
        }));
  return (function (_, self, x, c$1) {
      var self$1 = CamlinternalOO.create_object_opt(self, $$class);
      Curry._2(obj_init, self$1, x);
      self$1[c] = c$1;
      return CamlinternalOO.run_initializers_opt(self, self$1, $$class);
    });
}

var colored_point = CamlinternalOO.make_class([
      "move",
      "color",
      "get_offset",
      "get_x"
    ], colored_point_init);

var p$prime = Curry._3(colored_point[0], 0, 5, "red");

eq("File \"class4_test.ml\", line 67, characters 5-12", /* tuple */[
      5,
      "red"
    ], /* tuple */[
      Caml_oo_curry.js1(291546447, 1, p$prime),
      Caml_oo_curry.js1(-899911325, 2, p$prime)
    ]);

function get_succ_x(p) {
  return Caml_oo_curry.js1(291546447, 3, p) + 1 | 0;
}

eq("File \"class4_test.ml\", line 71, characters 12-19", 6, get_succ_x(p$prime));

function set_x(p) {
  return Caml_oo_curry.js1(-97543333, 4, p);
}

function incr(p) {
  return Curry._1(set_x(p), get_succ_x(p));
}

Mt.from_pair_suites("class4_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.restricted_point = restricted_point;
exports.restricted_point$prime = restricted_point$prime;
exports.restricted_point2$prime = restricted_point2$prime;
exports.Point = Point;
exports.abstract_point = abstract_point;
exports.point = point;
exports.colored_point = colored_point;
exports.p$prime = p$prime;
exports.get_succ_x = get_succ_x;
exports.set_x = set_x;
exports.incr = incr;
/* restricted_point Not a pure module */
