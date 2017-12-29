'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_oo_curry = require("../../lib/js/caml_oo_curry.js");
var CamlinternalOO = require("../../lib/js/camlinternalOO.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var shared = ["m"];

var shared$1 = [
  "move",
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

function point_init($$class) {
  var ids = CamlinternalOO.new_methods_variables($$class, shared$1, shared$2);
  var move = ids[0];
  var get_x = ids[1];
  var x = ids[2];
  CamlinternalOO.set_methods($$class, /* array */[
        get_x,
        (function (self$1) {
            return self$1[x];
          }),
        move,
        (function (self$1, d) {
            self$1[x] = self$1[x] + d | 0;
            return /* () */0;
          })
      ]);
  return (function (_, self, x_init) {
      var self$1 = CamlinternalOO.create_object_opt(self, $$class);
      self$1[x] = x_init;
      return self$1;
    });
}

var point = CamlinternalOO.make_class(shared$1, point_init);

function colored_point_init($$class) {
  var ids = CamlinternalOO.new_methods_variables($$class, ["color"], ["c"]);
  var color = ids[0];
  var c = ids[1];
  var inh = CamlinternalOO.inherits($$class, shared$2, 0, [
        "get_x",
        "move"
      ], point, 1);
  var obj_init = inh[0];
  CamlinternalOO.set_method($$class, color, (function (self$2) {
          return self$2[c];
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
      "get_x"
    ], colored_point_init);

function colored_point_to_point(cp) {
  return cp;
}

var p = Curry._2(point[0], 0, 3);

var q = Curry._3(colored_point[0], 0, 4, "blue");

function lookup_obj(obj, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var obj$prime = param[0];
      if (Caml_obj.caml_equal(obj, obj$prime)) {
        return obj$prime;
      } else {
        _param = param[1];
        continue ;
        
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function c_init($$class) {
  var m = CamlinternalOO.get_method_label($$class, "m");
  CamlinternalOO.set_method($$class, m, (function () {
          return 1;
        }));
  return (function (_, self) {
      return CamlinternalOO.create_object_opt(self, $$class);
    });
}

var c = CamlinternalOO.make_class(shared, c_init);

function d_init($$class) {
  var ids = CamlinternalOO.get_method_labels($$class, [
        "n",
        "as_c"
      ]);
  var n = ids[0];
  var as_c = ids[1];
  var inh = CamlinternalOO.inherits($$class, 0, 0, shared, c, 1);
  var obj_init = inh[0];
  CamlinternalOO.set_methods($$class, /* array */[
        n,
        (function () {
            return 2;
          }),
        as_c,
        (function (self$4) {
            return self$4;
          })
      ]);
  return (function (_, self) {
      var self$1 = CamlinternalOO.create_object_opt(self, $$class);
      Curry._1(obj_init, self$1);
      return CamlinternalOO.run_initializers_opt(self, self$1, $$class);
    });
}

var table = CamlinternalOO.create_table([
      "as_c",
      "m",
      "n"
    ]);

var env_init = d_init(table);

CamlinternalOO.init_class(table);

var d_000 = Curry._1(env_init, 0);

var d = [
  d_000,
  d_init,
  env_init,
  0
];

function c2$prime_001($$class) {
  CamlinternalOO.get_method_label($$class, "m");
  return (function (_, self) {
      return CamlinternalOO.create_object_opt(self, $$class);
    });
}

var c2$prime = [
  0,
  c2$prime_001,
  0,
  0
];

function functional_point_init($$class) {
  var ids = CamlinternalOO.new_methods_variables($$class, shared$1, shared$2);
  var move = ids[0];
  var get_x = ids[1];
  var x = ids[2];
  CamlinternalOO.set_methods($$class, /* array */[
        get_x,
        (function (self$6) {
            return self$6[x];
          }),
        move,
        (function (self$6, d) {
            var copy = Caml_exceptions.caml_set_oo_id(Caml_obj.caml_obj_dup(self$6));
            copy[x] = self$6[x] + d | 0;
            return copy;
          })
      ]);
  return (function (_, self, y) {
      var self$1 = CamlinternalOO.create_object_opt(self, $$class);
      self$1[x] = y;
      return self$1;
    });
}

var functional_point = CamlinternalOO.make_class(shared$1, functional_point_init);

var p$1 = Curry._2(functional_point[0], 0, 7);

var tmp = Caml_oo_curry.js2(-933174511, 2, p$1, 3);

eq("File \"class6_test.ml\", line 60, characters 5-12", /* tuple */[
      7,
      10,
      7
    ], /* tuple */[
      Caml_oo_curry.js1(291546447, 1, p$1),
      Caml_oo_curry.js1(291546447, 3, tmp),
      Caml_oo_curry.js1(291546447, 4, p$1)
    ]);

function bad_functional_point_init($$class) {
  var ids = CamlinternalOO.new_methods_variables($$class, shared$1, shared$2);
  var move = ids[0];
  var get_x = ids[1];
  var x = ids[2];
  CamlinternalOO.set_methods($$class, /* array */[
        get_x,
        (function (self$7) {
            return self$7[x];
          }),
        move,
        (function (self$7, d) {
            return Curry._2(bad_functional_point[0], 0, self$7[x] + d | 0);
          })
      ]);
  return (function (_, self, y) {
      var self$1 = CamlinternalOO.create_object_opt(self, $$class);
      self$1[x] = y;
      return self$1;
    });
}

var table$1 = CamlinternalOO.create_table(shared$1);

var env_init$1 = bad_functional_point_init(table$1);

CamlinternalOO.init_class(table$1);

var bad_functional_point_000 = Curry._1(env_init$1, 0);

var bad_functional_point = [
  bad_functional_point_000,
  bad_functional_point_init,
  env_init$1,
  0
];

var p$2 = Curry._2(bad_functional_point_000, 0, 7);

var tmp$1 = Caml_oo_curry.js2(-933174511, 6, p$2, 3);

eq("File \"class6_test.ml\", line 74, characters 5-12", /* tuple */[
      7,
      10,
      7
    ], /* tuple */[
      Caml_oo_curry.js1(291546447, 5, p$2),
      Caml_oo_curry.js1(291546447, 7, tmp$1),
      Caml_oo_curry.js1(291546447, 8, p$2)
    ]);

Mt.from_pair_suites("class6_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.point = point;
exports.colored_point = colored_point;
exports.colored_point_to_point = colored_point_to_point;
exports.p = p;
exports.q = q;
exports.lookup_obj = lookup_obj;
exports.c = c;
exports.d = d;
exports.c2$prime = c2$prime;
exports.functional_point = functional_point;
exports.bad_functional_point = bad_functional_point;
/* point Not a pure module */
