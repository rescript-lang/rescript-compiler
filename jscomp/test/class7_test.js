// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt             = require("./mt");
var CamlinternalOO = require("../stdlib/camlinternalOO");
var Caml_curry     = require("../runtime/caml_curry");
var Oo             = require("../stdlib/oo");

var shared = [
  "move",
  "get_x"
];

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      function () {
        return /* Eq */{
                0: x,
                1: y,
                length: 2,
                tag: 0
              };
      }
    ],
    suites[0]
  ];
  return /* () */0;
}

function point_init($$class) {
  var ids = CamlinternalOO.new_methods_variables($$class, shared, ["x"]);
  var move = ids[0];
  var get_x = ids[1];
  var x = ids[2];
  CamlinternalOO.set_methods($$class, /* array */[
        get_x,
        function (self$neg1) {
          return self$neg1[x];
        },
        move,
        function (self$neg1, d) {
          self$neg1[x] = self$neg1[x] + d | 0;
          return /* () */0;
        }
      ]);
  return function (_, self, x_init) {
    var self$1 = CamlinternalOO.create_object_opt(self, $$class);
    self$1[x] = x_init;
    return self$1;
  };
}

var point = CamlinternalOO.make_class(shared, point_init);

var p = Caml_curry.app2(point[0], 0, 55);

var q = Oo.copy(p);

if (q.tag === 248) {
  Caml_curry.js2(-933174511, 1, q, 7);
}
else {
  Caml_curry.app1(q.move.bind(q), 7);
}

eq('File "class7_test.ml", line 22, characters 5-12', /* tuple */[
      55,
      62
    ], /* tuple */[
      p.tag === 248 ? Caml_curry.js1(291546447, 2, p) : p.get_x,
      q.tag === 248 ? Caml_curry.js1(291546447, 3, q) : q.get_x
    ]);

Mt.from_pair_suites("class7_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
exports.point   = point;
/* point Not a pure module */
