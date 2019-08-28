'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Pervasives = require("../../lib/js/pervasives.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  console.log(/* tuple */[
        x,
        y
      ]);
  Pervasives.incr(test_id);
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  return /* () */0;
}

function f(x) {
  var y = Caml_obj.caml_obj_dup(x);
  return /* record */{
          a0: 1,
          a1: y.a1,
          a2: y.a2,
          a3: y.a3,
          a4: y.a4,
          a5: y.a5
        };
}

eq("File \"update_record_test.ml\", line 30, characters 5-12", 1, f(/* record */{
          a0: 0,
          a1: 0,
          a2: 0,
          a3: 0,
          a4: 0,
          a5: 0
        }).a0);

Mt.from_pair_suites("Update_record_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
/*  Not a pure module */
