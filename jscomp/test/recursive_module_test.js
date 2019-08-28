'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_module = require("../../lib/js/caml_module.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
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

function add(suite) {
  suites.contents = /* :: */[
    suite,
    suites.contents
  ];
  return /* () */0;
}

var Int3 = Caml_module.init_mod([
      "recursive_module_test.ml",
      13,
      6
    ], [[[
          0,
          "u"
        ]]]);

Caml_module.update_mod([[[
          0,
          "u"
        ]]], Int3, Int3);

var M = Caml_module.init_mod([
      "recursive_module_test.ml",
      20,
      20
    ], [[[
          0,
          "fact"
        ]]]);

function fact(n) {
  if (n <= 1) {
    return 1;
  } else {
    return Caml_int32.imul(n, Curry._1(M.fact, n - 1 | 0));
  }
}

Caml_module.update_mod([[[
          0,
          "fact"
        ]]], M, {
      fact: fact
    });

var fact$1 = M.fact;

var Fact = {
  M: M,
  fact: fact$1
};

eq("File \"recursive_module_test.ml\", line 30, characters 5-12", 120, Curry._1(fact$1, 5));

add(/* tuple */[
      "File \"recursive_module_test.ml\", line 34, characters 7-14",
      (function (param) {
          return /* ThrowAny */Block.__(7, [(function (param) {
                        Curry._1(Int3.u, 3);
                        return /* () */0;
                      })]);
        })
    ]);

Mt.from_pair_suites("Recursive_module_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.add = add;
exports.Int3 = Int3;
exports.Fact = Fact;
/* Int3 Not a pure module */
