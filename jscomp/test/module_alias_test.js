'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
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

function f(x) {
  console.log(x);
  console.log(List.length(x));
  return /* List */[
          List.length,
          List.compare_lengths,
          List.compare_length_with,
          List.cons,
          List.hd,
          List.tl,
          List.nth,
          List.nth_opt,
          List.rev,
          List.init,
          List.append,
          List.rev_append,
          List.concat,
          List.flatten,
          List.iter,
          List.iteri,
          List.map,
          List.mapi,
          List.rev_map,
          List.fold_left,
          List.fold_right,
          List.iter2,
          List.map2,
          List.rev_map2,
          List.fold_left2,
          List.fold_right2,
          List.for_all,
          List.exists,
          List.for_all2,
          List.exists2,
          List.mem,
          List.memq,
          List.find,
          List.find_opt,
          List.filter,
          List.find_all,
          List.partition,
          List.assoc,
          List.assoc_opt,
          List.assq,
          List.assq_opt,
          List.mem_assoc,
          List.mem_assq,
          List.remove_assoc,
          List.remove_assq,
          List.split,
          List.combine,
          List.sort,
          List.stable_sort,
          List.fast_sort,
          List.sort_uniq,
          List.merge
        ];
}

var h = f(/* [] */0);

var a = Curry._1(h[/* length */0], /* :: */[
      1,
      /* :: */[
        2,
        /* :: */[
          3,
          /* [] */0
        ]
      ]
    ]);

eq("File \"module_alias_test.ml\", line 30, characters 6-13", a, 3);

Mt.from_pair_suites("Module_alias_test", suites[0]);

var N = 0;

var V = 0;

var J = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.N = N;
exports.V = V;
exports.J = J;
exports.f = f;
exports.a = a;
/* h Not a pure module */
