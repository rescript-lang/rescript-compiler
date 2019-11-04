'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
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

var v = {
  contents: 0
};

function Make(U) {
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  return {
          length: U.length,
          compare_lengths: U.compare_lengths,
          compare_length_with: U.compare_length_with,
          cons: U.cons,
          hd: U.hd,
          tl: U.tl,
          nth: U.nth,
          nth_opt: U.nth_opt,
          rev: U.rev,
          init: U.init,
          append: U.append,
          rev_append: U.rev_append,
          concat: U.concat,
          flatten: U.flatten,
          iter: U.iter,
          iteri: U.iteri,
          map: U.map,
          mapi: U.mapi,
          rev_map: U.rev_map,
          fold_left: U.fold_left,
          fold_right: U.fold_right,
          iter2: U.iter2,
          map2: U.map2,
          rev_map2: U.rev_map2,
          fold_left2: U.fold_left2,
          fold_right2: U.fold_right2,
          for_all: U.for_all,
          exists: U.exists,
          for_all2: U.for_all2,
          exists2: U.exists2,
          mem: U.mem,
          memq: U.memq,
          find: U.find,
          find_opt: U.find_opt,
          filter: U.filter,
          find_all: U.find_all,
          partition: U.partition,
          assoc: U.assoc,
          assoc_opt: U.assoc_opt,
          assq: U.assq,
          assq_opt: U.assq_opt,
          mem_assoc: U.mem_assoc,
          mem_assq: U.mem_assq,
          remove_assoc: U.remove_assoc,
          remove_assq: U.remove_assq,
          split: U.split,
          combine: U.combine,
          sort: U.sort,
          stable_sort: U.stable_sort,
          fast_sort: U.fast_sort,
          sort_uniq: U.sort_uniq,
          merge: U.merge
        };
}

function f(param) {
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  return {
          length: List.length,
          compare_lengths: List.compare_lengths,
          compare_length_with: List.compare_length_with,
          cons: List.cons,
          hd: List.hd,
          tl: List.tl,
          nth: List.nth,
          nth_opt: List.nth_opt,
          rev: List.rev,
          init: List.init,
          append: List.append,
          rev_append: List.rev_append,
          concat: List.concat,
          flatten: List.flatten,
          iter: List.iter,
          iteri: List.iteri,
          map: List.map,
          mapi: List.mapi,
          rev_map: List.rev_map,
          fold_left: List.fold_left,
          fold_right: List.fold_right,
          iter2: List.iter2,
          map2: List.map2,
          rev_map2: List.rev_map2,
          fold_left2: List.fold_left2,
          fold_right2: List.fold_right2,
          for_all: List.for_all,
          exists: List.exists,
          for_all2: List.for_all2,
          exists2: List.exists2,
          mem: List.mem,
          memq: List.memq,
          find: List.find,
          find_opt: List.find_opt,
          filter: List.filter,
          find_all: List.find_all,
          partition: List.partition,
          assoc: List.assoc,
          assoc_opt: List.assoc_opt,
          assq: List.assq,
          assq_opt: List.assq_opt,
          mem_assoc: List.mem_assoc,
          mem_assq: List.mem_assq,
          remove_assoc: List.remove_assoc,
          remove_assq: List.remove_assq,
          split: List.split,
          combine: List.combine,
          sort: List.sort,
          stable_sort: List.stable_sort,
          fast_sort: List.fast_sort,
          sort_uniq: List.sort_uniq,
          merge: List.merge
        };
}

eq("File \"global_module_alias_test.ml\", line 51, characters 5-12", List.length(/* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]), 2);

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

var H = {
  length: List.length,
  compare_lengths: List.compare_lengths,
  compare_length_with: List.compare_length_with,
  cons: List.cons,
  hd: List.hd,
  tl: List.tl,
  nth: List.nth,
  nth_opt: List.nth_opt,
  rev: List.rev,
  init: List.init,
  append: List.append,
  rev_append: List.rev_append,
  concat: List.concat,
  flatten: List.flatten,
  iter: List.iter,
  iteri: List.iteri,
  map: List.map,
  mapi: List.mapi,
  rev_map: List.rev_map,
  fold_left: List.fold_left,
  fold_right: List.fold_right,
  iter2: List.iter2,
  map2: List.map2,
  rev_map2: List.rev_map2,
  fold_left2: List.fold_left2,
  fold_right2: List.fold_right2,
  for_all: List.for_all,
  exists: List.exists,
  for_all2: List.for_all2,
  exists2: List.exists2,
  mem: List.mem,
  memq: List.memq,
  find: List.find,
  find_opt: List.find_opt,
  filter: List.filter,
  find_all: List.find_all,
  partition: List.partition,
  assoc: List.assoc,
  assoc_opt: List.assoc_opt,
  assq: List.assq,
  assq_opt: List.assq_opt,
  mem_assoc: List.mem_assoc,
  mem_assq: List.mem_assq,
  remove_assoc: List.remove_assoc,
  remove_assq: List.remove_assq,
  split: List.split,
  combine: List.combine,
  sort: List.sort,
  stable_sort: List.stable_sort,
  fast_sort: List.fast_sort,
  sort_uniq: List.sort_uniq,
  merge: List.merge
};

eq("File \"global_module_alias_test.ml\", line 57, characters 5-12", v.contents, 12);

function g(param) {
  return List.length(/* :: */[
              1,
              /* :: */[
                2,
                /* :: */[
                  3,
                  /* :: */[
                    4,
                    /* [] */0
                  ]
                ]
              ]
            ]);
}

function xx(param) {
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  return {
          length: List.length,
          compare_lengths: List.compare_lengths,
          compare_length_with: List.compare_length_with,
          cons: List.cons,
          hd: List.hd,
          tl: List.tl,
          nth: List.nth,
          nth_opt: List.nth_opt,
          rev: List.rev,
          init: List.init,
          append: List.append,
          rev_append: List.rev_append,
          concat: List.concat,
          flatten: List.flatten,
          iter: List.iter,
          iteri: List.iteri,
          map: List.map,
          mapi: List.mapi,
          rev_map: List.rev_map,
          fold_left: List.fold_left,
          fold_right: List.fold_right,
          iter2: List.iter2,
          map2: List.map2,
          rev_map2: List.rev_map2,
          fold_left2: List.fold_left2,
          fold_right2: List.fold_right2,
          for_all: List.for_all,
          exists: List.exists,
          for_all2: List.for_all2,
          exists2: List.exists2,
          mem: List.mem,
          memq: List.memq,
          find: List.find,
          find_opt: List.find_opt,
          filter: List.filter,
          find_all: List.find_all,
          partition: List.partition,
          assoc: List.assoc,
          assoc_opt: List.assoc_opt,
          assq: List.assq,
          assq_opt: List.assq_opt,
          mem_assoc: List.mem_assoc,
          mem_assq: List.mem_assq,
          remove_assoc: List.remove_assoc,
          remove_assq: List.remove_assq,
          split: List.split,
          combine: List.combine,
          sort: List.sort,
          stable_sort: List.stable_sort,
          fast_sort: List.fast_sort,
          sort_uniq: List.sort_uniq,
          merge: List.merge
        };
}

eq("File \"global_module_alias_test.ml\", line 86, characters 5-12", g(/* () */0), 4);

var V = xx(/* () */0);

eq("File \"global_module_alias_test.ml\", line 92, characters 5-12", Curry._1(V.length, /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ]), 3);

eq("File \"global_module_alias_test.ml\", line 93, characters 5-12", v.contents, 15);

var H$1 = f(/* () */0);

eq("File \"global_module_alias_test.ml\", line 95, characters 5-12", Curry._1(H$1.length, /* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]), 2);

eq("File \"global_module_alias_test.ml\", line 96, characters 5-12", v.contents, 21);

Mt.from_pair_suites("Global_module_alias_test", suites.contents);

var A = 0;

var B = 0;

var C = 0;

var D = 0;

var E = 0;

var F = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.A = A;
exports.B = B;
exports.C = C;
exports.D = D;
exports.E = E;
exports.F = F;
exports.v = v;
exports.Make = Make;
exports.f = f;
exports.H = H;
exports.g = g;
exports.xx = xx;
/*  Not a pure module */
