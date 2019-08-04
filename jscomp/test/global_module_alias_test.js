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

var v = /* record */[/* contents */0];

function Make(U) {
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  return /* module */[
          /* length */U[0],
          /* compare_lengths */U[1],
          /* compare_length_with */U[2],
          /* cons */U[3],
          /* hd */U[4],
          /* tl */U[5],
          /* nth */U[6],
          /* nth_opt */U[7],
          /* rev */U[8],
          /* init */U[9],
          /* append */U[10],
          /* rev_append */U[11],
          /* concat */U[12],
          /* flatten */U[13],
          /* iter */U[14],
          /* iteri */U[15],
          /* map */U[16],
          /* mapi */U[17],
          /* rev_map */U[18],
          /* fold_left */U[19],
          /* fold_right */U[20],
          /* iter2 */U[21],
          /* map2 */U[22],
          /* rev_map2 */U[23],
          /* fold_left2 */U[24],
          /* fold_right2 */U[25],
          /* for_all */U[26],
          /* exists */U[27],
          /* for_all2 */U[28],
          /* exists2 */U[29],
          /* mem */U[30],
          /* memq */U[31],
          /* find */U[32],
          /* find_opt */U[33],
          /* filter */U[34],
          /* find_all */U[35],
          /* partition */U[36],
          /* assoc */U[37],
          /* assoc_opt */U[38],
          /* assq */U[39],
          /* assq_opt */U[40],
          /* mem_assoc */U[41],
          /* mem_assq */U[42],
          /* remove_assoc */U[43],
          /* remove_assq */U[44],
          /* split */U[45],
          /* combine */U[46],
          /* sort */U[47],
          /* stable_sort */U[48],
          /* fast_sort */U[49],
          /* sort_uniq */U[50],
          /* merge */U[51]
        ];
}

function f(param) {
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  return /* module */[
          /* length */List.length,
          /* compare_lengths */List.compare_lengths,
          /* compare_length_with */List.compare_length_with,
          /* cons */List.cons,
          /* hd */List.hd,
          /* tl */List.tl,
          /* nth */List.nth,
          /* nth_opt */List.nth_opt,
          /* rev */List.rev,
          /* init */List.init,
          /* append */List.append,
          /* rev_append */List.rev_append,
          /* concat */List.concat,
          /* flatten */List.flatten,
          /* iter */List.iter,
          /* iteri */List.iteri,
          /* map */List.map,
          /* mapi */List.mapi,
          /* rev_map */List.rev_map,
          /* fold_left */List.fold_left,
          /* fold_right */List.fold_right,
          /* iter2 */List.iter2,
          /* map2 */List.map2,
          /* rev_map2 */List.rev_map2,
          /* fold_left2 */List.fold_left2,
          /* fold_right2 */List.fold_right2,
          /* for_all */List.for_all,
          /* exists */List.exists,
          /* for_all2 */List.for_all2,
          /* exists2 */List.exists2,
          /* mem */List.mem,
          /* memq */List.memq,
          /* find */List.find,
          /* find_opt */List.find_opt,
          /* filter */List.filter,
          /* find_all */List.find_all,
          /* partition */List.partition,
          /* assoc */List.assoc,
          /* assoc_opt */List.assoc_opt,
          /* assq */List.assq,
          /* assq_opt */List.assq_opt,
          /* mem_assoc */List.mem_assoc,
          /* mem_assq */List.mem_assq,
          /* remove_assoc */List.remove_assoc,
          /* remove_assq */List.remove_assq,
          /* split */List.split,
          /* combine */List.combine,
          /* sort */List.sort,
          /* stable_sort */List.stable_sort,
          /* fast_sort */List.fast_sort,
          /* sort_uniq */List.sort_uniq,
          /* merge */List.merge
        ];
}

eq("File \"global_module_alias_test.ml\", line 51, characters 5-12", List.length(/* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]), 2);

v[0] = v[0] + 1 | 0;

v[0] = v[0] + 1 | 0;

v[0] = v[0] + 1 | 0;

v[0] = v[0] + 1 | 0;

v[0] = v[0] + 1 | 0;

v[0] = v[0] + 1 | 0;

v[0] = v[0] + 1 | 0;

v[0] = v[0] + 1 | 0;

v[0] = v[0] + 1 | 0;

v[0] = v[0] + 1 | 0;

v[0] = v[0] + 1 | 0;

v[0] = v[0] + 1 | 0;

var H = /* module */[
  /* length */List.length,
  /* compare_lengths */List.compare_lengths,
  /* compare_length_with */List.compare_length_with,
  /* cons */List.cons,
  /* hd */List.hd,
  /* tl */List.tl,
  /* nth */List.nth,
  /* nth_opt */List.nth_opt,
  /* rev */List.rev,
  /* init */List.init,
  /* append */List.append,
  /* rev_append */List.rev_append,
  /* concat */List.concat,
  /* flatten */List.flatten,
  /* iter */List.iter,
  /* iteri */List.iteri,
  /* map */List.map,
  /* mapi */List.mapi,
  /* rev_map */List.rev_map,
  /* fold_left */List.fold_left,
  /* fold_right */List.fold_right,
  /* iter2 */List.iter2,
  /* map2 */List.map2,
  /* rev_map2 */List.rev_map2,
  /* fold_left2 */List.fold_left2,
  /* fold_right2 */List.fold_right2,
  /* for_all */List.for_all,
  /* exists */List.exists,
  /* for_all2 */List.for_all2,
  /* exists2 */List.exists2,
  /* mem */List.mem,
  /* memq */List.memq,
  /* find */List.find,
  /* find_opt */List.find_opt,
  /* filter */List.filter,
  /* find_all */List.find_all,
  /* partition */List.partition,
  /* assoc */List.assoc,
  /* assoc_opt */List.assoc_opt,
  /* assq */List.assq,
  /* assq_opt */List.assq_opt,
  /* mem_assoc */List.mem_assoc,
  /* mem_assq */List.mem_assq,
  /* remove_assoc */List.remove_assoc,
  /* remove_assq */List.remove_assq,
  /* split */List.split,
  /* combine */List.combine,
  /* sort */List.sort,
  /* stable_sort */List.stable_sort,
  /* fast_sort */List.fast_sort,
  /* sort_uniq */List.sort_uniq,
  /* merge */List.merge
];

eq("File \"global_module_alias_test.ml\", line 57, characters 5-12", v[0], 12);

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
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  return /* module */[
          /* length */List.length,
          /* compare_lengths */List.compare_lengths,
          /* compare_length_with */List.compare_length_with,
          /* cons */List.cons,
          /* hd */List.hd,
          /* tl */List.tl,
          /* nth */List.nth,
          /* nth_opt */List.nth_opt,
          /* rev */List.rev,
          /* init */List.init,
          /* append */List.append,
          /* rev_append */List.rev_append,
          /* concat */List.concat,
          /* flatten */List.flatten,
          /* iter */List.iter,
          /* iteri */List.iteri,
          /* map */List.map,
          /* mapi */List.mapi,
          /* rev_map */List.rev_map,
          /* fold_left */List.fold_left,
          /* fold_right */List.fold_right,
          /* iter2 */List.iter2,
          /* map2 */List.map2,
          /* rev_map2 */List.rev_map2,
          /* fold_left2 */List.fold_left2,
          /* fold_right2 */List.fold_right2,
          /* for_all */List.for_all,
          /* exists */List.exists,
          /* for_all2 */List.for_all2,
          /* exists2 */List.exists2,
          /* mem */List.mem,
          /* memq */List.memq,
          /* find */List.find,
          /* find_opt */List.find_opt,
          /* filter */List.filter,
          /* find_all */List.find_all,
          /* partition */List.partition,
          /* assoc */List.assoc,
          /* assoc_opt */List.assoc_opt,
          /* assq */List.assq,
          /* assq_opt */List.assq_opt,
          /* mem_assoc */List.mem_assoc,
          /* mem_assq */List.mem_assq,
          /* remove_assoc */List.remove_assoc,
          /* remove_assq */List.remove_assq,
          /* split */List.split,
          /* combine */List.combine,
          /* sort */List.sort,
          /* stable_sort */List.stable_sort,
          /* fast_sort */List.fast_sort,
          /* sort_uniq */List.sort_uniq,
          /* merge */List.merge
        ];
}

eq("File \"global_module_alias_test.ml\", line 86, characters 5-12", g(/* () */0), 4);

var V = xx(/* () */0);

eq("File \"global_module_alias_test.ml\", line 92, characters 5-12", Curry._1(V[/* length */0], /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* [] */0
            ]
          ]
        ]), 3);

eq("File \"global_module_alias_test.ml\", line 93, characters 5-12", v[0], 15);

var H$1 = f(/* () */0);

eq("File \"global_module_alias_test.ml\", line 95, characters 5-12", Curry._1(H$1[/* length */0], /* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]), 2);

eq("File \"global_module_alias_test.ml\", line 96, characters 5-12", v[0], 21);

Mt.from_pair_suites("Global_module_alias_test", suites[0]);

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
