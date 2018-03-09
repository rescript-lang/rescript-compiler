'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
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

var v = [0];

function Make(U) {
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  return /* module */[
          /* length */U[0],
          /* hd */U[1],
          /* tl */U[2],
          /* nth */U[3],
          /* rev */U[4],
          /* append */U[5],
          /* rev_append */U[6],
          /* concat */U[7],
          /* flatten */U[8],
          /* iter */U[9],
          /* iteri */U[10],
          /* map */U[11],
          /* mapi */U[12],
          /* rev_map */U[13],
          /* fold_left */U[14],
          /* fold_right */U[15],
          /* iter2 */U[16],
          /* map2 */U[17],
          /* rev_map2 */U[18],
          /* fold_left2 */U[19],
          /* fold_right2 */U[20],
          /* for_all */U[21],
          /* exists */U[22],
          /* for_all2 */U[23],
          /* exists2 */U[24],
          /* mem */U[25],
          /* memq */U[26],
          /* find */U[27],
          /* filter */U[28],
          /* find_all */U[29],
          /* partition */U[30],
          /* assoc */U[31],
          /* assq */U[32],
          /* mem_assoc */U[33],
          /* mem_assq */U[34],
          /* remove_assoc */U[35],
          /* remove_assq */U[36],
          /* split */U[37],
          /* combine */U[38],
          /* sort */U[39],
          /* stable_sort */U[40],
          /* fast_sort */U[41],
          /* sort_uniq */U[42],
          /* merge */U[43]
        ];
}

function f() {
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  return /* module */[
          /* length */List.length,
          /* hd */List.hd,
          /* tl */List.tl,
          /* nth */List.nth,
          /* rev */List.rev,
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
          /* filter */List.filter,
          /* find_all */List.find_all,
          /* partition */List.partition,
          /* assoc */List.assoc,
          /* assq */List.assq,
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
  /* hd */List.hd,
  /* tl */List.tl,
  /* nth */List.nth,
  /* rev */List.rev,
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
  /* filter */List.filter,
  /* find_all */List.find_all,
  /* partition */List.partition,
  /* assoc */List.assoc,
  /* assq */List.assq,
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

function g() {
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

function xx() {
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  v[0] = v[0] + 1 | 0;
  return /* module */[
          /* length */List.length,
          /* hd */List.hd,
          /* tl */List.tl,
          /* nth */List.nth,
          /* rev */List.rev,
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
          /* filter */List.filter,
          /* find_all */List.find_all,
          /* partition */List.partition,
          /* assoc */List.assoc,
          /* assq */List.assq,
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

v[0] = v[0] + 1 | 0;

v[0] = v[0] + 1 | 0;

v[0] = v[0] + 1 | 0;

eq("File \"global_module_alias_test.ml\", line 92, characters 5-12", List.length(/* :: */[
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

Mt.from_pair_suites("global_module_alias_test.ml", suites[0]);

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
