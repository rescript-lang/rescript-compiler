// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (() => ({
        TAG: "Eq",
        _0: x,
        _1: y
      }))
    ],
    tl: suites.contents
  };
}

function hi (cb){
    cb ();
    return 0;
}
;

let xs = {
  contents: /* [] */0
};

hi(x => {
  xs.contents = {
    hd: x,
    tl: xs.contents
  };
});

hi(x => {
  xs.contents = {
    hd: x,
    tl: xs.contents
  };
});

eq("File \"bs_auto_uncurry_test.res\", line 24, characters 5-12", xs.contents, {
  hd: undefined,
  tl: {
    hd: undefined,
    tl: /* [] */0
  }
});

eq("File \"bs_auto_uncurry_test.res\", line 28, characters 5-12", [
  1,
  2,
  3
].map(x => x + 1 | 0), [
  2,
  3,
  4
]);

eq("File \"bs_auto_uncurry_test.res\", line 29, characters 5-12", [
  1,
  2,
  3
].map(x => x + 1 | 0), [
  2,
  3,
  4
]);

eq("File \"bs_auto_uncurry_test.res\", line 31, characters 5-12", [
  1,
  2,
  3
].reduce(((prim0, prim1) => prim0 + prim1 | 0), 0), 6);

eq("File \"bs_auto_uncurry_test.res\", line 33, characters 5-12", [
  1,
  2,
  3
].reduce(((x, y, i) => (x + y | 0) + i | 0), 0), 9);

eq("File \"bs_auto_uncurry_test.res\", line 35, characters 5-12", [
  1,
  2,
  3
].some(x => x < 1), false);

eq("File \"bs_auto_uncurry_test.res\", line 37, characters 5-12", [
  1,
  2,
  3
].every(x => x > 0), true);

Mt.from_pair_suites("Bs_auto_uncurry_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
