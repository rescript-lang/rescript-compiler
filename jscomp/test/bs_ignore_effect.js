'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
  
}

function add(x,y){
  return x + y
}
;

var v = {
  contents: 0
};

var h = (v.contents = v.contents + 1 | 0, {
    hi: 2,
    lo: 0
  });

var z = (v.contents = v.contents + 1 | 0, add(3.0, 2.0));

eq("File \"bs_ignore_effect.ml\", line 26, characters 5-12", v.contents, 2);

eq("File \"bs_ignore_effect.ml\", line 27, characters 5-12", z, 5.0);

Mt.from_pair_suites("Bs_ignore_effect", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v = v;
exports.h = h;
exports.z = z;
/*  Not a pure module */
