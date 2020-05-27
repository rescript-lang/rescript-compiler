'use strict';

var Mt = require("./mt.js");

function is_block(x) {
  return typeof x !== "number";
}

var suites_0 = [
  "is_block_test1",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: false,
              _1: false
            };
    })
];

var suites_1 = {
  hd: [
    "is_block_test2",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: true,
                _1: typeof ({
                    hd: 3,
                    tl: /* [] */0
                  }) !== "number"
              };
      })
  ],
  tl: {
    hd: [
      "is_block_test3",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: true,
                  _1: true
                };
        })
    ],
    tl: {
      hd: [
        "is_block_test4",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: false,
                    _1: false
                  };
          })
      ],
      tl: /* [] */0
    }
  }
};

var suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Obj_magic_test", suites);

exports.is_block = is_block;
exports.suites = suites;
/*  Not a pure module */
