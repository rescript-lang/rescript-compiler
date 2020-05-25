'use strict';

var Mt = require("./mt.js");

function is_block(x) {
  return typeof x !== "number";
}

var suites_0 = [
  "is_block_test1",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: false,
              _1: false
            };
    })
];

var suites_1 = /* :: */{
  _0: [
    "is_block_test2",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: true,
                _1: typeof /* :: */({
                    _0: 3,
                    _1: /* [] */0
                  }) !== "number"
              };
      })
  ],
  _1: /* :: */{
    _0: [
      "is_block_test3",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: true,
                  _1: true
                };
        })
    ],
    _1: /* :: */{
      _0: [
        "is_block_test4",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: false,
                    _1: false
                  };
          })
      ],
      _1: /* [] */0
    }
  }
};

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("Obj_magic_test", suites);

exports.is_block = is_block;
exports.suites = suites;
/*  Not a pure module */
