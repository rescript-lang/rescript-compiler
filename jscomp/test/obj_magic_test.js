'use strict';

var Mt = require("./mt.js");
var Obj = require("../../lib/js/obj.js");
var Block = require("../../lib/js/block.js");

var empty_backtrace = Block.__(Obj.abstract_tag, []);

function is_block(x) {
  return x.length !== undefined;
}

var suites_000 = /* tuple */[
  "is_block_test1",
  (function () {
      return /* Eq */Block.__(0, [
                /* false */0,
                (3).length !== undefined
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "is_block_test2",
    (function () {
        return /* Eq */Block.__(0, [
                  /* true */1,
                  /* :: */[
                    3,
                    /* [] */0
                  ].length !== undefined
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "is_block_test3",
      (function () {
          return /* Eq */Block.__(0, [
                    /* true */1,
                    "x".length !== undefined
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "is_block_test4",
        (function () {
            return /* Eq */Block.__(0, [
                      /* false */0,
                      (3.0).length !== undefined
                    ]);
          })
      ],
      /* [] */0
    ]
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("obj_magic_test.ml", suites);

exports.empty_backtrace = empty_backtrace;
exports.is_block = is_block;
exports.suites = suites;
/*  Not a pure module */
