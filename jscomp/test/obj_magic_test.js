'use strict';

var Mt = require("./mt.js");
var Obj = require("../../lib/js/obj.js");
var Block = require("../../lib/js/block.js");

var empty_backtrace = Block.__(Obj.abstract_tag, []);

function is_block(x) {
  return typeof x !== "number";
}

var suites_000 = /* tuple */[
  "is_block_test1",
  (function (param) {
      return /* Eq */Block.__(0, [
                false,
                false
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "is_block_test2",
    (function (param) {
        return /* Eq */Block.__(0, [
                  true,
                  typeof /* :: */[
                    3,
                    /* [] */0
                  ] !== "number"
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "is_block_test3",
      (function (param) {
          return /* Eq */Block.__(0, [
                    true,
                    true
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "is_block_test4",
        (function (param) {
            return /* Eq */Block.__(0, [
                      false,
                      false
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

Mt.from_pair_suites("Obj_magic_test", suites);

exports.empty_backtrace = empty_backtrace;
exports.is_block = is_block;
exports.suites = suites;
/*  Not a pure module */
