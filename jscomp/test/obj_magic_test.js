// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Obj = require("../stdlib/obj");
var Mt  = require("./mt");

var empty_backtrace = {
  length: 0,
  tag: Obj.abstract_tag
};

function is_block(x) {
  return x.length !== undefined;
}

var suites_000 = /* tuple */[
  "is_block_test1",
  function () {
    return /* Eq */{
            0: /* false */0,
            1: (3).length !== undefined,
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "is_block_test2",
    function () {
      return /* Eq */{
              0: /* true */1,
              1: /* :: */[
                3,
                /* [] */0
              ].length !== undefined,
              length: 2,
              tag: 0
            };
    }
  ],
  /* :: */[
    /* tuple */[
      "is_block_test3",
      function () {
        return /* Eq */{
                0: /* true */1,
                1: "x".length !== undefined,
                length: 2,
                tag: 0
              };
      }
    ],
    /* :: */[
      /* tuple */[
        "is_block_test4",
        function () {
          return /* Eq */{
                  0: /* false */0,
                  1: (3.0).length !== undefined,
                  length: 2,
                  tag: 0
                };
        }
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
exports.is_block        = is_block;
exports.suites          = suites;
/*  Not a pure module */
