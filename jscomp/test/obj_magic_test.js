'use strict';

var Mt = require("./mt.js");
var Obj = require("../../lib/js/obj.js");
var Block = require("../../lib/js/block.js");

var empty_backtrace = Block.__(Obj.abstract_tag, []);

function is_block(x) {
  return typeof x !== "number";
}

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "is_block_test1",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: false,
                Arg1: false
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "is_block_test2",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: true,
                  Arg1: typeof /* constructor */({
                      tag: "::",
                      Arg0: 3,
                      Arg1: "[]"
                    }) !== "number"
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "is_block_test3",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: true,
                    Arg1: true
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "is_block_test4",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: false,
                      Arg1: false
                    };
            })
        ],
        Arg1: "[]"
      }
    }
  }
};

Mt.from_pair_suites("Obj_magic_test", suites);

exports.empty_backtrace = empty_backtrace;
exports.is_block = is_block;
exports.suites = suites;
/*  Not a pure module */
