'use strict';

var Mt = require("./mt.js");

console.log(JSON.stringify(/* constructor */{
          tag: "::",
          Arg0: 1,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 2,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            }
          }
        }));

console.log("hey");

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "anything_to_string",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: "3",
                Arg1: String(3)
              };
      })
  ],
  Arg1: "[]"
};

Mt.from_pair_suites("Lib_js_test", suites);

exports.suites = suites;
/*  Not a pure module */
