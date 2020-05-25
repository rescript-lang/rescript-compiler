'use strict';

var Mt = require("./mt.js");

console.log(JSON.stringify(/* :: */{
          _0: 1,
          _1: /* :: */{
            _0: 2,
            _1: /* :: */{
              _0: 3,
              _1: /* [] */0
            }
          }
        }));

console.log("hey");

var suites_0 = [
  "anything_to_string",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: "3",
              _1: String(3)
            };
    })
];

var suites = /* :: */{
  _0: suites_0,
  _1: /* [] */0
};

Mt.from_pair_suites("Lib_js_test", suites);

exports.suites = suites;
/*  Not a pure module */
