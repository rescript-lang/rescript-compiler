'use strict';

var Mt = require("./mt.js");

var v = {
  RE_EXN_ID: "Not_found"
};

var u = {
  RE_EXN_ID: "Not_found"
};

var s = {
  RE_EXN_ID: "End_of_file"
};

var suites_0 = [
  "not_found_equal",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: u,
              _1: v
            };
    })
];

var suites_1 = /* :: */{
  _0: [
    "not_found_not_equal_end_of_file",
    (function (param) {
        return {
                TAG: /* Neq */1,
                _0: u,
                _1: s
              };
      })
  ],
  _1: /* [] */0
};

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("Global_exception_regression_test", suites);

exports.v = v;
exports.u = u;
exports.s = s;
exports.suites = suites;
/*  Not a pure module */
