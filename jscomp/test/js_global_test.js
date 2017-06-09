'use strict';

var Mt    = require("./mt.js");
var Block = require("../../lib/js/block.js");

var suites_000 = /* tuple */[
  "setTimeout/clearTimeout sanity check",
  (function () {
      var handle = setTimeout((function () {
              return /* () */0;
            }), 0);
      clearTimeout(handle);
      return /* Ok */Block.__(4, [/* true */1]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "setInerval/clearInterval sanity check",
    (function () {
        var handle = setInterval((function () {
                return /* () */0;
              }), 0);
        clearInterval(handle);
        return /* Ok */Block.__(4, [/* true */1]);
      })
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_global_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
