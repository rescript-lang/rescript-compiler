'use strict';

var Mt           = require("./mt.js");
var Block        = require("../../lib/js/block.js");
var Js_primitive = require("../../lib/js/js_primitive.js");

var suites_000 = /* tuple */[
  "Dom sanity check",
  (function () {
      return /* ThrowAny */Block.__(7, [(function () {
                    Js_primitive.null_to_opt(localStorage.getItem("key"));
                    return /* () */0;
                  })]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "Js.Dom sanity check",
    (function () {
        return /* ThrowAny */Block.__(7, [(function () {
                      Js_primitive.null_to_opt(localStorage.getItem("key"));
                      return /* () */0;
                    })]);
      })
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_dom_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
