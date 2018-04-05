'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");

var v = /* record */[
  /* syntax : None */0,
  /* imports */0,
  /* file_options */0,
  /* package */0,
  /* messages */0,
  /* enums */0,
  /* extends */0
];

var u_v = /* record */[
  /* syntax : None */0,
  /* imports */0,
  /* file_options */0,
  /* package */0,
  /* messages */0,
  /* enums */0,
  /* extends */0
];

function f(g, h) {
  var init = Curry._1(g, h);
  return /* record */[
          /* syntax */init[/* syntax */0],
          /* imports */0,
          /* file_options */init[/* file_options */2],
          /* package */init[/* package */3],
          /* messages */init[/* messages */4],
          /* enums */init[/* enums */5],
          /* extends */init[/* extends */6]
        ];
}

var suites_000 = /* tuple */[
  "eq_with",
  (function () {
      return /* Eq */Block.__(0, [
                v,
                u_v
              ]);
    })
];

var suites = /* :: */[
  suites_000,
  /* [] */0
];

Mt.from_pair_suites("record_with_test.ml", suites);

var uv = /* record */[
  /* syntax : None */0,
  /* imports */1,
  /* file_options */0,
  /* package */0,
  /* messages */0,
  /* enums */0,
  /* extends */0
];

exports.v = v;
exports.uv = uv;
exports.u_v = u_v;
exports.f = f;
exports.suites = suites;
/*  Not a pure module */
