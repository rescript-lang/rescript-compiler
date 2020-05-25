'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Complex = require("../../lib/js/complex.js");

var suites_0 = /* tuple */[
  "basic_add",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: {
                re: 2,
                im: 2
              },
              _1: Complex.add(Complex.add(Complex.add(Complex.one, Complex.one), Complex.i), Complex.i)
            };
    })
];

var suites = /* :: */{
  _0: suites_0,
  _1: /* [] */0
};

Mt.from_pair_suites("Complex_test", suites);

exports.suites = suites;
/*  Not a pure module */
