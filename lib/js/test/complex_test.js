'use strict';

var Complex = require("../complex");
var Mt      = require("./mt");
var Block   = require("../block");

var suites_000 = /* tuple */[
  "basic_add",
  function () {
    return /* Eq */Block.__(0, [
              /* float array */[
                2,
                2
              ],
              Complex.add(Complex.add(Complex.add(Complex.one, Complex.one), Complex.i), Complex.i)
            ]);
  }
];

var suites = /* :: */[
  suites_000,
  /* [] */0
];

Mt.from_pair_suites("complex_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
