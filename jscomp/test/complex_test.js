// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Complex = require("../stdlib/complex");
var Mt      = require("./mt");

var suites_000 = /* tuple */[
  "basic_add",
  function () {
    return /* Eq */{
            0: /* float array */[
              2,
              2
            ],
            1: Complex.add(Complex.add(Complex.add(Complex.one, Complex.one), Complex.i), Complex.i),
            length: 2,
            tag: 0
          };
  }
];

var suites = /* :: */[
  suites_000,
  /* [] */0
];

Mt.from_pair_suites("complex_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
