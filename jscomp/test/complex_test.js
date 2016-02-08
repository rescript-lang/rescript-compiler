// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Complex = require("../stdlib/complex");
var Mt      = require("./mt");

var suites_001 = [
  /* tuple */0,
  "basic_add",
  function () {
    return [
            /* Eq */0,
            /* float array */[
              2,
              2
            ],
            Complex.add(Complex.add(Complex.add(Complex.one, Complex.one), /* float array */[
                      0.0,
                      1.0
                    ]), /* float array */[
                  0.0,
                  1.0
                ])
          ];
  }
];

var suites = [
  /* :: */0,
  suites_001,
  /* [] */0
];

Mt.from_pair_suites("complex_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
