'use strict';

var Mt = require("./mt.js");
var Complex = require("../../lib/js/complex.js");

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "basic_add",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: /* record */[
                  /* re */2,
                  /* im */2
                ],
                Arg1: Complex.add(Complex.add(Complex.add(Complex.one, Complex.one), Complex.i), Complex.i)
              };
      })
  ],
  Arg1: "[]"
};

Mt.from_pair_suites("Complex_test", suites);

exports.suites = suites;
/*  Not a pure module */
