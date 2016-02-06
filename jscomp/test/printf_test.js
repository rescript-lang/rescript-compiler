// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt         = require("./mt");
var Printf     = require("../stdlib/printf");
var Caml_curry = require("../runtime/caml_curry");

var suites_001 = [
  /* tuple */0,
  "sprintf_simple",
  function () {
    return [
            /* Eq */0,
            "3232",
            Caml_curry.app2(Printf.sprintf([
                      /* Format */0,
                      [
                        /* String */2,
                        /* No_padding */0,
                        [
                          /* Int */4,
                          /* Int_d */0,
                          /* No_padding */0,
                          /* No_precision */0,
                          /* End_of_format */0
                        ]
                      ],
                      "%s%d"
                    ]), "32", 32)
          ];
  }
];

var suites = [
  /* :: */0,
  suites_001,
  /* [] */0
];

Mt.from_pair_suites("printf_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
