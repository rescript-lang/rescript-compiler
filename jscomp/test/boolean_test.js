'use strict';

var Mt = require("./mt.js");
var Test_bool_equal = require("./test_bool_equal.js");

Mt.from_suites("boolean", /* :: */[
      /* tuple */[
        "bool_equal",
        Test_bool_equal.assertions
      ],
      /* [] */0
    ]);

/*  Not a pure module */
