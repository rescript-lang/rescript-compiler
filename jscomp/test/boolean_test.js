'use strict';

var Mt = require("./mt.js");
var Test_bool_equal = require("./test_bool_equal.js");

Mt.from_suites("boolean", /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "bool_equal",
        Test_bool_equal.assertions
      ],
      Arg1: "[]"
    });

/*  Not a pure module */
