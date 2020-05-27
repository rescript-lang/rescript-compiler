'use strict';

var Mt = require("./mt.js");
var Test_bool_equal = require("./test_bool_equal.js");

Mt.from_suites("boolean", {
      hd: [
        "bool_equal",
        Test_bool_equal.assertions
      ],
      tl: /* [] */0
    });

/*  Not a pure module */
