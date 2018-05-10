'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Test_google_closure = require("./test_google_closure.js");

Mt.from_pair_suites("Closure", /* :: */[
      /* tuple */[
        "partial",
        (function () {
            return /* Eq */Block.__(0, [
                      /* tuple */[
                        Test_google_closure.a,
                        Test_google_closure.b,
                        Test_google_closure.c
                      ],
                      /* tuple */[
                        "3",
                        101,
                        /* array */[
                          1,
                          2
                        ]
                      ]
                    ]);
          })
      ],
      /* [] */0
    ]);

/*  Not a pure module */
