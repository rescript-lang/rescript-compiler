'use strict';

var Mt = require("./mt.js");
var Test_google_closure = require("./test_google_closure.js");

Mt.from_pair_suites("Closure", /* :: */{
      _0: /* tuple */[
        "partial",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: /* tuple */[
                      Test_google_closure.a,
                      Test_google_closure.b,
                      Test_google_closure.c
                    ],
                    _1: /* tuple */[
                      "3",
                      101,
                      [
                        1,
                        2
                      ]
                    ]
                  };
          })
      ],
      _1: /* [] */0
    });

/*  Not a pure module */
