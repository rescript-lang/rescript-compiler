'use strict';

var Mt = require("./mt.js");
var Test_google_closure = require("./test_google_closure.js");

Mt.from_pair_suites("Closure", {
      hd: [
        "partial",
        (function (param) {
            return {
                    TAG: "Eq",
                    _0: [
                      Test_google_closure.a,
                      Test_google_closure.b,
                      Test_google_closure.c
                    ],
                    _1: [
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
      tl: /* [] */0
    });

/*  Not a pure module */
