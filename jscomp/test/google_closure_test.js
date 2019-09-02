'use strict';

var Mt = require("./mt.js");
var Test_google_closure = require("./test_google_closure.js");

Mt.from_pair_suites("Closure", /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "partial",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: /* tuple */[
                      Test_google_closure.a,
                      Test_google_closure.b,
                      Test_google_closure.c
                    ],
                    Arg1: /* tuple */[
                      "3",
                      101,
                      /* array */[
                        1,
                        2
                      ]
                    ]
                  };
          })
      ],
      Arg1: "[]"
    });

/*  Not a pure module */
