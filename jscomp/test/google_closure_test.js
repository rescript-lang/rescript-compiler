// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Assert              = require("assert");
var Mt                  = require("./mt");
var Test_google_closure = require("./test_google_closure");

Mt.from_suites("Closure", /* :: */[
      /* tuple */[
        "partial",
        function () {
          var prim_002 = /* int array */[
            1,
            2,
            3
          ];
          var prim = /* tuple */[
            "3",
            101,
            prim_002
          ];
          var prim$1 = /* tuple */[
            Test_google_closure.a,
            Test_google_closure.b,
            Test_google_closure.c
          ];
          return Assert.deepEqual(prim$1, prim);
        }
      ],
      /* [] */0
    ]);

/*  Not a pure module */
