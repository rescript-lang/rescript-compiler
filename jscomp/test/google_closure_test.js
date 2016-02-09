// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Assert              = require("assert");
var Mt                  = require("./mt");
var Test_google_closure = require("./test_google_closure");

Mt.from_suites("Closure", [
      /* :: */0,
      [
        /* tuple */0,
        "partial",
        function () {
          var prim_003 = /* array */[
            1,
            2,
            3
          ];
          var prim = [
            /* tuple */0,
            "3",
            101,
            prim_003
          ];
          var prim$1 = [
            /* tuple */0,
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
