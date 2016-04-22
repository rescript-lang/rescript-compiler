// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt    = require("./mt");
var Block = require("../runtime/block");
var List  = require("../stdlib/list");

function length() {
  return 3;
}

Mt.from_pair_suites("es6_module_test.ml", /* :: */[
      /* tuple */[
        "list_length",
        function () {
          return /* Eq */Block.__(0, [
                    List.length(/* :: */[
                          1,
                          /* :: */[
                            2,
                            /* [] */0
                          ]
                        ]),
                    2
                  ]);
        }
      ],
      /* :: */[
        /* tuple */[
          "length",
          function () {
            return /* Eq */Block.__(0, [
                      3,
                      3
                    ]);
          }
        ],
        /* [] */0
      ]
    ]);

exports.length = length;
/*  Not a pure module */
