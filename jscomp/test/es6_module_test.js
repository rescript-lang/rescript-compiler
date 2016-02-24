// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt   = require("./mt");
var List = require("../stdlib/list");

function length() {
  return 3;
}

Mt.from_pair_suites("es6_module_test.ml", /* :: */[
      /* tuple */[
        "list_length",
        function () {
          return /* Eq */{
                  0: List.length(/* :: */[
                        1,
                        /* :: */[
                          2,
                          /* [] */0
                        ]
                      ]),
                  1: 2,
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* :: */[
        /* tuple */[
          "length",
          function () {
            return /* Eq */{
                    0: 3,
                    1: 3,
                    length: 2,
                    tag: 0
                  };
          }
        ],
        /* [] */0
      ]
    ]);

exports.length = length;
/*  Not a pure module */
