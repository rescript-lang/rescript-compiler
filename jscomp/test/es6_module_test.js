'use strict';

var Mt    = require("./mt");
var List  = require("../../lib/js/list");
var Block = require("../../lib/js/block");

function length() {
  return 3;
}

Mt.from_pair_suites("es6_module_test.ml", /* Nested :: */[
      /* tuple */[
        "list_length",function () {
          return /* Eq */Block.__(0, [
                    List.length(/* Nested :: */[
                          1,[
                            2,/* [] */0
                          ]
                        ]),2
                  ]);
        }
      ],[
        /* tuple */[
          "length",function () {
            return /* Eq */Block.__(0, [
                      3,3
                    ]);
          }
        ],/* [] */0
      ]
    ]);

exports.length = length;
/*  Not a pure module */
