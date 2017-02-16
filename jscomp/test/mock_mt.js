'use strict';

var List                    = require("../../lib/js/list");
var Curry                   = require("../../lib/js/curry");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");

function from_pair_suites(name, suites) {
  console.log(/* tuple */[
        name,
        "testing"
      ]);
  return List.iter(function (param) {
              var name = param[0];
              var match = Curry._1(param[1], /* () */0);
              switch (match.tag | 0) {
                case 0 : 
                    console.log(/* tuple */[
                          name,
                          match[0],
                          "eq?",
                          match[1]
                        ]);
                    return /* () */0;
                case 1 : 
                    console.log(/* tuple */[
                          name,
                          match[0],
                          "neq?",
                          match[1]
                        ]);
                    return /* () */0;
                case 2 : 
                    throw [
                          Caml_builtin_exceptions.match_failure,
                          [
                            "mock_mt.ml",
                            13,
                            14
                          ]
                        ];
                case 3 : 
                    console.log(/* tuple */[
                          name,
                          match[0],
                          "~",
                          match[1]
                        ]);
                    return /* () */0;
                case 4 : 
                    return /* () */0;
                
              }
            }, suites);
}

exports.from_pair_suites = from_pair_suites;
/* No side effect */
