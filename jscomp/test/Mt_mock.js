// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_curry = require("../runtime/caml_curry");
var List       = require("../stdlib/list");

function from_pair_suites(_, suites) {
  return List.iter(function (param) {
              var name = param[0];
              var match = Caml_curry.app1(param[1], /* () */0);
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
                    console.log(/* tuple */[
                          name,
                          match[0],
                          "~",
                          match[1]
                        ]);
                    return /* () */0;
                
              }
            }, suites);
}

exports.from_pair_suites = from_pair_suites;
/* No side effect */
