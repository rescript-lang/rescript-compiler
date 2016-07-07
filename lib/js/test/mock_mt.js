// GENERATED CODE BY BUCKLESCRIPT VERSION 0.7.1 , PLEASE EDIT WITH CARE
'use strict';

var Curry = require("../curry");
var List  = require("../list");

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
                    console.log(/* tuple */[
                          name,
                          match[0],
                          "~",
                          match[1]
                        ]);
                    return /* () */0;
                case 3 : 
                    return /* () */0;
                
              }
            }, suites);
}

exports.from_pair_suites = from_pair_suites;
/* No side effect */
