'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");

function from_pair_suites(name, suites) {
  console.log([
        name,
        "testing"
      ]);
  List.iter((function (param) {
          var name = param[0];
          var fn = Curry._1(param[1], undefined);
          switch (fn.TAG) {
            case "Eq" :
                console.log([
                      name,
                      fn._0,
                      "eq?",
                      fn._1
                    ]);
                return ;
            case "Neq" :
                console.log([
                      name,
                      fn._0,
                      "neq?",
                      fn._1
                    ]);
                return ;
            case "StrictEq" :
                console.log([
                      name,
                      fn._0,
                      "strict_eq?",
                      fn._1
                    ]);
                return ;
            case "StrictNeq" :
                console.log([
                      name,
                      fn._0,
                      "strict_neq?",
                      fn._1
                    ]);
                return ;
            case "Ok" :
                console.log([
                      name,
                      fn._0,
                      "ok?"
                    ]);
                return ;
            case "Approx" :
                console.log([
                      name,
                      fn._0,
                      "~",
                      fn._1
                    ]);
                return ;
            case "ApproxThreshold" :
                console.log([
                      name,
                      fn._1,
                      "~",
                      fn._2,
                      " (",
                      fn._0,
                      ")"
                    ]);
                return ;
            case "ThrowAny" :
                return ;
            case "Fail" :
                console.log("failed");
                return ;
            case "FailWith" :
                console.log("failed: " + fn._0);
                return ;
            
          }
        }), suites);
}

exports.from_pair_suites = from_pair_suites;
/* No side effect */
