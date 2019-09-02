'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");

function from_pair_suites(name, suites) {
  console.log(/* tuple */[
        name,
        "testing"
      ]);
  return List.iter((function (param) {
                var name = param[0];
                var match = Curry._1(param[1], /* () */0);
                switch (/* XXX */match.tag) {
                  case "Eq" :
                      console.log(/* tuple */[
                            name,
                            match.Arg0,
                            "eq?",
                            match.Arg1
                          ]);
                      return /* () */0;
                  case "Neq" :
                      console.log(/* tuple */[
                            name,
                            match.Arg0,
                            "neq?",
                            match.Arg1
                          ]);
                      return /* () */0;
                  case "StrictEq" :
                      console.log(/* tuple */[
                            name,
                            match.Arg0,
                            "strict_eq?",
                            match.Arg1
                          ]);
                      return /* () */0;
                  case "StrictNeq" :
                      console.log(/* tuple */[
                            name,
                            match.Arg0,
                            "strict_neq?",
                            match.Arg1
                          ]);
                      return /* () */0;
                  case "Ok" :
                      console.log(/* tuple */[
                            name,
                            match.Arg0,
                            "ok?"
                          ]);
                      return /* () */0;
                  case "Approx" :
                      console.log(/* tuple */[
                            name,
                            match.Arg0,
                            "~",
                            match.Arg1
                          ]);
                      return /* () */0;
                  case "ApproxThreshold" :
                      console.log(/* tuple */[
                            name,
                            match.Arg1,
                            "~",
                            match.Arg2,
                            " (",
                            match.Arg0,
                            ")"
                          ]);
                      return /* () */0;
                  case "ThrowAny" :
                      return /* () */0;
                  case "Fail" :
                      console.log("failed");
                      return /* () */0;
                  case "FailWith" :
                      console.log("failed: " + match.Arg0);
                      return /* () */0;
                  
                }
              }), suites);
}

exports.from_pair_suites = from_pair_suites;
/* No side effect */
