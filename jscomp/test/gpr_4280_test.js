'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var u = {
  contents: 0
};

function div(children, param) {
  for(var i = 0; i <= 1; ++i){
    u.contents = 300;
    console.log("nonline");
  }
  
}

function string(s) {
  for(var i = 0; i <= 1; ++i){
    u.contents = 200;
    console.log("no");
  }
  
}

function fn(authState, route) {
  var exit = 0;
  var onboardingRoute;
  if (typeof authState === "number") {
    if (typeof route === "number") {
      if (route >= -730831382) {
        if (!(route !== -384135774 && route !== -384133096)) {
          exit = 2;
        }
        
      } else if (!(route !== -799423340 && route < -730831383)) {
        exit = 2;
      }
      
    } else if (route[0] === 378129979) {
      onboardingRoute = route[1];
      exit = 1;
    }
    div(/* :: */[
          string("Redirect"),
          /* [] */0
        ], undefined);
    return 3;
  }
  if (typeof route !== "number" && route[0] === 378129979) {
    onboardingRoute = route[1];
    exit = 1;
  }
  console.log(authState[1]);
  div(/* :: */[
        string("VerifyEmail"),
        /* [] */0
      ], undefined);
  return 2;
  switch (exit) {
    case 1 :
        console.log(onboardingRoute);
        div(/* :: */[
              string("Onboarding"),
              /* [] */0
            ], undefined);
        return 0;
    case 2 :
        div(/* :: */[
              string("LoggedOut"),
              /* [] */0
            ], undefined);
        return 1;
    
  }
}

if (fn(/* Unauthenticated */-54822762, /* Invite */-730831383) !== 1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "gpr_4280_test.ml",
          40,
          3
        ]
      ];
}

exports.u = u;
exports.div = div;
exports.string = string;
exports.fn = fn;
/*  Not a pure module */
