'use strict';

var Mt                      = require("./mt");
var Block                   = require("../../lib/js/block");
var Curry                   = require("../../lib/js/curry");
var Caml_exceptions         = require("../../lib/js/caml_exceptions");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");

var Local = Caml_exceptions.create("Exception_raise_test.Local");

var B = Caml_exceptions.create("Exception_raise_test.B");

var C = Caml_exceptions.create("Exception_raise_test.C");

var D = Caml_exceptions.create("Exception_raise_test.D");

function appf(g, x) {
  var A = Caml_exceptions.create("A");
  try {
    return Curry._1(g, x);
  }
  catch (exn){
    var exit = 0;
    if (exn === Local) {
      return 3;
    } else if (exn === Caml_builtin_exceptions.not_found) {
      return 2;
    } else if (exn[0] === A) {
      return 3;
    } else if (exn[0] === B) {
      var match = exn[1];
      if (match) {
        var match$1 = match[1];
        if (match$1) {
          var match$2 = match$1[1];
          if (match$2) {
            return match$2[0];
          } else {
            exit = 1;
          }
        } else {
          exit = 1;
        }
      } else {
        exit = 1;
      }
    } else {
      exit = 1;
    }
    if (exit === 1) {
      if (exn[0] === C) {
        return exn[1];
      } else if (exn[0] === D) {
        return exn[1][0];
      } else {
        return 4;
      }
    }
    
  }
}

var A = Caml_exceptions.create("Exception_raise_test.A");

var f;

try {
  f = ( function () {throw (new Error ("x"))} ());
}
catch (exn){
  f = exn[0] === A ? exn[1] : 2;
}

var ff;

try {
  ff = ( function () {throw 3} ());
}
catch (exn$1){
  ff = exn$1[0] === A ? exn$1[1] : 2;
}

var fff;

try {
  fff = ( function () {throw 2} ());
}
catch (exn$2){
  fff = exn$2[0] === A ? exn$2[1] : 2;
}

Mt.from_pair_suites("exception_raise_test.ml", /* :: */[
      /* tuple */[
        "File \"exception_raise_test.ml\", line 97, characters 4-11",
        function () {
          return /* Eq */Block.__(0, [
                    /* tuple */[
                      f,
                      ff,
                      fff
                    ],
                    /* tuple */[
                      2,
                      2,
                      2
                    ]
                  ]);
        }
      ],
      /* [] */0
    ]);

exports.Local = Local;
exports.B     = B;
exports.C     = C;
exports.D     = D;
exports.appf  = appf;
exports.A     = A;
exports.f     = f;
exports.ff    = ff;
exports.fff   = fff;
/* f Not a pure module */
