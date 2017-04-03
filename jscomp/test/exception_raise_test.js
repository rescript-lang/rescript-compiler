'use strict';

var Mt                      = require("./mt.js");
var Block                   = require("../../lib/js/block.js");
var Curry                   = require("../../lib/js/curry.js");
var Js_exn                  = require("../../lib/js/js_exn.js");
var Caml_exceptions         = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

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
    var exn$1 = Js_exn.internalToOCamlException(exn);
    var exit = 0;
    if (exn$1 === Local) {
      return 3;
    } else if (exn$1 === Caml_builtin_exceptions.not_found) {
      return 2;
    } else if (exn$1[0] === A) {
      return 3;
    } else if (exn$1[0] === B) {
      var match = exn$1[1];
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
      if (exn$1[0] === C) {
        return exn$1[1];
      } else if (exn$1[0] === D) {
        return exn$1[1][0];
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
  var exn$1 = Js_exn.internalToOCamlException(exn);
  f = exn$1[0] === A ? exn$1[1] : 2;
}

var ff;

try {
  ff = ( function () {throw 3} ());
}
catch (exn$2){
  var exn$3 = Js_exn.internalToOCamlException(exn$2);
  ff = exn$3[0] === A ? exn$3[1] : 2;
}

var fff;

try {
  fff = ( function () {throw 2} ());
}
catch (exn$4){
  var exn$5 = Js_exn.internalToOCamlException(exn$4);
  fff = exn$5[0] === A ? exn$5[1] : 2;
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
