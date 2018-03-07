'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Js_exn = require("../../lib/js/js_exn.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
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
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
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
catch (raw_exn){
  var exn = Js_exn.internalToOCamlException(raw_exn);
  f = exn[0] === A ? exn[1] : 2;
}

var ff;

try {
  ff = ( function () {throw 3} ());
}
catch (raw_exn$1){
  var exn$1 = Js_exn.internalToOCamlException(raw_exn$1);
  ff = exn$1[0] === A ? exn$1[1] : 2;
}

var fff;

try {
  fff = ( function () {throw 2} ());
}
catch (raw_exn$2){
  var exn$2 = Js_exn.internalToOCamlException(raw_exn$2);
  fff = exn$2[0] === A ? exn$2[1] : 2;
}

var a0;

try {
  a0 = ( function (){throw 2} () );
}
catch (raw_exn$3){
  var exn$3 = Js_exn.internalToOCamlException(raw_exn$3);
  if (exn$3[0] === A || exn$3[0] === Js_exn.$$Error) {
    a0 = exn$3[1];
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "exception_raise_test.ml",
            100,
            9
          ]
        ];
  }
}

var a1;

try {
  a1 = ( function (){throw 2} () );
}
catch (raw_e){
  a1 = Js_exn.internalToOCamlException(raw_e);
}

var a2;

try {
  a2 = ( function (){throw (new Error("x"))} () );
}
catch (raw_e$1){
  a2 = Js_exn.internalToOCamlException(raw_e$1);
}

Mt.from_pair_suites("exception_raise_test.ml", /* :: */[
      /* tuple */[
        "File \"exception_raise_test.ml\", line 113, characters 4-11",
        (function () {
            return /* Eq */Block.__(0, [
                      /* tuple */[
                        f,
                        ff,
                        fff,
                        a0
                      ],
                      /* tuple */[
                        2,
                        2,
                        2,
                        2
                      ]
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "File \"exception_raise_test.ml\", line 115, characters 4-11",
          (function () {
              if (a1[0] === Js_exn.$$Error) {
                return /* Eq */Block.__(0, [
                          a1[1],
                          2
                        ]);
              } else {
                throw [
                      Caml_builtin_exceptions.assert_failure,
                      [
                        "exception_raise_test.ml",
                        118,
                        15
                      ]
                    ];
              }
            })
        ],
        /* [] */0
      ]
    ]);

exports.Local = Local;
exports.B = B;
exports.C = C;
exports.D = D;
exports.appf = appf;
exports.A = A;
exports.f = f;
exports.ff = ff;
exports.fff = fff;
exports.a0 = a0;
exports.a1 = a1;
exports.a2 = a2;
/* f Not a pure module */
