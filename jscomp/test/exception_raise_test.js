'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Js_exn = require("../../lib/js/js_exn.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
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
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
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
            return 4;
          }
        } else {
          return 4;
        }
      } else {
        return 4;
      }
    } else if (exn[0] === C) {
      return exn[1];
    } else if (exn[0] === D) {
      return exn[1][0];
    } else {
      return 4;
    }
  }
}

var A = Caml_exceptions.create("Exception_raise_test.A");

var f;

try {
  f = ( function () {throw (new Error ("x"))} ());
}
catch (raw_exn){
  var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
  f = exn[0] === A ? exn[1] : 2;
}

var ff;

try {
  ff = ( function () {throw 3} ());
}
catch (raw_exn$1){
  var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
  ff = exn$1[0] === A ? exn$1[1] : 2;
}

var fff;

try {
  fff = ( function () {throw 2} ());
}
catch (raw_exn$2){
  var exn$2 = Caml_js_exceptions.internalToOCamlException(raw_exn$2);
  fff = exn$2[0] === A ? exn$2[1] : 2;
}

var a0;

try {
  a0 = ( function (){throw 2} () );
}
catch (raw_exn$3){
  var exn$3 = Caml_js_exceptions.internalToOCamlException(raw_exn$3);
  if (exn$3[0] === A || exn$3[0] === Js_exn.$$Error) {
    a0 = exn$3[1];
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "exception_raise_test.ml",
            102,
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
  a1 = Caml_js_exceptions.internalToOCamlException(raw_e);
}

var a2;

try {
  a2 = ( function (){throw (new Error("x"))} () );
}
catch (raw_e$1){
  a2 = Caml_js_exceptions.internalToOCamlException(raw_e$1);
}

var suites = /* record */[/* contents : :: */[
    /* tuple */[
      "File \"exception_raise_test.ml\", line 114, characters 4-11",
      (function (param) {
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
        "File \"exception_raise_test.ml\", line 116, characters 4-11",
        (function (param) {
            if (a1[0] === Js_exn.$$Error) {
              return /* Eq */Block.__(0, [
                        a1[1],
                        2
                      ]);
            } else {
              throw [
                    Caml_builtin_exceptions.assert_failure,
                    /* tuple */[
                      "exception_raise_test.ml",
                      119,
                      15
                    ]
                  ];
            }
          })
      ],
      /* [] */0
    ]
  ]];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

try {
  (function (_){throw 2}(/* () */0));
}
catch (raw_e$2){
  var e = Caml_js_exceptions.internalToOCamlException(raw_e$2);
  eq("File \"exception_raise_test.ml\", line 131, characters 7-14", Caml_js_exceptions.caml_as_js_exn(e) !== undefined, true);
}

try {
  throw Caml_builtin_exceptions.not_found;
}
catch (raw_e$3){
  var e$1 = Caml_js_exceptions.internalToOCamlException(raw_e$3);
  eq("File \"exception_raise_test.ml\", line 138, characters 7-14", Caml_js_exceptions.caml_as_js_exn(e$1) !== undefined, false);
}

eq("File \"exception_raise_test.ml\", line 141, characters 5-12", function (a,b,c,_){return a + b + c }(1, 2, 3, 4), 6);

Mt.from_pair_suites("Exception_raise_test", suites[0]);

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
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/* f Not a pure module */
