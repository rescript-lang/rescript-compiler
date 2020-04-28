'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Js_exn = require("../../lib/js/js_exn.js");
var Pervasives = require("../../lib/js/pervasives.js");
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
    if (exn.CamlExt.CamlId === Local.CamlId) {
      return 3;
    }
    if (exn.CamlExt.CamlId === Caml_builtin_exceptions.not_found.CamlId) {
      return 2;
    }
    if (exn.CamlExt.CamlId === A.CamlId) {
      exn._1 !== 32;
      return 3;
    }
    if (exn.CamlExt.CamlId !== B.CamlId) {
      if (exn.CamlExt.CamlId === C.CamlId) {
        return exn._1;
      } else if (exn.CamlExt.CamlId === D.CamlId) {
        return exn._1[0];
      } else {
        return 4;
      }
    }
    var match = exn._1;
    if (!match) {
      return 4;
    }
    var match$1 = match[1];
    if (!match$1) {
      return 4;
    }
    var match$2 = match$1[1];
    if (match$2) {
      return match$2[0];
    } else {
      return 4;
    }
  }
}

var A = Caml_exceptions.create("Exception_raise_test.A");

var f;

try {
  f = (function () {throw (new Error ("x"))} ());
}
catch (raw_x){
  var x = Caml_js_exceptions.internalToOCamlException(raw_x);
  f = x.CamlExt.CamlId === A.CamlId ? x._1 : 2;
}

var ff;

try {
  ff = (function () {throw 3} ());
}
catch (raw_x$1){
  var x$1 = Caml_js_exceptions.internalToOCamlException(raw_x$1);
  ff = x$1.CamlExt.CamlId === A.CamlId ? x$1._1 : 2;
}

var fff;

try {
  fff = (function () {throw 2} ());
}
catch (raw_x$2){
  var x$2 = Caml_js_exceptions.internalToOCamlException(raw_x$2);
  fff = x$2.CamlExt.CamlId === A.CamlId ? x$2._1 : 2;
}

var a0;

try {
  a0 = (function (){throw 2} ());
}
catch (raw_x$3){
  var x$3 = Caml_js_exceptions.internalToOCamlException(raw_x$3);
  if (x$3.CamlExt.CamlId === A.CamlId || x$3.CamlExt.CamlId === Js_exn.$$Error.CamlId) {
    a0 = x$3._1;
  } else {
    throw {
          CamlExt: Caml_builtin_exceptions.assert_failure,
          _1: /* tuple */[
            "exception_raise_test.ml",
            102,
            9
          ]
        };
  }
}

var a1;

try {
  a1 = (function (){throw 2} ());
}
catch (raw_e){
  a1 = Caml_js_exceptions.internalToOCamlException(raw_e);
}

var a2;

try {
  a2 = (function (){throw (new Error("x"))} ());
}
catch (raw_e$1){
  a2 = Caml_js_exceptions.internalToOCamlException(raw_e$1);
}

var suites = {
  contents: /* :: */[
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
            if (a1.CamlExt.CamlId === Js_exn.$$Error.CamlId) {
              return /* Eq */Block.__(0, [
                        a1._1,
                        2
                      ]);
            }
            throw {
                  CamlExt: Caml_builtin_exceptions.assert_failure,
                  _1: /* tuple */[
                    "exception_raise_test.ml",
                    119,
                    15
                  ]
                };
          })
      ],
      /* [] */0
    ]
  ]
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

try {
  (function (_){throw 2}(undefined));
}
catch (raw_e$2){
  var e = Caml_js_exceptions.internalToOCamlException(raw_e$2);
  eq("File \"exception_raise_test.ml\", line 131, characters 7-14", Caml_js_exceptions.caml_as_js_exn(e) !== undefined, true);
}

try {
  throw {
        CamlExt: Caml_builtin_exceptions.not_found
      };
}
catch (raw_e$3){
  var e$1 = Caml_js_exceptions.internalToOCamlException(raw_e$3);
  eq("File \"exception_raise_test.ml\", line 138, characters 7-14", Caml_js_exceptions.caml_as_js_exn(e$1) !== undefined, false);
}

function fff0(x, g) {
  var val;
  try {
    val = Curry._1(x, undefined);
  }
  catch (exn){
    return 1;
  }
  return Curry._1(g, undefined);
}

function input_lines(ic, _acc) {
  while(true) {
    var acc = _acc;
    var line;
    try {
      line = Pervasives.input_line(ic);
    }
    catch (exn){
      return List.rev(acc);
    }
    _acc = /* :: */[
      line,
      acc
    ];
    continue ;
  };
}

eq("File \"exception_raise_test.ml\", line 150, characters 5-12", function (a,b,c,_){return a + b + c }(1, 2, 3, 4), 6);

Mt.from_pair_suites("Exception_raise_test", suites.contents);

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
exports.fff0 = fff0;
exports.input_lines = input_lines;
/* f Not a pure module */
