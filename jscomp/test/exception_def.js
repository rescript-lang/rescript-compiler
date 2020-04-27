'use strict';

var Printexc = require("../../lib/js/printexc.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var A = Caml_exceptions.create("Exception_def.A");

var A$1 = Caml_exceptions.create("Exception_def.U.A");

var U = {
  A: A$1
};

var H = { };

var Bx = Caml_exceptions.create("Exception_def.Bx");

var u = {
  CamlExt: Bx
};

var Ax = Caml_exceptions.create("Exception_def.Ax");

var XXX = Caml_exceptions.create("Exception_def.XXX");

var Aa = Caml_builtin_exceptions.match_failure;

var v_001 = /* tuple */[
  "",
  0,
  0
];

var v = {
  CamlExt: Aa,
  _1: v_001
};

Printexc.register_printer((function (s) {
        if (s.CamlExt === A) {
          return "A";
        }
        
      }));

var a = 3;

exports.A = A;
exports.U = U;
exports.H = H;
exports.Bx = Bx;
exports.a = a;
exports.u = u;
exports.Ax = Ax;
exports.XXX = XXX;
exports.Aa = Aa;
exports.v = v;
/*  Not a pure module */
