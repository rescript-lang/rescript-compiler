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

var u_000 = Bx.ExceptionID;

var u_001 = Bx.Debug;

var u = {
  ExceptionID: u_000,
  Debug: u_001
};

var Ax = Caml_exceptions.create("Exception_def.Ax");

var XXX = Caml_exceptions.create("Exception_def.XXX");

var Aa = Caml_builtin_exceptions.match_failure;

var v_000 = Aa.ExceptionID;

var v_001 = /* tuple */[
  "",
  0,
  0
];

var v_002 = Aa.Debug;

var v = {
  ExceptionID: v_000,
  _1: v_001,
  Debug: v_002
};

var H0 = Caml_builtin_exceptions.not_found;

var H1 = Caml_exceptions.create("Exception_def.H1");

var H2 = Caml_exceptions.create("Exception_def.H2");

var h2_000 = H2.ExceptionID;

var h2_001 = H2.Debug;

var h2 = {
  ExceptionID: h2_000,
  Debug: h2_001
};

var h3_000 = H2.ExceptionID;

var h3_001 = H2.Debug;

var h3 = {
  ExceptionID: h3_000,
  Debug: h3_001
};

var h4_000 = H0.ExceptionID;

var h4_001 = H0.Debug;

var h4 = {
  ExceptionID: h4_000,
  Debug: h4_001
};

var H4 = Caml_builtin_exceptions.invalid_argument;

var h5_000 = H4.ExceptionID;

var h5_002 = H4.Debug;

var h5 = {
  ExceptionID: h5_000,
  _1: "xx",
  Debug: h5_002
};

Printexc.register_printer((function (s) {
        if (s.ExceptionID === A.ExceptionID) {
          return "A";
        }
        
      }));

var a = 3;

var H3 = H2;

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
exports.H0 = H0;
exports.H1 = H1;
exports.H2 = H2;
exports.H3 = H3;
exports.h2 = h2;
exports.h3 = h3;
exports.h4 = h4;
exports.H4 = H4;
exports.h5 = h5;
/*  Not a pure module */
