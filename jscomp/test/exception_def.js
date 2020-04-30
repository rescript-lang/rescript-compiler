'use strict';

var Printexc = require("../../lib/js/printexc.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var A = Caml_exceptions.create("Exception_def.A");

var A$1 = Caml_exceptions.create("Exception_def.U.A");

var U = {
  A: A$1
};

var H = { };

var Bx = Caml_exceptions.create("Exception_def.Bx");

var u = {
  ExceptionID: Bx
};

var Ax = Caml_exceptions.create("Exception_def.Ax");

var XXX = Caml_exceptions.create("Exception_def.XXX");

var Aa = "Match_failure";

var v_001 = /* tuple */[
  "",
  0,
  0
];

var v = {
  ExceptionID: Aa,
  _1: v_001
};

var H0 = "Not_found";

var H1 = Caml_exceptions.create("Exception_def.H1");

var H2 = Caml_exceptions.create("Exception_def.H2");

var h2 = {
  ExceptionID: H2
};

var h3 = {
  ExceptionID: H2
};

var h4 = {
  ExceptionID: H0
};

var H4 = "Invalid_argument";

var h5 = {
  ExceptionID: H4,
  _1: "xx"
};

Printexc.register_printer((function (s) {
        if (s.ExceptionID === A) {
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
