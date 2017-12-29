'use strict';

var Printexc = require("../../lib/js/printexc.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var A = Caml_exceptions.create("Exception_def.A");

var A$1 = Caml_exceptions.create("Exception_def.U.A");

var U = /* module */[/* A */A$1];

var H = /* module */[];

var Bx = Caml_exceptions.create("Exception_def.Bx");

var Ax = Caml_exceptions.create("Exception_def.Ax");

var XXX = Caml_exceptions.create("Exception_def.XXX");

Printexc.register_printer((function (param) {
        if (param[0] === A) {
          return /* Some */["A"];
        } else {
          return /* None */0;
        }
      }));

var a = 3;

var u = Bx;

exports.A = A;
exports.U = U;
exports.H = H;
exports.Bx = Bx;
exports.a = a;
exports.u = u;
exports.Ax = Ax;
exports.XXX = XXX;
/*  Not a pure module */
