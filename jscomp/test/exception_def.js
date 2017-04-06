'use strict';

var Printexc        = require("../../lib/js/printexc.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var A = Caml_exceptions.create("Exception_def.A");

var A$1 = Caml_exceptions.create("Exception_def.U.A");

var U = /* module */[/* A */A$1];

Printexc.register_printer(function (param) {
      if (param[0] === A) {
        return /* Some */["A"];
      } else {
        return /* None */0;
      }
    });

exports.A = A;
exports.U = U;
/*  Not a pure module */
