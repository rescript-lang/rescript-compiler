// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_module = require("../../lib/js/caml_module.js");

var PA = Caml_module.init_mod([
      "gpr_3931_test.res",
      3,
      4
    ], {
      TAG: "Module",
      _0: [[
          "Function",
          "print"
        ]]
    });

var P = Caml_module.init_mod([
      "gpr_3931_test.res",
      12,
      4
    ], {
      TAG: "Module",
      _0: [[
          "Function",
          "print"
        ]]
    });

function print(a) {
  $$Array.iter(P.print, a);
}

Caml_module.update_mod({
      TAG: "Module",
      _0: [[
          "Function",
          "print"
        ]]
    }, PA, {
      print: print
    });

function print$1(i) {
  console.log(String(i));
}

Caml_module.update_mod({
      TAG: "Module",
      _0: [[
          "Function",
          "print"
        ]]
    }, P, {
      print: print$1
    });

Curry._1(PA.print, [
      1,
      2
    ]);

exports.PA = PA;
exports.P = P;
/* PA Not a pure module */
