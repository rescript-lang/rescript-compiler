// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt_global  = require("./mt_global");
var Scanf      = require("../stdlib/scanf");
var Caml_curry = require("../runtime/caml_curry");

var suites = [/* [] */0];

var test_id = [0];

function eq(f, param) {
  return Mt_global.collect_eq(test_id, suites, f, param[0], param[1]);
}

console.log(/* tuple */[
      Caml_curry.app1(Scanf.sscanf("32 31", /* Format */[
                /* Int */{
                  0: /* Int_d */0,
                  1: /* No_padding */0,
                  2: /* No_precision */0,
                  3: /* Char_literal */{
                    0: /* " " */32,
                    1: /* Int */{
                      0: /* Int_d */0,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* End_of_format */0,
                      length: 4,
                      tag: 4
                    },
                    length: 2,
                    tag: 12
                  },
                  length: 4,
                  tag: 4
                },
                "%d %d"
              ]), function (x, y) {
            return x + y | 0;
          }),
      63
    ]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
/*  Not a pure module */
