'use strict';

var Mt        = require("./mt");
var Block     = require("../../lib/js/block");
var Curry     = require("../../lib/js/curry");
var Scanf     = require("../../lib/js/scanf");
var Mt_global = require("./mt_global");

var suites = [/* [] */0];

var test_id = [0];

function eq(f, param) {
  return Mt_global.collect_eq(test_id, suites, f, param[0], param[1]);
}

eq("File \"scanf_test.ml\", line 6, characters 5-12", /* tuple */[
      Curry._1(Scanf.sscanf("32 31", /* Format */[
                /* Int */Block.__(4, [
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    /* Char_literal */Block.__(12, [
                        /* " " */32,
                        /* Int */Block.__(4, [
                            /* Int_d */0,
                            /* No_padding */0,
                            /* No_precision */0,
                            /* End_of_format */0
                          ])
                      ])
                  ]),
                "%d %d"
              ]), function (x, y) {
            return x + y | 0;
          }),
      63
    ]);

Mt.from_pair_suites("scanf_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
/*  Not a pure module */
