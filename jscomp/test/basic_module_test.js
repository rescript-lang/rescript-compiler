// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Pr6726    = require("./pr6726");
var Mt        = require("./mt");
var Offset    = require("./offset");
var Mt_global = require("./mt_global");
var Curry     = require("../runtime/curry");

var count = [0];

function F(M) {
  var test = function (set) {
    count[0] = Curry._1(M[/* Set */0][/* cardinal */18], set) + count[0] | 0;
    return /* () */0;
  };
  return /* module */[test];
}

var M = [Offset.M[/* Set */1]];

function test(set) {
  count[0] = Curry._1(M[/* Set */0][/* cardinal */18], set) + count[0] | 0;
  return /* () */0;
}

var M$1 = /* module */[test];

test(Curry._1(Offset.M[/* Set */1][/* singleton */4], "42"));

var v = Pr6726.Test[/* v */1];

var suites = [/* [] */0];

var test_id = [0];

function eq(f, a, b) {
  return Mt_global.collect_eq(test_id, suites, f, a, b);
}

eq('File "basic_module_test.ml", line 33, characters 12-19', count[0], 1);

Mt.from_pair_suites("basic_module_test.ml", suites[0]);

exports.F = F;
exports.M = M$1;
exports.v = v;
/*  Not a pure module */
