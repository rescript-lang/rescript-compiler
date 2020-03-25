'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Offset = require("./offset.js");
var Mt_global = require("./mt_global.js");

var count = {
  contents: 0
};

function test(set) {
  count.contents = Offset.$$Set.cardinal(set) + count.contents | 0;
  
}

test(Curry._1(Offset.M.$$Set.singleton, "42"));

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(f, a, b) {
  return Mt_global.collect_eq(test_id, suites, f, a, b);
}

eq("File \"basic_module_test.ml\", line 39, characters 12-19", count.contents, 1);

Mt.from_pair_suites("Basic_module_test", suites.contents);

/*  Not a pure module */
