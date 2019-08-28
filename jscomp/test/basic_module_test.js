'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Offset = require("./offset.js");
var Pr6726 = require("./pr6726.js");
var Mt_global = require("./mt_global.js");

var count = /* record */{
  contents: 0
};

var M = {
  Set: Offset.M.Set
};

function test(set) {
  count.contents = Curry._1(M.Set.cardinal, set) + count.contents | 0;
  return /* () */0;
}

test(Curry._1(Offset.M.Set.singleton, "42"));

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(f, a, b) {
  return Mt_global.collect_eq(test_id, suites, f, a, b);
}

eq("File \"basic_module_test.ml\", line 39, characters 12-19", count.contents, 1);

Mt.from_pair_suites("Basic_module_test", suites.contents);

/*  Not a pure module */
