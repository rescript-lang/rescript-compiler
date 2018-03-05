'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Caml_obj = require("../../lib/js/caml_obj.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  return Mt.bool_suites(test_id, suites, loc, x);
}

var Block$1 = /* module */[];

var v0 = /* A */Block.__(1, [
    0,
    1
  ]);

var Block$2 = /* module */[];

var v1 = /* A */Block.__(1, [
    0,
    1
  ]);

var N = /* module */[
  /* Block */Block$2,
  /* v1 */v1
];

var Caml_obj$1 = /* module */[];

var List$1 = /* module */[];

var V = /* module */[/* List */List$1];

var f = Caml_obj.caml_equal;

eq("File \"block_alias_test.ml\", line 32, characters 6-13", List.length(/* :: */[
          1,
          /* :: */[
            2,
            /* [] */0
          ]
        ]), 2);

b("File \"block_alias_test.ml\", line 33, characters 5-12", Caml_obj.caml_equal(v0, /* A */Block.__(1, [
            0,
            1
          ])));

eq("File \"block_alias_test.ml\", line 34, characters 6-13", v0, v1);

Mt.from_pair_suites("block_alias_test.ml", suites[0]);

var h = List.length;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.Block = Block$1;
exports.v0 = v0;
exports.N = N;
exports.Caml_obj = Caml_obj$1;
exports.V = V;
exports.f = f;
exports.h = h;
/*  Not a pure module */
