'use strict';

var Mt = require("./mt.js");
var Belt_MutableMapInt = require("../../lib/js/belt_MutableMapInt.js");
var Belt_internalMapInt = require("../../lib/js/belt_internalMapInt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var mockMap = {
  data: undefined
};

function add(id) {
  Belt_MutableMapInt.set(mockMap, id, id);
  return id;
}

function remove(id) {
  return Belt_MutableMapInt.remove(mockMap, id);
}

add(1726);

var n = add(6667);

add(486);

Belt_MutableMapInt.remove(mockMap, 1726);

var n1 = Belt_internalMapInt.getExn(mockMap.data, 6667);

eq("File \"gpr_4265_test.ml\", line 17, characters 6-13", n, n1);

Mt.from_pair_suites("gpr_4265_test.ml", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.mockMap = mockMap;
exports.add = add;
exports.remove = remove;
exports.n = n;
exports.n1 = n1;
/*  Not a pure module */
