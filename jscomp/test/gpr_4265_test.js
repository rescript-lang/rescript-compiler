'use strict';

var Belt_MutableMapInt = require("../../lib/js/belt_MutableMapInt.js");
var Belt_internalMapInt = require("../../lib/js/belt_internalMapInt.js");

var mockMap = {
  data: null
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

console.log("should be identical", n1 === n);

console.log(n);

console.log(n1);

exports.mockMap = mockMap;
exports.add = add;
exports.remove = remove;
exports.n = n;
exports.n1 = n1;
/*  Not a pure module */
