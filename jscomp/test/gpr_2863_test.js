'use strict';

var Belt_MutableSetInt = require("../../lib/js/belt_MutableSetInt.js");

var mySet = {
  data: undefined
};

Belt_MutableSetInt.add(mySet, 1);

Belt_MutableSetInt.add(mySet, 2);

Belt_MutableSetInt.remove(mySet, 1);

var a = 3;

exports.mySet = mySet;
exports.a = a;
/*  Not a pure module */
