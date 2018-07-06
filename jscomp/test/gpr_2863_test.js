'use strict';

var Belt_MutableSetInt = require("../../lib/js/belt_MutableSetInt.js");
var Belt_internalAVLset = require("../../lib/js/belt_internalAVLset.js");

var mySet = {
  data: Belt_internalAVLset.empty
};

Belt_MutableSetInt.add(mySet, 1);

Belt_MutableSetInt.add(mySet, 2);

Belt_MutableSetInt.remove(mySet, 1);

var a = 3;

exports.mySet = mySet;
exports.a = a;
/*  Not a pure module */
