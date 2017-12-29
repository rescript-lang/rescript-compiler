'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");
var Bs_internalSetInt = require("./bs_internalSetInt.js");

var empty = Bs_internalAVLset.empty0;

var isEmpty = Bs_internalAVLset.isEmpty0;

var mem = Bs_internalSetInt.mem;

var add = Bs_internalSetInt.addMutate;

var addArray = Bs_internalSetInt.addArrayMutate;

var ofArray = Bs_internalSetInt.ofArray;

var toArray = Bs_internalAVLset.toArray0;

var singleton = Bs_internalAVLset.singleton0;

var checkInvariant = Bs_internalAVLset.checkInvariant;

var length = Bs_internalAVLset.length0;

exports.empty = empty;
exports.isEmpty = isEmpty;
exports.mem = mem;
exports.add = add;
exports.addArray = addArray;
exports.ofArray = ofArray;
exports.toArray = toArray;
exports.singleton = singleton;
exports.checkInvariant = checkInvariant;
exports.length = length;
/* No side effect */
