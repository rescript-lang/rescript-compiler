'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");
var Bs_internalSetInt = require("./bs_internalSetInt.js");

var empty = Bs_internalAVLset.empty0;

var isEmpty = Bs_internalAVLset.isEmpty0;

var mem = Bs_internalSetInt.mem;

var add = Bs_internalSetInt.add;

var singleton = Bs_internalAVLset.singleton0;

var remove = Bs_internalSetInt.remove;

var union = Bs_internalSetInt.union;

var inter = Bs_internalSetInt.inter;

var diff = Bs_internalSetInt.diff;

var cmp = Bs_internalSetInt.cmp;

var eq = Bs_internalSetInt.eq;

var subset = Bs_internalSetInt.subset;

var iter = Bs_internalAVLset.iter0;

var fold = Bs_internalAVLset.fold0;

var forAll = Bs_internalAVLset.forAll0;

var exists = Bs_internalAVLset.exists0;

var filter = Bs_internalAVLset.filter0;

var partition = Bs_internalAVLset.partition0;

var length = Bs_internalAVLset.length0;

var toList = Bs_internalAVLset.toList0;

var toArray = Bs_internalAVLset.toArray0;

var ofArray = Bs_internalSetInt.ofArray;

var ofSortedArrayUnsafe = Bs_internalAVLset.ofSortedArrayUnsafe0;

var minOpt = Bs_internalAVLset.minOpt0;

var minNull = Bs_internalAVLset.minNull0;

var maxOpt = Bs_internalAVLset.maxOpt0;

var maxNull = Bs_internalAVLset.maxNull0;

var split = Bs_internalSetInt.split;

var findOpt = Bs_internalSetInt.findOpt;

var checkInvariant = Bs_internalAVLset.checkInvariant;

exports.empty = empty;
exports.isEmpty = isEmpty;
exports.mem = mem;
exports.add = add;
exports.singleton = singleton;
exports.remove = remove;
exports.union = union;
exports.inter = inter;
exports.diff = diff;
exports.cmp = cmp;
exports.eq = eq;
exports.subset = subset;
exports.iter = iter;
exports.fold = fold;
exports.forAll = forAll;
exports.exists = exists;
exports.filter = filter;
exports.partition = partition;
exports.length = length;
exports.toList = toList;
exports.toArray = toArray;
exports.ofArray = ofArray;
exports.ofSortedArrayUnsafe = ofSortedArrayUnsafe;
exports.minOpt = minOpt;
exports.minNull = minNull;
exports.maxOpt = maxOpt;
exports.maxNull = maxNull;
exports.split = split;
exports.findOpt = findOpt;
exports.checkInvariant = checkInvariant;
/* No side effect */
