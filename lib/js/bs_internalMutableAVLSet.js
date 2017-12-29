'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");
var Bs_internalSetInt = require("./bs_internalSetInt.js");

var N = 0;

var I = 0;

var empty = Bs_internalAVLset.empty0;

var isEmpty = Bs_internalAVLset.isEmpty0;

var singleton = Bs_internalAVLset.singleton0;

var min = Bs_internalAVLset.min0;

var max = Bs_internalAVLset.max0;

var iter = Bs_internalAVLset.iter0;

var fold = Bs_internalAVLset.fold0;

var forAll = Bs_internalAVLset.forAll0;

var exists = Bs_internalAVLset.exists0;

var filter = Bs_internalAVLset.filter0;

var partition = Bs_internalAVLset.partition0;

var length = Bs_internalAVLset.length0;

var elements = Bs_internalAVLset.elements0;

var toArray = Bs_internalAVLset.toArray0;

var checkInvariant = Bs_internalAVLset.checkInvariant;

var add = Bs_internalSetInt.addMutate;

var ofArray = Bs_internalSetInt.ofArray;

var cmp = Bs_internalSetInt.cmp;

var diff = Bs_internalSetInt.diff;

var eq = Bs_internalSetInt.eq;

var findOpt = Bs_internalSetInt.findOpt;

var split = Bs_internalSetInt.split;

var subset = Bs_internalSetInt.subset;

var inter = Bs_internalSetInt.inter;

var union = Bs_internalSetInt.union;

var remove = Bs_internalSetInt.remove;

var mem = Bs_internalSetInt.mem;

exports.N = N;
exports.I = I;
exports.empty = empty;
exports.isEmpty = isEmpty;
exports.singleton = singleton;
exports.min = min;
exports.max = max;
exports.iter = iter;
exports.fold = fold;
exports.forAll = forAll;
exports.exists = exists;
exports.filter = filter;
exports.partition = partition;
exports.length = length;
exports.elements = elements;
exports.toArray = toArray;
exports.checkInvariant = checkInvariant;
exports.add = add;
exports.ofArray = ofArray;
exports.cmp = cmp;
exports.diff = diff;
exports.eq = eq;
exports.findOpt = findOpt;
exports.split = split;
exports.subset = subset;
exports.inter = inter;
exports.union = union;
exports.remove = remove;
exports.mem = mem;
/* No side effect */
