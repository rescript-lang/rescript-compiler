'use strict';

var Bs_internalSet = require("./bs_internalSet.js");
var Bs_internalAVLset = require("./bs_internalAVLset.js");

function empty(dict) {
  return {
          dict: dict,
          data: Bs_internalAVLset.empty0
        };
}

function isEmpty(d) {
  return Bs_internalAVLset.isEmpty0(d.data);
}

function singleton(dict, x) {
  return {
          dict: dict,
          data: Bs_internalAVLset.singleton0(x)
        };
}

function minOpt(d) {
  return Bs_internalAVLset.minOpt0(d.data);
}

function maxOpt(d) {
  return Bs_internalAVLset.maxOpt0(d.data);
}

function iter(d, f) {
  return Bs_internalAVLset.iter0(d.data, f);
}

function fold(d, acc, cb) {
  return Bs_internalAVLset.fold0(d.data, acc, cb);
}

function forAll(d, p) {
  return Bs_internalAVLset.forAll0(d.data, p);
}

function exists(d, p) {
  return Bs_internalAVLset.exists0(d.data, p);
}

function filter(d, p) {
  var data = d.data;
  var dict = d.dict;
  return {
          dict: dict,
          data: Bs_internalAVLset.filter0(data, p)
        };
}

function partition(d, p) {
  var data = d.data;
  var dict = d.dict;
  var match = Bs_internalAVLset.partition0(data, p);
  return /* tuple */[
          {
            dict: dict,
            data: match[0]
          },
          {
            dict: dict,
            data: match[1]
          }
        ];
}

function length(d) {
  return Bs_internalAVLset.length0(d.data);
}

function toList(d) {
  return Bs_internalAVLset.toList0(d.data);
}

function toArray(d) {
  return Bs_internalAVLset.toArray0(d.data);
}

function ofSortedArrayUnsafe(dict, xs) {
  return {
          dict: dict,
          data: Bs_internalAVLset.ofSortedArrayUnsafe0(xs)
        };
}

function addOnly(m, e) {
  var dict = m.dict;
  var oldRoot = m.data;
  var newRoot = Bs_internalSet.addMutate(dict[/* cmp */0], oldRoot, e);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
    return /* () */0;
  } else {
    return 0;
  }
}

function add(m, e) {
  addOnly(m, e);
  return m;
}

function removeOnly(d, v) {
  var dict = d.dict;
  var oldRoot = d.data;
  var newRoot = Bs_internalSet.removeMutate(dict[/* cmp */0], oldRoot, v);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return /* () */0;
  } else {
    return 0;
  }
}

function remove(d, v) {
  removeOnly(d, v);
  return d;
}

function cmp(d0, d1) {
  var dict = d0.dict;
  return Bs_internalSet.cmp0(dict[/* cmp */0], d0.data, d1.data);
}

function diff(d0, d1) {
  var dict = d0.dict;
  return {
          dict: dict,
          data: Bs_internalSet.diff0(dict[/* cmp */0], d0.data, d1.data)
        };
}

function eq(d0, d1) {
  var dict = d0.dict;
  return Bs_internalSet.eq0(dict[/* cmp */0], d0.data, d1.data);
}

function findOpt(d, x) {
  var dict = d.dict;
  return Bs_internalSet.findOpt0(dict[/* cmp */0], d.data, x);
}

function findAssert(d, x) {
  var dict = d.dict;
  return Bs_internalSet.findAssert0(dict[/* cmp */0], d.data, x);
}

function ofArray(dict, data) {
  return {
          dict: dict,
          data: Bs_internalSet.ofArray0(dict[/* cmp */0], data)
        };
}

function split(d, p) {
  var dict = d.dict;
  var match = Bs_internalSet.split0(dict[/* cmp */0], d.data, p);
  return /* tuple */[
          {
            dict: dict,
            data: match[0]
          },
          match[1],
          {
            dict: dict,
            data: match[2]
          }
        ];
}

function subset(a, b) {
  var dict = a.dict;
  return Bs_internalSet.subset0(dict[/* cmp */0], a.data, b.data);
}

function inter(a, b) {
  var dict = a.dict;
  return {
          dict: dict,
          data: Bs_internalSet.inter0(dict[/* cmp */0], a.data, b.data)
        };
}

function union(a, b) {
  var dict = a.dict;
  return {
          dict: dict,
          data: Bs_internalSet.union0(dict[/* cmp */0], a.data, b.data)
        };
}

function mem(d, x) {
  var dict = d.dict;
  return Bs_internalSet.mem0(dict[/* cmp */0], d.data, x);
}

var empty0 = Bs_internalAVLset.empty0;

var ofArray0 = Bs_internalSet.ofArray0;

var isEmpty0 = Bs_internalAVLset.isEmpty0;

var mem0 = Bs_internalSet.mem0;

var add0 = Bs_internalSet.addMutate;

var singleton0 = Bs_internalAVLset.singleton0;

var remove0 = Bs_internalSet.removeMutate;

var union0 = Bs_internalSet.union0;

var inter0 = Bs_internalSet.inter0;

var diff0 = Bs_internalSet.diff0;

var subset0 = Bs_internalSet.subset0;

var cmp0 = Bs_internalSet.cmp0;

var eq0 = Bs_internalSet.eq0;

var iter0 = Bs_internalAVLset.iter0;

var fold0 = Bs_internalAVLset.fold0;

var forAll0 = Bs_internalAVLset.forAll0;

var exists0 = Bs_internalAVLset.exists0;

var filter0 = Bs_internalAVLset.filter0;

var partition0 = Bs_internalAVLset.partition0;

var length0 = Bs_internalAVLset.length0;

var toList0 = Bs_internalAVLset.toList0;

var toArray0 = Bs_internalAVLset.toArray0;

var minOpt0 = Bs_internalAVLset.minOpt0;

var maxOpt0 = Bs_internalAVLset.maxOpt0;

var split0 = Bs_internalSet.split0;

var ofSortedArrayUnsafe0 = Bs_internalAVLset.ofSortedArrayUnsafe0;

var findOpt0 = Bs_internalSet.findOpt0;

var findAssert0 = Bs_internalSet.findAssert0;

exports.empty = empty;
exports.ofArray = ofArray;
exports.isEmpty = isEmpty;
exports.mem = mem;
exports.add = add;
exports.singleton = singleton;
exports.remove = remove;
exports.union = union;
exports.inter = inter;
exports.diff = diff;
exports.subset = subset;
exports.cmp = cmp;
exports.eq = eq;
exports.iter = iter;
exports.fold = fold;
exports.forAll = forAll;
exports.exists = exists;
exports.filter = filter;
exports.partition = partition;
exports.length = length;
exports.toList = toList;
exports.toArray = toArray;
exports.minOpt = minOpt;
exports.maxOpt = maxOpt;
exports.split = split;
exports.ofSortedArrayUnsafe = ofSortedArrayUnsafe;
exports.findOpt = findOpt;
exports.findAssert = findAssert;
exports.empty0 = empty0;
exports.ofArray0 = ofArray0;
exports.isEmpty0 = isEmpty0;
exports.mem0 = mem0;
exports.add0 = add0;
exports.singleton0 = singleton0;
exports.remove0 = remove0;
exports.union0 = union0;
exports.inter0 = inter0;
exports.diff0 = diff0;
exports.subset0 = subset0;
exports.cmp0 = cmp0;
exports.eq0 = eq0;
exports.iter0 = iter0;
exports.fold0 = fold0;
exports.forAll0 = forAll0;
exports.exists0 = exists0;
exports.filter0 = filter0;
exports.partition0 = partition0;
exports.length0 = length0;
exports.toList0 = toList0;
exports.toArray0 = toArray0;
exports.minOpt0 = minOpt0;
exports.maxOpt0 = maxOpt0;
exports.split0 = split0;
exports.ofSortedArrayUnsafe0 = ofSortedArrayUnsafe0;
exports.findOpt0 = findOpt0;
exports.findAssert0 = findAssert0;
/* No side effect */
