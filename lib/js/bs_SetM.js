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

function minNull(d) {
  return Bs_internalAVLset.minNull0(d.data);
}

function maxOpt(d) {
  return Bs_internalAVLset.maxOpt0(d.data);
}

function maxNull(d) {
  return Bs_internalAVLset.maxNull0(d.data);
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

function addCheck(m, e) {
  var dict = m.dict;
  var oldRoot = m.data;
  var added = [/* false */0];
  var newRoot = Bs_internalSet.addMutateCheckAux(oldRoot, e, added, dict[/* cmp */0]);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
  }
  return added[0];
}

function addArrayOnly(d, xs) {
  var dict = d.dict;
  var oldRoot = d.data;
  var newRoot = Bs_internalSet.addArrayMutate(oldRoot, xs, dict[/* cmp */0]);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return /* () */0;
  } else {
    return 0;
  }
}

function addArray(d, xs) {
  addArrayOnly(d, xs);
  return d;
}

function removeArrayOnly(d, xs) {
  var dict = d.dict;
  var oldRoot = d.data;
  var newRoot = Bs_internalSet.removeArrayMutate(oldRoot, xs, dict[/* cmp */0]);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return /* () */0;
  } else {
    return 0;
  }
}

function removeArray(d, xs) {
  removeArrayOnly(d, xs);
  return d;
}

function removeOnly(d, v) {
  var dict = d.dict;
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var newRoot = Bs_internalSet.removeMutateAux(dict[/* cmp */0], oldRoot, v);
    if (newRoot !== oldRoot) {
      d.data = newRoot;
      return /* () */0;
    } else {
      return 0;
    }
  } else {
    return /* () */0;
  }
}

function removeCheck(d, v) {
  var dict = d.dict;
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var removed = [/* false */0];
    var newRoot = Bs_internalSet.removeMutateCheckAux(oldRoot, v, removed, dict[/* cmp */0]);
    if (newRoot !== oldRoot) {
      d.data = newRoot;
    }
    return removed[0];
  } else {
    return /* false */0;
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

function findNull(d, x) {
  var dict = d.dict;
  return Bs_internalSet.findNull0(dict[/* cmp */0], d.data, x);
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

exports.empty = empty;
exports.ofArray = ofArray;
exports.isEmpty = isEmpty;
exports.mem = mem;
exports.addOnly = addOnly;
exports.add = add;
exports.addCheck = addCheck;
exports.addArrayOnly = addArrayOnly;
exports.addArray = addArray;
exports.removeArrayOnly = removeArrayOnly;
exports.removeArray = removeArray;
exports.singleton = singleton;
exports.removeOnly = removeOnly;
exports.remove = remove;
exports.removeCheck = removeCheck;
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
exports.minNull = minNull;
exports.maxOpt = maxOpt;
exports.maxNull = maxNull;
exports.split = split;
exports.ofSortedArrayUnsafe = ofSortedArrayUnsafe;
exports.findOpt = findOpt;
exports.findNull = findNull;
/* No side effect */
