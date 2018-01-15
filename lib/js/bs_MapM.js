'use strict';

var Bs_internalAVLtree = require("./bs_internalAVLtree.js");

function removeMutateAux(cmp, nt, x) {
  var k = nt.key;
  var c = cmp(x, k);
  if (c) {
    if (c < 0) {
      var match = nt.left;
      if (match !== null) {
        nt.left = removeMutateAux(cmp, match, x);
        return Bs_internalAVLtree.balMutate(nt);
      } else {
        return nt;
      }
    } else {
      var match$1 = nt.right;
      if (match$1 !== null) {
        nt.right = removeMutateAux(cmp, match$1, x);
        return Bs_internalAVLtree.balMutate(nt);
      } else {
        return nt;
      }
    }
  } else {
    var l = nt.left;
    var r = nt.right;
    if (l !== null) {
      if (r !== null) {
        nt.right = Bs_internalAVLtree.removeMinAuxWithRootMutate(nt, r);
        return Bs_internalAVLtree.balMutate(nt);
      } else {
        return l;
      }
    } else if (r !== null) {
      return r;
    } else {
      return l;
    }
  }
}

function removeOnly(d, k) {
  var dict = d.dict;
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var newRoot = removeMutateAux(dict[/* cmp */0], oldRoot, k);
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

function remove(d, v) {
  removeOnly(d, v);
  return d;
}

function empty(dict) {
  return {
          dict: dict,
          data: Bs_internalAVLtree.empty0
        };
}

function isEmpty(d) {
  return Bs_internalAVLtree.isEmpty0(d.data);
}

function singleton(dict, x, v) {
  return {
          dict: dict,
          data: Bs_internalAVLtree.singleton0(x, v)
        };
}

function minKVOpt(m) {
  return Bs_internalAVLtree.minKVOpt0(m.data);
}

function minKVNull(m) {
  return Bs_internalAVLtree.minKVNull0(m.data);
}

function maxKVOpt(m) {
  return Bs_internalAVLtree.maxKVOpt0(m.data);
}

function maxKVNull(m) {
  return Bs_internalAVLtree.maxKVNull0(m.data);
}

function iter(d, f) {
  return Bs_internalAVLtree.iter0(d.data, f);
}

function fold(d, acc, cb) {
  return Bs_internalAVLtree.fold0(d.data, acc, cb);
}

function forAll(d, p) {
  return Bs_internalAVLtree.forAll0(d.data, p);
}

function exists(d, p) {
  return Bs_internalAVLtree.exists0(d.data, p);
}

function length(d) {
  return Bs_internalAVLtree.length0(d.data);
}

function toList(d) {
  return Bs_internalAVLtree.toList0(d.data);
}

function toArray(d) {
  return Bs_internalAVLtree.toArray0(d.data);
}

function addOnly(m, e, v) {
  var dict = m.dict;
  var oldRoot = m.data;
  var newRoot = Bs_internalAVLtree.addMutate(dict[/* cmp */0], oldRoot, e, v);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
    return /* () */0;
  } else {
    return 0;
  }
}

function add(m, e, v) {
  addOnly(m, e, v);
  return m;
}

function ofArray(dict, data) {
  return {
          dict: dict,
          data: Bs_internalAVLtree.ofArray0(dict[/* cmp */0], data)
        };
}

function cmp(m1, m2, cmp$1) {
  var dict = m1.dict;
  var m1_data = m1.data;
  var m2_data = m2.data;
  return Bs_internalAVLtree.cmp0(m1_data, m2_data, dict[/* cmp */0], cmp$1);
}

function eq(m1, m2, cmp) {
  var dict = m1.dict;
  var m1_data = m1.data;
  var m2_data = m2.data;
  return Bs_internalAVLtree.eq0(m1_data, m2_data, dict[/* cmp */0], cmp);
}

function map(m, f) {
  var dict = m.dict;
  var map$1 = m.data;
  return {
          dict: dict,
          data: Bs_internalAVLtree.map0(map$1, f)
        };
}

function mapi(map, f) {
  var dict = map.dict;
  var map$1 = map.data;
  return {
          dict: dict,
          data: Bs_internalAVLtree.mapi0(map$1, f)
        };
}

function findOpt(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.findOpt0(map$1, x, dict[/* cmp */0]);
}

function findNull(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.findNull0(map$1, x, dict[/* cmp */0]);
}

function findWithDefault(map, x, def) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.findWithDefault0(map$1, x, def, dict[/* cmp */0]);
}

function mem(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.mem0(map$1, x, dict[/* cmp */0]);
}

exports.empty = empty;
exports.ofArray = ofArray;
exports.isEmpty = isEmpty;
exports.mem = mem;
exports.add = add;
exports.singleton = singleton;
exports.remove = remove;
exports.cmp = cmp;
exports.eq = eq;
exports.iter = iter;
exports.fold = fold;
exports.forAll = forAll;
exports.exists = exists;
exports.length = length;
exports.toList = toList;
exports.toArray = toArray;
exports.minKVOpt = minKVOpt;
exports.minKVNull = minKVNull;
exports.maxKVOpt = maxKVOpt;
exports.maxKVNull = maxKVNull;
exports.findOpt = findOpt;
exports.findNull = findNull;
exports.findWithDefault = findWithDefault;
exports.map = map;
exports.mapi = mapi;
/* No side effect */
