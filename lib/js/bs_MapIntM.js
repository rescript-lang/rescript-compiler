'use strict';

var Bs_internalMapInt = require("./bs_internalMapInt.js");
var Bs_internalAVLtree = require("./bs_internalAVLtree.js");

function empty() {
  return {
          data: Bs_internalAVLtree.empty0
        };
}

function isEmpty(m) {
  return Bs_internalAVLtree.isEmpty0(m.data);
}

function singleton(k, v) {
  return {
          data: Bs_internalAVLtree.singleton0(k, v)
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

function addOnly(m, k, v) {
  var old_data = m.data;
  var v$1 = Bs_internalMapInt.addMutate(old_data, k, v);
  if (v$1 !== old_data) {
    m.data = v$1;
    return /* () */0;
  } else {
    return 0;
  }
}

function add(d, k, v) {
  addOnly(d, k, v);
  return d;
}

function iter(d, f) {
  return Bs_internalAVLtree.iter0(d.data, f);
}

function map(d, f) {
  return {
          data: Bs_internalAVLtree.map0(d.data, f)
        };
}

function mapi(d, f) {
  return {
          data: Bs_internalAVLtree.mapi0(d.data, f)
        };
}

function fold(d, acc, f) {
  return Bs_internalAVLtree.fold0(d.data, acc, f);
}

function forAll(d, f) {
  return Bs_internalAVLtree.forAll0(d.data, f);
}

function exists(d, f) {
  return Bs_internalAVLtree.exists0(d.data, f);
}

function length(d) {
  return Bs_internalAVLtree.length0(d.data);
}

function toList(d) {
  return Bs_internalAVLtree.toList0(d.data);
}

function checkInvariant(d) {
  return Bs_internalAVLtree.checkInvariant(d.data);
}

function mem(d, v) {
  return Bs_internalMapInt.mem(d.data, v);
}

function removeMutateAux(nt, x) {
  var k = nt.key;
  if (x === k) {
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
  } else if (x < k) {
    var match = nt.left;
    if (match !== null) {
      nt.left = removeMutateAux(match, x);
      return Bs_internalAVLtree.balMutate(nt);
    } else {
      return nt;
    }
  } else {
    var match$1 = nt.right;
    if (match$1 !== null) {
      nt.right = removeMutateAux(match$1, x);
      return Bs_internalAVLtree.balMutate(nt);
    } else {
      return nt;
    }
  }
}

function removeMutate(nt, x) {
  if (nt !== null) {
    return removeMutateAux(nt, x);
  } else {
    return nt;
  }
}

function removeOnly(d, v) {
  var old_data = d.data;
  var v$1 = removeMutate(old_data, v);
  if (v$1 !== old_data) {
    d.data = v$1;
    return /* () */0;
  } else {
    return 0;
  }
}

function remove(d, v) {
  removeOnly(d, v);
  return d;
}

function ofArray(xs) {
  return {
          data: Bs_internalMapInt.ofArray(xs)
        };
}

function cmp(d0, d1) {
  var partial_arg = d1.data;
  var partial_arg$1 = d0.data;
  return (function (param) {
      return Bs_internalMapInt.cmp(partial_arg$1, partial_arg, param);
    });
}

function eq(d0, d1) {
  var partial_arg = d1.data;
  var partial_arg$1 = d0.data;
  return (function (param) {
      return Bs_internalMapInt.eq(partial_arg$1, partial_arg, param);
    });
}

function findOpt(d, x) {
  return Bs_internalMapInt.findOpt(d.data, x);
}

function findNull(d, x) {
  return Bs_internalMapInt.findNull(d.data, x);
}

function findWithDefault(d, x, def) {
  return Bs_internalMapInt.findWithDefault(d.data, x, def);
}

function findExn(d, x) {
  return Bs_internalMapInt.findExn(d.data, x);
}

exports.empty = empty;
exports.ofArray = ofArray;
exports.isEmpty = isEmpty;
exports.mem = mem;
exports.addOnly = addOnly;
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
exports.minKVOpt = minKVOpt;
exports.minKVNull = minKVNull;
exports.maxKVOpt = maxKVOpt;
exports.maxKVNull = maxKVNull;
exports.findOpt = findOpt;
exports.findNull = findNull;
exports.findWithDefault = findWithDefault;
exports.findExn = findExn;
exports.map = map;
exports.mapi = mapi;
exports.checkInvariant = checkInvariant;
/* No side effect */
