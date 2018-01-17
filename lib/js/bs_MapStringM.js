'use strict';

var Bs_internalAVLtree = require("./bs_internalAVLtree.js");
var Bs_internalMapString = require("./bs_internalMapString.js");

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

function minKeyNull(m) {
  return Bs_internalAVLtree.minKeyNull0(m.data);
}

function minKeyOpt(m) {
  return Bs_internalAVLtree.minKeyOpt0(m.data);
}

function maxKeyNull(m) {
  return Bs_internalAVLtree.maxKeyNull0(m.data);
}

function maxKeyOpt(m) {
  return Bs_internalAVLtree.maxKeyOpt0(m.data);
}

function minKeyValueOpt(m) {
  return Bs_internalAVLtree.minKVOpt0(m.data);
}

function minKeyValueNull(m) {
  return Bs_internalAVLtree.minKVNull0(m.data);
}

function maxKeyValueOpt(m) {
  return Bs_internalAVLtree.maxKVOpt0(m.data);
}

function maxKeyValueNull(m) {
  return Bs_internalAVLtree.maxKVNull0(m.data);
}

function addOnly(m, k, v) {
  var old_data = m.data;
  var v$1 = Bs_internalMapString.addMutate(old_data, k, v);
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

function toArray(d) {
  return Bs_internalAVLtree.toArray0(d.data);
}

function keysToArray(d) {
  return Bs_internalAVLtree.keysToArray0(d.data);
}

function valuesToArray(d) {
  return Bs_internalAVLtree.valuesToArray0(d.data);
}

function checkInvariant(d) {
  return Bs_internalAVLtree.checkInvariant(d.data);
}

function mem(d, v) {
  return Bs_internalMapString.mem(d.data, v);
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
          data: Bs_internalMapString.ofArray(xs)
        };
}

function cmp(d0, d1) {
  var partial_arg = d1.data;
  var partial_arg$1 = d0.data;
  return (function (param) {
      return Bs_internalMapString.cmp(partial_arg$1, partial_arg, param);
    });
}

function eq(d0, d1) {
  var partial_arg = d1.data;
  var partial_arg$1 = d0.data;
  return (function (param) {
      return Bs_internalMapString.eq(partial_arg$1, partial_arg, param);
    });
}

function getOpt(d, x) {
  return Bs_internalMapString.findOpt(d.data, x);
}

function getNull(d, x) {
  return Bs_internalMapString.findNull(d.data, x);
}

function getWithDefault(d, x, def) {
  return Bs_internalMapString.findWithDefault(d.data, x, def);
}

function getExn(d, x) {
  return Bs_internalMapString.findExn(d.data, x);
}

exports.empty = empty;
exports.isEmpty = isEmpty;
exports.singleton = singleton;
exports.mem = mem;
exports.cmp = cmp;
exports.eq = eq;
exports.iter = iter;
exports.fold = fold;
exports.forAll = forAll;
exports.exists = exists;
exports.length = length;
exports.toList = toList;
exports.toArray = toArray;
exports.ofArray = ofArray;
exports.keysToArray = keysToArray;
exports.valuesToArray = valuesToArray;
exports.minKeyOpt = minKeyOpt;
exports.minKeyNull = minKeyNull;
exports.maxKeyOpt = maxKeyOpt;
exports.maxKeyNull = maxKeyNull;
exports.minKeyValueOpt = minKeyValueOpt;
exports.minKeyValueNull = minKeyValueNull;
exports.maxKeyValueOpt = maxKeyValueOpt;
exports.maxKeyValueNull = maxKeyValueNull;
exports.getOpt = getOpt;
exports.getNull = getNull;
exports.getWithDefault = getWithDefault;
exports.getExn = getExn;
exports.remove = remove;
exports.addOnly = addOnly;
exports.add = add;
exports.map = map;
exports.mapi = mapi;
exports.checkInvariant = checkInvariant;
/* No side effect */
