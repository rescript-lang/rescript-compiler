'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");
var Bs_internalSetString = require("./bs_internalSetString.js");

function removeMutateAux(nt, x) {
  var k = nt.key;
  if (x === k) {
    var l = nt.left;
    var r = nt.right;
    if (l !== null) {
      if (r !== null) {
        nt.right = Bs_internalAVLset.removeMinAuxWithRootMutate(nt, r);
        return Bs_internalAVLset.balMutate(nt);
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
      return Bs_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  } else {
    var match$1 = nt.right;
    if (match$1 !== null) {
      nt.right = removeMutateAux(match$1, x);
      return Bs_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  }
}

function addArrayMutate(t, xs) {
  var v = t;
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    v = Bs_internalSetString.addMutate(v, xs[i]);
  }
  return v;
}

function removeMutate(nt, x) {
  if (nt !== null) {
    return removeMutateAux(nt, x);
  } else {
    return nt;
  }
}

function empty() {
  return {
          data: Bs_internalAVLset.empty0
        };
}

function isEmpty(d) {
  return Bs_internalAVLset.isEmpty0(d.data);
}

function singleton(x) {
  return {
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
  return {
          data: Bs_internalAVLset.filter0(d.data, p)
        };
}

function partition(d, p) {
  var match = Bs_internalAVLset.partition0(d.data, p);
  return /* tuple */[
          {
            data: match[0]
          },
          {
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

function ofSortedArrayUnsafe(xs) {
  return {
          data: Bs_internalAVLset.ofSortedArrayUnsafe0(xs)
        };
}

function checkInvariant(d) {
  return Bs_internalAVLset.checkInvariant(d.data);
}

function addOnly(d, k) {
  var old_data = d.data;
  var v = Bs_internalSetString.addMutate(old_data, k);
  if (v !== old_data) {
    d.data = v;
    return /* () */0;
  } else {
    return 0;
  }
}

function add(d, k) {
  addOnly(d, k);
  return d;
}

function addArrayOnly(d, arr) {
  var old_data = d.data;
  var v = addArrayMutate(old_data, arr);
  if (v !== old_data) {
    d.data = v;
    return /* () */0;
  } else {
    return 0;
  }
}

function addArray(d, arr) {
  var old_data = d.data;
  var v = addArrayMutate(old_data, arr);
  if (v !== old_data) {
    d.data = v;
  }
  return d;
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
          data: Bs_internalSetString.ofArray(xs)
        };
}

function cmp(d0, d1) {
  return Bs_internalSetString.cmp(d0.data, d1.data);
}

function diff(d0, d1) {
  return {
          data: Bs_internalSetString.diff(d0.data, d1.data)
        };
}

function eq(d0, d1) {
  return Bs_internalSetString.eq(d0.data, d1.data);
}

function findOpt(d, x) {
  return Bs_internalSetString.findOpt(d.data, x);
}

function split(d, p) {
  var match = Bs_internalSetString.split(d.data, p);
  return /* tuple */[
          {
            data: match[0]
          },
          match[1],
          {
            data: match[2]
          }
        ];
}

function subset(a, b) {
  return Bs_internalSetString.subset(a.data, b.data);
}

function inter(a, b) {
  return {
          data: Bs_internalSetString.inter(a.data, b.data)
        };
}

function union(a, b) {
  return {
          data: Bs_internalSetString.union(a.data, b.data)
        };
}

function mem(d, x) {
  return Bs_internalSetString.mem(d.data, x);
}

exports.empty = empty;
exports.isEmpty = isEmpty;
exports.mem = mem;
exports.add = add;
exports.addOnly = addOnly;
exports.singleton = singleton;
exports.remove = remove;
exports.removeOnly = removeOnly;
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
exports.addArray = addArray;
exports.addArrayOnly = addArrayOnly;
exports.checkInvariant = checkInvariant;
/* No side effect */
