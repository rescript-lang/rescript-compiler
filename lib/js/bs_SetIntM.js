'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");
var Bs_internalSetInt = require("./bs_internalSetInt.js");

function empty() {
  return {
          root: Bs_internalAVLset.empty0
        };
}

function isEmpty(d) {
  return Bs_internalAVLset.isEmpty0(d.root);
}

function singleton(x) {
  return {
          root: Bs_internalAVLset.singleton0(x)
        };
}

function minOpt(d) {
  return Bs_internalAVLset.minOpt0(d.root);
}

function minNull(d) {
  return Bs_internalAVLset.minNull0(d.root);
}

function maxOpt(d) {
  return Bs_internalAVLset.maxOpt0(d.root);
}

function maxNull(d) {
  return Bs_internalAVLset.maxNull0(d.root);
}

function iter(d, f) {
  return Bs_internalAVLset.iter0(d.root, f);
}

function fold(d, acc, cb) {
  return Bs_internalAVLset.fold0(d.root, acc, cb);
}

function forAll(d, p) {
  return Bs_internalAVLset.forAll0(d.root, p);
}

function exists(d, p) {
  return Bs_internalAVLset.exists0(d.root, p);
}

function filter(d, p) {
  return {
          root: Bs_internalAVLset.filter0(d.root, p)
        };
}

function partition(d, p) {
  var match = Bs_internalAVLset.partition0(d.root, p);
  return /* tuple */[
          {
            root: match[0]
          },
          {
            root: match[1]
          }
        ];
}

function length(d) {
  return Bs_internalAVLset.length0(d.root);
}

function toList(d) {
  return Bs_internalAVLset.toList0(d.root);
}

function toArray(d) {
  return Bs_internalAVLset.toArray0(d.root);
}

function checkInvariant(d) {
  return Bs_internalAVLset.checkInvariant(d.root);
}

function add(d, k) {
  var old_root = d.root;
  var v = Bs_internalSetInt.addMutate(old_root, k);
  if (v !== old_root) {
    d.root = v;
  }
  return d;
}

function addOnly(d, k) {
  var old_root = d.root;
  var v = Bs_internalSetInt.addMutate(old_root, k);
  if (v !== old_root) {
    d.root = v;
    return /* () */0;
  } else {
    return 0;
  }
}

function addArrayOnly(d, arr) {
  var old_root = d.root;
  var v = Bs_internalSetInt.addArrayMutate(old_root, arr);
  if (v !== old_root) {
    d.root = v;
    return /* () */0;
  } else {
    return 0;
  }
}

function addArray(d, arr) {
  var old_root = d.root;
  var v = Bs_internalSetInt.addArrayMutate(old_root, arr);
  if (v !== old_root) {
    d.root = v;
  }
  return d;
}

function remove(d, v) {
  var old_root = d.root;
  var v$1 = Bs_internalSetInt.removeMutate(old_root, v);
  if (v$1 !== old_root) {
    d.root = v$1;
  }
  return d;
}

function removeOnly(d, v) {
  var old_root = d.root;
  var v$1 = Bs_internalSetInt.removeMutate(old_root, v);
  if (v$1 !== old_root) {
    d.root = v$1;
    return /* () */0;
  } else {
    return 0;
  }
}

function ofArray(xs) {
  return {
          root: Bs_internalSetInt.ofArray(xs)
        };
}

function cmp(d0, d1) {
  return Bs_internalSetInt.cmp(d0.root, d1.root);
}

function diff(d0, d1) {
  return {
          root: Bs_internalSetInt.diff(d0.root, d1.root)
        };
}

function eq(d0, d1) {
  return Bs_internalSetInt.eq(d0.root, d1.root);
}

function findOpt(d, x) {
  return Bs_internalSetInt.findOpt(d.root, x);
}

function split(d, p) {
  var match = Bs_internalSetInt.split(d.root, p);
  return /* tuple */[
          {
            root: match[0]
          },
          match[1],
          {
            root: match[2]
          }
        ];
}

function subset(a, b) {
  return Bs_internalSetInt.subset(a.root, b.root);
}

function inter(a, b) {
  return {
          root: Bs_internalSetInt.inter(a.root, b.root)
        };
}

function union(a, b) {
  return {
          root: Bs_internalSetInt.union(a.root, b.root)
        };
}

function mem(d, x) {
  return Bs_internalSetInt.mem(d.root, x);
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
