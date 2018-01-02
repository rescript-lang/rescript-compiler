'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");
var Bs_internalSetString = require("./bs_internalSetString.js");

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
  var v = Bs_internalSetString.addMutate(old_root, k);
  if (v !== old_root) {
    d.root = v;
  }
  return d;
}

function addOnly(d, k) {
  var old_root = d.root;
  var v = Bs_internalSetString.addMutate(old_root, k);
  if (v !== old_root) {
    d.root = v;
    return /* () */0;
  } else {
    return 0;
  }
}

function addArrayOnly(d, arr) {
  var old_root = d.root;
  var v = Bs_internalSetString.addArrayMutate(old_root, arr);
  if (v !== old_root) {
    d.root = v;
    return /* () */0;
  } else {
    return 0;
  }
}

function addArray(d, arr) {
  var old_root = d.root;
  var v = Bs_internalSetString.addArrayMutate(old_root, arr);
  if (v !== old_root) {
    d.root = v;
  }
  return d;
}

function remove(d, v) {
  var old_root = d.root;
  var v$1 = Bs_internalSetString.removeMutate(old_root, v);
  if (v$1 !== old_root) {
    d.root = v$1;
  }
  return d;
}

function removeOnly(d, v) {
  var old_root = d.root;
  var v$1 = Bs_internalSetString.removeMutate(old_root, v);
  if (v$1 !== old_root) {
    d.root = v$1;
    return /* () */0;
  } else {
    return 0;
  }
}

function ofArray(xs) {
  return {
          root: Bs_internalSetString.ofArray(xs)
        };
}

function mem(d, x) {
  return Bs_internalSetString.mem(d.root, x);
}

exports.empty = empty;
exports.isEmpty = isEmpty;
exports.mem = mem;
exports.add = add;
exports.addOnly = addOnly;
exports.remove = remove;
exports.removeOnly = removeOnly;
exports.addArray = addArray;
exports.addArrayOnly = addArrayOnly;
exports.ofArray = ofArray;
exports.toList = toList;
exports.toArray = toArray;
exports.minOpt = minOpt;
exports.minNull = minNull;
exports.maxOpt = maxOpt;
exports.maxNull = maxNull;
exports.singleton = singleton;
exports.checkInvariant = checkInvariant;
exports.length = length;
/* No side effect */
