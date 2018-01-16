'use strict';

var Bs_internalMapInt = require("./bs_internalMapInt.js");
var Bs_internalAVLtree = require("./bs_internalAVLtree.js");

function update(t, newK, newD) {
  if (t !== null) {
    var k = t.key;
    if (newK === k) {
      return Bs_internalAVLtree.updateKV(t, newK, newD);
    } else {
      var v = t.value;
      if (newK < k) {
        return Bs_internalAVLtree.bal(update(t.left, newK, newD), k, v, t.right);
      } else {
        return Bs_internalAVLtree.bal(t.left, k, v, update(t.right, newK, newD));
      }
    }
  } else {
    return Bs_internalAVLtree.singleton0(newK, newD);
  }
}

function updateWithOpt(t, x, f) {
  if (t !== null) {
    var k = t.key;
    if (x === k) {
      var match = f(/* Some */[k]);
      if (match) {
        return Bs_internalAVLtree.updateKV(t, x, match[0]);
      } else {
        return t;
      }
    } else {
      var v = t.value;
      if (x < k) {
        return Bs_internalAVLtree.bal(updateWithOpt(t.left, x, f), k, v, t.right);
      } else {
        return Bs_internalAVLtree.bal(t.left, k, v, updateWithOpt(t.right, x, f));
      }
    }
  } else {
    var match$1 = f(/* None */0);
    if (match$1) {
      return Bs_internalAVLtree.singleton0(x, match$1[0]);
    } else {
      return t;
    }
  }
}

function remove(n, x) {
  if (n !== null) {
    var l = n.left;
    var v = n.key;
    var r = n.right;
    if (x === v) {
      if (l !== null) {
        if (r !== null) {
          var kr = [r.key];
          var vr = [r.value];
          var r$1 = Bs_internalAVLtree.removeMinAuxWithRef(r, kr, vr);
          return Bs_internalAVLtree.bal(l, kr[0], vr[0], r$1);
        } else {
          return l;
        }
      } else {
        return r;
      }
    } else if (x < v) {
      return Bs_internalAVLtree.bal(remove(l, x), v, n.value, r);
    } else {
      return Bs_internalAVLtree.bal(l, v, n.value, remove(r, x));
    }
  } else {
    return n;
  }
}

var empty = Bs_internalAVLtree.empty0;

var ofArray = Bs_internalMapInt.ofArray;

var isEmpty = Bs_internalAVLtree.isEmpty0;

var mem = Bs_internalMapInt.mem;

var singleton = Bs_internalAVLtree.singleton0;

var merge = Bs_internalMapInt.merge;

var cmp = Bs_internalMapInt.cmp;

var eq = Bs_internalMapInt.eq;

var iter = Bs_internalAVLtree.iter0;

var fold = Bs_internalAVLtree.fold0;

var forAll = Bs_internalAVLtree.forAll0;

var exists = Bs_internalAVLtree.exists0;

var filter = Bs_internalAVLtree.filterShared0;

var partition = Bs_internalAVLtree.partitionShared0;

var length = Bs_internalAVLtree.length0;

var toList = Bs_internalAVLtree.toList0;

var minKVOpt = Bs_internalAVLtree.minKVOpt0;

var minKVNull = Bs_internalAVLtree.minKVNull0;

var maxKVOpt = Bs_internalAVLtree.maxKVOpt0;

var maxKVNull = Bs_internalAVLtree.maxKVNull0;

var split = Bs_internalMapInt.split;

var findOpt = Bs_internalMapInt.findOpt;

var findNull = Bs_internalMapInt.findNull;

var findWithDefault = Bs_internalMapInt.findWithDefault;

var findExn = Bs_internalMapInt.findExn;

var map = Bs_internalAVLtree.map0;

var mapi = Bs_internalAVLtree.mapi0;

var checkInvariant = Bs_internalAVLtree.checkInvariant;

exports.empty = empty;
exports.ofArray = ofArray;
exports.isEmpty = isEmpty;
exports.mem = mem;
exports.update = update;
exports.updateWithOpt = updateWithOpt;
exports.singleton = singleton;
exports.remove = remove;
exports.merge = merge;
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
exports.minKVOpt = minKVOpt;
exports.minKVNull = minKVNull;
exports.maxKVOpt = maxKVOpt;
exports.maxKVNull = maxKVNull;
exports.split = split;
exports.findOpt = findOpt;
exports.findNull = findNull;
exports.findWithDefault = findWithDefault;
exports.findExn = findExn;
exports.map = map;
exports.mapi = mapi;
exports.checkInvariant = checkInvariant;
/* No side effect */
