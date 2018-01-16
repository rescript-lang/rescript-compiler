'use strict';

var Bs_internalAVLtree = require("./bs_internalAVLtree.js");
var Bs_internalMapString = require("./bs_internalMapString.js");

function add(t, x, data) {
  if (t !== null) {
    var k = t.key;
    if (x === k) {
      return Bs_internalAVLtree.updateKV(t, x, data);
    } else {
      var v = t.value;
      if (x < k) {
        return Bs_internalAVLtree.bal(add(t.left, x, data), k, v, t.right);
      } else {
        return Bs_internalAVLtree.bal(t.left, k, v, add(t.right, x, data));
      }
    }
  } else {
    return Bs_internalAVLtree.singleton0(x, data);
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

var ofArray = Bs_internalMapString.ofArray;

var isEmpty = Bs_internalAVLtree.isEmpty0;

var mem = Bs_internalMapString.mem;

var singleton = Bs_internalAVLtree.singleton0;

var merge = Bs_internalMapString.merge;

var cmp = Bs_internalMapString.cmp;

var eq = Bs_internalMapString.eq;

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

var split = Bs_internalMapString.split;

var findOpt = Bs_internalMapString.findOpt;

var findNull = Bs_internalMapString.findNull;

var findWithDefault = Bs_internalMapString.findWithDefault;

var findExn = Bs_internalMapString.findExn;

var map = Bs_internalAVLtree.map0;

var mapi = Bs_internalAVLtree.mapi0;

var checkInvariant = Bs_internalAVLtree.checkInvariant;

exports.empty = empty;
exports.ofArray = ofArray;
exports.isEmpty = isEmpty;
exports.mem = mem;
exports.add = add;
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
