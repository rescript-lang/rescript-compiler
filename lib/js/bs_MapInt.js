'use strict';

var Bs_internalMapInt = require("./bs_internalMapInt.js");
var Bs_internalAVLtree = require("./bs_internalAVLtree.js");

function set(t, newK, newD) {
  if (t !== null) {
    var k = t.key;
    if (newK === k) {
      return Bs_internalAVLtree.updateKV(t, newK, newD);
    } else {
      var v = t.value;
      if (newK < k) {
        return Bs_internalAVLtree.bal(set(t.left, newK, newD), k, v, t.right);
      } else {
        return Bs_internalAVLtree.bal(t.left, k, v, set(t.right, newK, newD));
      }
    }
  } else {
    return Bs_internalAVLtree.singleton0(newK, newD);
  }
}

function setWithOpt(t, x, f) {
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
        return Bs_internalAVLtree.bal(setWithOpt(t.left, x, f), k, v, t.right);
      } else {
        return Bs_internalAVLtree.bal(t.left, k, v, setWithOpt(t.right, x, f));
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

function removeAux(n, x) {
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
    if (l !== null) {
      var ll = removeAux(l, x);
      if (ll === l) {
        return n;
      } else {
        return Bs_internalAVLtree.bal(ll, v, n.value, r);
      }
    } else {
      return n;
    }
  } else if (r !== null) {
    var rr = removeAux(r, x);
    return Bs_internalAVLtree.bal(l, v, n.value, rr);
  } else {
    return n;
  }
}

function remove(n, x) {
  if (n !== null) {
    return removeAux(n, x);
  } else {
    return Bs_internalAVLtree.empty0;
  }
}

function removeArray(t, keys) {
  var len = keys.length;
  if (t !== null) {
    var _t = t;
    var xs = keys;
    var _i = 0;
    var len$1 = len;
    while(true) {
      var i = _i;
      var t$1 = _t;
      if (i < len$1) {
        var ele = xs[i];
        var u = removeAux(t$1, ele);
        if (u !== null) {
          _i = i + 1 | 0;
          _t = u;
          continue ;
          
        } else {
          return u;
        }
      } else {
        return t$1;
      }
    };
  } else {
    return Bs_internalAVLtree.empty0;
  }
}

var empty = Bs_internalAVLtree.empty0;

var isEmpty = Bs_internalAVLtree.isEmpty0;

var singleton = Bs_internalAVLtree.singleton0;

var mem = Bs_internalMapInt.mem;

var cmp = Bs_internalMapInt.cmp;

var eq = Bs_internalMapInt.eq;

var iter = Bs_internalAVLtree.iter0;

var fold = Bs_internalAVLtree.fold0;

var forAll = Bs_internalAVLtree.forAll0;

var exists = Bs_internalAVLtree.exists0;

var length = Bs_internalAVLtree.length0;

var toList = Bs_internalAVLtree.toList0;

var toArray = Bs_internalAVLtree.toArray0;

var ofArray = Bs_internalMapInt.ofArray;

var keysToArray = Bs_internalAVLtree.keysToArray0;

var valuesToArray = Bs_internalAVLtree.valuesToArray0;

var minKeyOpt = Bs_internalAVLtree.minKeyOpt0;

var minKeyNull = Bs_internalAVLtree.minKeyNull0;

var maxKeyOpt = Bs_internalAVLtree.maxKeyOpt0;

var maxKeyNull = Bs_internalAVLtree.maxKeyNull0;

var minKeyValueOpt = Bs_internalAVLtree.minKVOpt0;

var minKeyValueNull = Bs_internalAVLtree.minKVNull0;

var maxKeyValueOpt = Bs_internalAVLtree.maxKVOpt0;

var maxKeyValueNull = Bs_internalAVLtree.maxKVNull0;

var get = Bs_internalMapInt.findOpt;

var getNull = Bs_internalMapInt.findNull;

var getWithDefault = Bs_internalMapInt.findWithDefault;

var getExn = Bs_internalMapInt.findExn;

var merge = Bs_internalMapInt.merge;

var filter = Bs_internalAVLtree.filterShared0;

var partition = Bs_internalAVLtree.partitionShared0;

var split = Bs_internalMapInt.split;

var map = Bs_internalAVLtree.map0;

var mapi = Bs_internalAVLtree.mapi0;

var checkInvariant = Bs_internalAVLtree.checkInvariant;

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
exports.get = get;
exports.getNull = getNull;
exports.getWithDefault = getWithDefault;
exports.getExn = getExn;
exports.remove = remove;
exports.removeArray = removeArray;
exports.set = set;
exports.setWithOpt = setWithOpt;
exports.merge = merge;
exports.filter = filter;
exports.partition = partition;
exports.split = split;
exports.map = map;
exports.mapi = mapi;
exports.checkInvariant = checkInvariant;
/* No side effect */
