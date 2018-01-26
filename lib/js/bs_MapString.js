'use strict';

var Bs_internalAVLtree = require("./bs_internalAVLtree.js");
var Bs_internalMapString = require("./bs_internalMapString.js");

function set(t, newK, newD) {
  if (t !== null) {
    var k = t.key;
    if (newK === k) {
      return Bs_internalAVLtree.updateValue(t, newD);
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

function update(t, x, f) {
  if (t !== null) {
    var k = t.key;
    if (x === k) {
      var match = f(/* Some */[t.value]);
      if (match) {
        return Bs_internalAVLtree.updateValue(t, match[0]);
      } else {
        var l = t.left;
        var r = t.right;
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
      }
    } else {
      var l$1 = t.left;
      var r$2 = t.right;
      var v = t.value;
      if (x < k) {
        var ll = update(l$1, x, f);
        if (l$1 === ll) {
          return t;
        } else {
          return Bs_internalAVLtree.bal(ll, k, v, r$2);
        }
      } else {
        var rr = update(r$2, x, f);
        if (r$2 === rr) {
          return t;
        } else {
          return Bs_internalAVLtree.bal(l$1, k, v, rr);
        }
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

function mergeArray(h, arr) {
  var len = arr.length;
  var v = h;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var match = arr[i];
    v = set(v, match[0], match[1]);
  }
  return v;
}

var empty = Bs_internalAVLtree.empty0;

var isEmpty = Bs_internalAVLtree.isEmpty0;

var has = Bs_internalMapString.mem;

var cmp = Bs_internalMapString.cmp;

var eq = Bs_internalMapString.eq;

var forEach = Bs_internalAVLtree.iter0;

var reduce = Bs_internalAVLtree.fold0;

var every = Bs_internalAVLtree.every0;

var some = Bs_internalAVLtree.some0;

var size = Bs_internalAVLtree.length0;

var toList = Bs_internalAVLtree.toList0;

var toArray = Bs_internalAVLtree.toArray0;

var ofArray = Bs_internalMapString.ofArray;

var keysToArray = Bs_internalAVLtree.keysToArray0;

var valuesToArray = Bs_internalAVLtree.valuesToArray0;

var minKey = Bs_internalAVLtree.minKeyOpt0;

var minKeyNull = Bs_internalAVLtree.minKeyNull0;

var maxKey = Bs_internalAVLtree.maxKeyOpt0;

var maxKeyNull = Bs_internalAVLtree.maxKeyNull0;

var minimum = Bs_internalAVLtree.minKVOpt0;

var minNull = Bs_internalAVLtree.minKVNull0;

var maximum = Bs_internalAVLtree.maxKVOpt0;

var maxNull = Bs_internalAVLtree.maxKVNull0;

var get = Bs_internalMapString.findOpt;

var getNull = Bs_internalMapString.findNull;

var getWithDefault = Bs_internalMapString.findWithDefault;

var getExn = Bs_internalMapString.findExn;

var merge = Bs_internalMapString.merge;

var keepBy = Bs_internalAVLtree.filterShared0;

var partition = Bs_internalAVLtree.partitionShared0;

var split = Bs_internalMapString.split;

var map = Bs_internalAVLtree.map0;

var mapWithKey = Bs_internalAVLtree.mapi0;

var checkInvariant = Bs_internalAVLtree.checkInvariant;

exports.empty = empty;
exports.isEmpty = isEmpty;
exports.has = has;
exports.cmp = cmp;
exports.eq = eq;
exports.forEach = forEach;
exports.reduce = reduce;
exports.every = every;
exports.some = some;
exports.size = size;
exports.toList = toList;
exports.toArray = toArray;
exports.ofArray = ofArray;
exports.keysToArray = keysToArray;
exports.valuesToArray = valuesToArray;
exports.minKey = minKey;
exports.minKeyNull = minKeyNull;
exports.maxKey = maxKey;
exports.maxKeyNull = maxKeyNull;
exports.minimum = minimum;
exports.minNull = minNull;
exports.maximum = maximum;
exports.maxNull = maxNull;
exports.get = get;
exports.getNull = getNull;
exports.getWithDefault = getWithDefault;
exports.getExn = getExn;
exports.remove = remove;
exports.removeArray = removeArray;
exports.set = set;
exports.update = update;
exports.mergeArray = mergeArray;
exports.merge = merge;
exports.keepBy = keepBy;
exports.partition = partition;
exports.split = split;
exports.map = map;
exports.mapWithKey = mapWithKey;
exports.checkInvariant = checkInvariant;
/* No side effect */
