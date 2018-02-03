'use strict';

var Curry = require("./curry.js");
var Bs_internalMapInt = require("./bs_internalMapInt.js");
var Bs_internalAVLtree = require("./bs_internalAVLtree.js");

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
    return Bs_internalAVLtree.singleton(newK, newD);
  }
}

function updateU(t, x, f) {
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
        var ll = updateU(l$1, x, f);
        if (l$1 === ll) {
          return t;
        } else {
          return Bs_internalAVLtree.bal(ll, k, v, r$2);
        }
      } else {
        var rr = updateU(r$2, x, f);
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
      return Bs_internalAVLtree.singleton(x, match$1[0]);
    } else {
      return t;
    }
  }
}

function update(t, x, f) {
  return updateU(t, x, Curry.__1(f));
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
    return Bs_internalAVLtree.empty;
  }
}

function removeMany(t, keys) {
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
    return Bs_internalAVLtree.empty;
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

var empty = Bs_internalAVLtree.empty;

var isEmpty = Bs_internalAVLtree.isEmpty;

var has = Bs_internalMapInt.has;

var cmpU = Bs_internalMapInt.cmpU;

var cmp = Bs_internalMapInt.cmp;

var eqU = Bs_internalMapInt.eqU;

var eq = Bs_internalMapInt.eq;

var forEachU = Bs_internalAVLtree.forEachU;

var forEach = Bs_internalAVLtree.forEach;

var reduceU = Bs_internalAVLtree.reduceU;

var reduce = Bs_internalAVLtree.reduce;

var everyU = Bs_internalAVLtree.everyU;

var every = Bs_internalAVLtree.every;

var someU = Bs_internalAVLtree.someU;

var some = Bs_internalAVLtree.some;

var size = Bs_internalAVLtree.size;

var toList = Bs_internalAVLtree.toList;

var toArray = Bs_internalAVLtree.toArray;

var ofArray = Bs_internalMapInt.ofArray;

var keysToArray = Bs_internalAVLtree.keysToArray;

var valuesToArray = Bs_internalAVLtree.valuesToArray;

var minKey = Bs_internalAVLtree.minKey;

var minKeyUndefined = Bs_internalAVLtree.minKeyUndefined;

var maxKey = Bs_internalAVLtree.maxKey;

var maxKeyUndefined = Bs_internalAVLtree.maxKeyUndefined;

var minimum = Bs_internalAVLtree.minimum;

var minUndefined = Bs_internalAVLtree.minUndefined;

var maximum = Bs_internalAVLtree.maximum;

var maxUndefined = Bs_internalAVLtree.maxUndefined;

var get = Bs_internalMapInt.get;

var getUndefined = Bs_internalMapInt.getUndefined;

var getWithDefault = Bs_internalMapInt.getWithDefault;

var getExn = Bs_internalMapInt.getExn;

var mergeU = Bs_internalMapInt.mergeU;

var merge = Bs_internalMapInt.merge;

var keepU = Bs_internalAVLtree.keepSharedU;

var keep = Bs_internalAVLtree.keepShared;

var partitionU = Bs_internalAVLtree.partitionSharedU;

var partition = Bs_internalAVLtree.partitionShared;

var split = Bs_internalMapInt.split;

var mapU = Bs_internalAVLtree.mapU;

var map = Bs_internalAVLtree.map;

var mapWithKeyU = Bs_internalAVLtree.mapWithKeyU;

var mapWithKey = Bs_internalAVLtree.mapWithKey;

var checkInvariantInternal = Bs_internalAVLtree.checkInvariantInternal;

exports.empty = empty;
exports.isEmpty = isEmpty;
exports.has = has;
exports.cmpU = cmpU;
exports.cmp = cmp;
exports.eqU = eqU;
exports.eq = eq;
exports.forEachU = forEachU;
exports.forEach = forEach;
exports.reduceU = reduceU;
exports.reduce = reduce;
exports.everyU = everyU;
exports.every = every;
exports.someU = someU;
exports.some = some;
exports.size = size;
exports.toList = toList;
exports.toArray = toArray;
exports.ofArray = ofArray;
exports.keysToArray = keysToArray;
exports.valuesToArray = valuesToArray;
exports.minKey = minKey;
exports.minKeyUndefined = minKeyUndefined;
exports.maxKey = maxKey;
exports.maxKeyUndefined = maxKeyUndefined;
exports.minimum = minimum;
exports.minUndefined = minUndefined;
exports.maximum = maximum;
exports.maxUndefined = maxUndefined;
exports.get = get;
exports.getUndefined = getUndefined;
exports.getWithDefault = getWithDefault;
exports.getExn = getExn;
exports.remove = remove;
exports.removeMany = removeMany;
exports.set = set;
exports.updateU = updateU;
exports.update = update;
exports.mergeArray = mergeArray;
exports.mergeU = mergeU;
exports.merge = merge;
exports.keepU = keepU;
exports.keep = keep;
exports.partitionU = partitionU;
exports.partition = partition;
exports.split = split;
exports.mapU = mapU;
exports.map = map;
exports.mapWithKeyU = mapWithKeyU;
exports.mapWithKey = mapWithKey;
exports.checkInvariantInternal = checkInvariantInternal;
/* No side effect */
