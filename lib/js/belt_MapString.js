'use strict';

var Curry = require("./curry.js");
var Belt_internalAVLtree = require("./belt_internalAVLtree.js");
var Belt_internalMapString = require("./belt_internalMapString.js");

function set(t, newK, newD) {
  var match = t === null ? /* None */0 : [t];
  if (match !== /* None */0) {
    var k = t.key;
    if (newK === k) {
      return Belt_internalAVLtree.updateValue(t, newD);
    } else {
      var v = t.value;
      if (newK < k) {
        return Belt_internalAVLtree.bal(set(t.left, newK, newD), k, v, t.right);
      } else {
        return Belt_internalAVLtree.bal(t.left, k, v, set(t.right, newK, newD));
      }
    }
  } else {
    return Belt_internalAVLtree.singleton(newK, newD);
  }
}

function updateU(t, x, f) {
  var match = t === null ? /* None */0 : [t];
  if (match !== /* None */0) {
    var k = t.key;
    if (x === k) {
      var match$1 = f(/* Some */[t.value]);
      if (match$1 !== /* None */0) {
        return Belt_internalAVLtree.updateValue(t, match$1[/* None */0]);
      } else {
        var l = t.left;
        var r = t.right;
        var match$2 = l === null ? /* None */0 : [l];
        var match$3 = r === null ? /* None */0 : [r];
        if (match$2 !== /* None */0) {
          if (match$3 !== /* None */0) {
            var kr = [r.key];
            var vr = [r.value];
            var r$1 = Belt_internalAVLtree.removeMinAuxWithRef(r, kr, vr);
            return Belt_internalAVLtree.bal(l, kr[0], vr[0], r$1);
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
          return Belt_internalAVLtree.bal(ll, k, v, r$2);
        }
      } else {
        var rr = updateU(r$2, x, f);
        if (r$2 === rr) {
          return t;
        } else {
          return Belt_internalAVLtree.bal(l$1, k, v, rr);
        }
      }
    }
  } else {
    var match$4 = f(/* None */0);
    if (match$4 !== /* None */0) {
      return Belt_internalAVLtree.singleton(x, match$4[/* None */0]);
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
    var match = l === null ? /* None */0 : [l];
    var match$1 = r === null ? /* None */0 : [r];
    if (match !== /* None */0) {
      if (match$1 !== /* None */0) {
        var kr = [r.key];
        var vr = [r.value];
        var r$1 = Belt_internalAVLtree.removeMinAuxWithRef(r, kr, vr);
        return Belt_internalAVLtree.bal(l, kr[0], vr[0], r$1);
      } else {
        return l;
      }
    } else {
      return r;
    }
  } else if (x < v) {
    var match$2 = l === null ? /* None */0 : [l];
    if (match$2 !== /* None */0) {
      var ll = removeAux(l, x);
      if (ll === l) {
        return n;
      } else {
        return Belt_internalAVLtree.bal(ll, v, n.value, r);
      }
    } else {
      return n;
    }
  } else {
    var match$3 = r === null ? /* None */0 : [r];
    if (match$3 !== /* None */0) {
      var rr = removeAux(r, x);
      return Belt_internalAVLtree.bal(l, v, n.value, rr);
    } else {
      return n;
    }
  }
}

function remove(n, x) {
  var match = n === null ? /* None */0 : [n];
  if (match !== /* None */0) {
    return removeAux(n, x);
  } else {
    return Belt_internalAVLtree.empty;
  }
}

function removeMany(t, keys) {
  var len = keys.length;
  var match = t === null ? /* None */0 : [t];
  if (match !== /* None */0) {
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
        var match$1 = u === null ? /* None */0 : [u];
        if (match$1 !== /* None */0) {
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
    return Belt_internalAVLtree.empty;
  }
}

function mergeMany(h, arr) {
  var len = arr.length;
  var v = h;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var match = arr[i];
    v = set(v, match[0], match[1]);
  }
  return v;
}

var empty = Belt_internalAVLtree.empty;

var isEmpty = Belt_internalAVLtree.isEmpty;

var has = Belt_internalMapString.has;

var cmpU = Belt_internalMapString.cmpU;

var cmp = Belt_internalMapString.cmp;

var eqU = Belt_internalMapString.eqU;

var eq = Belt_internalMapString.eq;

var forEachU = Belt_internalAVLtree.forEachU;

var forEach = Belt_internalAVLtree.forEach;

var reduceU = Belt_internalAVLtree.reduceU;

var reduce = Belt_internalAVLtree.reduce;

var everyU = Belt_internalAVLtree.everyU;

var every = Belt_internalAVLtree.every;

var someU = Belt_internalAVLtree.someU;

var some = Belt_internalAVLtree.some;

var size = Belt_internalAVLtree.size;

var toList = Belt_internalAVLtree.toList;

var toArray = Belt_internalAVLtree.toArray;

var fromArray = Belt_internalMapString.fromArray;

var keysToArray = Belt_internalAVLtree.keysToArray;

var valuesToArray = Belt_internalAVLtree.valuesToArray;

var minKey = Belt_internalAVLtree.minKey;

var minKeyUndefined = Belt_internalAVLtree.minKeyUndefined;

var maxKey = Belt_internalAVLtree.maxKey;

var maxKeyUndefined = Belt_internalAVLtree.maxKeyUndefined;

var minimum = Belt_internalAVLtree.minimum;

var minUndefined = Belt_internalAVLtree.minUndefined;

var maximum = Belt_internalAVLtree.maximum;

var maxUndefined = Belt_internalAVLtree.maxUndefined;

var get = Belt_internalMapString.get;

var getUndefined = Belt_internalMapString.getUndefined;

var getWithDefault = Belt_internalMapString.getWithDefault;

var getExn = Belt_internalMapString.getExn;

var checkInvariantInternal = Belt_internalAVLtree.checkInvariantInternal;

var mergeU = Belt_internalMapString.mergeU;

var merge = Belt_internalMapString.merge;

var keepU = Belt_internalAVLtree.keepSharedU;

var keep = Belt_internalAVLtree.keepShared;

var partitionU = Belt_internalAVLtree.partitionSharedU;

var partition = Belt_internalAVLtree.partitionShared;

var split = Belt_internalMapString.split;

var mapU = Belt_internalAVLtree.mapU;

var map = Belt_internalAVLtree.map;

var mapWithKeyU = Belt_internalAVLtree.mapWithKeyU;

var mapWithKey = Belt_internalAVLtree.mapWithKey;

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
exports.fromArray = fromArray;
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
exports.checkInvariantInternal = checkInvariantInternal;
exports.remove = remove;
exports.removeMany = removeMany;
exports.set = set;
exports.updateU = updateU;
exports.update = update;
exports.mergeU = mergeU;
exports.merge = merge;
exports.mergeMany = mergeMany;
exports.keepU = keepU;
exports.keep = keep;
exports.partitionU = partitionU;
exports.partition = partition;
exports.split = split;
exports.mapU = mapU;
exports.map = map;
exports.mapWithKeyU = mapWithKeyU;
exports.mapWithKey = mapWithKey;
/* No side effect */
