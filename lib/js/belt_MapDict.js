'use strict';

var Curry = require("./curry.js");
var Belt_internalAVLtree = require("./belt_internalAVLtree.js");

function set(t, newK, newD, cmp) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(newK, k);
    if (c === 0) {
      return Belt_internalAVLtree.updateValue(t, newD);
    } else {
      var l = t.left;
      var r = t.right;
      var v = t.value;
      if (c < 0) {
        return Belt_internalAVLtree.bal(set(l, newK, newD, cmp), k, v, r);
      } else {
        return Belt_internalAVLtree.bal(l, k, v, set(r, newK, newD, cmp));
      }
    }
  } else {
    return Belt_internalAVLtree.singleton(newK, newD);
  }
}

function updateU(t, newK, f, cmp) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(newK, k);
    if (c === 0) {
      var match = f(/* Some */[t.value]);
      if (match) {
        return Belt_internalAVLtree.updateValue(t, match[0]);
      } else {
        var l = t.left;
        var r = t.right;
        if (l !== null) {
          if (r !== null) {
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
      if (c < 0) {
        var ll = updateU(l$1, newK, f, cmp);
        if (l$1 === ll) {
          return t;
        } else {
          return Belt_internalAVLtree.bal(ll, k, v, r$2);
        }
      } else {
        var rr = updateU(r$2, newK, f, cmp);
        if (r$2 === rr) {
          return t;
        } else {
          return Belt_internalAVLtree.bal(l$1, k, v, rr);
        }
      }
    }
  } else {
    var match$1 = f(/* None */0);
    if (match$1) {
      return Belt_internalAVLtree.singleton(newK, match$1[0]);
    } else {
      return t;
    }
  }
}

function update(t, newK, f, cmp) {
  return updateU(t, newK, Curry.__1(f), cmp);
}

function removeAux0(n, x, cmp) {
  var l = n.left;
  var v = n.key;
  var r = n.right;
  var c = cmp(x, v);
  if (c === 0) {
    if (l !== null) {
      if (r !== null) {
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
  } else if (c < 0) {
    if (l !== null) {
      var ll = removeAux0(l, x, cmp);
      if (ll === l) {
        return n;
      } else {
        return Belt_internalAVLtree.bal(ll, v, n.value, r);
      }
    } else {
      return n;
    }
  } else if (r !== null) {
    var rr = removeAux0(r, x, cmp);
    if (rr === r) {
      return n;
    } else {
      return Belt_internalAVLtree.bal(l, v, n.value, rr);
    }
  } else {
    return n;
  }
}

function remove(n, x, cmp) {
  if (n !== null) {
    return removeAux0(n, x, cmp);
  } else {
    return Belt_internalAVLtree.empty;
  }
}

function mergeMany(h, arr, cmp) {
  var len = arr.length;
  var v = h;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var match = arr[i];
    v = set(v, match[0], match[1], cmp);
  }
  return v;
}

function splitAuxPivot(n, x, pres, cmp) {
  var l = n.left;
  var v = n.key;
  var d = n.value;
  var r = n.right;
  var c = cmp(x, v);
  if (c === 0) {
    pres[0] = /* Some */[d];
    return /* tuple */[
            l,
            r
          ];
  } else if (c < 0) {
    if (l !== null) {
      var match = splitAuxPivot(l, x, pres, cmp);
      return /* tuple */[
              match[0],
              Belt_internalAVLtree.join(match[1], v, d, r)
            ];
    } else {
      return /* tuple */[
              Belt_internalAVLtree.empty,
              n
            ];
    }
  } else if (r !== null) {
    var match$1 = splitAuxPivot(r, x, pres, cmp);
    return /* tuple */[
            Belt_internalAVLtree.join(l, v, d, match$1[0]),
            match$1[1]
          ];
  } else {
    return /* tuple */[
            n,
            Belt_internalAVLtree.empty
          ];
  }
}

function split(n, x, cmp) {
  if (n !== null) {
    var pres = [/* None */0];
    var v = splitAuxPivot(n, x, pres, cmp);
    return /* tuple */[
            v,
            pres[0]
          ];
  } else {
    return /* tuple */[
            /* tuple */[
              Belt_internalAVLtree.empty,
              Belt_internalAVLtree.empty
            ],
            /* None */0
          ];
  }
}

function mergeU(s1, s2, f, cmp) {
  if (s1 !== null) {
    if (s2 !== null) {
      if (s1.height >= s2.height) {
        var l1 = s1.left;
        var v1 = s1.key;
        var d1 = s1.value;
        var r1 = s1.right;
        var d2 = [/* None */0];
        var match = splitAuxPivot(s2, v1, d2, cmp);
        var d2$1 = d2[0];
        var newLeft = mergeU(l1, match[0], f, cmp);
        var newD = f(v1, /* Some */[d1], d2$1);
        var newRight = mergeU(r1, match[1], f, cmp);
        return Belt_internalAVLtree.concatOrJoin(newLeft, v1, newD, newRight);
      } else {
        var l2 = s2.left;
        var v2 = s2.key;
        var d2$2 = s2.value;
        var r2 = s2.right;
        var d1$1 = [/* None */0];
        var match$1 = splitAuxPivot(s1, v2, d1$1, cmp);
        var d1$2 = d1$1[0];
        var newLeft$1 = mergeU(match$1[0], l2, f, cmp);
        var newD$1 = f(v2, d1$2, /* Some */[d2$2]);
        var newRight$1 = mergeU(match$1[1], r2, f, cmp);
        return Belt_internalAVLtree.concatOrJoin(newLeft$1, v2, newD$1, newRight$1);
      }
    } else {
      return Belt_internalAVLtree.keepMapU(s1, (function (k, v) {
                    return f(k, /* Some */[v], /* None */0);
                  }));
    }
  } else if (s2 !== null) {
    return Belt_internalAVLtree.keepMapU(s2, (function (k, v) {
                  return f(k, /* None */0, /* Some */[v]);
                }));
  } else {
    return Belt_internalAVLtree.empty;
  }
}

function merge(s1, s2, f, cmp) {
  return mergeU(s1, s2, Curry.__3(f), cmp);
}

function removeMany(t, keys, cmp) {
  var len = keys.length;
  if (t !== null) {
    var _t = t;
    var xs = keys;
    var _i = 0;
    var len$1 = len;
    var cmp$1 = cmp;
    while(true) {
      var i = _i;
      var t$1 = _t;
      if (i < len$1) {
        var ele = xs[i];
        var u = removeAux0(t$1, ele, cmp$1);
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
    return Belt_internalAVLtree.empty;
  }
}

var empty = Belt_internalAVLtree.empty;

var isEmpty = Belt_internalAVLtree.isEmpty;

var has = Belt_internalAVLtree.has;

var cmpU = Belt_internalAVLtree.cmpU;

var cmp = Belt_internalAVLtree.cmp;

var eqU = Belt_internalAVLtree.eqU;

var eq = Belt_internalAVLtree.eq;

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

var fromArray = Belt_internalAVLtree.fromArray;

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

var get = Belt_internalAVLtree.get;

var getUndefined = Belt_internalAVLtree.getUndefined;

var getWithDefault = Belt_internalAVLtree.getWithDefault;

var getExn = Belt_internalAVLtree.getExn;

var checkInvariantInternal = Belt_internalAVLtree.checkInvariantInternal;

var keepU = Belt_internalAVLtree.keepSharedU;

var keep = Belt_internalAVLtree.keepShared;

var partitionU = Belt_internalAVLtree.partitionSharedU;

var partition = Belt_internalAVLtree.partitionShared;

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
