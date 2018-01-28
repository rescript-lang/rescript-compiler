'use strict';

var Bs_internalAVLtree = require("./bs_internalAVLtree.js");

function set(t, newK, newD, cmp) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(newK, k);
    if (c) {
      var l = t.left;
      var r = t.right;
      var v = t.value;
      if (c < 0) {
        return Bs_internalAVLtree.bal(set(l, newK, newD, cmp), k, v, r);
      } else {
        return Bs_internalAVLtree.bal(l, k, v, set(r, newK, newD, cmp));
      }
    } else {
      return Bs_internalAVLtree.updateValue(t, newD);
    }
  } else {
    return Bs_internalAVLtree.singleton0(newK, newD);
  }
}

function update(t, newK, f, cmp) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(newK, k);
    if (c) {
      var l = t.left;
      var r = t.right;
      var v = t.value;
      if (c < 0) {
        var ll = update(l, newK, f, cmp);
        if (l === ll) {
          return t;
        } else {
          return Bs_internalAVLtree.bal(ll, k, v, r);
        }
      } else {
        var rr = update(r, newK, f, cmp);
        if (r === rr) {
          return t;
        } else {
          return Bs_internalAVLtree.bal(l, k, v, rr);
        }
      }
    } else {
      var match = f(/* Some */[t.value]);
      if (match) {
        return Bs_internalAVLtree.updateValue(t, match[0]);
      } else {
        var l$1 = t.left;
        var r$1 = t.right;
        if (l$1 !== null) {
          if (r$1 !== null) {
            var kr = [r$1.key];
            var vr = [r$1.value];
            var r$2 = Bs_internalAVLtree.removeMinAuxWithRef(r$1, kr, vr);
            return Bs_internalAVLtree.bal(l$1, kr[0], vr[0], r$2);
          } else {
            return l$1;
          }
        } else {
          return r$1;
        }
      }
    }
  } else {
    var match$1 = f(/* None */0);
    if (match$1) {
      return Bs_internalAVLtree.singleton0(newK, match$1[0]);
    } else {
      return t;
    }
  }
}

function removeAux0(n, x, cmp) {
  var l = n.left;
  var v = n.key;
  var r = n.right;
  var c = cmp(x, v);
  if (c) {
    if (c < 0) {
      if (l !== null) {
        var ll = removeAux0(l, x, cmp);
        if (ll === l) {
          return n;
        } else {
          return Bs_internalAVLtree.bal(ll, v, n.value, r);
        }
      } else {
        return n;
      }
    } else if (r !== null) {
      var rr = removeAux0(r, x, cmp);
      if (rr === r) {
        return n;
      } else {
        return Bs_internalAVLtree.bal(l, v, n.value, rr);
      }
    } else {
      return n;
    }
  } else if (l !== null) {
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

function remove(n, x, cmp) {
  if (n !== null) {
    return removeAux0(n, x, cmp);
  } else {
    return Bs_internalAVLtree.empty0;
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
  if (c) {
    if (c < 0) {
      if (l !== null) {
        var match = splitAuxPivot(l, x, pres, cmp);
        return /* tuple */[
                match[0],
                Bs_internalAVLtree.join(match[1], v, d, r)
              ];
      } else {
        return /* tuple */[
                null,
                n
              ];
      }
    } else if (r !== null) {
      var match$1 = splitAuxPivot(r, x, pres, cmp);
      return /* tuple */[
              Bs_internalAVLtree.join(l, v, d, match$1[0]),
              match$1[1]
            ];
    } else {
      return /* tuple */[
              n,
              null
            ];
    }
  } else {
    pres[0] = /* Some */[d];
    return /* tuple */[
            l,
            r
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
              null,
              null
            ],
            /* None */0
          ];
  }
}

function merge(s1, s2, f, cmp) {
  if (s1 !== null) {
    if (s2 !== null) {
      if (s1.h >= s2.h) {
        var l1 = s1.left;
        var v1 = s1.key;
        var d1 = s1.value;
        var r1 = s1.right;
        var d2 = [/* None */0];
        var match = splitAuxPivot(s2, v1, d2, cmp);
        var d2$1 = d2[0];
        var newLeft = merge(l1, match[0], f, cmp);
        var newD = f(v1, /* Some */[d1], d2$1);
        var newRight = merge(r1, match[1], f, cmp);
        return Bs_internalAVLtree.concatOrJoin(newLeft, v1, newD, newRight);
      } else {
        var l2 = s2.left;
        var v2 = s2.key;
        var d2$2 = s2.value;
        var r2 = s2.right;
        var d1$1 = [/* None */0];
        var match$1 = splitAuxPivot(s1, v2, d1$1, cmp);
        var d1$2 = d1$1[0];
        var newLeft$1 = merge(match$1[0], l2, f, cmp);
        var newD$1 = f(v2, d1$2, /* Some */[d2$2]);
        var newRight$1 = merge(match$1[1], r2, f, cmp);
        return Bs_internalAVLtree.concatOrJoin(newLeft$1, v2, newD$1, newRight$1);
      }
    } else {
      return Bs_internalAVLtree.filterMap0(s1, (function (k, v) {
                    return f(k, /* Some */[v], /* None */0);
                  }));
    }
  } else if (s2 !== null) {
    return Bs_internalAVLtree.filterMap0(s2, (function (k, v) {
                  return f(k, /* None */0, /* Some */[v]);
                }));
  } else {
    return null;
  }
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
    return Bs_internalAVLtree.empty0;
  }
}

var empty = Bs_internalAVLtree.empty0;

var isEmpty = Bs_internalAVLtree.isEmpty0;

var has = Bs_internalAVLtree.mem0;

var cmp = Bs_internalAVLtree.cmp0;

var eq = Bs_internalAVLtree.eq0;

var forEach = Bs_internalAVLtree.iter0;

var reduce = Bs_internalAVLtree.fold0;

var every = Bs_internalAVLtree.every0;

var some = Bs_internalAVLtree.some0;

var size = Bs_internalAVLtree.length0;

var toList = Bs_internalAVLtree.toList0;

var toArray = Bs_internalAVLtree.toArray0;

var ofArray = Bs_internalAVLtree.ofArray0;

var keysToArray = Bs_internalAVLtree.keysToArray0;

var valuesToArray = Bs_internalAVLtree.valuesToArray0;

var minKey = Bs_internalAVLtree.minKey0;

var minKeyUndefined = Bs_internalAVLtree.minKeyUndefined0;

var maxKey = Bs_internalAVLtree.maxKey0;

var maxKeyUndefined = Bs_internalAVLtree.maxKeyUndefined0;

var minimum = Bs_internalAVLtree.minimum0;

var minUndefined = Bs_internalAVLtree.minUndefined0;

var maximum = Bs_internalAVLtree.maximum0;

var maxUndefined = Bs_internalAVLtree.maxUndefined0;

var get = Bs_internalAVLtree.get0;

var getUndefined = Bs_internalAVLtree.getUndefined0;

var getWithDefault = Bs_internalAVLtree.getWithDefault0;

var getExn = Bs_internalAVLtree.getExn0;

var checkInvariantInternal = Bs_internalAVLtree.checkInvariantInternal;

var keepBy = Bs_internalAVLtree.filterShared0;

var partition = Bs_internalAVLtree.partitionShared0;

var map = Bs_internalAVLtree.map0;

var mapWithKey = Bs_internalAVLtree.mapi0;

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
exports.update = update;
exports.merge = merge;
exports.mergeMany = mergeMany;
exports.keepBy = keepBy;
exports.partition = partition;
exports.split = split;
exports.map = map;
exports.mapWithKey = mapWithKey;
/* No side effect */
