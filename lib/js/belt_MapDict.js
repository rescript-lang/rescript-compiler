'use strict';

var Curry = require("./curry.js");
var Caml_option = require("./caml_option.js");
var Belt_internalAVLtree = require("./belt_internalAVLtree.js");

function set(t, newK, newD, cmp) {
  if (t === undefined) {
    return Belt_internalAVLtree.singleton(newK, newD);
  }
  var k = t.k;
  var c = cmp(newK, k);
  if (c === 0) {
    return Belt_internalAVLtree.updateValue(t, newD);
  }
  var l = t.l;
  var r = t.r;
  var v = t.v;
  if (c < 0) {
    return Belt_internalAVLtree.bal(set(l, newK, newD, cmp), k, v, r);
  } else {
    return Belt_internalAVLtree.bal(l, k, v, set(r, newK, newD, cmp));
  }
}

function updateU(t, newK, f, cmp) {
  if (t !== undefined) {
    var k = t.k;
    var c = cmp(newK, k);
    if (c === 0) {
      var newD = f(Caml_option.some(t.v));
      if (newD !== undefined) {
        return Belt_internalAVLtree.updateValue(t, Caml_option.valFromOption(newD));
      }
      var l = t.l;
      var r = t.r;
      if (l === undefined) {
        return r;
      }
      if (r === undefined) {
        return l;
      }
      var kr = {
        contents: r.k
      };
      var vr = {
        contents: r.v
      };
      var r$1 = Belt_internalAVLtree.removeMinAuxWithRef(r, kr, vr);
      return Belt_internalAVLtree.bal(l, kr.contents, vr.contents, r$1);
    }
    var l$1 = t.l;
    var r$2 = t.r;
    var v = t.v;
    if (c < 0) {
      var ll = updateU(l$1, newK, f, cmp);
      if (l$1 === ll) {
        return t;
      } else {
        return Belt_internalAVLtree.bal(ll, k, v, r$2);
      }
    }
    var rr = updateU(r$2, newK, f, cmp);
    if (r$2 === rr) {
      return t;
    } else {
      return Belt_internalAVLtree.bal(l$1, k, v, rr);
    }
  }
  var newD$1 = f(undefined);
  if (newD$1 !== undefined) {
    return Belt_internalAVLtree.singleton(newK, Caml_option.valFromOption(newD$1));
  } else {
    return t;
  }
}

function update(t, newK, f, cmp) {
  return updateU(t, newK, Curry.__1(f), cmp);
}

function removeAux0(n, x, cmp) {
  var v = n.k;
  var l = n.l;
  var r = n.r;
  var c = cmp(x, v);
  if (c === 0) {
    if (l === undefined) {
      return r;
    }
    if (r === undefined) {
      return l;
    }
    var kr = {
      contents: r.k
    };
    var vr = {
      contents: r.v
    };
    var r$1 = Belt_internalAVLtree.removeMinAuxWithRef(r, kr, vr);
    return Belt_internalAVLtree.bal(l, kr.contents, vr.contents, r$1);
  }
  if (c < 0) {
    if (l === undefined) {
      return n;
    }
    var ll = removeAux0(l, x, cmp);
    if (ll === l) {
      return n;
    } else {
      return Belt_internalAVLtree.bal(ll, v, n.v, r);
    }
  }
  if (r === undefined) {
    return n;
  }
  var rr = removeAux0(r, x, cmp);
  if (rr === r) {
    return n;
  } else {
    return Belt_internalAVLtree.bal(l, v, n.v, rr);
  }
}

function remove(n, x, cmp) {
  if (n !== undefined) {
    return removeAux0(n, x, cmp);
  }
  
}

function mergeMany(h, arr, cmp) {
  var len = arr.length;
  var v = h;
  for(var i = 0; i < len; ++i){
    var match = arr[i];
    v = set(v, match[0], match[1], cmp);
  }
  return v;
}

function splitAuxPivot(n, x, pres, cmp) {
  var v = n.k;
  var d = n.v;
  var l = n.l;
  var r = n.r;
  var c = cmp(x, v);
  if (c === 0) {
    pres.contents = Caml_option.some(d);
    return [
            l,
            r
          ];
  }
  if (c < 0) {
    if (l === undefined) {
      return [
              undefined,
              n
            ];
    }
    var match = splitAuxPivot(l, x, pres, cmp);
    return [
            match[0],
            Belt_internalAVLtree.join(match[1], v, d, r)
          ];
  }
  if (r === undefined) {
    return [
            n,
            undefined
          ];
  }
  var match$1 = splitAuxPivot(r, x, pres, cmp);
  return [
          Belt_internalAVLtree.join(l, v, d, match$1[0]),
          match$1[1]
        ];
}

function split(n, x, cmp) {
  if (n === undefined) {
    return [
            [
              undefined,
              undefined
            ],
            undefined
          ];
  }
  var pres = {
    contents: undefined
  };
  var v = splitAuxPivot(n, x, pres, cmp);
  return [
          v,
          pres.contents
        ];
}

function mergeU(s1, s2, f, cmp) {
  if (s1 === undefined) {
    if (s2 !== undefined) {
      return Belt_internalAVLtree.keepMapU(s2, (function (k, v) {
                    return f(k, undefined, Caml_option.some(v));
                  }));
    } else {
      return ;
    }
  }
  if (s2 === undefined) {
    return Belt_internalAVLtree.keepMapU(s1, (function (k, v) {
                  return f(k, Caml_option.some(v), undefined);
                }));
  }
  if (s1.h >= s2.h) {
    var v1 = s1.k;
    var d1 = s1.v;
    var l1 = s1.l;
    var r1 = s1.r;
    var d2 = {
      contents: undefined
    };
    var match = splitAuxPivot(s2, v1, d2, cmp);
    var d2$1 = d2.contents;
    var newLeft = mergeU(l1, match[0], f, cmp);
    var newD = f(v1, Caml_option.some(d1), d2$1);
    var newRight = mergeU(r1, match[1], f, cmp);
    return Belt_internalAVLtree.concatOrJoin(newLeft, v1, newD, newRight);
  }
  var v2 = s2.k;
  var d2$2 = s2.v;
  var l2 = s2.l;
  var r2 = s2.r;
  var d1$1 = {
    contents: undefined
  };
  var match$1 = splitAuxPivot(s1, v2, d1$1, cmp);
  var d1$2 = d1$1.contents;
  var newLeft$1 = mergeU(match$1[0], l2, f, cmp);
  var newD$1 = f(v2, d1$2, Caml_option.some(d2$2));
  var newRight$1 = mergeU(match$1[1], r2, f, cmp);
  return Belt_internalAVLtree.concatOrJoin(newLeft$1, v2, newD$1, newRight$1);
}

function merge(s1, s2, f, cmp) {
  return mergeU(s1, s2, Curry.__3(f), cmp);
}

function removeMany(t, keys, cmp) {
  var len = keys.length;
  if (t !== undefined) {
    var _t = t;
    var _i = 0;
    while(true) {
      var i = _i;
      var t$1 = _t;
      if (i >= len) {
        return t$1;
      }
      var ele = keys[i];
      var u = removeAux0(t$1, ele, cmp);
      if (u === undefined) {
        return u;
      }
      _i = i + 1 | 0;
      _t = u;
      continue ;
    };
  }
  
}

var empty;

var isEmpty = Belt_internalAVLtree.isEmpty;

var has = Belt_internalAVLtree.has;

var cmpU = Belt_internalAVLtree.cmpU;

var cmp = Belt_internalAVLtree.cmp;

var eqU = Belt_internalAVLtree.eqU;

var eq = Belt_internalAVLtree.eq;

var findFirstByU = Belt_internalAVLtree.findFirstByU;

var findFirstBy = Belt_internalAVLtree.findFirstBy;

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
exports.findFirstByU = findFirstByU;
exports.findFirstBy = findFirstBy;
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
