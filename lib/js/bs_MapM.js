'use strict';

var Bs_internalAVLtree = require("./bs_internalAVLtree.js");

function removeMutateAux(nt, x, cmp) {
  var k = nt.key;
  var c = cmp(x, k);
  if (c) {
    if (c < 0) {
      var match = nt.left;
      if (match !== null) {
        nt.left = removeMutateAux(match, x, cmp);
        return Bs_internalAVLtree.balMutate(nt);
      } else {
        return nt;
      }
    } else {
      var match$1 = nt.right;
      if (match$1 !== null) {
        nt.right = removeMutateAux(match$1, x, cmp);
        return Bs_internalAVLtree.balMutate(nt);
      } else {
        return nt;
      }
    }
  } else {
    var l = nt.left;
    var r = nt.right;
    if (l !== null) {
      if (r !== null) {
        nt.right = Bs_internalAVLtree.removeMinAuxWithRootMutate(nt, r);
        return Bs_internalAVLtree.balMutate(nt);
      } else {
        return l;
      }
    } else if (r !== null) {
      return r;
    } else {
      return l;
    }
  }
}

function remove(d, k) {
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var M = d.dict;
    var newRoot = removeMutateAux(oldRoot, k, M[/* cmp */0]);
    if (newRoot !== oldRoot) {
      d.data = newRoot;
      return /* () */0;
    } else {
      return 0;
    }
  } else {
    return /* () */0;
  }
}

function removeArrayMutateAux(_t, xs, _i, len, cmp) {
  while(true) {
    var i = _i;
    var t = _t;
    if (i < len) {
      var ele = xs[i];
      var u = removeMutateAux(t, ele, cmp);
      if (u !== null) {
        _i = i + 1 | 0;
        _t = u;
        continue ;
        
      } else {
        return Bs_internalAVLtree.empty0;
      }
    } else {
      return t;
    }
  };
}

function removeMany(d, xs) {
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var len = xs.length;
    var M = d.dict;
    var newRoot = removeArrayMutateAux(oldRoot, xs, 0, len, M[/* cmp */0]);
    if (newRoot !== oldRoot) {
      d.data = newRoot;
      return /* () */0;
    } else {
      return 0;
    }
  } else {
    return /* () */0;
  }
}

function updateDone0(t, x, f, cmp) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(x, k);
    if (c) {
      var l = t.left;
      var r = t.right;
      if (c < 0) {
        var ll = updateDone0(l, x, f, cmp);
        t.left = ll;
      } else {
        t.right = updateDone0(r, x, f, cmp);
      }
      return Bs_internalAVLtree.balMutate(t);
    } else {
      var match = f(/* Some */[t.value]);
      if (match) {
        t.value = match[0];
        return t;
      } else {
        var l$1 = t.left;
        var r$1 = t.right;
        if (l$1 !== null) {
          if (r$1 !== null) {
            t.right = Bs_internalAVLtree.removeMinAuxWithRootMutate(t, r$1);
            return Bs_internalAVLtree.balMutate(t);
          } else {
            return l$1;
          }
        } else if (r$1 !== null) {
          return r$1;
        } else {
          return l$1;
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

function update(t, x, f) {
  var oldRoot = t.data;
  var M = t.dict;
  var newRoot = updateDone0(oldRoot, x, f, M[/* cmp */0]);
  if (newRoot !== oldRoot) {
    t.data = newRoot;
    return /* () */0;
  } else {
    return 0;
  }
}

function empty(dict) {
  return {
          dict: dict,
          data: Bs_internalAVLtree.empty0
        };
}

function isEmpty(d) {
  return Bs_internalAVLtree.isEmpty0(d.data);
}

function minKey(m) {
  return Bs_internalAVLtree.minKey0(m.data);
}

function minKeyUndefined(m) {
  return Bs_internalAVLtree.minKeyUndefined0(m.data);
}

function maxKey(m) {
  return Bs_internalAVLtree.maxKey0(m.data);
}

function maxKeyUndefined(m) {
  return Bs_internalAVLtree.maxKeyUndefined0(m.data);
}

function minimum(m) {
  return Bs_internalAVLtree.minimum0(m.data);
}

function minUndefined(m) {
  return Bs_internalAVLtree.minUndefined0(m.data);
}

function maximum(m) {
  return Bs_internalAVLtree.maximum0(m.data);
}

function maxUndefined(m) {
  return Bs_internalAVLtree.maxUndefined0(m.data);
}

function forEach(d, f) {
  return Bs_internalAVLtree.iter0(d.data, f);
}

function reduce(d, acc, cb) {
  return Bs_internalAVLtree.fold0(d.data, acc, cb);
}

function every(d, p) {
  return Bs_internalAVLtree.every0(d.data, p);
}

function some(d, p) {
  return Bs_internalAVLtree.some0(d.data, p);
}

function size(d) {
  return Bs_internalAVLtree.length0(d.data);
}

function toList(d) {
  return Bs_internalAVLtree.toList0(d.data);
}

function toArray(d) {
  return Bs_internalAVLtree.toArray0(d.data);
}

function keysToArray(d) {
  return Bs_internalAVLtree.keysToArray0(d.data);
}

function valuesToArray(d) {
  return Bs_internalAVLtree.valuesToArray0(d.data);
}

function checkInvariantInternal(d) {
  return Bs_internalAVLtree.checkInvariantInternal(d.data);
}

function cmp(m1, m2, cmp$1) {
  var dict = m1.dict;
  var m1_data = m1.data;
  var m2_data = m2.data;
  return Bs_internalAVLtree.cmp0(m1_data, m2_data, dict[/* cmp */0], cmp$1);
}

function eq(m1, m2, cmp) {
  var dict = m1.dict;
  var m1_data = m1.data;
  var m2_data = m2.data;
  return Bs_internalAVLtree.eq0(m1_data, m2_data, dict[/* cmp */0], cmp);
}

function map(m, f) {
  var dict = m.dict;
  var map$1 = m.data;
  return {
          dict: dict,
          data: Bs_internalAVLtree.map0(map$1, f)
        };
}

function mapWithKey(map, f) {
  var dict = map.dict;
  var map$1 = map.data;
  return {
          dict: dict,
          data: Bs_internalAVLtree.mapi0(map$1, f)
        };
}

function get(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.get0(map$1, x, dict[/* cmp */0]);
}

function getUndefined(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.getUndefined0(map$1, x, dict[/* cmp */0]);
}

function getWithDefault(map, x, def) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.getWithDefault0(map$1, x, def, dict[/* cmp */0]);
}

function getExn(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.getExn0(map$1, x, dict[/* cmp */0]);
}

function has(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.mem0(map$1, x, dict[/* cmp */0]);
}

function ofArray(data, dict) {
  return {
          dict: dict,
          data: Bs_internalAVLtree.ofArray0(data, dict[/* cmp */0])
        };
}

function set(m, e, v) {
  var oldRoot = m.data;
  var M = m.dict;
  var newRoot = Bs_internalAVLtree.updateMutate(oldRoot, e, v, M[/* cmp */0]);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
    return /* () */0;
  } else {
    return 0;
  }
}

function mergeArrayAux(t, xs, cmp) {
  var v = t;
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    var match = xs[i];
    v = Bs_internalAVLtree.updateMutate(v, match[0], match[1], cmp);
  }
  return v;
}

function mergeMany(d, xs) {
  var oldRoot = d.data;
  var M = d.dict;
  var newRoot = mergeArrayAux(oldRoot, xs, M[/* cmp */0]);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return /* () */0;
  } else {
    return 0;
  }
}

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
exports.mergeMany = mergeMany;
exports.map = map;
exports.mapWithKey = mapWithKey;
/* No side effect */
