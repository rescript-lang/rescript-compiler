'use strict';

var Curry = require("./curry.js");
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
    var newRoot = removeMutateAux(oldRoot, k, d.cmp);
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
        return Bs_internalAVLtree.empty;
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
    var newRoot = removeArrayMutateAux(oldRoot, xs, 0, len, d.cmp);
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

function updateDone(t, x, f, cmp) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(x, k);
    if (c) {
      var l = t.left;
      var r = t.right;
      if (c < 0) {
        var ll = updateDone(l, x, f, cmp);
        t.left = ll;
      } else {
        t.right = updateDone(r, x, f, cmp);
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
      return Bs_internalAVLtree.singleton(x, match$1[0]);
    } else {
      return t;
    }
  }
}

function updateU(t, x, f) {
  var oldRoot = t.data;
  var newRoot = updateDone(oldRoot, x, f, t.cmp);
  if (newRoot !== oldRoot) {
    t.data = newRoot;
    return /* () */0;
  } else {
    return 0;
  }
}

function update(t, x, f) {
  return updateU(t, x, Curry.__1(f));
}

function make(dict) {
  return {
          cmp: dict[/* cmp */0],
          data: Bs_internalAVLtree.empty
        };
}

function clear(m) {
  m.data = Bs_internalAVLtree.empty;
  return /* () */0;
}

function isEmpty(d) {
  return Bs_internalAVLtree.isEmpty(d.data);
}

function minKey(m) {
  return Bs_internalAVLtree.minKey(m.data);
}

function minKeyUndefined(m) {
  return Bs_internalAVLtree.minKeyUndefined(m.data);
}

function maxKey(m) {
  return Bs_internalAVLtree.maxKey(m.data);
}

function maxKeyUndefined(m) {
  return Bs_internalAVLtree.maxKeyUndefined(m.data);
}

function minimum(m) {
  return Bs_internalAVLtree.minimum(m.data);
}

function minUndefined(m) {
  return Bs_internalAVLtree.minUndefined(m.data);
}

function maximum(m) {
  return Bs_internalAVLtree.maximum(m.data);
}

function maxUndefined(m) {
  return Bs_internalAVLtree.maxUndefined(m.data);
}

function forEachU(d, f) {
  return Bs_internalAVLtree.forEachU(d.data, f);
}

function forEach(d, f) {
  return Bs_internalAVLtree.forEachU(d.data, Curry.__2(f));
}

function reduceU(d, acc, cb) {
  return Bs_internalAVLtree.reduceU(d.data, acc, cb);
}

function reduce(d, acc, cb) {
  return reduceU(d, acc, Curry.__3(cb));
}

function everyU(d, p) {
  return Bs_internalAVLtree.everyU(d.data, p);
}

function every(d, p) {
  return Bs_internalAVLtree.everyU(d.data, Curry.__2(p));
}

function someU(d, p) {
  return Bs_internalAVLtree.someU(d.data, p);
}

function some(d, p) {
  return Bs_internalAVLtree.someU(d.data, Curry.__2(p));
}

function size(d) {
  return Bs_internalAVLtree.size(d.data);
}

function toList(d) {
  return Bs_internalAVLtree.toList(d.data);
}

function toArray(d) {
  return Bs_internalAVLtree.toArray(d.data);
}

function keysToArray(d) {
  return Bs_internalAVLtree.keysToArray(d.data);
}

function valuesToArray(d) {
  return Bs_internalAVLtree.valuesToArray(d.data);
}

function checkInvariantInternal(d) {
  return Bs_internalAVLtree.checkInvariantInternal(d.data);
}

function cmpU(m1, m2, cmp) {
  return Bs_internalAVLtree.cmpU(m1.data, m2.data, m1.cmp, cmp);
}

function cmp(m1, m2, cmp$1) {
  return cmpU(m1, m2, Curry.__2(cmp$1));
}

function eqU(m1, m2, cmp) {
  return Bs_internalAVLtree.eqU(m1.data, m2.data, m1.cmp, cmp);
}

function eq(m1, m2, cmp) {
  return eqU(m1, m2, Curry.__2(cmp));
}

function mapU(m, f) {
  return {
          cmp: m.cmp,
          data: Bs_internalAVLtree.mapU(m.data, f)
        };
}

function map(m, f) {
  return mapU(m, Curry.__1(f));
}

function mapWithKeyU(m, f) {
  return {
          cmp: m.cmp,
          data: Bs_internalAVLtree.mapWithKeyU(m.data, f)
        };
}

function mapWithKey(m, f) {
  return mapWithKeyU(m, Curry.__2(f));
}

function get(m, x) {
  return Bs_internalAVLtree.get(m.data, x, m.cmp);
}

function getUndefined(m, x) {
  return Bs_internalAVLtree.getUndefined(m.data, x, m.cmp);
}

function getWithDefault(m, x, def) {
  return Bs_internalAVLtree.getWithDefault(m.data, x, def, m.cmp);
}

function getExn(m, x) {
  return Bs_internalAVLtree.getExn(m.data, x, m.cmp);
}

function has(m, x) {
  return Bs_internalAVLtree.has(m.data, x, m.cmp);
}

function ofArray(data, dict) {
  var cmp = dict[/* cmp */0];
  return {
          cmp: cmp,
          data: Bs_internalAVLtree.ofArray(data, cmp)
        };
}

function set(m, e, v) {
  var oldRoot = m.data;
  var newRoot = Bs_internalAVLtree.updateMutate(oldRoot, e, v, m.cmp);
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
  var newRoot = mergeArrayAux(oldRoot, xs, d.cmp);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return /* () */0;
  } else {
    return 0;
  }
}

var Int = 0;

var $$String = 0;

exports.Int = Int;
exports.$$String = $$String;
exports.make = make;
exports.clear = clear;
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
exports.checkInvariantInternal = checkInvariantInternal;
exports.remove = remove;
exports.removeMany = removeMany;
exports.set = set;
exports.updateU = updateU;
exports.update = update;
exports.mergeMany = mergeMany;
exports.mapU = mapU;
exports.map = map;
exports.mapWithKeyU = mapWithKeyU;
exports.mapWithKey = mapWithKey;
/* No side effect */
