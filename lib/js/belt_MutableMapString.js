'use strict';

var Curry = require("./curry.js");
var Belt_internalAVLtree = require("./belt_internalAVLtree.js");
var Belt_internalMapString = require("./belt_internalMapString.js");

function make() {
  return {
          data: Belt_internalAVLtree.empty
        };
}

function isEmpty(m) {
  var x = m.data;
  return x === null;
}

function clear(m) {
  m.data = Belt_internalAVLtree.empty;
  return /* () */0;
}

function minKeyUndefined(m) {
  return Belt_internalAVLtree.minKeyUndefined(m.data);
}

function minKey(m) {
  return Belt_internalAVLtree.minKey(m.data);
}

function maxKeyUndefined(m) {
  return Belt_internalAVLtree.maxKeyUndefined(m.data);
}

function maxKey(m) {
  return Belt_internalAVLtree.maxKey(m.data);
}

function minimum(m) {
  return Belt_internalAVLtree.minimum(m.data);
}

function minUndefined(m) {
  return Belt_internalAVLtree.minUndefined(m.data);
}

function maximum(m) {
  return Belt_internalAVLtree.maximum(m.data);
}

function maxUndefined(m) {
  return Belt_internalAVLtree.maxUndefined(m.data);
}

function set(m, k, v) {
  var old_data = m.data;
  var v$1 = Belt_internalMapString.addMutate(old_data, k, v);
  if (v$1 !== old_data) {
    m.data = v$1;
    return /* () */0;
  } else {
    return 0;
  }
}

function forEachU(d, f) {
  return Belt_internalAVLtree.forEachU(d.data, f);
}

function forEach(d, f) {
  return Belt_internalAVLtree.forEachU(d.data, Curry.__2(f));
}

function mapU(d, f) {
  return {
          data: Belt_internalAVLtree.mapU(d.data, f)
        };
}

function map(d, f) {
  return mapU(d, Curry.__1(f));
}

function mapWithKeyU(d, f) {
  return {
          data: Belt_internalAVLtree.mapWithKeyU(d.data, f)
        };
}

function mapWithKey(d, f) {
  return mapWithKeyU(d, Curry.__2(f));
}

function reduceU(d, acc, f) {
  return Belt_internalAVLtree.reduceU(d.data, acc, f);
}

function reduce(d, acc, f) {
  return reduceU(d, acc, Curry.__3(f));
}

function everyU(d, f) {
  return Belt_internalAVLtree.everyU(d.data, f);
}

function every(d, f) {
  return Belt_internalAVLtree.everyU(d.data, Curry.__2(f));
}

function someU(d, f) {
  return Belt_internalAVLtree.someU(d.data, f);
}

function some(d, f) {
  return Belt_internalAVLtree.someU(d.data, Curry.__2(f));
}

function size(d) {
  return Belt_internalAVLtree.size(d.data);
}

function toList(d) {
  return Belt_internalAVLtree.toList(d.data);
}

function toArray(d) {
  return Belt_internalAVLtree.toArray(d.data);
}

function keysToArray(d) {
  return Belt_internalAVLtree.keysToArray(d.data);
}

function valuesToArray(d) {
  return Belt_internalAVLtree.valuesToArray(d.data);
}

function checkInvariantInternal(d) {
  return Belt_internalAVLtree.checkInvariantInternal(d.data);
}

function has(d, v) {
  return Belt_internalMapString.has(d.data, v);
}

function removeMutateAux(nt, x) {
  var k = nt.key;
  if (x === k) {
    var l = nt.left;
    var r = nt.right;
    if (l !== null) {
      if (r !== null) {
        nt.right = Belt_internalAVLtree.removeMinAuxWithRootMutate(nt, r);
        return Belt_internalAVLtree.balMutate(nt);
      } else {
        return l;
      }
    } else {
      return r;
    }
  } else if (x < k) {
    var match = nt.left;
    if (match !== null) {
      nt.left = removeMutateAux(match, x);
      return Belt_internalAVLtree.balMutate(nt);
    } else {
      return nt;
    }
  } else {
    var match$1 = nt.right;
    if (match$1 !== null) {
      nt.right = removeMutateAux(match$1, x);
      return Belt_internalAVLtree.balMutate(nt);
    } else {
      return nt;
    }
  }
}

function remove(d, v) {
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var newRoot = removeMutateAux(oldRoot, v);
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

function updateDone(t, x, f) {
  if (t !== null) {
    var k = t.key;
    if (k === x) {
      var match = f(/* Some */[t.value]);
      if (match) {
        t.value = match[0];
        return t;
      } else {
        var l = t.left;
        var r = t.right;
        if (l !== null) {
          if (r !== null) {
            t.right = Belt_internalAVLtree.removeMinAuxWithRootMutate(t, r);
            return Belt_internalAVLtree.balMutate(t);
          } else {
            return l;
          }
        } else {
          return r;
        }
      }
    } else {
      var l$1 = t.left;
      var r$1 = t.right;
      if (x < k) {
        var ll = updateDone(l$1, x, f);
        t.left = ll;
      } else {
        t.right = updateDone(r$1, x, f);
      }
      return Belt_internalAVLtree.balMutate(t);
    }
  } else {
    var match$1 = f(/* None */0);
    if (match$1) {
      return Belt_internalAVLtree.singleton(x, match$1[0]);
    } else {
      return t;
    }
  }
}

function updateU(t, x, f) {
  var oldRoot = t.data;
  var newRoot = updateDone(oldRoot, x, f);
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

function removeArrayMutateAux(_t, xs, _i, len) {
  while(true) {
    var i = _i;
    var t = _t;
    if (i < len) {
      var ele = xs[i];
      var u = removeMutateAux(t, ele);
      if (u !== null) {
        _i = i + 1 | 0;
        _t = u;
        continue ;
      } else {
        return Belt_internalAVLtree.empty;
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
    var newRoot = removeArrayMutateAux(oldRoot, xs, 0, len);
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

function fromArray(xs) {
  return {
          data: Belt_internalMapString.fromArray(xs)
        };
}

function cmpU(d0, d1, f) {
  return Belt_internalMapString.cmpU(d0.data, d1.data, f);
}

function cmp(d0, d1, f) {
  return cmpU(d0, d1, Curry.__2(f));
}

function eqU(d0, d1, f) {
  return Belt_internalMapString.eqU(d0.data, d1.data, f);
}

function eq(d0, d1, f) {
  return eqU(d0, d1, Curry.__2(f));
}

function get(d, x) {
  return Belt_internalMapString.get(d.data, x);
}

function getUndefined(d, x) {
  return Belt_internalMapString.getUndefined(d.data, x);
}

function getWithDefault(d, x, def) {
  return Belt_internalMapString.getWithDefault(d.data, x, def);
}

function getExn(d, x) {
  return Belt_internalMapString.getExn(d.data, x);
}

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
exports.mapU = mapU;
exports.map = map;
exports.mapWithKeyU = mapWithKeyU;
exports.mapWithKey = mapWithKey;
/* No side effect */
