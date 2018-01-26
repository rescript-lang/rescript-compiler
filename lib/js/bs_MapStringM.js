'use strict';

var Bs_internalAVLtree = require("./bs_internalAVLtree.js");
var Bs_internalMapString = require("./bs_internalMapString.js");

function empty() {
  return {
          data: Bs_internalAVLtree.empty0
        };
}

function isEmpty(m) {
  return Bs_internalAVLtree.isEmpty0(m.data);
}

function minKeyNull(m) {
  return Bs_internalAVLtree.minKeyNull0(m.data);
}

function minKey(m) {
  return Bs_internalAVLtree.minKeyOpt0(m.data);
}

function maxKeyNull(m) {
  return Bs_internalAVLtree.maxKeyNull0(m.data);
}

function maxKey(m) {
  return Bs_internalAVLtree.maxKeyOpt0(m.data);
}

function minimum(m) {
  return Bs_internalAVLtree.minKVOpt0(m.data);
}

function minNull(m) {
  return Bs_internalAVLtree.minKVNull0(m.data);
}

function maximum(m) {
  return Bs_internalAVLtree.maxKVOpt0(m.data);
}

function maxNull(m) {
  return Bs_internalAVLtree.maxKVNull0(m.data);
}

function set(m, k, v) {
  var old_data = m.data;
  var v$1 = Bs_internalMapString.addMutate(old_data, k, v);
  if (v$1 !== old_data) {
    m.data = v$1;
    return /* () */0;
  } else {
    return 0;
  }
}

function forEach(d, f) {
  return Bs_internalAVLtree.iter0(d.data, f);
}

function map(d, f) {
  return {
          data: Bs_internalAVLtree.map0(d.data, f)
        };
}

function mapWithKey(d, f) {
  return {
          data: Bs_internalAVLtree.mapi0(d.data, f)
        };
}

function reduce(d, acc, f) {
  return Bs_internalAVLtree.fold0(d.data, acc, f);
}

function every(d, f) {
  return Bs_internalAVLtree.every0(d.data, f);
}

function some(d, f) {
  return Bs_internalAVLtree.some0(d.data, f);
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

function checkInvariant(d) {
  return Bs_internalAVLtree.checkInvariant(d.data);
}

function has(d, v) {
  return Bs_internalMapString.mem(d.data, v);
}

function removeMutateAux(nt, x) {
  var k = nt.key;
  if (x === k) {
    var l = nt.left;
    var r = nt.right;
    if (l !== null) {
      if (r !== null) {
        nt.right = Bs_internalAVLtree.removeMinAuxWithRootMutate(nt, r);
        return Bs_internalAVLtree.balMutate(nt);
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
      return Bs_internalAVLtree.balMutate(nt);
    } else {
      return nt;
    }
  } else {
    var match$1 = nt.right;
    if (match$1 !== null) {
      nt.right = removeMutateAux(match$1, x);
      return Bs_internalAVLtree.balMutate(nt);
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

function updateDone0(t, x, f) {
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
            t.right = Bs_internalAVLtree.removeMinAuxWithRootMutate(t, r);
            return Bs_internalAVLtree.balMutate(t);
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
        var ll = updateDone0(l$1, x, f);
        t.left = ll;
      } else {
        t.right = updateDone0(r$1, x, f);
      }
      return Bs_internalAVLtree.balMutate(t);
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
  var newRoot = updateDone0(oldRoot, x, f);
  if (newRoot !== oldRoot) {
    t.data = newRoot;
    return /* () */0;
  } else {
    return 0;
  }
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

function ofArray(xs) {
  return {
          data: Bs_internalMapString.ofArray(xs)
        };
}

function cmp(d0, d1) {
  var partial_arg = d1.data;
  var partial_arg$1 = d0.data;
  return (function (param) {
      return Bs_internalMapString.cmp(partial_arg$1, partial_arg, param);
    });
}

function eq(d0, d1) {
  var partial_arg = d1.data;
  var partial_arg$1 = d0.data;
  return (function (param) {
      return Bs_internalMapString.eq(partial_arg$1, partial_arg, param);
    });
}

function get(d, x) {
  return Bs_internalMapString.findOpt(d.data, x);
}

function getNull(d, x) {
  return Bs_internalMapString.findNull(d.data, x);
}

function getWithDefault(d, x, def) {
  return Bs_internalMapString.findWithDefault(d.data, x, def);
}

function getExn(d, x) {
  return Bs_internalMapString.findExn(d.data, x);
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
exports.checkInvariant = checkInvariant;
exports.remove = remove;
exports.removeMany = removeMany;
exports.set = set;
exports.update = update;
exports.map = map;
exports.mapWithKey = mapWithKey;
/* No side effect */
