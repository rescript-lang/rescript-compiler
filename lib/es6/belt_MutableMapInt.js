

import * as Curry from "./curry.js";
import * as Caml_option from "./caml_option.js";
import * as Belt_internalMapInt from "./belt_internalMapInt.js";
import * as Belt_internalAVLtree from "./belt_internalAVLtree.js";

function make(param) {
  return {
          data: undefined
        };
}

function isEmpty(m) {
  var x = m.data;
  return x === undefined;
}

function clear(m) {
  m.data = undefined;
  
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
  var v$1 = Belt_internalMapInt.addMutate(old_data, k, v);
  if (v$1 !== old_data) {
    m.data = v$1;
    return ;
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
  return Belt_internalMapInt.has(d.data, v);
}

function removeMutateAux(nt, x) {
  var k = nt.k;
  if (x === k) {
    var l = nt.l;
    var r = nt.r;
    if (l !== undefined) {
      if (r !== undefined) {
        nt.r = Belt_internalAVLtree.removeMinAuxWithRootMutate(nt, r);
        return Belt_internalAVLtree.balMutate(nt);
      } else {
        return l;
      }
    } else {
      return r;
    }
  }
  if (x < k) {
    var l$1 = nt.l;
    if (l$1 !== undefined) {
      nt.l = removeMutateAux(l$1, x);
      return Belt_internalAVLtree.balMutate(nt);
    } else {
      return nt;
    }
  }
  var r$1 = nt.r;
  if (r$1 !== undefined) {
    nt.r = removeMutateAux(r$1, x);
    return Belt_internalAVLtree.balMutate(nt);
  } else {
    return nt;
  }
}

function remove(d, v) {
  var oldRoot = d.data;
  if (oldRoot === undefined) {
    return ;
  }
  var newRoot = removeMutateAux(oldRoot, v);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return ;
  }
  
}

function updateDone(t, x, f) {
  if (t !== undefined) {
    var k = t.k;
    if (k === x) {
      var data = f(Caml_option.some(t.v));
      if (data !== undefined) {
        t.v = Caml_option.valFromOption(data);
        return t;
      }
      var l = t.l;
      var r = t.r;
      if (l !== undefined) {
        if (r !== undefined) {
          t.r = Belt_internalAVLtree.removeMinAuxWithRootMutate(t, r);
          return Belt_internalAVLtree.balMutate(t);
        } else {
          return l;
        }
      } else {
        return r;
      }
    }
    var l$1 = t.l;
    var r$1 = t.r;
    if (x < k) {
      var ll = updateDone(l$1, x, f);
      t.l = ll;
    } else {
      t.r = updateDone(r$1, x, f);
    }
    return Belt_internalAVLtree.balMutate(t);
  }
  var data$1 = f(undefined);
  if (data$1 !== undefined) {
    return Belt_internalAVLtree.singleton(x, Caml_option.valFromOption(data$1));
  } else {
    return t;
  }
}

function updateU(t, x, f) {
  var oldRoot = t.data;
  var newRoot = updateDone(oldRoot, x, f);
  if (newRoot !== oldRoot) {
    t.data = newRoot;
    return ;
  }
  
}

function update(t, x, f) {
  return updateU(t, x, Curry.__1(f));
}

function removeArrayMutateAux(_t, xs, _i, len) {
  while(true) {
    var i = _i;
    var t = _t;
    if (i >= len) {
      return t;
    }
    var ele = xs[i];
    var u = removeMutateAux(t, ele);
    if (u === undefined) {
      return ;
    }
    _i = i + 1 | 0;
    _t = u;
    continue ;
  };
}

function removeMany(d, xs) {
  var oldRoot = d.data;
  if (oldRoot === undefined) {
    return ;
  }
  var len = xs.length;
  var newRoot = removeArrayMutateAux(oldRoot, xs, 0, len);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return ;
  }
  
}

function fromArray(xs) {
  return {
          data: Belt_internalMapInt.fromArray(xs)
        };
}

function cmpU(d0, d1, f) {
  return Belt_internalMapInt.cmpU(d0.data, d1.data, f);
}

function cmp(d0, d1, f) {
  return cmpU(d0, d1, Curry.__2(f));
}

function eqU(d0, d1, f) {
  return Belt_internalMapInt.eqU(d0.data, d1.data, f);
}

function eq(d0, d1, f) {
  return eqU(d0, d1, Curry.__2(f));
}

function get(d, x) {
  return Belt_internalMapInt.get(d.data, x);
}

function getUndefined(d, x) {
  return Belt_internalMapInt.getUndefined(d.data, x);
}

function getWithDefault(d, x, def) {
  return Belt_internalMapInt.getWithDefault(d.data, x, def);
}

function getExn(d, x) {
  return Belt_internalMapInt.getExn(d.data, x);
}

export {
  make ,
  clear ,
  isEmpty ,
  has ,
  cmpU ,
  cmp ,
  eqU ,
  eq ,
  forEachU ,
  forEach ,
  reduceU ,
  reduce ,
  everyU ,
  every ,
  someU ,
  some ,
  size ,
  toList ,
  toArray ,
  fromArray ,
  keysToArray ,
  valuesToArray ,
  minKey ,
  minKeyUndefined ,
  maxKey ,
  maxKeyUndefined ,
  minimum ,
  minUndefined ,
  maximum ,
  maxUndefined ,
  get ,
  getUndefined ,
  getWithDefault ,
  getExn ,
  checkInvariantInternal ,
  remove ,
  removeMany ,
  set ,
  updateU ,
  update ,
  mapU ,
  map ,
  mapWithKeyU ,
  mapWithKey ,
  
}
/* No side effect */
