

import * as Curry from "./curry.mjs";
import * as Caml_option from "./caml_option.mjs";
import * as Belt_internalAVLtree from "./belt_internalAVLtree.mjs";

function removeMutateAux(nt, x, cmp) {
  var k = nt.k;
  var c = cmp(x, k);
  if (c === 0) {
    var l = nt.l;
    var r = nt.r;
    if (l !== undefined) {
      if (r !== undefined) {
        nt.r = Belt_internalAVLtree.removeMinAuxWithRootMutate(nt, r);
        return Belt_internalAVLtree.balMutate(nt);
      } else {
        return l;
      }
    } else if (r !== undefined) {
      return r;
    } else {
      return l;
    }
  }
  if (c < 0) {
    var l$1 = nt.l;
    if (l$1 !== undefined) {
      nt.l = removeMutateAux(l$1, x, cmp);
      return Belt_internalAVLtree.balMutate(nt);
    } else {
      return nt;
    }
  }
  var r$1 = nt.r;
  if (r$1 !== undefined) {
    nt.r = removeMutateAux(r$1, x, cmp);
    return Belt_internalAVLtree.balMutate(nt);
  } else {
    return nt;
  }
}

function remove(d, k) {
  var oldRoot = d.data;
  if (oldRoot === undefined) {
    return ;
  }
  var newRoot = removeMutateAux(oldRoot, k, d.cmp);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return ;
  }
  
}

function removeArrayMutateAux(_t, xs, _i, len, cmp) {
  while(true) {
    var i = _i;
    var t = _t;
    if (i >= len) {
      return t;
    }
    var ele = xs[i];
    var u = removeMutateAux(t, ele, cmp);
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
  var newRoot = removeArrayMutateAux(oldRoot, xs, 0, len, d.cmp);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return ;
  }
  
}

function updateDone(t, x, f, cmp) {
  if (t !== undefined) {
    var k = t.k;
    var c = cmp(x, k);
    if (c === 0) {
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
      } else if (r !== undefined) {
        return r;
      } else {
        return l;
      }
    }
    if (c < 0) {
      t.l = updateDone(t.l, x, f, cmp);
    } else {
      t.r = updateDone(t.r, x, f, cmp);
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
  var newRoot = updateDone(oldRoot, x, f, t.cmp);
  if (newRoot !== oldRoot) {
    t.data = newRoot;
    return ;
  }
  
}

function update(t, x, f) {
  return updateU(t, x, Curry.__1(f));
}

function make(id) {
  return {
          cmp: id.cmp,
          data: undefined
        };
}

function clear(m) {
  m.data = undefined;
  
}

function isEmpty(d) {
  var x = d.data;
  return x === undefined;
}

function minKey(m) {
  return Belt_internalAVLtree.minKey(m.data);
}

function minKeyUndefined(m) {
  return Belt_internalAVLtree.minKeyUndefined(m.data);
}

function maxKey(m) {
  return Belt_internalAVLtree.maxKey(m.data);
}

function maxKeyUndefined(m) {
  return Belt_internalAVLtree.maxKeyUndefined(m.data);
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

function forEachU(d, f) {
  return Belt_internalAVLtree.forEachU(d.data, f);
}

function forEach(d, f) {
  return Belt_internalAVLtree.forEachU(d.data, Curry.__2(f));
}

function reduceU(d, acc, cb) {
  return Belt_internalAVLtree.reduceU(d.data, acc, cb);
}

function reduce(d, acc, cb) {
  return reduceU(d, acc, Curry.__3(cb));
}

function everyU(d, p) {
  return Belt_internalAVLtree.everyU(d.data, p);
}

function every(d, p) {
  return Belt_internalAVLtree.everyU(d.data, Curry.__2(p));
}

function someU(d, p) {
  return Belt_internalAVLtree.someU(d.data, p);
}

function some(d, p) {
  return Belt_internalAVLtree.someU(d.data, Curry.__2(p));
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

function cmpU(m1, m2, cmp) {
  return Belt_internalAVLtree.cmpU(m1.data, m2.data, m1.cmp, cmp);
}

function cmp(m1, m2, cmp$1) {
  return cmpU(m1, m2, Curry.__2(cmp$1));
}

function eqU(m1, m2, cmp) {
  return Belt_internalAVLtree.eqU(m1.data, m2.data, m1.cmp, cmp);
}

function eq(m1, m2, cmp) {
  return eqU(m1, m2, Curry.__2(cmp));
}

function mapU(m, f) {
  return {
          cmp: m.cmp,
          data: Belt_internalAVLtree.mapU(m.data, f)
        };
}

function map(m, f) {
  return mapU(m, Curry.__1(f));
}

function mapWithKeyU(m, f) {
  return {
          cmp: m.cmp,
          data: Belt_internalAVLtree.mapWithKeyU(m.data, f)
        };
}

function mapWithKey(m, f) {
  return mapWithKeyU(m, Curry.__2(f));
}

function get(m, x) {
  return Belt_internalAVLtree.get(m.data, x, m.cmp);
}

function getUndefined(m, x) {
  return Belt_internalAVLtree.getUndefined(m.data, x, m.cmp);
}

function getWithDefault(m, x, def) {
  return Belt_internalAVLtree.getWithDefault(m.data, x, def, m.cmp);
}

function getExn(m, x) {
  return Belt_internalAVLtree.getExn(m.data, x, m.cmp);
}

function has(m, x) {
  return Belt_internalAVLtree.has(m.data, x, m.cmp);
}

function fromArray(data, id) {
  var cmp = id.cmp;
  return {
          cmp: cmp,
          data: Belt_internalAVLtree.fromArray(data, cmp)
        };
}

function set(m, e, v) {
  var oldRoot = m.data;
  var newRoot = Belt_internalAVLtree.updateMutate(oldRoot, e, v, m.cmp);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
    return ;
  }
  
}

function mergeManyAux(t, xs, cmp) {
  var v = t;
  for(var i = 0 ,i_finish = xs.length; i < i_finish; ++i){
    var match = xs[i];
    v = Belt_internalAVLtree.updateMutate(v, match[0], match[1], cmp);
  }
  return v;
}

function mergeMany(d, xs) {
  var oldRoot = d.data;
  var newRoot = mergeManyAux(oldRoot, xs, d.cmp);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return ;
  }
  
}

var Int;

var $$String;

export {
  Int ,
  $$String ,
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
  mergeMany ,
  mapU ,
  map ,
  mapWithKeyU ,
  mapWithKey ,
  
}
/* No side effect */
