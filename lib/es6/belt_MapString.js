

import * as Curry from "./curry.js";
import * as Caml_option from "./caml_option.js";
import * as Belt_internalAVLtree from "./belt_internalAVLtree.js";
import * as Belt_internalMapString from "./belt_internalMapString.js";

function set(t, newK, newD) {
  if (t === undefined) {
    return Belt_internalAVLtree.singleton(newK, newD);
  }
  var k = t.key;
  if (newK === k) {
    return Belt_internalAVLtree.updateValue(t, newD);
  }
  var v = t.value;
  if (newK < k) {
    return Belt_internalAVLtree.bal(set(t.left, newK, newD), k, v, t.right);
  } else {
    return Belt_internalAVLtree.bal(t.left, k, v, set(t.right, newK, newD));
  }
}

function updateU(t, x, f) {
  if (t !== undefined) {
    var k = t.key;
    if (x === k) {
      var data = f(Caml_option.some(t.value));
      if (data !== undefined) {
        return Belt_internalAVLtree.updateValue(t, Caml_option.valFromOption(data));
      }
      var l = t.left;
      var r = t.right;
      if (l === undefined) {
        return r;
      }
      if (r === undefined) {
        return l;
      }
      var kr = {
        contents: r.key
      };
      var vr = {
        contents: r.value
      };
      var r$1 = Belt_internalAVLtree.removeMinAuxWithRef(r, kr, vr);
      return Belt_internalAVLtree.bal(l, kr.contents, vr.contents, r$1);
    }
    var v = t.value;
    var l$1 = t.left;
    var r$2 = t.right;
    if (x < k) {
      var ll = updateU(l$1, x, f);
      if (l$1 === ll) {
        return t;
      } else {
        return Belt_internalAVLtree.bal(ll, k, v, r$2);
      }
    }
    var rr = updateU(r$2, x, f);
    if (r$2 === rr) {
      return t;
    } else {
      return Belt_internalAVLtree.bal(l$1, k, v, rr);
    }
  }
  var data$1 = f(undefined);
  if (data$1 !== undefined) {
    return Belt_internalAVLtree.singleton(x, Caml_option.valFromOption(data$1));
  } else {
    return t;
  }
}

function update(t, x, f) {
  return updateU(t, x, Curry.__1(f));
}

function removeAux(n, x) {
  var v = n.key;
  var l = n.left;
  var r = n.right;
  if (x === v) {
    if (l === undefined) {
      return r;
    }
    if (r === undefined) {
      return l;
    }
    var kr = {
      contents: r.key
    };
    var vr = {
      contents: r.value
    };
    var r$1 = Belt_internalAVLtree.removeMinAuxWithRef(r, kr, vr);
    return Belt_internalAVLtree.bal(l, kr.contents, vr.contents, r$1);
  }
  if (x < v) {
    if (l === undefined) {
      return n;
    }
    var ll = removeAux(l, x);
    if (ll === l) {
      return n;
    } else {
      return Belt_internalAVLtree.bal(ll, v, n.value, r);
    }
  }
  if (r === undefined) {
    return n;
  }
  var rr = removeAux(r, x);
  return Belt_internalAVLtree.bal(l, v, n.value, rr);
}

function remove(n, x) {
  if (n !== undefined) {
    return removeAux(n, x);
  }
  
}

function removeMany(t, keys) {
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
      var u = removeAux(t$1, ele);
      if (u === undefined) {
        return u;
      }
      _i = i + 1 | 0;
      _t = u;
      continue ;
    };
  }
  
}

function mergeMany(h, arr) {
  var len = arr.length;
  var v = h;
  for(var i = 0; i < len; ++i){
    var match = arr[i];
    v = set(v, match[0], match[1]);
  }
  return v;
}

var empty;

var isEmpty = Belt_internalAVLtree.isEmpty;

var has = Belt_internalMapString.has;

var cmpU = Belt_internalMapString.cmpU;

var cmp = Belt_internalMapString.cmp;

var eqU = Belt_internalMapString.eqU;

var eq = Belt_internalMapString.eq;

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

export {
  empty ,
  isEmpty ,
  has ,
  cmpU ,
  cmp ,
  eqU ,
  eq ,
  findFirstByU ,
  findFirstBy ,
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
  mergeU ,
  merge ,
  mergeMany ,
  keepU ,
  keep ,
  partitionU ,
  partition ,
  split ,
  mapU ,
  map ,
  mapWithKeyU ,
  mapWithKey ,
  
}
/* No side effect */
