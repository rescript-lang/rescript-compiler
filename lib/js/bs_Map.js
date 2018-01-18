'use strict';

var Bs_internalAVLtree = require("./bs_internalAVLtree.js");

function set0(t, newK, newD, cmp) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(newK, k);
    if (c) {
      var l = t.left;
      var r = t.right;
      var v = t.value;
      if (c < 0) {
        return Bs_internalAVLtree.bal(set0(l, newK, newD, cmp), k, v, r);
      } else {
        return Bs_internalAVLtree.bal(l, k, v, set0(r, newK, newD, cmp));
      }
    } else {
      return Bs_internalAVLtree.updateValue(t, newD);
    }
  } else {
    return Bs_internalAVLtree.singleton0(newK, newD);
  }
}

function update0(t, newK, f, cmp) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(newK, k);
    if (c) {
      var l = t.left;
      var r = t.right;
      var v = t.value;
      if (c < 0) {
        var ll = update0(l, newK, f, cmp);
        if (l === ll) {
          return t;
        } else {
          return Bs_internalAVLtree.bal(ll, k, v, r);
        }
      } else {
        var rr = update0(r, newK, f, cmp);
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

function remove0(n, x, cmp) {
  if (n !== null) {
    return removeAux0(n, x, cmp);
  } else {
    return Bs_internalAVLtree.empty0;
  }
}

function mergeArray0(h, arr, cmp) {
  var len = arr.length;
  var v = h;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var match = arr[i];
    v = set0(v, match[0], match[1], cmp);
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

function split0(cmp, n, x) {
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

function merge0(s1, s2, f, cmp) {
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
        var newLeft = merge0(l1, match[0], f, cmp);
        var newD = f(v1, /* Some */[d1], d2$1);
        var newRight = merge0(r1, match[1], f, cmp);
        return Bs_internalAVLtree.concatOrJoin(newLeft, v1, newD, newRight);
      } else {
        var l2 = s2.left;
        var v2 = s2.key;
        var d2$2 = s2.value;
        var r2 = s2.right;
        var d1$1 = [/* None */0];
        var match$1 = splitAuxPivot(s1, v2, d1$1, cmp);
        var d1$2 = d1$1[0];
        var newLeft$1 = merge0(match$1[0], l2, f, cmp);
        var newD$1 = f(v2, d1$2, /* Some */[d2$2]);
        var newRight$1 = merge0(match$1[1], r2, f, cmp);
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

function removeArrayAux(_t, xs, _i, len, cmp) {
  while(true) {
    var i = _i;
    var t = _t;
    if (i < len) {
      var ele = xs[i];
      var u = removeAux0(t, ele, cmp);
      if (u !== null) {
        _i = i + 1 | 0;
        _t = u;
        continue ;
        
      } else {
        return u;
      }
    } else {
      return t;
    }
  };
}

function removeArray0(t, keys, cmp) {
  var len = keys.length;
  if (t !== null) {
    return removeArrayAux(t, keys, 0, len, cmp);
  } else {
    return Bs_internalAVLtree.empty0;
  }
}

function ofArray(data, dict) {
  return {
          dict: dict,
          data: Bs_internalAVLtree.ofArray0(dict[/* cmp */0], data)
        };
}

function remove(m, x) {
  var odata = m.data;
  if (odata !== null) {
    var dict = m.dict;
    var newData = removeAux0(odata, x, dict[/* cmp */0]);
    if (newData === odata) {
      return m;
    } else {
      return {
              dict: dict,
              data: newData
            };
    }
  } else {
    return m;
  }
}

function removeArray(m, xs) {
  var odata = m.data;
  if (odata !== null) {
    var dict = m.dict;
    var len = xs.length;
    var newData = removeArrayAux(odata, xs, 0, len, dict[/* cmp */0]);
    if (newData === odata) {
      return m;
    } else {
      return {
              dict: dict,
              data: newData
            };
    }
  } else {
    return m;
  }
}

function set(map, key, data) {
  var dict = map.dict;
  var map$1 = map.data;
  return {
          dict: dict,
          data: set0(map$1, key, data, dict[/* cmp */0])
        };
}

function mergeArray(m, e) {
  var dict = m.dict;
  var data = m.data;
  var newData = mergeArray0(data, e, dict[/* cmp */0]);
  return {
          dict: dict,
          data: newData
        };
}

function update(map, key, f) {
  var dict = map.dict;
  var map$1 = map.data;
  return {
          dict: dict,
          data: update0(map$1, key, f, dict[/* cmp */0])
        };
}

function split(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  var match = split0(dict[/* cmp */0], map$1, x);
  var match$1 = match[0];
  return /* tuple */[
          /* tuple */[
            {
              dict: dict,
              data: match$1[0]
            },
            {
              dict: dict,
              data: match$1[1]
            }
          ],
          match[1]
        ];
}

function merge(s1, s2, f) {
  var dict = s1.dict;
  var s1_data = s1.data;
  var s2_data = s2.data;
  return {
          dict: dict,
          data: merge0(s1_data, s2_data, f, dict[/* cmp */0])
        };
}

function empty(dict) {
  return {
          dict: dict,
          data: Bs_internalAVLtree.empty0
        };
}

function isEmpty(map) {
  return Bs_internalAVLtree.isEmpty0(map.data);
}

function singleton(k, v, dict) {
  return {
          dict: dict,
          data: Bs_internalAVLtree.singleton0(k, v)
        };
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

function iter(m, f) {
  return Bs_internalAVLtree.iter0(m.data, f);
}

function fold(m, acc, f) {
  return Bs_internalAVLtree.fold0(m.data, acc, f);
}

function forAll(m, f) {
  return Bs_internalAVLtree.forAll0(m.data, f);
}

function exists(m, f) {
  return Bs_internalAVLtree.exists0(m.data, f);
}

function filter(m, f) {
  var dict = m.dict;
  var map = m.data;
  return {
          dict: dict,
          data: Bs_internalAVLtree.filterShared0(map, f)
        };
}

function partition(m, p) {
  var dict = m.dict;
  var map = m.data;
  var match = Bs_internalAVLtree.partitionShared0(map, p);
  return /* tuple */[
          {
            dict: dict,
            data: match[0]
          },
          {
            dict: dict,
            data: match[1]
          }
        ];
}

function map(m, f) {
  var dict = m.dict;
  var map$1 = m.data;
  return {
          dict: dict,
          data: Bs_internalAVLtree.map0(map$1, f)
        };
}

function mapi(m, f) {
  var dict = m.dict;
  var map = m.data;
  return {
          dict: dict,
          data: Bs_internalAVLtree.mapi0(map, f)
        };
}

function length(map) {
  return Bs_internalAVLtree.length0(map.data);
}

function toList(map) {
  return Bs_internalAVLtree.toList0(map.data);
}

function toArray(m) {
  return Bs_internalAVLtree.toArray0(m.data);
}

function keysToArray(m) {
  return Bs_internalAVLtree.keysToArray0(m.data);
}

function valuesToArray(m) {
  return Bs_internalAVLtree.valuesToArray0(m.data);
}

function minKeyOpt(m) {
  return Bs_internalAVLtree.minKeyOpt0(m.data);
}

function minKeyNull(m) {
  return Bs_internalAVLtree.minKeyNull0(m.data);
}

function maxKeyOpt(m) {
  return Bs_internalAVLtree.maxKeyOpt0(m.data);
}

function maxKeyNull(m) {
  return Bs_internalAVLtree.maxKeyNull0(m.data);
}

function minKeyValueOpt(m) {
  return Bs_internalAVLtree.minKVOpt0(m.data);
}

function minKeyValueNull(m) {
  return Bs_internalAVLtree.minKVNull0(m.data);
}

function maxKeyValueOpt(m) {
  return Bs_internalAVLtree.maxKVOpt0(m.data);
}

function maxKeyValueNull(m) {
  return Bs_internalAVLtree.maxKVNull0(m.data);
}

function get(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.findOpt0(map$1, x, dict[/* cmp */0]);
}

function getNull(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.findNull0(map$1, x, dict[/* cmp */0]);
}

function getWithDefault(map, x, def) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.findWithDefault0(map$1, x, def, dict[/* cmp */0]);
}

function getExn(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.findExn0(map$1, x, dict[/* cmp */0]);
}

function mem(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.mem0(map$1, x, dict[/* cmp */0]);
}

function checkInvariant(m) {
  return Bs_internalAVLtree.checkInvariant(m.data);
}

function getData(prim) {
  return prim.data;
}

function getDict(prim) {
  return prim.dict;
}

function packDictData(prim, prim$1) {
  return {
          dict: prim,
          data: prim$1
        };
}

var empty0 = Bs_internalAVLtree.empty0;

var ofArray0 = Bs_internalAVLtree.ofArray0;

var isEmpty0 = Bs_internalAVLtree.isEmpty0;

var mem0 = Bs_internalAVLtree.mem0;

var singleton0 = Bs_internalAVLtree.singleton0;

var cmp0 = Bs_internalAVLtree.cmp0;

var eq0 = Bs_internalAVLtree.eq0;

var iter0 = Bs_internalAVLtree.iter0;

var fold0 = Bs_internalAVLtree.fold0;

var forAll0 = Bs_internalAVLtree.forAll0;

var exists0 = Bs_internalAVLtree.exists0;

var filter0 = Bs_internalAVLtree.filterShared0;

var partition0 = Bs_internalAVLtree.partitionShared0;

var length0 = Bs_internalAVLtree.length0;

var toList0 = Bs_internalAVLtree.toList0;

var minKVOpt0 = Bs_internalAVLtree.minKVOpt0;

var maxKVOpt0 = Bs_internalAVLtree.maxKVOpt0;

var findOpt0 = Bs_internalAVLtree.findOpt0;

var findNull0 = Bs_internalAVLtree.findNull0;

var findWithDefault0 = Bs_internalAVLtree.findWithDefault0;

var findExn0 = Bs_internalAVLtree.findExn0;

var map0 = Bs_internalAVLtree.map0;

var mapi0 = Bs_internalAVLtree.mapi0;

exports.empty = empty;
exports.isEmpty = isEmpty;
exports.singleton = singleton;
exports.mem = mem;
exports.cmp = cmp;
exports.eq = eq;
exports.iter = iter;
exports.fold = fold;
exports.forAll = forAll;
exports.exists = exists;
exports.length = length;
exports.toList = toList;
exports.toArray = toArray;
exports.ofArray = ofArray;
exports.keysToArray = keysToArray;
exports.valuesToArray = valuesToArray;
exports.minKeyOpt = minKeyOpt;
exports.minKeyNull = minKeyNull;
exports.maxKeyOpt = maxKeyOpt;
exports.maxKeyNull = maxKeyNull;
exports.minKeyValueOpt = minKeyValueOpt;
exports.minKeyValueNull = minKeyValueNull;
exports.maxKeyValueOpt = maxKeyValueOpt;
exports.maxKeyValueNull = maxKeyValueNull;
exports.get = get;
exports.getNull = getNull;
exports.getWithDefault = getWithDefault;
exports.getExn = getExn;
exports.checkInvariant = checkInvariant;
exports.remove = remove;
exports.removeArray = removeArray;
exports.set = set;
exports.update = update;
exports.mergeArray = mergeArray;
exports.merge = merge;
exports.filter = filter;
exports.partition = partition;
exports.split = split;
exports.map = map;
exports.mapi = mapi;
exports.getData = getData;
exports.getDict = getDict;
exports.packDictData = packDictData;
exports.empty0 = empty0;
exports.ofArray0 = ofArray0;
exports.isEmpty0 = isEmpty0;
exports.mem0 = mem0;
exports.set0 = set0;
exports.update0 = update0;
exports.singleton0 = singleton0;
exports.remove0 = remove0;
exports.removeArray0 = removeArray0;
exports.merge0 = merge0;
exports.cmp0 = cmp0;
exports.eq0 = eq0;
exports.iter0 = iter0;
exports.fold0 = fold0;
exports.forAll0 = forAll0;
exports.exists0 = exists0;
exports.filter0 = filter0;
exports.partition0 = partition0;
exports.length0 = length0;
exports.toList0 = toList0;
exports.minKVOpt0 = minKVOpt0;
exports.maxKVOpt0 = maxKVOpt0;
exports.split0 = split0;
exports.findOpt0 = findOpt0;
exports.findNull0 = findNull0;
exports.findWithDefault0 = findWithDefault0;
exports.findExn0 = findExn0;
exports.map0 = map0;
exports.mapi0 = mapi0;
/* No side effect */
