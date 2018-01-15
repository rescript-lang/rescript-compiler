'use strict';

var Bs_internalAVLtree = require("./bs_internalAVLtree.js");

function add0(t, x, data, cmp) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(x, k);
    if (c) {
      var v = t.value;
      if (c < 0) {
        return Bs_internalAVLtree.bal(add0(t.left, x, data, cmp), k, v, t.right);
      } else {
        return Bs_internalAVLtree.bal(t.left, k, v, add0(t.right, x, data, cmp));
      }
    } else {
      return Bs_internalAVLtree.updateKV(t, x, data);
    }
  } else {
    return Bs_internalAVLtree.singleton0(x, data);
  }
}

function remove0(n, x, cmp) {
  if (n !== null) {
    var l = n.left;
    var v = n.key;
    var r = n.right;
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        return Bs_internalAVLtree.bal(remove0(l, x, cmp), v, n.value, r);
      } else {
        return Bs_internalAVLtree.bal(l, v, n.value, remove0(r, x, cmp));
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
  } else {
    return n;
  }
}

function splitAuxPivot(cmp, n, x, pres) {
  var l = n.left;
  var v = n.key;
  var d = n.value;
  var r = n.right;
  var c = cmp(x, v);
  if (c) {
    if (c < 0) {
      if (l !== null) {
        var match = splitAuxPivot(cmp, l, x, pres);
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
      var match$1 = splitAuxPivot(cmp, r, x, pres);
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
    var v = splitAuxPivot(cmp, n, x, pres);
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
        var match = splitAuxPivot(cmp, s2, v1, d2);
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
        var match$1 = splitAuxPivot(cmp, s1, v2, d1$1);
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

function empty(dict) {
  return {
          dict: dict,
          data: Bs_internalAVLtree.empty0
        };
}

function isEmpty(map) {
  return Bs_internalAVLtree.isEmpty0(map.data);
}

function singleton(dict, k, v) {
  return {
          dict: dict,
          data: Bs_internalAVLtree.singleton0(k, v)
        };
}

function iter(map, f) {
  return Bs_internalAVLtree.iter0(map.data, f);
}

function fold(map, acc, f) {
  return Bs_internalAVLtree.fold0(map.data, acc, f);
}

function forAll(map, f) {
  return Bs_internalAVLtree.forAll0(map.data, f);
}

function exists(map, f) {
  return Bs_internalAVLtree.exists0(map.data, f);
}

function filter(f, map) {
  var dict = map.dict;
  var map$1 = map.data;
  return {
          dict: dict,
          data: Bs_internalAVLtree.filterShared0(f, map$1)
        };
}

function partition(p, map) {
  var dict = map.dict;
  var map$1 = map.data;
  var match = Bs_internalAVLtree.partitionShared0(p, map$1);
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

function minKVOpt(m) {
  return Bs_internalAVLtree.minKVOpt0(m.data);
}

function minKVNull(m) {
  return Bs_internalAVLtree.minKVNull0(m.data);
}

function maxKVOpt(m) {
  return Bs_internalAVLtree.maxKVOpt0(m.data);
}

function maxKVNull(m) {
  return Bs_internalAVLtree.maxKVNull0(m.data);
}

function map(m, f) {
  var dict = m.dict;
  var map$1 = m.data;
  return {
          dict: dict,
          data: Bs_internalAVLtree.map0(map$1, f)
        };
}

function mapi(map, f) {
  var dict = map.dict;
  var map$1 = map.data;
  return {
          dict: dict,
          data: Bs_internalAVLtree.mapi0(map$1, f)
        };
}

function add(map, key, data) {
  var dict = map.dict;
  var map$1 = map.data;
  return {
          dict: dict,
          data: add0(map$1, key, data, dict[/* cmp */0])
        };
}

function ofArray(dict, data) {
  return {
          dict: dict,
          data: Bs_internalAVLtree.ofArray0(dict[/* cmp */0], data)
        };
}

function findOpt(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.findOpt0(map$1, x, dict[/* cmp */0]);
}

function findNull(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.findNull0(map$1, x, dict[/* cmp */0]);
}

function findWithDefault(map, x, def) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.findWithDefault0(map$1, x, def, dict[/* cmp */0]);
}

function mem(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return Bs_internalAVLtree.mem0(map$1, x, dict[/* cmp */0]);
}

function remove(map, x) {
  var dict = map.dict;
  var map$1 = map.data;
  return {
          dict: dict,
          data: remove0(map$1, x, dict[/* cmp */0])
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

var map0 = Bs_internalAVLtree.map0;

var mapi0 = Bs_internalAVLtree.mapi0;

exports.empty = empty;
exports.ofArray = ofArray;
exports.isEmpty = isEmpty;
exports.mem = mem;
exports.add = add;
exports.singleton = singleton;
exports.remove = remove;
exports.merge = merge;
exports.cmp = cmp;
exports.eq = eq;
exports.iter = iter;
exports.fold = fold;
exports.forAll = forAll;
exports.exists = exists;
exports.filter = filter;
exports.partition = partition;
exports.length = length;
exports.toList = toList;
exports.toArray = toArray;
exports.keysToArray = keysToArray;
exports.valuesToArray = valuesToArray;
exports.minKVOpt = minKVOpt;
exports.minKVNull = minKVNull;
exports.maxKVOpt = maxKVOpt;
exports.maxKVNull = maxKVNull;
exports.split = split;
exports.findOpt = findOpt;
exports.findNull = findNull;
exports.findWithDefault = findWithDefault;
exports.map = map;
exports.mapi = mapi;
exports.empty0 = empty0;
exports.ofArray0 = ofArray0;
exports.isEmpty0 = isEmpty0;
exports.mem0 = mem0;
exports.add0 = add0;
exports.singleton0 = singleton0;
exports.remove0 = remove0;
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
exports.map0 = map0;
exports.mapi0 = mapi0;
/* No side effect */
