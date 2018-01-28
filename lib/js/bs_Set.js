'use strict';

var Bs_SortedSetDict = require("./bs_SortedSetDict.js");

function ofArray(data, dict) {
  return {
          dict: dict,
          data: Bs_SortedSetDict.ofArray(data, dict[/* cmp */0])
        };
}

function remove(m, e) {
  var M = m.dict;
  var data = m.data;
  var newData = Bs_SortedSetDict.remove(data, e, M[/* cmp */0]);
  if (newData === data) {
    return m;
  } else {
    return {
            dict: M,
            data: newData
          };
  }
}

function add(m, e) {
  var M = m.dict;
  var data = m.data;
  var newData = Bs_SortedSetDict.add(data, e, M[/* cmp */0]);
  if (newData === data) {
    return m;
  } else {
    return {
            dict: M,
            data: newData
          };
  }
}

function mergeMany(m, e) {
  var M = m.dict;
  return {
          dict: M,
          data: Bs_SortedSetDict.mergeMany(m.data, e, M[/* cmp */0])
        };
}

function removeMany(m, e) {
  var M = m.dict;
  return {
          dict: M,
          data: Bs_SortedSetDict.removeMany(m.data, e, M[/* cmp */0])
        };
}

function union(m, n) {
  var M = m.dict;
  return {
          dict: M,
          data: Bs_SortedSetDict.union(m.data, n.data, M[/* cmp */0])
        };
}

function intersect(m, n) {
  var M = m.dict;
  return {
          dict: M,
          data: Bs_SortedSetDict.intersect(m.data, n.data, M[/* cmp */0])
        };
}

function diff(m, n) {
  var M = m.dict;
  return {
          dict: M,
          data: Bs_SortedSetDict.diff(m.data, n.data, M[/* cmp */0])
        };
}

function subset(m, n) {
  var M = m.dict;
  return Bs_SortedSetDict.subset(m.data, n.data, M[/* cmp */0]);
}

function split(m, e) {
  var M = m.dict;
  var match = Bs_SortedSetDict.split(m.data, e, M[/* cmp */0]);
  var match$1 = match[0];
  return /* tuple */[
          /* tuple */[
            {
              dict: M,
              data: match$1[0]
            },
            {
              dict: M,
              data: match$1[1]
            }
          ],
          match[1]
        ];
}

function empty(dict) {
  return {
          dict: dict,
          data: Bs_SortedSetDict.empty
        };
}

function isEmpty(m) {
  return Bs_SortedSetDict.isEmpty(m.data);
}

function cmp(m, n) {
  var M = m.dict;
  return Bs_SortedSetDict.cmp(m.data, n.data, M[/* cmp */0]);
}

function eq(m, n) {
  var M = m.dict;
  return Bs_SortedSetDict.eq(m.data, n.data, M[/* cmp */0]);
}

function forEach(m, f) {
  return Bs_SortedSetDict.forEach(m.data, f);
}

function reduce(m, acc, f) {
  return Bs_SortedSetDict.reduce(m.data, acc, f);
}

function every(m, f) {
  return Bs_SortedSetDict.every(m.data, f);
}

function some(m, f) {
  return Bs_SortedSetDict.some(m.data, f);
}

function keepBy(m, f) {
  return {
          dict: m.dict,
          data: Bs_SortedSetDict.keepBy(m.data, f)
        };
}

function partition(m, f) {
  var match = Bs_SortedSetDict.partition(m.data, f);
  var dict = m.dict;
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

function size(m) {
  return Bs_SortedSetDict.size(m.data);
}

function toList(m) {
  return Bs_SortedSetDict.toList(m.data);
}

function toArray(m) {
  return Bs_SortedSetDict.toArray(m.data);
}

function minimum(m) {
  return Bs_SortedSetDict.minimum(m.data);
}

function minUndefined(m) {
  return Bs_SortedSetDict.minUndefined(m.data);
}

function maximum(m) {
  return Bs_SortedSetDict.maximum(m.data);
}

function maxUndefined(m) {
  return Bs_SortedSetDict.maxUndefined(m.data);
}

function get(m, e) {
  var M = m.dict;
  return Bs_SortedSetDict.get(m.data, e, M[/* cmp */0]);
}

function getUndefined(m, e) {
  var M = m.dict;
  return Bs_SortedSetDict.getUndefined(m.data, e, M[/* cmp */0]);
}

function getExn(m, e) {
  var M = m.dict;
  return Bs_SortedSetDict.getExn(m.data, e, M[/* cmp */0]);
}

function has(m, e) {
  var M = m.dict;
  return Bs_SortedSetDict.has(m.data, e, M[/* cmp */0]);
}

function ofSortedArrayUnsafe(xs, dict) {
  return {
          dict: dict,
          data: Bs_SortedSetDict.ofSortedArrayUnsafe(xs)
        };
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

function checkInvariantInternal(d) {
  return Bs_SortedSetDict.checkInvariantInternal(d.data);
}

exports.empty = empty;
exports.ofArray = ofArray;
exports.ofSortedArrayUnsafe = ofSortedArrayUnsafe;
exports.isEmpty = isEmpty;
exports.has = has;
exports.add = add;
exports.mergeMany = mergeMany;
exports.remove = remove;
exports.removeMany = removeMany;
exports.union = union;
exports.intersect = intersect;
exports.diff = diff;
exports.subset = subset;
exports.cmp = cmp;
exports.eq = eq;
exports.forEach = forEach;
exports.reduce = reduce;
exports.every = every;
exports.some = some;
exports.keepBy = keepBy;
exports.partition = partition;
exports.size = size;
exports.toList = toList;
exports.toArray = toArray;
exports.minimum = minimum;
exports.minUndefined = minUndefined;
exports.maximum = maximum;
exports.maxUndefined = maxUndefined;
exports.get = get;
exports.getUndefined = getUndefined;
exports.getExn = getExn;
exports.split = split;
exports.checkInvariantInternal = checkInvariantInternal;
exports.getData = getData;
exports.getDict = getDict;
exports.packDictData = packDictData;
/* No side effect */
