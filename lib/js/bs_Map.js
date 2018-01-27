'use strict';

var Bs_SortedMapDict = require("./bs_SortedMapDict.js");

function ofArray(data, dict) {
  return {
          dict: dict,
          data: Bs_SortedMapDict.ofArray(data, dict[/* cmp */0])
        };
}

function remove(m, x) {
  var M = m.dict;
  var odata = m.data;
  var newData = Bs_SortedMapDict.remove(odata, x, M[/* cmp */0]);
  if (newData === odata) {
    return m;
  } else {
    return {
            dict: M,
            data: newData
          };
  }
}

function removeMany(m, x) {
  var M = m.dict;
  var odata = m.data;
  var newData = Bs_SortedMapDict.removeMany(odata, x, M[/* cmp */0]);
  if (newData === odata) {
    return m;
  } else {
    return {
            dict: M,
            data: newData
          };
  }
}

function set(m, key, d) {
  var X = m.dict;
  return {
          dict: X,
          data: Bs_SortedMapDict.set(m.data, key, d, X[/* cmp */0])
        };
}

function mergeMany(m, e) {
  var M = m.dict;
  return {
          dict: M,
          data: Bs_SortedMapDict.mergeMany(m.data, e, M[/* cmp */0])
        };
}

function update(m, key, f) {
  var X = m.dict;
  return {
          dict: X,
          data: Bs_SortedMapDict.update(m.data, key, f, X[/* cmp */0])
        };
}

function split(m, x) {
  var M = m.dict;
  var match = Bs_SortedMapDict.split(m.data, x, M[/* cmp */0]);
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

function merge(s1, s2, f) {
  var X = s1.dict;
  return {
          dict: X,
          data: Bs_SortedMapDict.merge(s1.data, s2.data, f, X[/* cmp */0])
        };
}

function empty(dict) {
  return {
          dict: dict,
          data: Bs_SortedMapDict.empty
        };
}

function isEmpty(map) {
  return Bs_SortedMapDict.isEmpty(map.data);
}

function cmp(m1, m2, cmp$1) {
  var X = m1.dict;
  return Bs_SortedMapDict.cmp(m1.data, m2.data, X[/* cmp */0], cmp$1);
}

function eq(m1, m2, cmp) {
  var X = m1.dict;
  return Bs_SortedMapDict.eq(m1.data, m2.data, X[/* cmp */0], cmp);
}

function forEach(m, f) {
  return Bs_SortedMapDict.forEach(m.data, f);
}

function reduce(m, acc, f) {
  return Bs_SortedMapDict.reduce(m.data, acc, f);
}

function every(m, f) {
  return Bs_SortedMapDict.every(m.data, f);
}

function some(m, f) {
  return Bs_SortedMapDict.some(m.data, f);
}

function keepBy(m, f) {
  return {
          dict: m.dict,
          data: Bs_SortedMapDict.keepBy(m.data, f)
        };
}

function partition(m, p) {
  var dict = m.dict;
  var match = Bs_SortedMapDict.partition(m.data, p);
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
  return {
          dict: m.dict,
          data: Bs_SortedMapDict.map(m.data, f)
        };
}

function mapWithKey(m, f) {
  return {
          dict: m.dict,
          data: Bs_SortedMapDict.mapWithKey(m.data, f)
        };
}

function size(map) {
  return Bs_SortedMapDict.size(map.data);
}

function toList(map) {
  return Bs_SortedMapDict.toList(map.data);
}

function toArray(m) {
  return Bs_SortedMapDict.toArray(m.data);
}

function keysToArray(m) {
  return Bs_SortedMapDict.keysToArray(m.data);
}

function valuesToArray(m) {
  return Bs_SortedMapDict.valuesToArray(m.data);
}

function minKey(m) {
  return Bs_SortedMapDict.minKey(m.data);
}

function minKeyNull(m) {
  return Bs_SortedMapDict.minKeyNull(m.data);
}

function maxKey(m) {
  return Bs_SortedMapDict.maxKey(m.data);
}

function maxKeyNull(m) {
  return Bs_SortedMapDict.maxKeyNull(m.data);
}

function minimum(m) {
  return Bs_SortedMapDict.minimum(m.data);
}

function minNull(m) {
  return Bs_SortedMapDict.minNull(m.data);
}

function maximum(m) {
  return Bs_SortedMapDict.maximum(m.data);
}

function maxNull(m) {
  return Bs_SortedMapDict.maxNull(m.data);
}

function get(map, x) {
  var X = map.dict;
  return Bs_SortedMapDict.get(map.data, x, X[/* cmp */0]);
}

function getNull(map, x) {
  var X = map.dict;
  return Bs_SortedMapDict.getNull(map.data, x, X[/* cmp */0]);
}

function getWithDefault(map, x, def) {
  var X = map.dict;
  return Bs_SortedMapDict.getWithDefault(map.data, x, def, X[/* cmp */0]);
}

function getExn(map, x) {
  var X = map.dict;
  return Bs_SortedMapDict.getExn(map.data, x, X[/* cmp */0]);
}

function has(map, x) {
  var X = map.dict;
  return Bs_SortedMapDict.has(map.data, x, X[/* cmp */0]);
}

function checkInvariantInternal(m) {
  return Bs_SortedMapDict.checkInvariantInternal(m.data);
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
exports.checkInvariantInternal = checkInvariantInternal;
exports.remove = remove;
exports.removeMany = removeMany;
exports.set = set;
exports.update = update;
exports.merge = merge;
exports.mergeMany = mergeMany;
exports.keepBy = keepBy;
exports.partition = partition;
exports.split = split;
exports.map = map;
exports.mapWithKey = mapWithKey;
exports.getDict = getDict;
exports.getData = getData;
exports.packDictData = packDictData;
/* No side effect */
