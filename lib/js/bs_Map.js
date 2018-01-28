'use strict';

var Bs_SortedMapDict = require("./bs_SortedMapDict.js");

function ofArray(data, dict) {
  var cmp = dict[/* cmp */0];
  return {
          cmp: cmp,
          data: Bs_SortedMapDict.ofArray(data, cmp)
        };
}

function remove(m, x) {
  var cmp = m.cmp;
  var odata = m.data;
  var newData = Bs_SortedMapDict.remove(odata, x, cmp);
  if (newData === odata) {
    return m;
  } else {
    return {
            cmp: cmp,
            data: newData
          };
  }
}

function removeMany(m, x) {
  var cmp = m.cmp;
  var odata = m.data;
  var newData = Bs_SortedMapDict.removeMany(odata, x, cmp);
  if (newData === odata) {
    return m;
  } else {
    return {
            cmp: cmp,
            data: newData
          };
  }
}

function set(m, key, d) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Bs_SortedMapDict.set(m.data, key, d, cmp)
        };
}

function mergeMany(m, e) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Bs_SortedMapDict.mergeMany(m.data, e, cmp)
        };
}

function update(m, key, f) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Bs_SortedMapDict.update(m.data, key, f, cmp)
        };
}

function split(m, x) {
  var cmp = m.cmp;
  var match = Bs_SortedMapDict.split(m.data, x, cmp);
  var match$1 = match[0];
  return /* tuple */[
          /* tuple */[
            {
              cmp: cmp,
              data: match$1[0]
            },
            {
              cmp: cmp,
              data: match$1[1]
            }
          ],
          match[1]
        ];
}

function merge(s1, s2, f) {
  var cmp = s1.cmp;
  return {
          cmp: cmp,
          data: Bs_SortedMapDict.merge(s1.data, s2.data, f, cmp)
        };
}

function empty(dict) {
  return {
          cmp: dict[/* cmp */0],
          data: Bs_SortedMapDict.empty
        };
}

function isEmpty(map) {
  return Bs_SortedMapDict.isEmpty(map.data);
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
          cmp: m.cmp,
          data: Bs_SortedMapDict.keepBy(m.data, f)
        };
}

function partition(m, p) {
  var cmp = m.cmp;
  var match = Bs_SortedMapDict.partition(m.data, p);
  return /* tuple */[
          {
            cmp: cmp,
            data: match[0]
          },
          {
            cmp: cmp,
            data: match[1]
          }
        ];
}

function map(m, f) {
  return {
          cmp: m.cmp,
          data: Bs_SortedMapDict.map(m.data, f)
        };
}

function mapWithKey(m, f) {
  return {
          cmp: m.cmp,
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

function minKeyUndefined(m) {
  return Bs_SortedMapDict.minKeyUndefined(m.data);
}

function maxKey(m) {
  return Bs_SortedMapDict.maxKey(m.data);
}

function maxKeyUndefined(m) {
  return Bs_SortedMapDict.maxKeyUndefined(m.data);
}

function minimum(m) {
  return Bs_SortedMapDict.minimum(m.data);
}

function minUndefined(m) {
  return Bs_SortedMapDict.minUndefined(m.data);
}

function maximum(m) {
  return Bs_SortedMapDict.maximum(m.data);
}

function maxUndefined(m) {
  return Bs_SortedMapDict.maxUndefined(m.data);
}

function get(map, x) {
  return Bs_SortedMapDict.get(map.data, x, map.cmp);
}

function getUndefined(map, x) {
  return Bs_SortedMapDict.getUndefined(map.data, x, map.cmp);
}

function getWithDefault(map, x, def) {
  return Bs_SortedMapDict.getWithDefault(map.data, x, def, map.cmp);
}

function getExn(map, x) {
  return Bs_SortedMapDict.getExn(map.data, x, map.cmp);
}

function has(map, x) {
  return Bs_SortedMapDict.has(map.data, x, map.cmp);
}

function checkInvariantInternal(m) {
  return Bs_SortedMapDict.checkInvariantInternal(m.data);
}

function eq(m1, m2, veq) {
  return Bs_SortedMapDict.eq(m1.data, m2.data, m1.cmp, veq);
}

function cmp(m1, m2, vcmp) {
  return Bs_SortedMapDict.cmp(m1.data, m2.data, m1.cmp, vcmp);
}

function getData(prim) {
  return prim.data;
}

function getDict(m) {
  var cmp = m.cmp;
  return /* module */[/* cmp */cmp];
}

function packDictData(dict, data) {
  return {
          cmp: dict[/* cmp */0],
          data: data
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
