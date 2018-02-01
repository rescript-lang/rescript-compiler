'use strict';

var Bs_MapDict = require("./bs_MapDict.js");

function ofArray(data, dict) {
  var cmp = dict[/* cmp */0];
  return {
          cmp: cmp,
          data: Bs_MapDict.ofArray(data, cmp)
        };
}

function remove(m, x) {
  var cmp = m.cmp;
  var odata = m.data;
  var newData = Bs_MapDict.remove(odata, x, cmp);
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
  var newData = Bs_MapDict.removeMany(odata, x, cmp);
  return {
          cmp: cmp,
          data: newData
        };
}

function set(m, key, d) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Bs_MapDict.set(m.data, key, d, cmp)
        };
}

function mergeMany(m, e) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Bs_MapDict.mergeMany(m.data, e, cmp)
        };
}

function update(m, key, f) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Bs_MapDict.update(m.data, key, f, cmp)
        };
}

function split(m, x) {
  var cmp = m.cmp;
  var match = Bs_MapDict.split(m.data, x, cmp);
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
          data: Bs_MapDict.merge(s1.data, s2.data, f, cmp)
        };
}

function make(dict) {
  return {
          cmp: dict[/* cmp */0],
          data: Bs_MapDict.empty
        };
}

function isEmpty(map) {
  return Bs_MapDict.isEmpty(map.data);
}

function forEach(m, f) {
  return Bs_MapDict.forEach(m.data, f);
}

function reduce(m, acc, f) {
  return Bs_MapDict.reduce(m.data, acc, f);
}

function every(m, f) {
  return Bs_MapDict.every(m.data, f);
}

function some(m, f) {
  return Bs_MapDict.some(m.data, f);
}

function keep(m, f) {
  return {
          cmp: m.cmp,
          data: Bs_MapDict.keep(m.data, f)
        };
}

function partition(m, p) {
  var cmp = m.cmp;
  var match = Bs_MapDict.partition(m.data, p);
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
          data: Bs_MapDict.map(m.data, f)
        };
}

function mapWithKey(m, f) {
  return {
          cmp: m.cmp,
          data: Bs_MapDict.mapWithKey(m.data, f)
        };
}

function size(map) {
  return Bs_MapDict.size(map.data);
}

function toList(map) {
  return Bs_MapDict.toList(map.data);
}

function toArray(m) {
  return Bs_MapDict.toArray(m.data);
}

function keysToArray(m) {
  return Bs_MapDict.keysToArray(m.data);
}

function valuesToArray(m) {
  return Bs_MapDict.valuesToArray(m.data);
}

function minKey(m) {
  return Bs_MapDict.minKey(m.data);
}

function minKeyUndefined(m) {
  return Bs_MapDict.minKeyUndefined(m.data);
}

function maxKey(m) {
  return Bs_MapDict.maxKey(m.data);
}

function maxKeyUndefined(m) {
  return Bs_MapDict.maxKeyUndefined(m.data);
}

function minimum(m) {
  return Bs_MapDict.minimum(m.data);
}

function minUndefined(m) {
  return Bs_MapDict.minUndefined(m.data);
}

function maximum(m) {
  return Bs_MapDict.maximum(m.data);
}

function maxUndefined(m) {
  return Bs_MapDict.maxUndefined(m.data);
}

function get(map, x) {
  return Bs_MapDict.get(map.data, x, map.cmp);
}

function getUndefined(map, x) {
  return Bs_MapDict.getUndefined(map.data, x, map.cmp);
}

function getWithDefault(map, x, def) {
  return Bs_MapDict.getWithDefault(map.data, x, def, map.cmp);
}

function getExn(map, x) {
  return Bs_MapDict.getExn(map.data, x, map.cmp);
}

function has(map, x) {
  return Bs_MapDict.has(map.data, x, map.cmp);
}

function checkInvariantInternal(m) {
  return Bs_MapDict.checkInvariantInternal(m.data);
}

function eq(m1, m2, veq) {
  return Bs_MapDict.eq(m1.data, m2.data, m1.cmp, veq);
}

function cmp(m1, m2, vcmp) {
  return Bs_MapDict.cmp(m1.data, m2.data, m1.cmp, vcmp);
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

var Int = 0;

var $$String = 0;

exports.make = make;
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
exports.keep = keep;
exports.partition = partition;
exports.split = split;
exports.map = map;
exports.mapWithKey = mapWithKey;
exports.getDict = getDict;
exports.getData = getData;
exports.packDictData = packDictData;
exports.Int = Int;
exports.$$String = $$String;
/* No side effect */
