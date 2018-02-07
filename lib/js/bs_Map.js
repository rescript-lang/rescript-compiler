'use strict';

var Curry = require("./curry.js");
var Bs_MapDict = require("./bs_MapDict.js");

function ofArray(data, id) {
  var cmp = id[/* cmp */0];
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

function updateU(m, key, f) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Bs_MapDict.updateU(m.data, key, f, cmp)
        };
}

function update(m, key, f) {
  return updateU(m, key, Curry.__1(f));
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

function mergeU(s1, s2, f) {
  var cmp = s1.cmp;
  return {
          cmp: cmp,
          data: Bs_MapDict.mergeU(s1.data, s2.data, f, cmp)
        };
}

function merge(s1, s2, f) {
  return mergeU(s1, s2, Curry.__3(f));
}

function make(id) {
  return {
          cmp: id[/* cmp */0],
          data: Bs_MapDict.empty
        };
}

function isEmpty(map) {
  return Bs_MapDict.isEmpty(map.data);
}

function forEachU(m, f) {
  return Bs_MapDict.forEachU(m.data, f);
}

function forEach(m, f) {
  return Bs_MapDict.forEachU(m.data, Curry.__2(f));
}

function reduceU(m, acc, f) {
  return Bs_MapDict.reduceU(m.data, acc, f);
}

function reduce(m, acc, f) {
  return reduceU(m, acc, Curry.__3(f));
}

function everyU(m, f) {
  return Bs_MapDict.everyU(m.data, f);
}

function every(m, f) {
  return Bs_MapDict.everyU(m.data, Curry.__2(f));
}

function someU(m, f) {
  return Bs_MapDict.someU(m.data, f);
}

function some(m, f) {
  return Bs_MapDict.someU(m.data, Curry.__2(f));
}

function keepU(m, f) {
  return {
          cmp: m.cmp,
          data: Bs_MapDict.keepU(m.data, f)
        };
}

function keep(m, f) {
  return keepU(m, Curry.__2(f));
}

function partitionU(m, p) {
  var cmp = m.cmp;
  var match = Bs_MapDict.partitionU(m.data, p);
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

function partition(m, p) {
  return partitionU(m, Curry.__2(p));
}

function mapU(m, f) {
  return {
          cmp: m.cmp,
          data: Bs_MapDict.mapU(m.data, f)
        };
}

function map(m, f) {
  return mapU(m, Curry.__1(f));
}

function mapWithKeyU(m, f) {
  return {
          cmp: m.cmp,
          data: Bs_MapDict.mapWithKeyU(m.data, f)
        };
}

function mapWithKey(m, f) {
  return mapWithKeyU(m, Curry.__2(f));
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

function eqU(m1, m2, veq) {
  return Bs_MapDict.eqU(m1.data, m2.data, m1.cmp, veq);
}

function eq(m1, m2, veq) {
  return eqU(m1, m2, Curry.__2(veq));
}

function cmpU(m1, m2, vcmp) {
  return Bs_MapDict.cmpU(m1.data, m2.data, m1.cmp, vcmp);
}

function cmp(m1, m2, vcmp) {
  return cmpU(m1, m2, Curry.__2(vcmp));
}

function getData(prim) {
  return prim.data;
}

function getId(m) {
  var cmp = m.cmp;
  return /* module */[/* cmp */cmp];
}

function packIdData(id, data) {
  return {
          cmp: id[/* cmp */0],
          data: data
        };
}

var Int = 0;

var $$String = 0;

var Dict = 0;

exports.Int = Int;
exports.$$String = $$String;
exports.Dict = Dict;
exports.make = make;
exports.isEmpty = isEmpty;
exports.has = has;
exports.cmpU = cmpU;
exports.cmp = cmp;
exports.eqU = eqU;
exports.eq = eq;
exports.forEachU = forEachU;
exports.forEach = forEach;
exports.reduceU = reduceU;
exports.reduce = reduce;
exports.everyU = everyU;
exports.every = every;
exports.someU = someU;
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
exports.updateU = updateU;
exports.update = update;
exports.mergeU = mergeU;
exports.merge = merge;
exports.mergeMany = mergeMany;
exports.keepU = keepU;
exports.keep = keep;
exports.partitionU = partitionU;
exports.partition = partition;
exports.split = split;
exports.mapU = mapU;
exports.map = map;
exports.mapWithKeyU = mapWithKeyU;
exports.mapWithKey = mapWithKey;
exports.getId = getId;
exports.getData = getData;
exports.packIdData = packIdData;
/* No side effect */
