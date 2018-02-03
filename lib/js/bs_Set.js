'use strict';

var Curry = require("./curry.js");
var Bs_SetDict = require("./bs_SetDict.js");

function ofArray(data, dict) {
  var cmp = dict[/* cmp */0];
  return {
          cmp: cmp,
          data: Bs_SetDict.ofArray(data, cmp)
        };
}

function remove(m, e) {
  var cmp = m.cmp;
  var data = m.data;
  var newData = Bs_SetDict.remove(data, e, cmp);
  if (newData === data) {
    return m;
  } else {
    return {
            cmp: cmp,
            data: newData
          };
  }
}

function add(m, e) {
  var cmp = m.cmp;
  var data = m.data;
  var newData = Bs_SetDict.add(data, e, cmp);
  if (newData === data) {
    return m;
  } else {
    return {
            cmp: cmp,
            data: newData
          };
  }
}

function mergeMany(m, e) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Bs_SetDict.mergeMany(m.data, e, cmp)
        };
}

function removeMany(m, e) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Bs_SetDict.removeMany(m.data, e, cmp)
        };
}

function union(m, n) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Bs_SetDict.union(m.data, n.data, cmp)
        };
}

function intersect(m, n) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Bs_SetDict.intersect(m.data, n.data, cmp)
        };
}

function diff(m, n) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Bs_SetDict.diff(m.data, n.data, cmp)
        };
}

function subset(m, n) {
  var cmp = m.cmp;
  return Bs_SetDict.subset(m.data, n.data, cmp);
}

function split(m, e) {
  var cmp = m.cmp;
  var match = Bs_SetDict.split(m.data, e, cmp);
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

function make(dict) {
  return {
          cmp: dict[/* cmp */0],
          data: Bs_SetDict.empty
        };
}

function isEmpty(m) {
  return Bs_SetDict.isEmpty(m.data);
}

function cmp(m, n) {
  var cmp$1 = m.cmp;
  return Bs_SetDict.cmp(m.data, n.data, cmp$1);
}

function eq(m, n) {
  return Bs_SetDict.eq(m.data, n.data, m.cmp);
}

function forEachU(m, f) {
  return Bs_SetDict.forEachU(m.data, f);
}

function forEach(m, f) {
  return Bs_SetDict.forEachU(m.data, Curry.__1(f));
}

function reduceU(m, acc, f) {
  return Bs_SetDict.reduceU(m.data, acc, f);
}

function reduce(m, acc, f) {
  return reduceU(m, acc, Curry.__2(f));
}

function everyU(m, f) {
  return Bs_SetDict.everyU(m.data, f);
}

function every(m, f) {
  return Bs_SetDict.everyU(m.data, Curry.__1(f));
}

function someU(m, f) {
  return Bs_SetDict.someU(m.data, f);
}

function some(m, f) {
  return Bs_SetDict.someU(m.data, Curry.__1(f));
}

function keepU(m, f) {
  return {
          cmp: m.cmp,
          data: Bs_SetDict.keepU(m.data, f)
        };
}

function keep(m, f) {
  return keepU(m, Curry.__1(f));
}

function partitionU(m, f) {
  var match = Bs_SetDict.partitionU(m.data, f);
  var cmp = m.cmp;
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

function partition(m, f) {
  return partitionU(m, Curry.__1(f));
}

function size(m) {
  return Bs_SetDict.size(m.data);
}

function toList(m) {
  return Bs_SetDict.toList(m.data);
}

function toArray(m) {
  return Bs_SetDict.toArray(m.data);
}

function minimum(m) {
  return Bs_SetDict.minimum(m.data);
}

function minUndefined(m) {
  return Bs_SetDict.minUndefined(m.data);
}

function maximum(m) {
  return Bs_SetDict.maximum(m.data);
}

function maxUndefined(m) {
  return Bs_SetDict.maxUndefined(m.data);
}

function get(m, e) {
  return Bs_SetDict.get(m.data, e, m.cmp);
}

function getUndefined(m, e) {
  return Bs_SetDict.getUndefined(m.data, e, m.cmp);
}

function getExn(m, e) {
  return Bs_SetDict.getExn(m.data, e, m.cmp);
}

function has(m, e) {
  return Bs_SetDict.has(m.data, e, m.cmp);
}

function ofSortedArrayUnsafe(xs, dict) {
  return {
          cmp: dict[/* cmp */0],
          data: Bs_SetDict.ofSortedArrayUnsafe(xs)
        };
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

function checkInvariantInternal(d) {
  return Bs_SetDict.checkInvariantInternal(d.data);
}

var Int = 0;

var $$String = 0;

var Dict = 0;

exports.Int = Int;
exports.$$String = $$String;
exports.Dict = Dict;
exports.make = make;
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
exports.forEachU = forEachU;
exports.forEach = forEach;
exports.reduceU = reduceU;
exports.reduce = reduce;
exports.everyU = everyU;
exports.every = every;
exports.someU = someU;
exports.some = some;
exports.keepU = keepU;
exports.keep = keep;
exports.partitionU = partitionU;
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
