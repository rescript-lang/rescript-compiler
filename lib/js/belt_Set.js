'use strict';

var Curry = require("./curry.js");
var Belt_SetDict = require("./belt_SetDict.js");

function fromArray(data, id) {
  var cmp = id.cmp;
  return {
          cmp: cmp,
          data: Belt_SetDict.fromArray(data, cmp)
        };
}

function remove(m, e) {
  var data = m.data;
  var cmp = m.cmp;
  var newData = Belt_SetDict.remove(data, e, cmp);
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
  var data = m.data;
  var cmp = m.cmp;
  var newData = Belt_SetDict.add(data, e, cmp);
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
          data: Belt_SetDict.mergeMany(m.data, e, cmp)
        };
}

function removeMany(m, e) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Belt_SetDict.removeMany(m.data, e, cmp)
        };
}

function union(m, n) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Belt_SetDict.union(m.data, n.data, cmp)
        };
}

function intersect(m, n) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Belt_SetDict.intersect(m.data, n.data, cmp)
        };
}

function diff(m, n) {
  var cmp = m.cmp;
  return {
          cmp: cmp,
          data: Belt_SetDict.diff(m.data, n.data, cmp)
        };
}

function subset(m, n) {
  var cmp = m.cmp;
  return Belt_SetDict.subset(m.data, n.data, cmp);
}

function split(m, e) {
  var cmp = m.cmp;
  var match = Belt_SetDict.split(m.data, e, cmp);
  var match$1 = match[0];
  return [
          [
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

function make(id) {
  return {
          cmp: id.cmp,
          data: undefined
        };
}

function isEmpty(m) {
  return Belt_SetDict.isEmpty(m.data);
}

function cmp(m, n) {
  var cmp$1 = m.cmp;
  return Belt_SetDict.cmp(m.data, n.data, cmp$1);
}

function eq(m, n) {
  return Belt_SetDict.eq(m.data, n.data, m.cmp);
}

function forEachU(m, f) {
  Belt_SetDict.forEachU(m.data, f);
}

function forEach(m, f) {
  Belt_SetDict.forEachU(m.data, Curry.__1(f));
}

function reduceU(m, acc, f) {
  return Belt_SetDict.reduceU(m.data, acc, f);
}

function reduce(m, acc, f) {
  return reduceU(m, acc, Curry.__2(f));
}

function everyU(m, f) {
  return Belt_SetDict.everyU(m.data, f);
}

function every(m, f) {
  return Belt_SetDict.everyU(m.data, Curry.__1(f));
}

function someU(m, f) {
  return Belt_SetDict.someU(m.data, f);
}

function some(m, f) {
  return Belt_SetDict.someU(m.data, Curry.__1(f));
}

function keepU(m, f) {
  return {
          cmp: m.cmp,
          data: Belt_SetDict.keepU(m.data, f)
        };
}

function keep(m, f) {
  return keepU(m, Curry.__1(f));
}

function partitionU(m, f) {
  var match = Belt_SetDict.partitionU(m.data, f);
  var cmp = m.cmp;
  return [
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
  return Belt_SetDict.size(m.data);
}

function toList(m) {
  return Belt_SetDict.toList(m.data);
}

function toArray(m) {
  return Belt_SetDict.toArray(m.data);
}

function minimum(m) {
  return Belt_SetDict.minimum(m.data);
}

function minUndefined(m) {
  return Belt_SetDict.minUndefined(m.data);
}

function maximum(m) {
  return Belt_SetDict.maximum(m.data);
}

function maxUndefined(m) {
  return Belt_SetDict.maxUndefined(m.data);
}

function get(m, e) {
  return Belt_SetDict.get(m.data, e, m.cmp);
}

function getUndefined(m, e) {
  return Belt_SetDict.getUndefined(m.data, e, m.cmp);
}

function getExn(m, e) {
  return Belt_SetDict.getExn(m.data, e, m.cmp);
}

function has(m, e) {
  return Belt_SetDict.has(m.data, e, m.cmp);
}

function fromSortedArrayUnsafe(xs, id) {
  return {
          cmp: id.cmp,
          data: Belt_SetDict.fromSortedArrayUnsafe(xs)
        };
}

function getData(m) {
  return m.data;
}

function getId(m) {
  var cmp = m.cmp;
  return {
          cmp: cmp
        };
}

function packIdData(id, data) {
  return {
          cmp: id.cmp,
          data: data
        };
}

function checkInvariantInternal(d) {
  Belt_SetDict.checkInvariantInternal(d.data);
}

var Int;

var $$String;

var Dict;

exports.Int = Int;
exports.$$String = $$String;
exports.Dict = Dict;
exports.make = make;
exports.fromArray = fromArray;
exports.fromSortedArrayUnsafe = fromSortedArrayUnsafe;
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
exports.toArray = toArray;
exports.toList = toList;
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
exports.getId = getId;
exports.packIdData = packIdData;
/* No side effect */
