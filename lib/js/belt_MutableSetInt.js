'use strict';

var Curry = require("./curry.js");
var Belt_SortArrayInt = require("./belt_SortArrayInt.js");
var Belt_internalAVLset = require("./belt_internalAVLset.js");
var Belt_internalSetInt = require("./belt_internalSetInt.js");

function remove0(nt, x) {
  var k = nt.v;
  if (x === k) {
    var l = nt.l;
    var r = nt.r;
    if (l !== undefined) {
      if (r !== undefined) {
        nt.r = Belt_internalAVLset.removeMinAuxWithRootMutate(nt, r);
        return Belt_internalAVLset.balMutate(nt);
      } else {
        return l;
      }
    } else {
      return r;
    }
  }
  if (x < k) {
    var l$1 = nt.l;
    if (l$1 !== undefined) {
      nt.l = remove0(l$1, x);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  }
  var r$1 = nt.r;
  if (r$1 !== undefined) {
    nt.r = remove0(r$1, x);
    return Belt_internalAVLset.balMutate(nt);
  } else {
    return nt;
  }
}

function remove(d, v) {
  var oldRoot = d.data;
  if (oldRoot === undefined) {
    return ;
  }
  var newRoot = remove0(oldRoot, v);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return ;
  }
  
}

function removeMany0(_t, xs, _i, len) {
  while(true) {
    var i = _i;
    var t = _t;
    if (i >= len) {
      return t;
    }
    var ele = xs[i];
    var u = remove0(t, ele);
    if (u === undefined) {
      return ;
    }
    _i = i + 1 | 0;
    _t = u;
    continue ;
  };
}

function removeMany(d, xs) {
  var oldRoot = d.data;
  if (oldRoot === undefined) {
    return ;
  }
  var len = xs.length;
  d.data = removeMany0(oldRoot, xs, 0, len);
  
}

function removeCheck0(nt, x, removed) {
  var k = nt.v;
  if (x === k) {
    removed.contents = true;
    var l = nt.l;
    var r = nt.r;
    if (l !== undefined) {
      if (r !== undefined) {
        nt.r = Belt_internalAVLset.removeMinAuxWithRootMutate(nt, r);
        return Belt_internalAVLset.balMutate(nt);
      } else {
        return l;
      }
    } else {
      return r;
    }
  }
  if (x < k) {
    var l$1 = nt.l;
    if (l$1 !== undefined) {
      nt.l = removeCheck0(l$1, x, removed);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  }
  var r$1 = nt.r;
  if (r$1 !== undefined) {
    nt.r = removeCheck0(r$1, x, removed);
    return Belt_internalAVLset.balMutate(nt);
  } else {
    return nt;
  }
}

function removeCheck(d, v) {
  var oldRoot = d.data;
  if (oldRoot === undefined) {
    return false;
  }
  var removed = {
    contents: false
  };
  var newRoot = removeCheck0(oldRoot, v, removed);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
  }
  return removed.contents;
}

function addCheck0(t, x, added) {
  if (t !== undefined) {
    var k = t.v;
    if (x === k) {
      return t;
    }
    var l = t.l;
    var r = t.r;
    if (x < k) {
      var ll = addCheck0(l, x, added);
      t.l = ll;
    } else {
      t.r = addCheck0(r, x, added);
    }
    return Belt_internalAVLset.balMutate(t);
  }
  added.contents = true;
  return Belt_internalAVLset.singleton(x);
}

function addCheck(m, e) {
  var oldRoot = m.data;
  var added = {
    contents: false
  };
  var newRoot = addCheck0(oldRoot, e, added);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
  }
  return added.contents;
}

function add(d, k) {
  var oldRoot = d.data;
  var v = Belt_internalSetInt.addMutate(oldRoot, k);
  if (v !== oldRoot) {
    d.data = v;
    return ;
  }
  
}

function addArrayMutate(t, xs) {
  var v = t;
  for(var i = 0 ,i_finish = xs.length; i < i_finish; ++i){
    v = Belt_internalSetInt.addMutate(v, xs[i]);
  }
  return v;
}

function mergeMany(d, arr) {
  d.data = addArrayMutate(d.data, arr);
  
}

function make(param) {
  return {
          data: undefined
        };
}

function isEmpty(d) {
  var n = d.data;
  return n === undefined;
}

function minimum(d) {
  return Belt_internalAVLset.minimum(d.data);
}

function minUndefined(d) {
  return Belt_internalAVLset.minUndefined(d.data);
}

function maximum(d) {
  return Belt_internalAVLset.maximum(d.data);
}

function maxUndefined(d) {
  return Belt_internalAVLset.maxUndefined(d.data);
}

function forEachU(d, f) {
  return Belt_internalAVLset.forEachU(d.data, f);
}

function forEach(d, f) {
  return Belt_internalAVLset.forEachU(d.data, Curry.__1(f));
}

function reduceU(d, acc, cb) {
  return Belt_internalAVLset.reduceU(d.data, acc, cb);
}

function reduce(d, acc, cb) {
  return reduceU(d, acc, Curry.__2(cb));
}

function everyU(d, p) {
  return Belt_internalAVLset.everyU(d.data, p);
}

function every(d, p) {
  return Belt_internalAVLset.everyU(d.data, Curry.__1(p));
}

function someU(d, p) {
  return Belt_internalAVLset.someU(d.data, p);
}

function some(d, p) {
  return Belt_internalAVLset.someU(d.data, Curry.__1(p));
}

function size(d) {
  return Belt_internalAVLset.size(d.data);
}

function toList(d) {
  return Belt_internalAVLset.toList(d.data);
}

function toArray(d) {
  return Belt_internalAVLset.toArray(d.data);
}

function fromSortedArrayUnsafe(xs) {
  return {
          data: Belt_internalAVLset.fromSortedArrayUnsafe(xs)
        };
}

function checkInvariantInternal(d) {
  return Belt_internalAVLset.checkInvariantInternal(d.data);
}

function fromArray(xs) {
  return {
          data: Belt_internalSetInt.fromArray(xs)
        };
}

function cmp(d0, d1) {
  return Belt_internalSetInt.cmp(d0.data, d1.data);
}

function eq(d0, d1) {
  return Belt_internalSetInt.eq(d0.data, d1.data);
}

function get(d, x) {
  return Belt_internalSetInt.get(d.data, x);
}

function getUndefined(d, x) {
  return Belt_internalSetInt.getUndefined(d.data, x);
}

function getExn(d, x) {
  return Belt_internalSetInt.getExn(d.data, x);
}

function split(d, key) {
  var arr = Belt_internalAVLset.toArray(d.data);
  var i = Belt_SortArrayInt.binarySearch(arr, key);
  var len = arr.length;
  if (i >= 0) {
    return [
            [
              {
                data: Belt_internalAVLset.fromSortedArrayAux(arr, 0, i)
              },
              {
                data: Belt_internalAVLset.fromSortedArrayAux(arr, i + 1 | 0, (len - i | 0) - 1 | 0)
              }
            ],
            true
          ];
  }
  var next = (-i | 0) - 1 | 0;
  return [
          [
            {
              data: Belt_internalAVLset.fromSortedArrayAux(arr, 0, next)
            },
            {
              data: Belt_internalAVLset.fromSortedArrayAux(arr, next, len - next | 0)
            }
          ],
          false
        ];
}

function keepU(d, p) {
  return {
          data: Belt_internalAVLset.keepCopyU(d.data, p)
        };
}

function keep(d, p) {
  return keepU(d, Curry.__1(p));
}

function partitionU(d, p) {
  var match = Belt_internalAVLset.partitionCopyU(d.data, p);
  return [
          {
            data: match[0]
          },
          {
            data: match[1]
          }
        ];
}

function partition(d, p) {
  return partitionU(d, Curry.__1(p));
}

function subset(a, b) {
  return Belt_internalSetInt.subset(a.data, b.data);
}

function intersect(dataa, datab) {
  var dataa$1 = dataa.data;
  var datab$1 = datab.data;
  if (dataa$1 === undefined) {
    return {
            data: undefined
          };
  }
  if (datab$1 === undefined) {
    return {
            data: undefined
          };
  }
  var sizea = Belt_internalAVLset.lengthNode(dataa$1);
  var sizeb = Belt_internalAVLset.lengthNode(datab$1);
  var totalSize = sizea + sizeb | 0;
  var tmp = new Array(totalSize);
  Belt_internalAVLset.fillArray(dataa$1, 0, tmp);
  Belt_internalAVLset.fillArray(datab$1, sizea, tmp);
  if (tmp[sizea - 1 | 0] < tmp[sizea] || tmp[totalSize - 1 | 0] < tmp[0]) {
    return {
            data: undefined
          };
  }
  var tmp2 = new Array(sizea < sizeb ? sizea : sizeb);
  var k = Belt_SortArrayInt.intersect(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0);
  return {
          data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
        };
}

function diff(dataa, datab) {
  var dataa$1 = dataa.data;
  var datab$1 = datab.data;
  if (dataa$1 === undefined) {
    return {
            data: undefined
          };
  }
  if (datab$1 === undefined) {
    return {
            data: Belt_internalAVLset.copy(dataa$1)
          };
  }
  var sizea = Belt_internalAVLset.lengthNode(dataa$1);
  var sizeb = Belt_internalAVLset.lengthNode(datab$1);
  var totalSize = sizea + sizeb | 0;
  var tmp = new Array(totalSize);
  Belt_internalAVLset.fillArray(dataa$1, 0, tmp);
  Belt_internalAVLset.fillArray(datab$1, sizea, tmp);
  if (tmp[sizea - 1 | 0] < tmp[sizea] || tmp[totalSize - 1 | 0] < tmp[0]) {
    return {
            data: Belt_internalAVLset.copy(dataa$1)
          };
  }
  var tmp2 = new Array(sizea);
  var k = Belt_SortArrayInt.diff(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0);
  return {
          data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
        };
}

function union(dataa, datab) {
  var dataa$1 = dataa.data;
  var datab$1 = datab.data;
  if (dataa$1 === undefined) {
    return {
            data: Belt_internalAVLset.copy(datab$1)
          };
  }
  if (datab$1 === undefined) {
    return {
            data: Belt_internalAVLset.copy(dataa$1)
          };
  }
  var sizea = Belt_internalAVLset.lengthNode(dataa$1);
  var sizeb = Belt_internalAVLset.lengthNode(datab$1);
  var totalSize = sizea + sizeb | 0;
  var tmp = new Array(totalSize);
  Belt_internalAVLset.fillArray(dataa$1, 0, tmp);
  Belt_internalAVLset.fillArray(datab$1, sizea, tmp);
  if (tmp[sizea - 1 | 0] < tmp[sizea]) {
    return {
            data: Belt_internalAVLset.fromSortedArrayAux(tmp, 0, totalSize)
          };
  }
  var tmp2 = new Array(totalSize);
  var k = Belt_SortArrayInt.union(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0);
  return {
          data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
        };
}

function has(d, x) {
  return Belt_internalSetInt.has(d.data, x);
}

function copy(d) {
  return {
          data: Belt_internalAVLset.copy(d.data)
        };
}

exports.make = make;
exports.fromArray = fromArray;
exports.fromSortedArrayUnsafe = fromSortedArrayUnsafe;
exports.copy = copy;
exports.isEmpty = isEmpty;
exports.has = has;
exports.add = add;
exports.addCheck = addCheck;
exports.mergeMany = mergeMany;
exports.remove = remove;
exports.removeCheck = removeCheck;
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
/* No side effect */
