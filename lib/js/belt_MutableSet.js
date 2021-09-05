'use strict';

var Curry = require("./curry.js");
var Belt_SortArray = require("./belt_SortArray.js");
var Belt_internalAVLset = require("./belt_internalAVLset.js");

function remove0(nt, x, cmp) {
  var k = nt.v;
  var c = cmp(x, k);
  if (c === 0) {
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
  if (c < 0) {
    var l$1 = nt.l;
    if (l$1 !== undefined) {
      nt.l = remove0(l$1, x, cmp);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  }
  var r$1 = nt.r;
  if (r$1 !== undefined) {
    nt.r = remove0(r$1, x, cmp);
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
  var newRoot = remove0(oldRoot, v, d.cmp);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return ;
  }
  
}

function removeMany0(_t, xs, _i, len, cmp) {
  while(true) {
    var i = _i;
    var t = _t;
    if (i >= len) {
      return t;
    }
    var ele = xs[i];
    var u = remove0(t, ele, cmp);
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
  d.data = removeMany0(oldRoot, xs, 0, len, d.cmp);
}

function removeCheck0(nt, x, removed, cmp) {
  var k = nt.v;
  var c = cmp(x, k);
  if (c === 0) {
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
  if (c < 0) {
    var l$1 = nt.l;
    if (l$1 !== undefined) {
      nt.l = removeCheck0(l$1, x, removed, cmp);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  }
  var r$1 = nt.r;
  if (r$1 !== undefined) {
    nt.r = removeCheck0(r$1, x, removed, cmp);
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
  var newRoot = removeCheck0(oldRoot, v, removed, d.cmp);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
  }
  return removed.contents;
}

function addCheck0(t, x, added, cmp) {
  if (t !== undefined) {
    var k = t.v;
    var c = cmp(x, k);
    if (c === 0) {
      return t;
    }
    var l = t.l;
    var r = t.r;
    if (c < 0) {
      var ll = addCheck0(l, x, added, cmp);
      t.l = ll;
    } else {
      t.r = addCheck0(r, x, added, cmp);
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
  var newRoot = addCheck0(oldRoot, e, added, m.cmp);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
  }
  return added.contents;
}

function add(m, e) {
  var oldRoot = m.data;
  var newRoot = Belt_internalAVLset.addMutate(m.cmp, oldRoot, e);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
    return ;
  }
  
}

function addArrayMutate(t, xs, cmp) {
  var v = t;
  for(var i = 0 ,i_finish = xs.length; i < i_finish; ++i){
    v = Belt_internalAVLset.addMutate(cmp, v, xs[i]);
  }
  return v;
}

function mergeMany(d, xs) {
  d.data = addArrayMutate(d.data, xs, d.cmp);
}

function make(id) {
  return {
          cmp: id.cmp,
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
  Belt_internalAVLset.forEachU(d.data, f);
}

function forEach(d, f) {
  Belt_internalAVLset.forEachU(d.data, Curry.__1(f));
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

function fromSortedArrayUnsafe(xs, id) {
  return {
          cmp: id.cmp,
          data: Belt_internalAVLset.fromSortedArrayUnsafe(xs)
        };
}

function checkInvariantInternal(d) {
  Belt_internalAVLset.checkInvariantInternal(d.data);
}

function fromArray(data, id) {
  var cmp = id.cmp;
  return {
          cmp: cmp,
          data: Belt_internalAVLset.fromArray(data, cmp)
        };
}

function cmp(d0, d1) {
  return Belt_internalAVLset.cmp(d0.data, d1.data, d0.cmp);
}

function eq(d0, d1) {
  return Belt_internalAVLset.eq(d0.data, d1.data, d0.cmp);
}

function get(d, x) {
  return Belt_internalAVLset.get(d.data, x, d.cmp);
}

function getUndefined(d, x) {
  return Belt_internalAVLset.getUndefined(d.data, x, d.cmp);
}

function getExn(d, x) {
  return Belt_internalAVLset.getExn(d.data, x, d.cmp);
}

function split(d, key) {
  var arr = Belt_internalAVLset.toArray(d.data);
  var cmp = d.cmp;
  var i = Belt_SortArray.binarySearchByU(arr, key, cmp);
  var len = arr.length;
  if (i >= 0) {
    return [
            [
              {
                cmp: cmp,
                data: Belt_internalAVLset.fromSortedArrayAux(arr, 0, i)
              },
              {
                cmp: cmp,
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
              cmp: cmp,
              data: Belt_internalAVLset.fromSortedArrayAux(arr, 0, next)
            },
            {
              cmp: cmp,
              data: Belt_internalAVLset.fromSortedArrayAux(arr, next, len - next | 0)
            }
          ],
          false
        ];
}

function keepU(d, p) {
  return {
          cmp: d.cmp,
          data: Belt_internalAVLset.keepCopyU(d.data, p)
        };
}

function keep(d, p) {
  return keepU(d, Curry.__1(p));
}

function partitionU(d, p) {
  var cmp = d.cmp;
  var match = Belt_internalAVLset.partitionCopyU(d.data, p);
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

function partition(d, p) {
  return partitionU(d, Curry.__1(p));
}

function subset(a, b) {
  return Belt_internalAVLset.subset(a.data, b.data, a.cmp);
}

function intersect(a, b) {
  var cmp = a.cmp;
  var match = a.data;
  var match$1 = b.data;
  if (match === undefined) {
    return {
            cmp: cmp,
            data: undefined
          };
  }
  if (match$1 === undefined) {
    return {
            cmp: cmp,
            data: undefined
          };
  }
  var sizea = Belt_internalAVLset.lengthNode(match);
  var sizeb = Belt_internalAVLset.lengthNode(match$1);
  var totalSize = sizea + sizeb | 0;
  var tmp = new Array(totalSize);
  Belt_internalAVLset.fillArray(match, 0, tmp);
  Belt_internalAVLset.fillArray(match$1, sizea, tmp);
  if (cmp(tmp[sizea - 1 | 0], tmp[sizea]) < 0 || cmp(tmp[totalSize - 1 | 0], tmp[0]) < 0) {
    return {
            cmp: cmp,
            data: undefined
          };
  }
  var tmp2 = new Array(sizea < sizeb ? sizea : sizeb);
  var k = Belt_SortArray.intersectU(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0, cmp);
  return {
          cmp: cmp,
          data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
        };
}

function diff(a, b) {
  var cmp = a.cmp;
  var dataa = a.data;
  var match = b.data;
  if (dataa === undefined) {
    return {
            cmp: cmp,
            data: undefined
          };
  }
  if (match === undefined) {
    return {
            cmp: cmp,
            data: Belt_internalAVLset.copy(dataa)
          };
  }
  var sizea = Belt_internalAVLset.lengthNode(dataa);
  var sizeb = Belt_internalAVLset.lengthNode(match);
  var totalSize = sizea + sizeb | 0;
  var tmp = new Array(totalSize);
  Belt_internalAVLset.fillArray(dataa, 0, tmp);
  Belt_internalAVLset.fillArray(match, sizea, tmp);
  if (cmp(tmp[sizea - 1 | 0], tmp[sizea]) < 0 || cmp(tmp[totalSize - 1 | 0], tmp[0]) < 0) {
    return {
            cmp: cmp,
            data: Belt_internalAVLset.copy(dataa)
          };
  }
  var tmp2 = new Array(sizea);
  var k = Belt_SortArray.diffU(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0, cmp);
  return {
          cmp: cmp,
          data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
        };
}

function union(a, b) {
  var cmp = a.cmp;
  var dataa = a.data;
  var datab = b.data;
  if (dataa === undefined) {
    return {
            cmp: cmp,
            data: Belt_internalAVLset.copy(datab)
          };
  }
  if (datab === undefined) {
    return {
            cmp: cmp,
            data: Belt_internalAVLset.copy(dataa)
          };
  }
  var sizea = Belt_internalAVLset.lengthNode(dataa);
  var sizeb = Belt_internalAVLset.lengthNode(datab);
  var totalSize = sizea + sizeb | 0;
  var tmp = new Array(totalSize);
  Belt_internalAVLset.fillArray(dataa, 0, tmp);
  Belt_internalAVLset.fillArray(datab, sizea, tmp);
  if (cmp(tmp[sizea - 1 | 0], tmp[sizea]) < 0) {
    return {
            cmp: cmp,
            data: Belt_internalAVLset.fromSortedArrayAux(tmp, 0, totalSize)
          };
  }
  var tmp2 = new Array(totalSize);
  var k = Belt_SortArray.unionU(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0, cmp);
  return {
          cmp: cmp,
          data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
        };
}

function has(d, x) {
  return Belt_internalAVLset.has(d.data, x, d.cmp);
}

function copy(d) {
  return {
          cmp: d.cmp,
          data: Belt_internalAVLset.copy(d.data)
        };
}

var Int;

var $$String;

exports.Int = Int;
exports.$$String = $$String;
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
