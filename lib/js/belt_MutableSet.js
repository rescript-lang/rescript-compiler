'use strict';

var Curry = require("./curry.js");
var Belt_SortArray = require("./belt_SortArray.js");
var Belt_internalAVLset = require("./belt_internalAVLset.js");

function remove0(nt, x, cmp) {
  var k = nt.value;
  var c = cmp(x, k);
  if (c === 0) {
    var l = nt.left;
    var r = nt.right;
    if (l !== null) {
      if (r !== null) {
        nt.right = Belt_internalAVLset.removeMinAuxWithRootMutate(nt, r);
        return Belt_internalAVLset.balMutate(nt);
      } else {
        return l;
      }
    } else {
      return r;
    }
  } else if (c < 0) {
    var match = nt.left;
    if (match !== null) {
      nt.left = remove0(match, x, cmp);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  } else {
    var match$1 = nt.right;
    if (match$1 !== null) {
      nt.right = remove0(match$1, x, cmp);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  }
}

function remove(d, v) {
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var newRoot = remove0(oldRoot, v, d.cmp);
    if (newRoot !== oldRoot) {
      d.data = newRoot;
      return /* () */0;
    } else {
      return 0;
    }
  } else {
    return /* () */0;
  }
}

function removeMany0(_t, xs, _i, len, cmp) {
  while(true) {
    var i = _i;
    var t = _t;
    if (i < len) {
      var ele = xs[i];
      var u = remove0(t, ele, cmp);
      if (u !== null) {
        _i = i + 1 | 0;
        _t = u;
        continue ;
      } else {
        return Belt_internalAVLset.empty;
      }
    } else {
      return t;
    }
  };
}

function removeMany(d, xs) {
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var len = xs.length;
    d.data = removeMany0(oldRoot, xs, 0, len, d.cmp);
    return /* () */0;
  } else {
    return /* () */0;
  }
}

function removeCheck0(nt, x, removed, cmp) {
  var k = nt.value;
  var c = cmp(x, k);
  if (c === 0) {
    removed[0] = true;
    var l = nt.left;
    var r = nt.right;
    if (l !== null) {
      if (r !== null) {
        nt.right = Belt_internalAVLset.removeMinAuxWithRootMutate(nt, r);
        return Belt_internalAVLset.balMutate(nt);
      } else {
        return l;
      }
    } else {
      return r;
    }
  } else if (c < 0) {
    var match = nt.left;
    if (match !== null) {
      nt.left = removeCheck0(match, x, removed, cmp);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  } else {
    var match$1 = nt.right;
    if (match$1 !== null) {
      nt.right = removeCheck0(match$1, x, removed, cmp);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  }
}

function removeCheck(d, v) {
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var removed = /* record */[/* contents */false];
    var newRoot = removeCheck0(oldRoot, v, removed, d.cmp);
    if (newRoot !== oldRoot) {
      d.data = newRoot;
    }
    return removed[0];
  } else {
    return false;
  }
}

function addCheck0(t, x, added, cmp) {
  if (t !== null) {
    var k = t.value;
    var c = cmp(x, k);
    if (c === 0) {
      return t;
    } else {
      var l = t.left;
      var r = t.right;
      if (c < 0) {
        var ll = addCheck0(l, x, added, cmp);
        t.left = ll;
      } else {
        t.right = addCheck0(r, x, added, cmp);
      }
      return Belt_internalAVLset.balMutate(t);
    }
  } else {
    added[0] = true;
    return Belt_internalAVLset.singleton(x);
  }
}

function addCheck(m, e) {
  var oldRoot = m.data;
  var added = /* record */[/* contents */false];
  var newRoot = addCheck0(oldRoot, e, added, m.cmp);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
  }
  return added[0];
}

function add(m, e) {
  var oldRoot = m.data;
  var newRoot = Belt_internalAVLset.addMutate(m.cmp, oldRoot, e);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
    return /* () */0;
  } else {
    return 0;
  }
}

function addArrayMutate(t, xs, cmp) {
  var v = t;
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    v = Belt_internalAVLset.addMutate(cmp, v, xs[i]);
  }
  return v;
}

function mergeMany(d, xs) {
  d.data = addArrayMutate(d.data, xs, d.cmp);
  return /* () */0;
}

function make(id) {
  return {
          cmp: id[/* cmp */0],
          data: Belt_internalAVLset.empty
        };
}

function isEmpty(d) {
  var n = d.data;
  return n === null;
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

function fromSortedArrayUnsafe(xs, id) {
  return {
          cmp: id[/* cmp */0],
          data: Belt_internalAVLset.fromSortedArrayUnsafe(xs)
        };
}

function checkInvariantInternal(d) {
  return Belt_internalAVLset.checkInvariantInternal(d.data);
}

function fromArray(data, id) {
  var cmp = id[/* cmp */0];
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
  if (i < 0) {
    var next = (-i | 0) - 1 | 0;
    return /* tuple */[
            /* tuple */[
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
  } else {
    return /* tuple */[
            /* tuple */[
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
  if (match !== null) {
    if (match$1 !== null) {
      var sizea = Belt_internalAVLset.lengthNode(match);
      var sizeb = Belt_internalAVLset.lengthNode(match$1);
      var totalSize = sizea + sizeb | 0;
      var tmp = new Array(totalSize);
      Belt_internalAVLset.fillArray(match, 0, tmp);
      Belt_internalAVLset.fillArray(match$1, sizea, tmp);
      if (cmp(tmp[sizea - 1 | 0], tmp[sizea]) < 0 || cmp(tmp[totalSize - 1 | 0], tmp[0]) < 0) {
        return {
                cmp: cmp,
                data: Belt_internalAVLset.empty
              };
      } else {
        var tmp2 = new Array(sizea < sizeb ? sizea : sizeb);
        var k = Belt_SortArray.intersectU(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0, cmp);
        return {
                cmp: cmp,
                data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
              };
      }
    } else {
      return {
              cmp: cmp,
              data: Belt_internalAVLset.empty
            };
    }
  } else {
    return {
            cmp: cmp,
            data: Belt_internalAVLset.empty
          };
  }
}

function diff(a, b) {
  var cmp = a.cmp;
  var dataa = a.data;
  var match = b.data;
  if (dataa !== null) {
    if (match !== null) {
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
      } else {
        var tmp2 = new Array(sizea);
        var k = Belt_SortArray.diffU(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0, cmp);
        return {
                cmp: cmp,
                data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
              };
      }
    } else {
      return {
              cmp: cmp,
              data: Belt_internalAVLset.copy(dataa)
            };
    }
  } else {
    return {
            cmp: cmp,
            data: Belt_internalAVLset.empty
          };
  }
}

function union(a, b) {
  var cmp = a.cmp;
  var dataa = a.data;
  var datab = b.data;
  if (dataa !== null) {
    if (datab !== null) {
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
      } else {
        var tmp2 = new Array(totalSize);
        var k = Belt_SortArray.unionU(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0, cmp);
        return {
                cmp: cmp,
                data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
              };
      }
    } else {
      return {
              cmp: cmp,
              data: Belt_internalAVLset.copy(dataa)
            };
    }
  } else {
    return {
            cmp: cmp,
            data: Belt_internalAVLset.copy(datab)
          };
  }
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

var Int = 0;

var $$String = 0;

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
