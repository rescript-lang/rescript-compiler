'use strict';

var Curry = require("./curry.js");
var Belt_internalAVLset = require("./belt_internalAVLset.js");
var Belt_SortArrayString = require("./belt_SortArrayString.js");
var Belt_internalSetString = require("./belt_internalSetString.js");

function remove0(nt, x) {
  var k = nt.value;
  if (x === k) {
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
  } else if (x < k) {
    var match = nt.left;
    if (match !== null) {
      nt.left = remove0(match, x);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  } else {
    var match$1 = nt.right;
    if (match$1 !== null) {
      nt.right = remove0(match$1, x);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  }
}

function remove(d, v) {
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var newRoot = remove0(oldRoot, v);
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

function removeMany0(_t, xs, _i, len) {
  while(true) {
    var i = _i;
    var t = _t;
    if (i < len) {
      var ele = xs[i];
      var u = remove0(t, ele);
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
    d.data = removeMany0(oldRoot, xs, 0, len);
    return /* () */0;
  } else {
    return /* () */0;
  }
}

function removeCheck0(nt, x, removed) {
  var k = nt.value;
  if (x === k) {
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
  } else if (x < k) {
    var match = nt.left;
    if (match !== null) {
      nt.left = removeCheck0(match, x, removed);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  } else {
    var match$1 = nt.right;
    if (match$1 !== null) {
      nt.right = removeCheck0(match$1, x, removed);
      return Belt_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  }
}

function removeCheck(d, v) {
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var removed = [false];
    var newRoot = removeCheck0(oldRoot, v, removed);
    if (newRoot !== oldRoot) {
      d.data = newRoot;
    }
    return removed[0];
  } else {
    return false;
  }
}

function addCheck0(t, x, added) {
  if (t !== null) {
    var k = t.value;
    if (x === k) {
      return t;
    } else {
      var l = t.left;
      var r = t.right;
      if (x < k) {
        var ll = addCheck0(l, x, added);
        t.left = ll;
      } else {
        t.right = addCheck0(r, x, added);
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
  var added = [false];
  var newRoot = addCheck0(oldRoot, e, added);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
  }
  return added[0];
}

function add(d, k) {
  var oldRoot = d.data;
  var v = Belt_internalSetString.addMutate(oldRoot, k);
  if (v !== oldRoot) {
    d.data = v;
    return /* () */0;
  } else {
    return 0;
  }
}

function addArrayMutate(t, xs) {
  var v = t;
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    v = Belt_internalSetString.addMutate(v, xs[i]);
  }
  return v;
}

function mergeMany(d, arr) {
  d.data = addArrayMutate(d.data, arr);
  return /* () */0;
}

function make() {
  return {
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
          data: Belt_internalSetString.fromArray(xs)
        };
}

function cmp(d0, d1) {
  return Belt_internalSetString.cmp(d0.data, d1.data);
}

function eq(d0, d1) {
  return Belt_internalSetString.eq(d0.data, d1.data);
}

function get(d, x) {
  return Belt_internalSetString.get(d.data, x);
}

function getUndefined(d, x) {
  return Belt_internalSetString.getUndefined(d.data, x);
}

function getExn(d, x) {
  return Belt_internalSetString.getExn(d.data, x);
}

function split(d, key) {
  var arr = Belt_internalAVLset.toArray(d.data);
  var i = Belt_SortArrayString.binarySearch(arr, key);
  var len = arr.length;
  if (i < 0) {
    var next = (-i | 0) - 1 | 0;
    return /* tuple */[
            /* tuple */[
              {
                data: Belt_internalAVLset.fromSortedArrayAux(arr, 0, next)
              },
              {
                data: Belt_internalAVLset.fromSortedArrayAux(arr, next, len - next | 0)
              }
            ],
            false
          ];
  } else {
    return /* tuple */[
            /* tuple */[
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
  return /* tuple */[
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
  return Belt_internalSetString.subset(a.data, b.data);
}

function intersect(dataa, datab) {
  var dataa$1 = dataa.data;
  var datab$1 = datab.data;
  if (dataa$1 !== null) {
    if (datab$1 !== null) {
      var sizea = Belt_internalAVLset.lengthNode(dataa$1);
      var sizeb = Belt_internalAVLset.lengthNode(datab$1);
      var totalSize = sizea + sizeb | 0;
      var tmp = new Array(totalSize);
      Belt_internalAVLset.fillArray(dataa$1, 0, tmp);
      Belt_internalAVLset.fillArray(datab$1, sizea, tmp);
      if (tmp[sizea - 1 | 0] < tmp[sizea] || tmp[totalSize - 1 | 0] < tmp[0]) {
        return {
                data: Belt_internalAVLset.empty
              };
      } else {
        var tmp2 = new Array(sizea < sizeb ? sizea : sizeb);
        var k = Belt_SortArrayString.intersect(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0);
        return {
                data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
              };
      }
    } else {
      return {
              data: Belt_internalAVLset.empty
            };
    }
  } else {
    return {
            data: Belt_internalAVLset.empty
          };
  }
}

function diff(dataa, datab) {
  var dataa$1 = dataa.data;
  var datab$1 = datab.data;
  if (dataa$1 !== null) {
    if (datab$1 !== null) {
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
      } else {
        var tmp2 = new Array(sizea);
        var k = Belt_SortArrayString.diff(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0);
        return {
                data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
              };
      }
    } else {
      return {
              data: Belt_internalAVLset.copy(dataa$1)
            };
    }
  } else {
    return {
            data: Belt_internalAVLset.empty
          };
  }
}

function union(dataa, datab) {
  var dataa$1 = dataa.data;
  var datab$1 = datab.data;
  if (dataa$1 !== null) {
    if (datab$1 !== null) {
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
      } else {
        var tmp2 = new Array(totalSize);
        var k = Belt_SortArrayString.union(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0);
        return {
                data: Belt_internalAVLset.fromSortedArrayAux(tmp2, 0, k)
              };
      }
    } else {
      return {
              data: Belt_internalAVLset.copy(dataa$1)
            };
    }
  } else {
    return {
            data: Belt_internalAVLset.copy(datab$1)
          };
  }
}

function has(d, x) {
  return Belt_internalSetString.has(d.data, x);
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
