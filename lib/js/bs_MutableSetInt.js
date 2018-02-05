'use strict';

var Curry = require("./curry.js");
var Bs_SortArrayInt = require("./bs_SortArrayInt.js");
var Bs_internalAVLset = require("./bs_internalAVLset.js");
var Bs_internalSetInt = require("./bs_internalSetInt.js");

function remove0(nt, x) {
  var k = nt.key;
  if (x === k) {
    var l = nt.left;
    var r = nt.right;
    if (l !== null) {
      if (r !== null) {
        nt.right = Bs_internalAVLset.removeMinAuxWithRootMutate(nt, r);
        return Bs_internalAVLset.balMutate(nt);
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
      return Bs_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  } else {
    var match$1 = nt.right;
    if (match$1 !== null) {
      nt.right = remove0(match$1, x);
      return Bs_internalAVLset.balMutate(nt);
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
        return Bs_internalAVLset.empty;
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
  var k = nt.key;
  if (x === k) {
    removed[0] = /* true */1;
    var l = nt.left;
    var r = nt.right;
    if (l !== null) {
      if (r !== null) {
        nt.right = Bs_internalAVLset.removeMinAuxWithRootMutate(nt, r);
        return Bs_internalAVLset.balMutate(nt);
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
      return Bs_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  } else {
    var match$1 = nt.right;
    if (match$1 !== null) {
      nt.right = removeCheck0(match$1, x, removed);
      return Bs_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  }
}

function removeCheck(d, v) {
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var removed = [/* false */0];
    var newRoot = removeCheck0(oldRoot, v, removed);
    if (newRoot !== oldRoot) {
      d.data = newRoot;
    }
    return removed[0];
  } else {
    return /* false */0;
  }
}

function addCheck0(t, x, added) {
  if (t !== null) {
    var k = t.key;
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
      return Bs_internalAVLset.balMutate(t);
    }
  } else {
    added[0] = /* true */1;
    return Bs_internalAVLset.singleton(x);
  }
}

function addCheck(m, e) {
  var oldRoot = m.data;
  var added = [/* false */0];
  var newRoot = addCheck0(oldRoot, e, added);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
  }
  return added[0];
}

function add(d, k) {
  var oldRoot = d.data;
  var v = Bs_internalSetInt.addMutate(oldRoot, k);
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
    v = Bs_internalSetInt.addMutate(v, xs[i]);
  }
  return v;
}

function mergeMany(d, arr) {
  d.data = addArrayMutate(d.data, arr);
  return /* () */0;
}

function make() {
  return {
          data: Bs_internalAVLset.empty
        };
}

function isEmpty(d) {
  return Bs_internalAVLset.isEmpty(d.data);
}

function minimum(d) {
  return Bs_internalAVLset.minimum(d.data);
}

function minUndefined(d) {
  return Bs_internalAVLset.minUndefined(d.data);
}

function maximum(d) {
  return Bs_internalAVLset.maximum(d.data);
}

function maxUndefined(d) {
  return Bs_internalAVLset.maxUndefined(d.data);
}

function forEachU(d, f) {
  return Bs_internalAVLset.forEachU(d.data, f);
}

function forEach(d, f) {
  return Bs_internalAVLset.forEachU(d.data, Curry.__1(f));
}

function reduceU(d, acc, cb) {
  return Bs_internalAVLset.reduceU(d.data, acc, cb);
}

function reduce(d, acc, cb) {
  return reduceU(d, acc, Curry.__2(cb));
}

function everyU(d, p) {
  return Bs_internalAVLset.everyU(d.data, p);
}

function every(d, p) {
  return Bs_internalAVLset.everyU(d.data, Curry.__1(p));
}

function someU(d, p) {
  return Bs_internalAVLset.someU(d.data, p);
}

function some(d, p) {
  return Bs_internalAVLset.someU(d.data, Curry.__1(p));
}

function size(d) {
  return Bs_internalAVLset.size(d.data);
}

function toList(d) {
  return Bs_internalAVLset.toList(d.data);
}

function toArray(d) {
  return Bs_internalAVLset.toArray(d.data);
}

function ofSortedArrayUnsafe(xs) {
  return {
          data: Bs_internalAVLset.ofSortedArrayUnsafe(xs)
        };
}

function checkInvariantInternal(d) {
  return Bs_internalAVLset.checkInvariantInternal(d.data);
}

function ofArray(xs) {
  return {
          data: Bs_internalSetInt.ofArray(xs)
        };
}

function cmp(d0, d1) {
  return Bs_internalSetInt.cmp(d0.data, d1.data);
}

function eq(d0, d1) {
  return Bs_internalSetInt.eq(d0.data, d1.data);
}

function get(d, x) {
  return Bs_internalSetInt.get(d.data, x);
}

function getUndefined(d, x) {
  return Bs_internalSetInt.getUndefined(d.data, x);
}

function getExn(d, x) {
  return Bs_internalSetInt.getExn(d.data, x);
}

function split(d, key) {
  var arr = Bs_internalAVLset.toArray(d.data);
  var i = Bs_SortArrayInt.binarySearch(arr, key);
  var len = arr.length;
  if (i < 0) {
    var next = (-i | 0) - 1 | 0;
    return /* tuple */[
            /* tuple */[
              {
                data: Bs_internalAVLset.ofSortedArrayAux(arr, 0, next)
              },
              {
                data: Bs_internalAVLset.ofSortedArrayAux(arr, next, len - next | 0)
              }
            ],
            /* false */0
          ];
  } else {
    return /* tuple */[
            /* tuple */[
              {
                data: Bs_internalAVLset.ofSortedArrayAux(arr, 0, i)
              },
              {
                data: Bs_internalAVLset.ofSortedArrayAux(arr, i + 1 | 0, (len - i | 0) - 1 | 0)
              }
            ],
            /* true */1
          ];
  }
}

function keepU(d, p) {
  return {
          data: Bs_internalAVLset.keepCopyU(d.data, p)
        };
}

function keep(d, p) {
  return keepU(d, Curry.__1(p));
}

function partitionU(d, p) {
  var match = Bs_internalAVLset.partitionCopyU(d.data, p);
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
  return Bs_internalSetInt.subset(a.data, b.data);
}

function intersect(dataa, datab) {
  var dataa$1 = dataa.data;
  var datab$1 = datab.data;
  if (dataa$1 !== null) {
    if (datab$1 !== null) {
      var sizea = Bs_internalAVLset.lengthNode(dataa$1);
      var sizeb = Bs_internalAVLset.lengthNode(datab$1);
      var totalSize = sizea + sizeb | 0;
      var tmp = new Array(totalSize);
      Bs_internalAVLset.fillArray(dataa$1, 0, tmp);
      Bs_internalAVLset.fillArray(datab$1, sizea, tmp);
      if (tmp[sizea - 1 | 0] < tmp[sizea] || tmp[totalSize - 1 | 0] < tmp[0]) {
        return {
                data: Bs_internalAVLset.empty
              };
      } else {
        var tmp2 = new Array(sizea < sizeb ? sizea : sizeb);
        var k = Bs_SortArrayInt.intersect(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0);
        return {
                data: Bs_internalAVLset.ofSortedArrayAux(tmp2, 0, k)
              };
      }
    } else {
      return {
              data: Bs_internalAVLset.empty
            };
    }
  } else {
    return {
            data: Bs_internalAVLset.empty
          };
  }
}

function diff(dataa, datab) {
  var dataa$1 = dataa.data;
  var datab$1 = datab.data;
  if (dataa$1 !== null) {
    if (datab$1 !== null) {
      var sizea = Bs_internalAVLset.lengthNode(dataa$1);
      var sizeb = Bs_internalAVLset.lengthNode(datab$1);
      var totalSize = sizea + sizeb | 0;
      var tmp = new Array(totalSize);
      Bs_internalAVLset.fillArray(dataa$1, 0, tmp);
      Bs_internalAVLset.fillArray(datab$1, sizea, tmp);
      if (tmp[sizea - 1 | 0] < tmp[sizea] || tmp[totalSize - 1 | 0] < tmp[0]) {
        return {
                data: Bs_internalAVLset.copy(dataa$1)
              };
      } else {
        var tmp2 = new Array(sizea);
        var k = Bs_SortArrayInt.diff(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0);
        return {
                data: Bs_internalAVLset.ofSortedArrayAux(tmp2, 0, k)
              };
      }
    } else {
      return {
              data: Bs_internalAVLset.copy(dataa$1)
            };
    }
  } else {
    return {
            data: Bs_internalAVLset.empty
          };
  }
}

function union(dataa, datab) {
  var dataa$1 = dataa.data;
  var datab$1 = datab.data;
  if (dataa$1 !== null) {
    if (datab$1 !== null) {
      var sizea = Bs_internalAVLset.lengthNode(dataa$1);
      var sizeb = Bs_internalAVLset.lengthNode(datab$1);
      var totalSize = sizea + sizeb | 0;
      var tmp = new Array(totalSize);
      Bs_internalAVLset.fillArray(dataa$1, 0, tmp);
      Bs_internalAVLset.fillArray(datab$1, sizea, tmp);
      if (tmp[sizea - 1 | 0] < tmp[sizea]) {
        return {
                data: Bs_internalAVLset.ofSortedArrayAux(tmp, 0, totalSize)
              };
      } else {
        var tmp2 = new Array(totalSize);
        var k = Bs_SortArrayInt.union(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0);
        return {
                data: Bs_internalAVLset.ofSortedArrayAux(tmp2, 0, k)
              };
      }
    } else {
      return {
              data: Bs_internalAVLset.copy(dataa$1)
            };
    }
  } else {
    return {
            data: Bs_internalAVLset.copy(datab$1)
          };
  }
}

function has(d, x) {
  return Bs_internalSetInt.has(d.data, x);
}

function copy(d) {
  return {
          data: Bs_internalAVLset.copy(d.data)
        };
}

var I = 0;

var S = 0;

var N = 0;

var A = 0;

exports.I = I;
exports.S = S;
exports.N = N;
exports.A = A;
exports.remove0 = remove0;
exports.remove = remove;
exports.removeMany0 = removeMany0;
exports.removeMany = removeMany;
exports.removeCheck0 = removeCheck0;
exports.removeCheck = removeCheck;
exports.addCheck0 = addCheck0;
exports.addCheck = addCheck;
exports.add = add;
exports.addArrayMutate = addArrayMutate;
exports.mergeMany = mergeMany;
exports.make = make;
exports.isEmpty = isEmpty;
exports.minimum = minimum;
exports.minUndefined = minUndefined;
exports.maximum = maximum;
exports.maxUndefined = maxUndefined;
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
exports.ofSortedArrayUnsafe = ofSortedArrayUnsafe;
exports.checkInvariantInternal = checkInvariantInternal;
exports.ofArray = ofArray;
exports.cmp = cmp;
exports.eq = eq;
exports.get = get;
exports.getUndefined = getUndefined;
exports.getExn = getExn;
exports.split = split;
exports.keepU = keepU;
exports.keep = keep;
exports.partitionU = partitionU;
exports.partition = partition;
exports.subset = subset;
exports.intersect = intersect;
exports.diff = diff;
exports.union = union;
exports.has = has;
exports.copy = copy;
/* No side effect */
