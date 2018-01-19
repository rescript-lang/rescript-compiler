'use strict';

var Bs_Sort = require("./bs_Sort.js");
var Bs_internalAVLset = require("./bs_internalAVLset.js");

function removeMutateAux(nt, x, cmp) {
  var k = nt.key;
  var c = cmp(x, k);
  if (c) {
    if (c < 0) {
      var match = nt.left;
      if (match !== null) {
        nt.left = removeMutateAux(match, x, cmp);
        return Bs_internalAVLset.balMutate(nt);
      } else {
        return nt;
      }
    } else {
      var match$1 = nt.right;
      if (match$1 !== null) {
        nt.right = removeMutateAux(match$1, x, cmp);
        return Bs_internalAVLset.balMutate(nt);
      } else {
        return nt;
      }
    }
  } else {
    var l = nt.left;
    var r = nt.right;
    if (l !== null) {
      if (r !== null) {
        nt.right = Bs_internalAVLset.removeMinAuxWithRootMutate(nt, r);
        return Bs_internalAVLset.balMutate(nt);
      } else {
        return l;
      }
    } else if (r !== null) {
      return r;
    } else {
      return l;
    }
  }
}

function removeDone(d, v) {
  var dict = d.dict;
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var newRoot = removeMutateAux(oldRoot, v, dict[/* cmp */0]);
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

function remove(d, v) {
  removeDone(d, v);
  return d;
}

function removeArrayMutateAux(_t, xs, _i, len, cmp) {
  while(true) {
    var i = _i;
    var t = _t;
    if (i < len) {
      var ele = xs[i];
      var u = removeMutateAux(t, ele, cmp);
      if (u !== null) {
        _i = i + 1 | 0;
        _t = u;
        continue ;
        
      } else {
        return Bs_internalAVLset.empty0;
      }
    } else {
      return t;
    }
  };
}

function removeArrayDone(d, xs) {
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var len = xs.length;
    var dict = d.dict;
    var newRoot = removeArrayMutateAux(oldRoot, xs, 0, len, dict[/* cmp */0]);
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

function removeArray(d, xs) {
  removeArrayDone(d, xs);
  return d;
}

function removeMutateCheckAux(nt, x, removed, cmp) {
  var k = nt.key;
  var c = cmp(x, k);
  if (c) {
    if (c < 0) {
      var match = nt.left;
      if (match !== null) {
        nt.left = removeMutateCheckAux(match, x, removed, cmp);
        return Bs_internalAVLset.balMutate(nt);
      } else {
        return nt;
      }
    } else {
      var match$1 = nt.right;
      if (match$1 !== null) {
        nt.right = removeMutateCheckAux(match$1, x, removed, cmp);
        return Bs_internalAVLset.balMutate(nt);
      } else {
        return nt;
      }
    }
  } else {
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
    } else if (r !== null) {
      return r;
    } else {
      return l;
    }
  }
}

function removeCheck(d, v) {
  var dict = d.dict;
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var removed = [/* false */0];
    var newRoot = removeMutateCheckAux(oldRoot, v, removed, dict[/* cmp */0]);
    if (newRoot !== oldRoot) {
      d.data = newRoot;
    }
    return removed[0];
  } else {
    return /* false */0;
  }
}

function addMutateCheckAux(t, x, added, cmp) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(x, k);
    if (c) {
      var l = t.left;
      var r = t.right;
      if (c < 0) {
        var ll = addMutateCheckAux(l, x, added, cmp);
        t.left = ll;
      } else {
        t.right = addMutateCheckAux(r, x, added, cmp);
      }
      return Bs_internalAVLset.balMutate(t);
    } else {
      return t;
    }
  } else {
    added[0] = /* true */1;
    return Bs_internalAVLset.singleton0(x);
  }
}

function addCheck(m, e) {
  var dict = m.dict;
  var oldRoot = m.data;
  var added = [/* false */0];
  var newRoot = addMutateCheckAux(oldRoot, e, added, dict[/* cmp */0]);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
  }
  return added[0];
}

function split(d, key) {
  var dict = d.dict;
  var s = d.data;
  var arr = Bs_internalAVLset.toArray0(s);
  var i = Bs_Sort.binSearch(arr, key, dict[/* cmp */0]);
  var len = arr.length;
  if (i < 0) {
    var next = (-i | 0) - 1 | 0;
    return /* tuple */[
            /* tuple */[
              {
                dict: dict,
                data: Bs_internalAVLset.ofSortedArrayAux(arr, 0, next)
              },
              {
                dict: dict,
                data: Bs_internalAVLset.ofSortedArrayAux(arr, next, len - next | 0)
              }
            ],
            /* false */0
          ];
  } else {
    return /* tuple */[
            /* tuple */[
              {
                dict: dict,
                data: Bs_internalAVLset.ofSortedArrayAux(arr, 0, i)
              },
              {
                dict: dict,
                data: Bs_internalAVLset.ofSortedArrayAux(arr, i + 1 | 0, (len - i | 0) - 1 | 0)
              }
            ],
            /* true */1
          ];
  }
}

function filter(d, p) {
  var data = d.data;
  var dict = d.dict;
  return {
          dict: dict,
          data: Bs_internalAVLset.filterCopy(data, p)
        };
}

function partition(d, p) {
  var data = d.data;
  var dict = d.dict;
  var match = Bs_internalAVLset.partitionCopy(data, p);
  return /* tuple */[
          {
            dict: dict,
            data: match[0]
          },
          {
            dict: dict,
            data: match[1]
          }
        ];
}

function empty(dict) {
  return {
          dict: dict,
          data: Bs_internalAVLset.empty0
        };
}

function isEmpty(d) {
  return Bs_internalAVLset.isEmpty0(d.data);
}

function singleton(x, dict) {
  return {
          dict: dict,
          data: Bs_internalAVLset.singleton0(x)
        };
}

function minimum(d) {
  return Bs_internalAVLset.minOpt0(d.data);
}

function minNull(d) {
  return Bs_internalAVLset.minNull0(d.data);
}

function maximum(d) {
  return Bs_internalAVLset.maxOpt0(d.data);
}

function maxNull(d) {
  return Bs_internalAVLset.maxNull0(d.data);
}

function forEach(d, f) {
  return Bs_internalAVLset.iter0(d.data, f);
}

function fold(d, acc, cb) {
  return Bs_internalAVLset.fold0(d.data, acc, cb);
}

function forAll(d, p) {
  return Bs_internalAVLset.forAll0(d.data, p);
}

function exists(d, p) {
  return Bs_internalAVLset.exists0(d.data, p);
}

function size(d) {
  return Bs_internalAVLset.length0(d.data);
}

function toList(d) {
  return Bs_internalAVLset.toList0(d.data);
}

function toArray(d) {
  return Bs_internalAVLset.toArray0(d.data);
}

function ofSortedArrayUnsafe(xs, dict) {
  return {
          dict: dict,
          data: Bs_internalAVLset.ofSortedArrayUnsafe0(xs)
        };
}

function checkInvariant(d) {
  return Bs_internalAVLset.checkInvariant(d.data);
}

function cmp(d0, d1) {
  var M = d0.dict;
  return Bs_internalAVLset.cmp0(d0.data, d1.data, M[/* cmp */0]);
}

function eq(d0, d1) {
  var M = d0.dict;
  return Bs_internalAVLset.eq0(M[/* cmp */0], d0.data, d1.data);
}

function get(d, x) {
  var M = d.dict;
  return Bs_internalAVLset.findOpt0(M[/* cmp */0], d.data, x);
}

function getNull(d, x) {
  var M = d.dict;
  return Bs_internalAVLset.findNull0(M[/* cmp */0], d.data, x);
}

function getExn(d, x) {
  var dict = d.dict;
  return Bs_internalAVLset.findExn0(dict[/* cmp */0], d.data, x);
}

function has(d, x) {
  var dict = d.dict;
  return Bs_internalAVLset.mem0(dict[/* cmp */0], d.data, x);
}

function ofArray(data, dict) {
  return {
          dict: dict,
          data: Bs_internalAVLset.ofArray0(dict[/* cmp */0], data)
        };
}

function addDone(m, e) {
  var dict = m.dict;
  var oldRoot = m.data;
  var newRoot = Bs_internalAVLset.addMutate(dict[/* cmp */0], oldRoot, e);
  if (newRoot !== oldRoot) {
    m.data = newRoot;
    return /* () */0;
  } else {
    return 0;
  }
}

function add(m, e) {
  addDone(m, e);
  return m;
}

function addArrayMutate(t, xs, cmp) {
  var v = t;
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    v = Bs_internalAVLset.addMutate(cmp, v, xs[i]);
  }
  return v;
}

function mergeArrayDone(d, xs) {
  var dict = d.dict;
  var oldRoot = d.data;
  var newRoot = addArrayMutate(oldRoot, xs, dict[/* cmp */0]);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return /* () */0;
  } else {
    return 0;
  }
}

function mergeArray(d, xs) {
  mergeArrayDone(d, xs);
  return d;
}

function subset(a, b) {
  var dict = a.dict;
  return Bs_internalAVLset.subset0(dict[/* cmp */0], a.data, b.data);
}

function inter(a, b) {
  var dict = a.dict;
  var dataa = a.data;
  var datab = b.data;
  if (dataa !== null) {
    if (datab !== null) {
      var sizea = Bs_internalAVLset.lengthNode(dataa);
      var sizeb = Bs_internalAVLset.lengthNode(datab);
      var totalSize = sizea + sizeb | 0;
      var tmp = new Array(totalSize);
      Bs_internalAVLset.fillArray(dataa, 0, tmp);
      Bs_internalAVLset.fillArray(datab, sizea, tmp);
      var p = dict[/* cmp */0];
      if (p(tmp[sizea - 1 | 0], tmp[sizea]) < 0 || p(tmp[totalSize - 1 | 0], tmp[0]) < 0) {
        return {
                dict: dict,
                data: Bs_internalAVLset.empty0
              };
      } else {
        var tmp2 = new Array(sizea < sizeb ? sizea : sizeb);
        var k = Bs_Sort.inter(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0, p);
        return {
                dict: dict,
                data: Bs_internalAVLset.ofSortedArrayAux(tmp2, 0, k)
              };
      }
    } else {
      return {
              dict: dict,
              data: Bs_internalAVLset.empty0
            };
    }
  } else {
    return {
            dict: dict,
            data: Bs_internalAVLset.empty0
          };
  }
}

function diff(a, b) {
  var dict = a.dict;
  var dataa = a.data;
  var datab = b.data;
  if (dataa !== null) {
    if (datab !== null) {
      var sizea = Bs_internalAVLset.lengthNode(dataa);
      var sizeb = Bs_internalAVLset.lengthNode(datab);
      var totalSize = sizea + sizeb | 0;
      var tmp = new Array(totalSize);
      Bs_internalAVLset.fillArray(dataa, 0, tmp);
      Bs_internalAVLset.fillArray(datab, sizea, tmp);
      var p = dict[/* cmp */0];
      if (p(tmp[sizea - 1 | 0], tmp[sizea]) < 0 || p(tmp[totalSize - 1 | 0], tmp[0]) < 0) {
        return {
                dict: dict,
                data: Bs_internalAVLset.copy(dataa)
              };
      } else {
        var tmp2 = new Array(sizea);
        var k = Bs_Sort.diff(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0, p);
        return {
                dict: dict,
                data: Bs_internalAVLset.ofSortedArrayAux(tmp2, 0, k)
              };
      }
    } else {
      return {
              dict: dict,
              data: Bs_internalAVLset.copy(dataa)
            };
    }
  } else {
    return {
            dict: dict,
            data: Bs_internalAVLset.empty0
          };
  }
}

function union(a, b) {
  var dict = a.dict;
  var dataa = a.data;
  var datab = b.data;
  if (dataa !== null) {
    if (datab !== null) {
      var sizea = Bs_internalAVLset.lengthNode(dataa);
      var sizeb = Bs_internalAVLset.lengthNode(datab);
      var totalSize = sizea + sizeb | 0;
      var tmp = new Array(totalSize);
      Bs_internalAVLset.fillArray(dataa, 0, tmp);
      Bs_internalAVLset.fillArray(datab, sizea, tmp);
      var p = dict[/* cmp */0];
      if (p(tmp[sizea - 1 | 0], tmp[sizea]) < 0) {
        return {
                dict: dict,
                data: Bs_internalAVLset.ofSortedArrayAux(tmp, 0, totalSize)
              };
      } else {
        var tmp2 = new Array(totalSize);
        var k = Bs_Sort.union(tmp, 0, sizea, tmp, sizea, sizeb, tmp2, 0, p);
        return {
                dict: dict,
                data: Bs_internalAVLset.ofSortedArrayAux(tmp2, 0, k)
              };
      }
    } else {
      return {
              dict: dict,
              data: Bs_internalAVLset.copy(dataa)
            };
    }
  } else {
    return {
            dict: dict,
            data: Bs_internalAVLset.copy(datab)
          };
  }
}

exports.empty = empty;
exports.singleton = singleton;
exports.ofArray = ofArray;
exports.ofSortedArrayUnsafe = ofSortedArrayUnsafe;
exports.isEmpty = isEmpty;
exports.has = has;
exports.addDone = addDone;
exports.add = add;
exports.addCheck = addCheck;
exports.mergeArrayDone = mergeArrayDone;
exports.mergeArray = mergeArray;
exports.removeDone = removeDone;
exports.remove = remove;
exports.removeCheck = removeCheck;
exports.removeArrayDone = removeArrayDone;
exports.removeArray = removeArray;
exports.union = union;
exports.inter = inter;
exports.diff = diff;
exports.subset = subset;
exports.cmp = cmp;
exports.eq = eq;
exports.forEach = forEach;
exports.fold = fold;
exports.forAll = forAll;
exports.exists = exists;
exports.filter = filter;
exports.partition = partition;
exports.size = size;
exports.toList = toList;
exports.toArray = toArray;
exports.minimum = minimum;
exports.minNull = minNull;
exports.maximum = maximum;
exports.maxNull = maxNull;
exports.get = get;
exports.getNull = getNull;
exports.getExn = getExn;
exports.split = split;
exports.checkInvariant = checkInvariant;
/* No side effect */
