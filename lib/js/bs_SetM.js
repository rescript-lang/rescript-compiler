'use strict';

var Bs_Sort = require("./bs_Sort.js");
var Bs_internalAVLset = require("./bs_internalAVLset.js");

function removeMutateAux(cmp, nt, x) {
  var k = nt.key;
  var c = cmp(x, k);
  if (c) {
    if (c < 0) {
      var match = nt.left;
      if (match !== null) {
        nt.left = removeMutateAux(cmp, match, x);
        return Bs_internalAVLset.balMutate(nt);
      } else {
        return nt;
      }
    } else {
      var match$1 = nt.right;
      if (match$1 !== null) {
        nt.right = removeMutateAux(cmp, match$1, x);
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

function addArrayMutate(t, xs, cmp) {
  var v = t;
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    v = Bs_internalAVLset.addMutate(cmp, v, xs[i]);
  }
  return v;
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

function removeArrayMutate(t, xs, cmp) {
  if (t !== null) {
    var len = xs.length;
    var _t = t;
    var xs$1 = xs;
    var _i = 0;
    var len$1 = len;
    var cmp$1 = cmp;
    while(true) {
      var i = _i;
      var t$1 = _t;
      if (i < len$1) {
        var ele = xs$1[i];
        var u = removeMutateAux(cmp$1, t$1, ele);
        if (u !== null) {
          _i = i + 1 | 0;
          _t = u;
          continue ;
          
        } else {
          return Bs_internalAVLset.empty0;
        }
      } else {
        return t$1;
      }
    };
  } else {
    return t;
  }
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

function empty(dict) {
  return {
          dict: dict,
          data: Bs_internalAVLset.empty0
        };
}

function isEmpty(d) {
  return Bs_internalAVLset.isEmpty0(d.data);
}

function singleton(dict, x) {
  return {
          dict: dict,
          data: Bs_internalAVLset.singleton0(x)
        };
}

function minOpt(d) {
  return Bs_internalAVLset.minOpt0(d.data);
}

function minNull(d) {
  return Bs_internalAVLset.minNull0(d.data);
}

function maxOpt(d) {
  return Bs_internalAVLset.maxOpt0(d.data);
}

function maxNull(d) {
  return Bs_internalAVLset.maxNull0(d.data);
}

function iter(d, f) {
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

function length(d) {
  return Bs_internalAVLset.length0(d.data);
}

function toList(d) {
  return Bs_internalAVLset.toList0(d.data);
}

function toArray(d) {
  return Bs_internalAVLset.toArray0(d.data);
}

function ofSortedArrayUnsafe(dict, xs) {
  return {
          dict: dict,
          data: Bs_internalAVLset.ofSortedArrayUnsafe0(xs)
        };
}

function addOnly(m, e) {
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
  addOnly(m, e);
  return m;
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

function addArrayOnly(d, xs) {
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

function addArray(d, xs) {
  addArrayOnly(d, xs);
  return d;
}

function removeArrayOnly(d, xs) {
  var dict = d.dict;
  var oldRoot = d.data;
  var newRoot = removeArrayMutate(oldRoot, xs, dict[/* cmp */0]);
  if (newRoot !== oldRoot) {
    d.data = newRoot;
    return /* () */0;
  } else {
    return 0;
  }
}

function removeArray(d, xs) {
  removeArrayOnly(d, xs);
  return d;
}

function removeOnly(d, v) {
  var dict = d.dict;
  var oldRoot = d.data;
  if (oldRoot !== null) {
    var newRoot = removeMutateAux(dict[/* cmp */0], oldRoot, v);
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

function remove(d, v) {
  removeOnly(d, v);
  return d;
}

function cmp(d0, d1) {
  var dict = d0.dict;
  return Bs_internalAVLset.cmp0(d0.data, d1.data, dict[/* cmp */0]);
}

function eq(d0, d1) {
  var dict = d0.dict;
  return Bs_internalAVLset.eq0(dict[/* cmp */0], d0.data, d1.data);
}

function findOpt(d, x) {
  var dict = d.dict;
  return Bs_internalAVLset.findOpt0(dict[/* cmp */0], d.data, x);
}

function findNull(d, x) {
  var dict = d.dict;
  return Bs_internalAVLset.findNull0(dict[/* cmp */0], d.data, x);
}

function ofArray(dict, data) {
  return {
          dict: dict,
          data: Bs_internalAVLset.ofArray0(dict[/* cmp */0], data)
        };
}

function subset(a, b) {
  var dict = a.dict;
  return Bs_internalAVLset.subset0(dict[/* cmp */0], a.data, b.data);
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

function mem(d, x) {
  var dict = d.dict;
  return Bs_internalAVLset.mem0(dict[/* cmp */0], d.data, x);
}

exports.empty = empty;
exports.ofArray = ofArray;
exports.isEmpty = isEmpty;
exports.mem = mem;
exports.addOnly = addOnly;
exports.add = add;
exports.addCheck = addCheck;
exports.addArrayOnly = addArrayOnly;
exports.addArray = addArray;
exports.removeArrayOnly = removeArrayOnly;
exports.removeArray = removeArray;
exports.singleton = singleton;
exports.removeOnly = removeOnly;
exports.remove = remove;
exports.removeCheck = removeCheck;
exports.union = union;
exports.subset = subset;
exports.cmp = cmp;
exports.eq = eq;
exports.iter = iter;
exports.fold = fold;
exports.forAll = forAll;
exports.exists = exists;
exports.filter = filter;
exports.partition = partition;
exports.length = length;
exports.toList = toList;
exports.toArray = toArray;
exports.minOpt = minOpt;
exports.minNull = minNull;
exports.maxOpt = maxOpt;
exports.maxNull = maxNull;
exports.split = split;
exports.ofSortedArrayUnsafe = ofSortedArrayUnsafe;
exports.findOpt = findOpt;
exports.findNull = findNull;
/* No side effect */
