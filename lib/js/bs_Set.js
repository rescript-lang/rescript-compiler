'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");

function add0(t, x, cmp) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(x, k);
    if (c) {
      var l = t.left;
      var r = t.right;
      if (c < 0) {
        var ll = add0(l, x, cmp);
        if (ll === l) {
          return t;
        } else {
          return Bs_internalAVLset.bal(ll, k, r);
        }
      } else {
        var rr = add0(r, x, cmp);
        if (rr === r) {
          return t;
        } else {
          return Bs_internalAVLset.bal(l, k, rr);
        }
      }
    } else {
      return t;
    }
  } else {
    return Bs_internalAVLset.singleton0(x);
  }
}

function remove0(t, x, cmp) {
  if (t !== null) {
    var l = t.left;
    var v = t.key;
    var r = t.right;
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        var ll = remove0(l, x, cmp);
        if (ll === l) {
          return t;
        } else {
          return Bs_internalAVLset.bal(ll, v, r);
        }
      } else {
        var rr = remove0(r, x, cmp);
        if (rr === r) {
          return t;
        } else {
          return Bs_internalAVLset.bal(l, v, rr);
        }
      }
    } else if (l !== null) {
      if (r !== null) {
        var v$1 = [r.key];
        var r$1 = Bs_internalAVLset.removeMinAuxWithRef(r, v$1);
        return Bs_internalAVLset.bal(l, v$1[0], r$1);
      } else {
        return l;
      }
    } else {
      return r;
    }
  } else {
    return t;
  }
}

function mergeArray0(h, arr, cmp) {
  var len = arr.length;
  var v = h;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var key = arr[i];
    v = add0(v, key, cmp);
  }
  return v;
}

function removeArray0(h, arr, cmp) {
  var len = arr.length;
  var v = h;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var key = arr[i];
    v = remove0(v, key, cmp);
  }
  return v;
}

function splitAuxNoPivot(cmp, n, x) {
  var l = n.left;
  var v = n.key;
  var r = n.right;
  var c = cmp(x, v);
  if (c) {
    if (c < 0) {
      if (l !== null) {
        var match = splitAuxNoPivot(cmp, l, x);
        return /* tuple */[
                match[0],
                Bs_internalAVLset.joinShared(match[1], v, r)
              ];
      } else {
        return /* tuple */[
                null,
                n
              ];
      }
    } else if (r !== null) {
      var match$1 = splitAuxNoPivot(cmp, r, x);
      return /* tuple */[
              Bs_internalAVLset.joinShared(l, v, match$1[0]),
              match$1[1]
            ];
    } else {
      return /* tuple */[
              n,
              null
            ];
    }
  } else {
    return /* tuple */[
            l,
            r
          ];
  }
}

function splitAuxPivot(cmp, n, x, pres) {
  var l = n.left;
  var v = n.key;
  var r = n.right;
  var c = cmp(x, v);
  if (c) {
    if (c < 0) {
      if (l !== null) {
        var match = splitAuxPivot(cmp, l, x, pres);
        return /* tuple */[
                match[0],
                Bs_internalAVLset.joinShared(match[1], v, r)
              ];
      } else {
        return /* tuple */[
                null,
                n
              ];
      }
    } else if (r !== null) {
      var match$1 = splitAuxPivot(cmp, r, x, pres);
      return /* tuple */[
              Bs_internalAVLset.joinShared(l, v, match$1[0]),
              match$1[1]
            ];
    } else {
      return /* tuple */[
              n,
              null
            ];
    }
  } else {
    pres[0] = /* true */1;
    return /* tuple */[
            l,
            r
          ];
  }
}

function split0(t, x, cmp) {
  if (t !== null) {
    var pres = [/* false */0];
    var v = splitAuxPivot(cmp, t, x, pres);
    return /* tuple */[
            v,
            pres[0]
          ];
  } else {
    return /* tuple */[
            /* tuple */[
              null,
              null
            ],
            /* false */0
          ];
  }
}

function union0(s1, s2, cmp) {
  if (s1 !== null) {
    if (s2 !== null) {
      var h1 = s1.h;
      var h2 = s2.h;
      if (h1 >= h2) {
        if (h2 === 1) {
          return add0(s1, s2.key, cmp);
        } else {
          var l1 = s1.left;
          var v1 = s1.key;
          var r1 = s1.right;
          var match = splitAuxNoPivot(cmp, s2, v1);
          return Bs_internalAVLset.joinShared(union0(l1, match[0], cmp), v1, union0(r1, match[1], cmp));
        }
      } else if (h1 === 1) {
        return add0(s2, s1.key, cmp);
      } else {
        var l2 = s2.left;
        var v2 = s2.key;
        var r2 = s2.right;
        var match$1 = splitAuxNoPivot(cmp, s1, v2);
        return Bs_internalAVLset.joinShared(union0(match$1[0], l2, cmp), v2, union0(match$1[1], r2, cmp));
      }
    } else {
      return s1;
    }
  } else {
    return s2;
  }
}

function inter0(s1, s2, cmp) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.key;
      var r1 = s1.right;
      var pres = [/* false */0];
      var match = splitAuxPivot(cmp, s2, v1, pres);
      var ll = inter0(l1, match[0], cmp);
      var rr = inter0(r1, match[1], cmp);
      if (pres[0]) {
        return Bs_internalAVLset.joinShared(ll, v1, rr);
      } else {
        return Bs_internalAVLset.concatShared(ll, rr);
      }
    } else {
      return null;
    }
  } else {
    return null;
  }
}

function diff0(s1, s2, cmp) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.key;
      var r1 = s1.right;
      var pres = [/* false */0];
      var match = splitAuxPivot(cmp, s2, v1, pres);
      var ll = diff0(l1, match[0], cmp);
      var rr = diff0(r1, match[1], cmp);
      if (pres[0]) {
        return Bs_internalAVLset.concatShared(ll, rr);
      } else {
        return Bs_internalAVLset.joinShared(ll, v1, rr);
      }
    } else {
      return s1;
    }
  } else {
    return s1;
  }
}

function ofArray(dict, data) {
  return {
          dict: dict,
          data: Bs_internalAVLset.ofArray0(data, dict[/* cmp */0])
        };
}

function remove(m, e) {
  var dict = m.dict;
  var data = m.data;
  var newData = remove0(data, e, dict[/* cmp */0]);
  if (newData === data) {
    return m;
  } else {
    return {
            dict: dict,
            data: newData
          };
  }
}

function add(m, e) {
  var dict = m.dict;
  var data = m.data;
  var newData = add0(data, e, dict[/* cmp */0]);
  if (newData === data) {
    return m;
  } else {
    return {
            dict: dict,
            data: newData
          };
  }
}

function mergeMany(m, e) {
  var dict = m.dict;
  var data = m.data;
  var newData = mergeArray0(data, e, dict[/* cmp */0]);
  return {
          dict: dict,
          data: newData
        };
}

function removeMany(m, e) {
  var dict = m.dict;
  var data = m.data;
  var newData = removeArray0(data, e, dict[/* cmp */0]);
  if (newData === data) {
    return m;
  } else {
    return {
            dict: dict,
            data: newData
          };
  }
}

function union(m, n) {
  var dict = m.dict;
  var mdata = m.data;
  var ndata = n.data;
  return {
          dict: dict,
          data: union0(mdata, ndata, dict[/* cmp */0])
        };
}

function intersect(m, n) {
  var dict = m.dict;
  var mdata = m.data;
  var ndata = n.data;
  return {
          dict: dict,
          data: inter0(mdata, ndata, dict[/* cmp */0])
        };
}

function diff(m, n) {
  var dict = m.dict;
  var mdata = m.data;
  var ndata = n.data;
  return {
          dict: dict,
          data: diff0(mdata, ndata, dict[/* cmp */0])
        };
}

function subset(m, n) {
  var dict = m.dict;
  var mdata = m.data;
  var ndata = n.data;
  return Bs_internalAVLset.subset0(mdata, ndata, dict[/* cmp */0]);
}

function split(m, e) {
  var dict = m.dict;
  var data = m.data;
  var match = split0(data, e, dict[/* cmp */0]);
  var match$1 = match[0];
  return /* tuple */[
          /* tuple */[
            {
              dict: dict,
              data: match$1[0]
            },
            {
              dict: dict,
              data: match$1[1]
            }
          ],
          match[1]
        ];
}

function empty(dict) {
  return {
          dict: dict,
          data: Bs_internalAVLset.empty0
        };
}

function isEmpty(m) {
  return Bs_internalAVLset.isEmpty0(m.data);
}

function cmp(m, n) {
  var dict = m.dict;
  var mdata = m.data;
  var ndata = n.data;
  return Bs_internalAVLset.cmp0(mdata, ndata, dict[/* cmp */0]);
}

function eq(m, n) {
  var dict = m.dict;
  var mdata = m.data;
  var ndata = n.data;
  return Bs_internalAVLset.eq0(mdata, ndata, dict[/* cmp */0]);
}

function forEach(m, f) {
  return Bs_internalAVLset.iter0(m.data, f);
}

function reduce(m, acc, f) {
  return Bs_internalAVLset.fold0(m.data, acc, f);
}

function every(m, f) {
  return Bs_internalAVLset.every0(m.data, f);
}

function some(m, f) {
  return Bs_internalAVLset.some0(m.data, f);
}

function keepBy(m, f) {
  var data = m.data;
  var dict = m.dict;
  return {
          dict: dict,
          data: Bs_internalAVLset.filterShared0(data, f)
        };
}

function partition(m, f) {
  var mdata = m.data;
  var dict = m.dict;
  var match = Bs_internalAVLset.partitionShared0(mdata, f);
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

function size(m) {
  return Bs_internalAVLset.length0(m.data);
}

function toList(m) {
  return Bs_internalAVLset.toList0(m.data);
}

function toArray(m) {
  return Bs_internalAVLset.toArray0(m.data);
}

function minimum(m) {
  return Bs_internalAVLset.minOpt0(m.data);
}

function minNull(m) {
  return Bs_internalAVLset.minNull0(m.data);
}

function maximum(m) {
  return Bs_internalAVLset.maxOpt0(m.data);
}

function maxNull(m) {
  return Bs_internalAVLset.maxNull0(m.data);
}

function get(m, e) {
  var dict = m.dict;
  var data = m.data;
  return Bs_internalAVLset.findOpt0(data, e, dict[/* cmp */0]);
}

function getNull(m, e) {
  var dict = m.dict;
  var data = m.data;
  return Bs_internalAVLset.findNull0(data, e, dict[/* cmp */0]);
}

function getExn(m, e) {
  var dict = m.dict;
  var data = m.data;
  return Bs_internalAVLset.findExn0(data, e, dict[/* cmp */0]);
}

function has(m, e) {
  var dict = m.dict;
  var data = m.data;
  return Bs_internalAVLset.mem0(data, e, dict[/* cmp */0]);
}

function ofSortedArrayUnsafe(xs, dict) {
  return {
          dict: dict,
          data: Bs_internalAVLset.ofSortedArrayUnsafe0(xs)
        };
}

function getData(prim) {
  return prim.data;
}

function getDict(prim) {
  return prim.dict;
}

function packDictData(prim, prim$1) {
  return {
          dict: prim,
          data: prim$1
        };
}

function checkInvariantInternal(d) {
  return Bs_internalAVLset.checkInvariantInternal(d.data);
}

var empty0 = Bs_internalAVLset.empty0;

var ofArray0 = Bs_internalAVLset.ofArray0;

var isEmpty0 = Bs_internalAVLset.isEmpty0;

var has0 = Bs_internalAVLset.mem0;

var subset0 = Bs_internalAVLset.subset0;

var cmp0 = Bs_internalAVLset.cmp0;

var eq0 = Bs_internalAVLset.eq0;

var forEach0 = Bs_internalAVLset.iter0;

var reduce0 = Bs_internalAVLset.fold0;

var every0 = Bs_internalAVLset.every0;

var some0 = Bs_internalAVLset.some0;

var filter0 = Bs_internalAVLset.filterShared0;

var partition0 = Bs_internalAVLset.partitionShared0;

var size0 = Bs_internalAVLset.length0;

var toList0 = Bs_internalAVLset.toList0;

var toArray0 = Bs_internalAVLset.toArray0;

var minimum0 = Bs_internalAVLset.minOpt0;

var maximum0 = Bs_internalAVLset.maxOpt0;

var ofSortedArrayUnsafe0 = Bs_internalAVLset.ofSortedArrayUnsafe0;

var get0 = Bs_internalAVLset.findOpt0;

var getNull0 = Bs_internalAVLset.findNull0;

exports.empty = empty;
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
exports.forEach = forEach;
exports.reduce = reduce;
exports.every = every;
exports.some = some;
exports.keepBy = keepBy;
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
exports.checkInvariantInternal = checkInvariantInternal;
exports.getData = getData;
exports.getDict = getDict;
exports.packDictData = packDictData;
exports.empty0 = empty0;
exports.ofArray0 = ofArray0;
exports.isEmpty0 = isEmpty0;
exports.has0 = has0;
exports.add0 = add0;
exports.remove0 = remove0;
exports.mergeArray0 = mergeArray0;
exports.removeArray0 = removeArray0;
exports.union0 = union0;
exports.inter0 = inter0;
exports.diff0 = diff0;
exports.subset0 = subset0;
exports.cmp0 = cmp0;
exports.eq0 = eq0;
exports.forEach0 = forEach0;
exports.reduce0 = reduce0;
exports.every0 = every0;
exports.some0 = some0;
exports.filter0 = filter0;
exports.partition0 = partition0;
exports.size0 = size0;
exports.toList0 = toList0;
exports.toArray0 = toArray0;
exports.minimum0 = minimum0;
exports.maximum0 = maximum0;
exports.split0 = split0;
exports.ofSortedArrayUnsafe0 = ofSortedArrayUnsafe0;
exports.get0 = get0;
exports.getNull0 = getNull0;
/* No side effect */
