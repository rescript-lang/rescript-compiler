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

function addArray0(h, arr, cmp) {
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

function split0(cmp, t, x) {
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

function union0(cmp, s1, s2) {
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
          return Bs_internalAVLset.joinShared(union0(cmp, l1, match[0]), v1, union0(cmp, r1, match[1]));
        }
      } else if (h1 === 1) {
        return add0(s2, s1.key, cmp);
      } else {
        var l2 = s2.left;
        var v2 = s2.key;
        var r2 = s2.right;
        var match$1 = splitAuxNoPivot(cmp, s1, v2);
        return Bs_internalAVLset.joinShared(union0(cmp, match$1[0], l2), v2, union0(cmp, match$1[1], r2));
      }
    } else {
      return s1;
    }
  } else {
    return s2;
  }
}

function inter0(cmp, s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.key;
      var r1 = s1.right;
      var pres = [/* false */0];
      var match = splitAuxPivot(cmp, s2, v1, pres);
      var ll = inter0(cmp, l1, match[0]);
      var rr = inter0(cmp, r1, match[1]);
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

function diff0(cmp, s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.key;
      var r1 = s1.right;
      var pres = [/* false */0];
      var match = splitAuxPivot(cmp, s2, v1, pres);
      var ll = diff0(cmp, l1, match[0]);
      var rr = diff0(cmp, r1, match[1]);
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
          data: Bs_internalAVLset.ofArray0(dict[/* cmp */0], data)
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

function addArray(m, e) {
  var dict = m.dict;
  var data = m.data;
  var newData = addArray0(data, e, dict[/* cmp */0]);
  return {
          dict: dict,
          data: newData
        };
}

function removeArray(m, e) {
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
          data: union0(dict[/* cmp */0], mdata, ndata)
        };
}

function inter(m, n) {
  var dict = m.dict;
  var mdata = m.data;
  var ndata = n.data;
  return {
          dict: dict,
          data: inter0(dict[/* cmp */0], mdata, ndata)
        };
}

function diff(m, n) {
  var dict = m.dict;
  var mdata = m.data;
  var ndata = n.data;
  return {
          dict: dict,
          data: diff0(dict[/* cmp */0], mdata, ndata)
        };
}

function subset(m, n) {
  var dict = m.dict;
  var mdata = m.data;
  var ndata = n.data;
  return Bs_internalAVLset.subset0(dict[/* cmp */0], mdata, ndata);
}

function split(m, e) {
  var dict = m.dict;
  var data = m.data;
  var match = split0(dict[/* cmp */0], data, e);
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

function singleton(e, dict) {
  return {
          dict: dict,
          data: Bs_internalAVLset.singleton0(e)
        };
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
  return Bs_internalAVLset.eq0(dict[/* cmp */0], mdata, ndata);
}

function forEach(m, f) {
  return Bs_internalAVLset.iter0(m.data, f);
}

function fold(m, acc, f) {
  return Bs_internalAVLset.fold0(m.data, acc, f);
}

function forAll(m, f) {
  return Bs_internalAVLset.forAll0(m.data, f);
}

function exists(m, f) {
  return Bs_internalAVLset.exists0(m.data, f);
}

function filter(m, f) {
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
  return Bs_internalAVLset.findOpt0(dict[/* cmp */0], data, e);
}

function getNull(m, e) {
  var dict = m.dict;
  var data = m.data;
  return Bs_internalAVLset.findNull0(dict[/* cmp */0], data, e);
}

function getExn(m, e) {
  var dict = m.dict;
  var data = m.data;
  return Bs_internalAVLset.findExn0(dict[/* cmp */0], data, e);
}

function has(m, e) {
  var dict = m.dict;
  var data = m.data;
  return Bs_internalAVLset.mem0(dict[/* cmp */0], data, e);
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

function checkInvariant(d) {
  return Bs_internalAVLset.checkInvariant(d.data);
}

var empty0 = Bs_internalAVLset.empty0;

var ofArray0 = Bs_internalAVLset.ofArray0;

var isEmpty0 = Bs_internalAVLset.isEmpty0;

var mem0 = Bs_internalAVLset.mem0;

var singleton0 = Bs_internalAVLset.singleton0;

var subset0 = Bs_internalAVLset.subset0;

var cmp0 = Bs_internalAVLset.cmp0;

var eq0 = Bs_internalAVLset.eq0;

var iter0 = Bs_internalAVLset.iter0;

var fold0 = Bs_internalAVLset.fold0;

var forAll0 = Bs_internalAVLset.forAll0;

var exists0 = Bs_internalAVLset.exists0;

var filter0 = Bs_internalAVLset.filterShared0;

var partition0 = Bs_internalAVLset.partitionShared0;

var length0 = Bs_internalAVLset.length0;

var toList0 = Bs_internalAVLset.toList0;

var toArray0 = Bs_internalAVLset.toArray0;

var minOpt0 = Bs_internalAVLset.minOpt0;

var maxOpt0 = Bs_internalAVLset.maxOpt0;

var ofSortedArrayUnsafe0 = Bs_internalAVLset.ofSortedArrayUnsafe0;

var findOpt0 = Bs_internalAVLset.findOpt0;

var findNull0 = Bs_internalAVLset.findNull0;

exports.empty = empty;
exports.singleton = singleton;
exports.ofArray = ofArray;
exports.ofSortedArrayUnsafe = ofSortedArrayUnsafe;
exports.isEmpty = isEmpty;
exports.has = has;
exports.add = add;
exports.addArray = addArray;
exports.remove = remove;
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
exports.getData = getData;
exports.getDict = getDict;
exports.packDictData = packDictData;
exports.empty0 = empty0;
exports.ofArray0 = ofArray0;
exports.isEmpty0 = isEmpty0;
exports.mem0 = mem0;
exports.add0 = add0;
exports.addArray0 = addArray0;
exports.removeArray0 = removeArray0;
exports.singleton0 = singleton0;
exports.remove0 = remove0;
exports.union0 = union0;
exports.inter0 = inter0;
exports.diff0 = diff0;
exports.subset0 = subset0;
exports.cmp0 = cmp0;
exports.eq0 = eq0;
exports.iter0 = iter0;
exports.fold0 = fold0;
exports.forAll0 = forAll0;
exports.exists0 = exists0;
exports.filter0 = filter0;
exports.partition0 = partition0;
exports.length0 = length0;
exports.toList0 = toList0;
exports.toArray0 = toArray0;
exports.minOpt0 = minOpt0;
exports.maxOpt0 = maxOpt0;
exports.split0 = split0;
exports.ofSortedArrayUnsafe0 = ofSortedArrayUnsafe0;
exports.findOpt0 = findOpt0;
exports.findNull0 = findNull0;
/* No side effect */
