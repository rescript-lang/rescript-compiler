'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");

function add(t, x, cmp) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(x, k);
    if (c) {
      var l = t.left;
      var r = t.right;
      if (c < 0) {
        var ll = add(l, x, cmp);
        if (ll === l) {
          return t;
        } else {
          return Bs_internalAVLset.bal(ll, k, r);
        }
      } else {
        var rr = add(r, x, cmp);
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

function remove(t, x, cmp) {
  if (t !== null) {
    var l = t.left;
    var v = t.key;
    var r = t.right;
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        var ll = remove(l, x, cmp);
        if (ll === l) {
          return t;
        } else {
          return Bs_internalAVLset.bal(ll, v, r);
        }
      } else {
        var rr = remove(r, x, cmp);
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

function mergeMany(h, arr, cmp) {
  var len = arr.length;
  var v = h;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var key = arr[i];
    v = add(v, key, cmp);
  }
  return v;
}

function removeMany(h, arr, cmp) {
  var len = arr.length;
  var v = h;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var key = arr[i];
    v = remove(v, key, cmp);
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

function split(t, x, cmp) {
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

function union(s1, s2, cmp) {
  if (s1 !== null) {
    if (s2 !== null) {
      var h1 = s1.h;
      var h2 = s2.h;
      if (h1 >= h2) {
        if (h2 === 1) {
          return add(s1, s2.key, cmp);
        } else {
          var l1 = s1.left;
          var v1 = s1.key;
          var r1 = s1.right;
          var match = splitAuxNoPivot(cmp, s2, v1);
          return Bs_internalAVLset.joinShared(union(l1, match[0], cmp), v1, union(r1, match[1], cmp));
        }
      } else if (h1 === 1) {
        return add(s2, s1.key, cmp);
      } else {
        var l2 = s2.left;
        var v2 = s2.key;
        var r2 = s2.right;
        var match$1 = splitAuxNoPivot(cmp, s1, v2);
        return Bs_internalAVLset.joinShared(union(match$1[0], l2, cmp), v2, union(match$1[1], r2, cmp));
      }
    } else {
      return s1;
    }
  } else {
    return s2;
  }
}

function intersect(s1, s2, cmp) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.key;
      var r1 = s1.right;
      var pres = [/* false */0];
      var match = splitAuxPivot(cmp, s2, v1, pres);
      var ll = intersect(l1, match[0], cmp);
      var rr = intersect(r1, match[1], cmp);
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

function diff(s1, s2, cmp) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.key;
      var r1 = s1.right;
      var pres = [/* false */0];
      var match = splitAuxPivot(cmp, s2, v1, pres);
      var ll = diff(l1, match[0], cmp);
      var rr = diff(r1, match[1], cmp);
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

var empty = Bs_internalAVLset.empty0;

var ofArray = Bs_internalAVLset.ofArray0;

var ofSortedArrayUnsafe = Bs_internalAVLset.ofSortedArrayUnsafe0;

var isEmpty = Bs_internalAVLset.isEmpty0;

var has = Bs_internalAVLset.mem0;

var subset = Bs_internalAVLset.subset0;

var cmp = Bs_internalAVLset.cmp0;

var eq = Bs_internalAVLset.eq0;

var forEach = Bs_internalAVLset.iter0;

var reduce = Bs_internalAVLset.fold0;

var every = Bs_internalAVLset.every0;

var some = Bs_internalAVLset.some0;

var keepBy = Bs_internalAVLset.filterShared0;

var partition = Bs_internalAVLset.partitionShared0;

var size = Bs_internalAVLset.length0;

var toList = Bs_internalAVLset.toList0;

var toArray = Bs_internalAVLset.toArray0;

var minimum = Bs_internalAVLset.minimum0;

var minUndefined = Bs_internalAVLset.minUndefined0;

var maximum = Bs_internalAVLset.maximum0;

var maxUndefined = Bs_internalAVLset.maxUndefined0;

var get = Bs_internalAVLset.get0;

var getUndefined = Bs_internalAVLset.getUndefined0;

var getExn = Bs_internalAVLset.getExn0;

var checkInvariantInternal = Bs_internalAVLset.checkInvariantInternal;

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
exports.minUndefined = minUndefined;
exports.maximum = maximum;
exports.maxUndefined = maxUndefined;
exports.get = get;
exports.getUndefined = getUndefined;
exports.getExn = getExn;
exports.split = split;
exports.checkInvariantInternal = checkInvariantInternal;
/* No side effect */
