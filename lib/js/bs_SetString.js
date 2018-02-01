'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");
var Bs_internalSetString = require("./bs_internalSetString.js");

function add(t, x) {
  if (t !== null) {
    var v = t.key;
    if (x === v) {
      return t;
    } else {
      var l = t.left;
      var r = t.right;
      if (x < v) {
        var ll = add(l, x);
        if (ll === l) {
          return t;
        } else {
          return Bs_internalAVLset.bal(ll, v, r);
        }
      } else {
        var rr = add(r, x);
        if (rr === r) {
          return t;
        } else {
          return Bs_internalAVLset.bal(l, v, add(r, x));
        }
      }
    }
  } else {
    return Bs_internalAVLset.singleton(x);
  }
}

function mergeMany(h, arr) {
  var len = arr.length;
  var v = h;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var key = arr[i];
    v = add(v, key);
  }
  return v;
}

function remove(t, x) {
  if (t !== null) {
    var l = t.left;
    var v = t.key;
    var r = t.right;
    if (x === v) {
      if (l !== null) {
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
    } else if (x < v) {
      var ll = remove(l, x);
      if (ll === l) {
        return t;
      } else {
        return Bs_internalAVLset.bal(ll, v, r);
      }
    } else {
      var rr = remove(r, x);
      if (rr === r) {
        return t;
      } else {
        return Bs_internalAVLset.bal(l, v, rr);
      }
    }
  } else {
    return t;
  }
}

function removeMany(h, arr) {
  var len = arr.length;
  var v = h;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var key = arr[i];
    v = remove(v, key);
  }
  return v;
}

function splitAuxNoPivot(n, x) {
  var l = n.left;
  var v = n.key;
  var r = n.right;
  if (x === v) {
    return /* tuple */[
            l,
            r
          ];
  } else if (x < v) {
    if (l !== null) {
      var match = splitAuxNoPivot(l, x);
      return /* tuple */[
              match[0],
              Bs_internalAVLset.joinShared(match[1], v, r)
            ];
    } else {
      return /* tuple */[
              Bs_internalAVLset.empty,
              n
            ];
    }
  } else if (r !== null) {
    var match$1 = splitAuxNoPivot(r, x);
    return /* tuple */[
            Bs_internalAVLset.joinShared(l, v, match$1[0]),
            match$1[1]
          ];
  } else {
    return /* tuple */[
            n,
            Bs_internalAVLset.empty
          ];
  }
}

function splitAuxPivot(n, x, pres) {
  var l = n.left;
  var v = n.key;
  var r = n.right;
  if (x === v) {
    pres[0] = /* true */1;
    return /* tuple */[
            l,
            r
          ];
  } else if (x < v) {
    if (l !== null) {
      var match = splitAuxPivot(l, x, pres);
      return /* tuple */[
              match[0],
              Bs_internalAVLset.joinShared(match[1], v, r)
            ];
    } else {
      return /* tuple */[
              Bs_internalAVLset.empty,
              n
            ];
    }
  } else if (r !== null) {
    var match$1 = splitAuxPivot(r, x, pres);
    return /* tuple */[
            Bs_internalAVLset.joinShared(l, v, match$1[0]),
            match$1[1]
          ];
  } else {
    return /* tuple */[
            n,
            Bs_internalAVLset.empty
          ];
  }
}

function split(t, x) {
  if (t !== null) {
    var pres = [/* false */0];
    var v = splitAuxPivot(t, x, pres);
    return /* tuple */[
            v,
            pres[0]
          ];
  } else {
    return /* tuple */[
            /* tuple */[
              Bs_internalAVLset.empty,
              Bs_internalAVLset.empty
            ],
            /* false */0
          ];
  }
}

function union(s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var h1 = s1.h;
      var h2 = s2.h;
      if (h1 >= h2) {
        if (h2 === 1) {
          return add(s1, s2.key);
        } else {
          var l1 = s1.left;
          var v1 = s1.key;
          var r1 = s1.right;
          var match = splitAuxNoPivot(s2, v1);
          return Bs_internalAVLset.joinShared(union(l1, match[0]), v1, union(r1, match[1]));
        }
      } else if (h1 === 1) {
        return add(s2, s1.key);
      } else {
        var l2 = s2.left;
        var v2 = s2.key;
        var r2 = s2.right;
        var match$1 = splitAuxNoPivot(s1, v2);
        return Bs_internalAVLset.joinShared(union(match$1[0], l2), v2, union(match$1[1], r2));
      }
    } else {
      return s1;
    }
  } else {
    return s2;
  }
}

function intersect(s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.key;
      var r1 = s1.right;
      var pres = [/* false */0];
      var match = splitAuxPivot(s2, v1, pres);
      var ll = intersect(l1, match[0]);
      var rr = intersect(r1, match[1]);
      if (pres[0]) {
        return Bs_internalAVLset.joinShared(ll, v1, rr);
      } else {
        return Bs_internalAVLset.concatShared(ll, rr);
      }
    } else {
      return Bs_internalAVLset.empty;
    }
  } else {
    return Bs_internalAVLset.empty;
  }
}

function diff(s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.key;
      var r1 = s1.right;
      var pres = [/* false */0];
      var match = splitAuxPivot(s2, v1, pres);
      var ll = diff(l1, match[0]);
      var rr = diff(r1, match[1]);
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

var empty = Bs_internalAVLset.empty;

var ofArray = Bs_internalSetString.ofArray;

var ofSortedArrayUnsafe = Bs_internalAVLset.ofSortedArrayUnsafe;

var isEmpty = Bs_internalAVLset.isEmpty;

var has = Bs_internalSetString.has;

var subset = Bs_internalSetString.subset;

var cmp = Bs_internalSetString.cmp;

var eq = Bs_internalSetString.eq;

var forEach = Bs_internalAVLset.forEach;

var reduce = Bs_internalAVLset.reduce;

var every = Bs_internalAVLset.every;

var some = Bs_internalAVLset.some;

var keep = Bs_internalAVLset.filterShared;

var partition = Bs_internalAVLset.partitionShared;

var size = Bs_internalAVLset.size;

var toList = Bs_internalAVLset.toList;

var toArray = Bs_internalAVLset.toArray;

var minimum = Bs_internalAVLset.minimum;

var minUndefined = Bs_internalAVLset.minUndefined;

var maximum = Bs_internalAVLset.maximum;

var maxUndefined = Bs_internalAVLset.maxUndefined;

var get = Bs_internalSetString.get;

var getUndefined = Bs_internalSetString.getUndefined;

var getExn = Bs_internalSetString.getExn;

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
exports.keep = keep;
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
