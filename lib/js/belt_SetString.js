'use strict';

var Belt_internalAVLset = require("./belt_internalAVLset.js");
var Belt_internalSetString = require("./belt_internalSetString.js");

function add(t, x) {
  if (t !== null) {
    var v = t.value;
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
          return Belt_internalAVLset.bal(ll, v, r);
        }
      } else {
        var rr = add(r, x);
        if (rr === r) {
          return t;
        } else {
          return Belt_internalAVLset.bal(l, v, rr);
        }
      }
    }
  } else {
    return Belt_internalAVLset.singleton(x);
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
    var v = t.value;
    var r = t.right;
    if (x === v) {
      if (l !== null) {
        if (r !== null) {
          var v$1 = /* record */[/* contents */r.value];
          var r$1 = Belt_internalAVLset.removeMinAuxWithRef(r, v$1);
          return Belt_internalAVLset.bal(l, v$1[0], r$1);
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
        return Belt_internalAVLset.bal(ll, v, r);
      }
    } else {
      var rr = remove(r, x);
      if (rr === r) {
        return t;
      } else {
        return Belt_internalAVLset.bal(l, v, rr);
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
  var v = n.value;
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
              Belt_internalAVLset.joinShared(match[1], v, r)
            ];
    } else {
      return /* tuple */[
              Belt_internalAVLset.empty,
              n
            ];
    }
  } else if (r !== null) {
    var match$1 = splitAuxNoPivot(r, x);
    return /* tuple */[
            Belt_internalAVLset.joinShared(l, v, match$1[0]),
            match$1[1]
          ];
  } else {
    return /* tuple */[
            n,
            Belt_internalAVLset.empty
          ];
  }
}

function splitAuxPivot(n, x, pres) {
  var l = n.left;
  var v = n.value;
  var r = n.right;
  if (x === v) {
    pres[0] = true;
    return /* tuple */[
            l,
            r
          ];
  } else if (x < v) {
    if (l !== null) {
      var match = splitAuxPivot(l, x, pres);
      return /* tuple */[
              match[0],
              Belt_internalAVLset.joinShared(match[1], v, r)
            ];
    } else {
      return /* tuple */[
              Belt_internalAVLset.empty,
              n
            ];
    }
  } else if (r !== null) {
    var match$1 = splitAuxPivot(r, x, pres);
    return /* tuple */[
            Belt_internalAVLset.joinShared(l, v, match$1[0]),
            match$1[1]
          ];
  } else {
    return /* tuple */[
            n,
            Belt_internalAVLset.empty
          ];
  }
}

function split(t, x) {
  if (t !== null) {
    var pres = /* record */[/* contents */false];
    var v = splitAuxPivot(t, x, pres);
    return /* tuple */[
            v,
            pres[0]
          ];
  } else {
    return /* tuple */[
            /* tuple */[
              Belt_internalAVLset.empty,
              Belt_internalAVLset.empty
            ],
            false
          ];
  }
}

function union(s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var h1 = s1.height;
      var h2 = s2.height;
      if (h1 >= h2) {
        if (h2 === 1) {
          return add(s1, s2.value);
        } else {
          var l1 = s1.left;
          var v1 = s1.value;
          var r1 = s1.right;
          var match = splitAuxNoPivot(s2, v1);
          return Belt_internalAVLset.joinShared(union(l1, match[0]), v1, union(r1, match[1]));
        }
      } else if (h1 === 1) {
        return add(s2, s1.value);
      } else {
        var l2 = s2.left;
        var v2 = s2.value;
        var r2 = s2.right;
        var match$1 = splitAuxNoPivot(s1, v2);
        return Belt_internalAVLset.joinShared(union(match$1[0], l2), v2, union(match$1[1], r2));
      }
    } else {
      return s1;
    }
  } else {
    return s2;
  }
}

function intersect(s1, s2) {
  if (s1 !== null && s2 !== null) {
    var l1 = s1.left;
    var v1 = s1.value;
    var r1 = s1.right;
    var pres = /* record */[/* contents */false];
    var match = splitAuxPivot(s2, v1, pres);
    var ll = intersect(l1, match[0]);
    var rr = intersect(r1, match[1]);
    if (pres[0]) {
      return Belt_internalAVLset.joinShared(ll, v1, rr);
    } else {
      return Belt_internalAVLset.concatShared(ll, rr);
    }
  } else {
    return Belt_internalAVLset.empty;
  }
}

function diff(s1, s2) {
  if (s1 !== null && s2 !== null) {
    var l1 = s1.left;
    var v1 = s1.value;
    var r1 = s1.right;
    var pres = /* record */[/* contents */false];
    var match = splitAuxPivot(s2, v1, pres);
    var ll = diff(l1, match[0]);
    var rr = diff(r1, match[1]);
    if (pres[0]) {
      return Belt_internalAVLset.concatShared(ll, rr);
    } else {
      return Belt_internalAVLset.joinShared(ll, v1, rr);
    }
  } else {
    return s1;
  }
}

var empty = Belt_internalAVLset.empty;

var fromArray = Belt_internalSetString.fromArray;

var fromSortedArrayUnsafe = Belt_internalAVLset.fromSortedArrayUnsafe;

var isEmpty = Belt_internalAVLset.isEmpty;

var has = Belt_internalSetString.has;

var subset = Belt_internalSetString.subset;

var cmp = Belt_internalSetString.cmp;

var eq = Belt_internalSetString.eq;

var forEachU = Belt_internalAVLset.forEachU;

var forEach = Belt_internalAVLset.forEach;

var reduceU = Belt_internalAVLset.reduceU;

var reduce = Belt_internalAVLset.reduce;

var everyU = Belt_internalAVLset.everyU;

var every = Belt_internalAVLset.every;

var someU = Belt_internalAVLset.someU;

var some = Belt_internalAVLset.some;

var keepU = Belt_internalAVLset.keepSharedU;

var keep = Belt_internalAVLset.keepShared;

var partitionU = Belt_internalAVLset.partitionSharedU;

var partition = Belt_internalAVLset.partitionShared;

var size = Belt_internalAVLset.size;

var toList = Belt_internalAVLset.toList;

var toArray = Belt_internalAVLset.toArray;

var minimum = Belt_internalAVLset.minimum;

var minUndefined = Belt_internalAVLset.minUndefined;

var maximum = Belt_internalAVLset.maximum;

var maxUndefined = Belt_internalAVLset.maxUndefined;

var get = Belt_internalSetString.get;

var getUndefined = Belt_internalSetString.getUndefined;

var getExn = Belt_internalSetString.getExn;

var checkInvariantInternal = Belt_internalAVLset.checkInvariantInternal;

exports.empty = empty;
exports.fromArray = fromArray;
exports.fromSortedArrayUnsafe = fromSortedArrayUnsafe;
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
