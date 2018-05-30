'use strict';

var Curry = require("./curry.js");
var Belt_SortArray = require("./belt_SortArray.js");

function treeHeight(n) {
  if (n !== null) {
    return n.height;
  } else {
    return 0;
  }
}

function copy(n) {
  if (n !== null) {
    var l = n.left;
    var r = n.right;
    return {
            value: n.value,
            height: n.height,
            left: copy(l),
            right: copy(r)
          };
  } else {
    return n;
  }
}

function create(l, v, r) {
  var hl = l !== null ? l.height : 0;
  var hr = r !== null ? r.height : 0;
  return {
          value: v,
          height: hl >= hr ? hl + 1 | 0 : hr + 1 | 0,
          left: l,
          right: r
        };
}

function singleton(x) {
  return {
          value: x,
          height: 1,
          left: null,
          right: null
        };
}

function heightGe(l, r) {
  if (r !== null) {
    if (l !== null) {
      return l.height >= r.height;
    } else {
      return false;
    }
  } else {
    return true;
  }
}

function bal(l, v, r) {
  var hl = l !== null ? l.height : 0;
  var hr = r !== null ? r.height : 0;
  if (hl > (hr + 2 | 0)) {
    var ll = l.left;
    var lv = l.value;
    var lr = l.right;
    if (heightGe(ll, lr)) {
      return create(ll, lv, create(lr, v, r));
    } else {
      var lrl = lr.left;
      var lrv = lr.value;
      var lrr = lr.right;
      return create(create(ll, lv, lrl), lrv, create(lrr, v, r));
    }
  } else if (hr > (hl + 2 | 0)) {
    var rl = r.left;
    var rv = r.value;
    var rr = r.right;
    if (heightGe(rr, rl)) {
      return create(create(l, v, rl), rv, rr);
    } else {
      var rll = rl.left;
      var rlv = rl.value;
      var rlr = rl.right;
      return create(create(l, v, rll), rlv, create(rlr, rv, rr));
    }
  } else {
    return {
            value: v,
            height: hl >= hr ? hl + 1 | 0 : hr + 1 | 0,
            left: l,
            right: r
          };
  }
}

function min0Aux(_n) {
  while(true) {
    var n = _n;
    var match = n.left;
    if (match !== null) {
      _n = match;
      continue ;
    } else {
      return n.value;
    }
  };
}

function minimum(n) {
  if (n !== null) {
    return /* Some */[min0Aux(n)];
  } else {
    return /* None */0;
  }
}

function minUndefined(n) {
  if (n !== null) {
    return min0Aux(n);
  } else {
    return undefined;
  }
}

function max0Aux(_n) {
  while(true) {
    var n = _n;
    var match = n.right;
    if (match !== null) {
      _n = match;
      continue ;
    } else {
      return n.value;
    }
  };
}

function maximum(n) {
  if (n !== null) {
    return /* Some */[max0Aux(n)];
  } else {
    return /* None */0;
  }
}

function maxUndefined(n) {
  if (n !== null) {
    return max0Aux(n);
  } else {
    return undefined;
  }
}

function removeMinAuxWithRef(n, v) {
  var ln = n.left;
  var rn = n.right;
  var kn = n.value;
  if (ln !== null) {
    return bal(removeMinAuxWithRef(ln, v), kn, rn);
  } else {
    v[0] = kn;
    return rn;
  }
}

function isEmpty(n) {
  return n === null;
}

function stackAllLeft(_v, _s) {
  while(true) {
    var s = _s;
    var v = _v;
    if (v !== null) {
      _s = /* :: */[
        v,
        s
      ];
      _v = v.left;
      continue ;
    } else {
      return s;
    }
  };
}

function forEachU(_n, f) {
  while(true) {
    var n = _n;
    if (n !== null) {
      forEachU(n.left, f);
      f(n.value);
      _n = n.right;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function forEach(n, f) {
  return forEachU(n, Curry.__1(f));
}

function reduceU(_s, _accu, f) {
  while(true) {
    var accu = _accu;
    var s = _s;
    if (s !== null) {
      var l = s.left;
      var k = s.value;
      var r = s.right;
      _accu = f(reduceU(l, accu, f), k);
      _s = r;
      continue ;
    } else {
      return accu;
    }
  };
}

function reduce(s, accu, f) {
  return reduceU(s, accu, Curry.__2(f));
}

function everyU(_n, p) {
  while(true) {
    var n = _n;
    if (n !== null) {
      if (p(n.value) && everyU(n.left, p)) {
        _n = n.right;
        continue ;
      } else {
        return false;
      }
    } else {
      return true;
    }
  };
}

function every(n, p) {
  return everyU(n, Curry.__1(p));
}

function someU(_n, p) {
  while(true) {
    var n = _n;
    if (n !== null) {
      if (p(n.value) || someU(n.left, p)) {
        return true;
      } else {
        _n = n.right;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function some(n, p) {
  return someU(n, Curry.__1(p));
}

function addMinElement(n, v) {
  if (n !== null) {
    return bal(addMinElement(n.left, v), n.value, n.right);
  } else {
    return singleton(v);
  }
}

function addMaxElement(n, v) {
  if (n !== null) {
    return bal(n.left, n.value, addMaxElement(n.right, v));
  } else {
    return singleton(v);
  }
}

function joinShared(ln, v, rn) {
  if (ln !== null) {
    if (rn !== null) {
      var lh = ln.height;
      var rh = rn.height;
      if (lh > (rh + 2 | 0)) {
        return bal(ln.left, ln.value, joinShared(ln.right, v, rn));
      } else if (rh > (lh + 2 | 0)) {
        return bal(joinShared(ln, v, rn.left), rn.value, rn.right);
      } else {
        return create(ln, v, rn);
      }
    } else {
      return addMaxElement(ln, v);
    }
  } else {
    return addMinElement(rn, v);
  }
}

function concatShared(t1, t2) {
  if (t1 !== null) {
    if (t2 !== null) {
      var v = [t2.value];
      var t2r = removeMinAuxWithRef(t2, v);
      return joinShared(t1, v[0], t2r);
    } else {
      return t1;
    }
  } else {
    return t2;
  }
}

function partitionSharedU(n, p) {
  if (n !== null) {
    var value = n.value;
    var match = partitionSharedU(n.left, p);
    var lf = match[1];
    var lt = match[0];
    var pv = p(value);
    var match$1 = partitionSharedU(n.right, p);
    var rf = match$1[1];
    var rt = match$1[0];
    if (pv) {
      return /* tuple */[
              joinShared(lt, value, rt),
              concatShared(lf, rf)
            ];
    } else {
      return /* tuple */[
              concatShared(lt, rt),
              joinShared(lf, value, rf)
            ];
    }
  } else {
    return /* tuple */[
            null,
            null
          ];
  }
}

function partitionShared(n, p) {
  return partitionSharedU(n, Curry.__1(p));
}

function lengthNode(n) {
  var l = n.left;
  var r = n.right;
  var sizeL = l !== null ? lengthNode(l) : 0;
  var sizeR = r !== null ? lengthNode(r) : 0;
  return (1 + sizeL | 0) + sizeR | 0;
}

function size(n) {
  if (n !== null) {
    return lengthNode(n);
  } else {
    return 0;
  }
}

function toListAux(_n, _accu) {
  while(true) {
    var accu = _accu;
    var n = _n;
    if (n !== null) {
      _accu = /* :: */[
        n.value,
        toListAux(n.right, accu)
      ];
      _n = n.left;
      continue ;
    } else {
      return accu;
    }
  };
}

function toList(s) {
  return toListAux(s, /* [] */0);
}

function checkInvariantInternal(_v) {
  while(true) {
    var v = _v;
    if (v !== null) {
      var l = v.left;
      var r = v.right;
      var diff = treeHeight(l) - treeHeight(r) | 0;
      if (!(diff <= 2 && diff >= -2)) {
        throw new Error("File \"belt_internalAVLset.ml\", line 302, characters 6-12");
      }
      checkInvariantInternal(l);
      _v = r;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function fillArray(_n, _i, arr) {
  while(true) {
    var i = _i;
    var n = _n;
    var l = n.left;
    var v = n.value;
    var r = n.right;
    var next = l !== null ? fillArray(l, i, arr) : i;
    arr[next] = v;
    var rnext = next + 1 | 0;
    if (r !== null) {
      _i = rnext;
      _n = r;
      continue ;
    } else {
      return rnext;
    }
  };
}

function fillArrayWithPartition(_n, cursor, arr, p) {
  while(true) {
    var n = _n;
    var l = n.left;
    var v = n.value;
    var r = n.right;
    if (l !== null) {
      fillArrayWithPartition(l, cursor, arr, p);
    }
    if (p(v)) {
      var c = cursor.forward;
      arr[c] = v;
      cursor.forward = c + 1 | 0;
    } else {
      var c$1 = cursor.backward;
      arr[c$1] = v;
      cursor.backward = c$1 - 1 | 0;
    }
    if (r !== null) {
      _n = r;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function fillArrayWithFilter(_n, _i, arr, p) {
  while(true) {
    var i = _i;
    var n = _n;
    var l = n.left;
    var v = n.value;
    var r = n.right;
    var next = l !== null ? fillArrayWithFilter(l, i, arr, p) : i;
    var rnext = p(v) ? (arr[next] = v, next + 1 | 0) : next;
    if (r !== null) {
      _i = rnext;
      _n = r;
      continue ;
    } else {
      return rnext;
    }
  };
}

function toArray(n) {
  if (n !== null) {
    var size = lengthNode(n);
    var v = new Array(size);
    fillArray(n, 0, v);
    return v;
  } else {
    return /* array */[];
  }
}

function fromSortedArrayRevAux(arr, off, len) {
  if (len > 3 || len < 0) {
    var nl = len / 2 | 0;
    var left = fromSortedArrayRevAux(arr, off, nl);
    var mid = arr[off - nl | 0];
    var right = fromSortedArrayRevAux(arr, (off - nl | 0) - 1 | 0, (len - nl | 0) - 1 | 0);
    return create(left, mid, right);
  } else {
    switch (len) {
      case 0 : 
          return null;
      case 1 : 
          return singleton(arr[off]);
      case 2 : 
          var x0 = arr[off];
          var x1 = arr[off - 1 | 0];
          return {
                  value: x1,
                  height: 2,
                  left: singleton(x0),
                  right: null
                };
      case 3 : 
          var x0$1 = arr[off];
          var x1$1 = arr[off - 1 | 0];
          var x2 = arr[off - 2 | 0];
          return {
                  value: x1$1,
                  height: 2,
                  left: singleton(x0$1),
                  right: singleton(x2)
                };
      
    }
  }
}

function fromSortedArrayAux(arr, off, len) {
  if (len > 3 || len < 0) {
    var nl = len / 2 | 0;
    var left = fromSortedArrayAux(arr, off, nl);
    var mid = arr[off + nl | 0];
    var right = fromSortedArrayAux(arr, (off + nl | 0) + 1 | 0, (len - nl | 0) - 1 | 0);
    return create(left, mid, right);
  } else {
    switch (len) {
      case 0 : 
          return null;
      case 1 : 
          return singleton(arr[off]);
      case 2 : 
          var x0 = arr[off];
          var x1 = arr[off + 1 | 0];
          return {
                  value: x1,
                  height: 2,
                  left: singleton(x0),
                  right: null
                };
      case 3 : 
          var x0$1 = arr[off];
          var x1$1 = arr[off + 1 | 0];
          var x2 = arr[off + 2 | 0];
          return {
                  value: x1$1,
                  height: 2,
                  left: singleton(x0$1),
                  right: singleton(x2)
                };
      
    }
  }
}

function fromSortedArrayUnsafe(arr) {
  return fromSortedArrayAux(arr, 0, arr.length);
}

function keepSharedU(n, p) {
  if (n !== null) {
    var l = n.left;
    var v = n.value;
    var r = n.right;
    var newL = keepSharedU(l, p);
    var pv = p(v);
    var newR = keepSharedU(r, p);
    if (pv) {
      if (l === newL && r === newR) {
        return n;
      } else {
        return joinShared(newL, v, newR);
      }
    } else {
      return concatShared(newL, newR);
    }
  } else {
    return null;
  }
}

function keepShared(n, p) {
  return keepSharedU(n, Curry.__1(p));
}

function keepCopyU(n, p) {
  if (n !== null) {
    var size = lengthNode(n);
    var v = new Array(size);
    var last = fillArrayWithFilter(n, 0, v, p);
    return fromSortedArrayAux(v, 0, last);
  } else {
    return null;
  }
}

function keepCopy(n, p) {
  return keepCopyU(n, Curry.__1(p));
}

function partitionCopyU(n, p) {
  if (n !== null) {
    var size = lengthNode(n);
    var v = new Array(size);
    var backward = size - 1 | 0;
    var cursor = {
      forward: 0,
      backward: backward
    };
    fillArrayWithPartition(n, cursor, v, p);
    var forwardLen = cursor.forward;
    return /* tuple */[
            fromSortedArrayAux(v, 0, forwardLen),
            fromSortedArrayRevAux(v, backward, size - forwardLen | 0)
          ];
  } else {
    return /* tuple */[
            null,
            null
          ];
  }
}

function partitionCopy(n, p) {
  return partitionCopyU(n, Curry.__1(p));
}

function has(_t, x, cmp) {
  while(true) {
    var t = _t;
    if (t !== null) {
      var v = t.value;
      var c = cmp(x, v);
      if (c === 0) {
        return true;
      } else {
        _t = c < 0 ? t.left : t.right;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function cmp(s1, s2, cmp$1) {
  var len1 = size(s1);
  var len2 = size(s2);
  if (len1 === len2) {
    var _e1 = stackAllLeft(s1, /* [] */0);
    var _e2 = stackAllLeft(s2, /* [] */0);
    var cmp$2 = cmp$1;
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1 && e2) {
        var h2 = e2[0];
        var h1 = e1[0];
        var c = cmp$2(h1.value, h2.value);
        if (c === 0) {
          _e2 = stackAllLeft(h2.right, e2[1]);
          _e1 = stackAllLeft(h1.right, e1[1]);
          continue ;
        } else {
          return c;
        }
      } else {
        return 0;
      }
    };
  } else if (len1 < len2) {
    return -1;
  } else {
    return 1;
  }
}

function eq(s1, s2, c) {
  return cmp(s1, s2, c) === 0;
}

function subset(_s1, _s2, cmp) {
  while(true) {
    var s2 = _s2;
    var s1 = _s1;
    if (s1 !== null) {
      if (s2 !== null) {
        var l1 = s1.left;
        var v1 = s1.value;
        var r1 = s1.right;
        var l2 = s2.left;
        var v2 = s2.value;
        var r2 = s2.right;
        var c = cmp(v1, v2);
        if (c === 0) {
          if (subset(l1, l2, cmp)) {
            _s2 = r2;
            _s1 = r1;
            continue ;
          } else {
            return false;
          }
        } else if (c < 0) {
          if (subset(create(l1, v1, null), l2, cmp)) {
            _s1 = r1;
            continue ;
          } else {
            return false;
          }
        } else if (subset(create(null, v1, r1), r2, cmp)) {
          _s1 = l1;
          continue ;
        } else {
          return false;
        }
      } else {
        return false;
      }
    } else {
      return true;
    }
  };
}

function get(_n, x, cmp) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.value;
      var c = cmp(x, v);
      if (c === 0) {
        return /* Some */[v];
      } else {
        _n = c < 0 ? n.left : n.right;
        continue ;
      }
    } else {
      return /* None */0;
    }
  };
}

function getUndefined(_n, x, cmp) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.value;
      var c = cmp(x, v);
      if (c === 0) {
        return v;
      } else {
        _n = c < 0 ? n.left : n.right;
        continue ;
      }
    } else {
      return undefined;
    }
  };
}

function getExn(_n, x, cmp) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.value;
      var c = cmp(x, v);
      if (c === 0) {
        return v;
      } else {
        _n = c < 0 ? n.left : n.right;
        continue ;
      }
    } else {
      throw new Error("getExn0");
    }
  };
}

function rotateWithLeftChild(k2) {
  var k1 = k2.left;
  k2.left = k1.right;
  k1.right = k2;
  var hlk2 = treeHeight(k2.left);
  var hrk2 = treeHeight(k2.right);
  k2.height = (
    hlk2 > hrk2 ? hlk2 : hrk2
  ) + 1 | 0;
  var hlk1 = treeHeight(k1.left);
  var hk2 = k2.height;
  k1.height = (
    hlk1 > hk2 ? hlk1 : hk2
  ) + 1 | 0;
  return k1;
}

function rotateWithRightChild(k1) {
  var k2 = k1.right;
  k1.right = k2.left;
  k2.left = k1;
  var hlk1 = treeHeight(k1.left);
  var hrk1 = treeHeight(k1.right);
  k1.height = (
    hlk1 > hrk1 ? hlk1 : hrk1
  ) + 1 | 0;
  var hrk2 = treeHeight(k2.right);
  var hk1 = k1.height;
  k2.height = (
    hrk2 > hk1 ? hrk2 : hk1
  ) + 1 | 0;
  return k2;
}

function doubleWithLeftChild(k3) {
  var v = rotateWithRightChild(k3.left);
  k3.left = v;
  return rotateWithLeftChild(k3);
}

function doubleWithRightChild(k2) {
  var v = rotateWithLeftChild(k2.right);
  k2.right = v;
  return rotateWithRightChild(k2);
}

function heightUpdateMutate(t) {
  var hlt = treeHeight(t.left);
  var hrt = treeHeight(t.right);
  t.height = (
    hlt > hrt ? hlt : hrt
  ) + 1 | 0;
  return t;
}

function balMutate(nt) {
  var l = nt.left;
  var r = nt.right;
  var hl = treeHeight(l);
  var hr = treeHeight(r);
  if (hl > (2 + hr | 0)) {
    var ll = l.left;
    var lr = l.right;
    if (heightGe(ll, lr)) {
      return heightUpdateMutate(rotateWithLeftChild(nt));
    } else {
      return heightUpdateMutate(doubleWithLeftChild(nt));
    }
  } else if (hr > (2 + hl | 0)) {
    var rl = r.left;
    var rr = r.right;
    if (heightGe(rr, rl)) {
      return heightUpdateMutate(rotateWithRightChild(nt));
    } else {
      return heightUpdateMutate(doubleWithRightChild(nt));
    }
  } else {
    nt.height = (
      hl > hr ? hl : hr
    ) + 1 | 0;
    return nt;
  }
}

function addMutate(cmp, t, x) {
  if (t !== null) {
    var k = t.value;
    var c = cmp(x, k);
    if (c === 0) {
      return t;
    } else {
      var l = t.left;
      var r = t.right;
      if (c < 0) {
        var ll = addMutate(cmp, l, x);
        t.left = ll;
      } else {
        t.right = addMutate(cmp, r, x);
      }
      return balMutate(t);
    }
  } else {
    return singleton(x);
  }
}

function fromArray(xs, cmp) {
  var len = xs.length;
  if (len === 0) {
    return null;
  } else {
    var next = Belt_SortArray.strictlySortedLengthU(xs, (function (x, y) {
            return cmp(x, y) < 0;
          }));
    var result;
    if (next >= 0) {
      result = fromSortedArrayAux(xs, 0, next);
    } else {
      next = -next | 0;
      result = fromSortedArrayRevAux(xs, next - 1 | 0, next);
    }
    for(var i = next ,i_finish = len - 1 | 0; i <= i_finish; ++i){
      result = addMutate(cmp, result, xs[i]);
    }
    return result;
  }
}

function removeMinAuxWithRootMutate(nt, n) {
  var rn = n.right;
  var ln = n.left;
  if (ln !== null) {
    n.left = removeMinAuxWithRootMutate(nt, ln);
    return balMutate(n);
  } else {
    nt.value = n.value;
    return rn;
  }
}

var empty = null;

exports.copy = copy;
exports.create = create;
exports.bal = bal;
exports.singleton = singleton;
exports.minimum = minimum;
exports.minUndefined = minUndefined;
exports.maximum = maximum;
exports.maxUndefined = maxUndefined;
exports.removeMinAuxWithRef = removeMinAuxWithRef;
exports.empty = empty;
exports.isEmpty = isEmpty;
exports.stackAllLeft = stackAllLeft;
exports.forEachU = forEachU;
exports.forEach = forEach;
exports.reduceU = reduceU;
exports.reduce = reduce;
exports.everyU = everyU;
exports.every = every;
exports.someU = someU;
exports.some = some;
exports.joinShared = joinShared;
exports.concatShared = concatShared;
exports.keepSharedU = keepSharedU;
exports.keepShared = keepShared;
exports.keepCopyU = keepCopyU;
exports.keepCopy = keepCopy;
exports.partitionSharedU = partitionSharedU;
exports.partitionShared = partitionShared;
exports.partitionCopyU = partitionCopyU;
exports.partitionCopy = partitionCopy;
exports.lengthNode = lengthNode;
exports.size = size;
exports.toList = toList;
exports.checkInvariantInternal = checkInvariantInternal;
exports.fillArray = fillArray;
exports.toArray = toArray;
exports.fromSortedArrayAux = fromSortedArrayAux;
exports.fromSortedArrayRevAux = fromSortedArrayRevAux;
exports.fromSortedArrayUnsafe = fromSortedArrayUnsafe;
exports.has = has;
exports.cmp = cmp;
exports.eq = eq;
exports.subset = subset;
exports.get = get;
exports.getUndefined = getUndefined;
exports.getExn = getExn;
exports.fromArray = fromArray;
exports.addMutate = addMutate;
exports.balMutate = balMutate;
exports.removeMinAuxWithRootMutate = removeMinAuxWithRootMutate;
/* No side effect */
