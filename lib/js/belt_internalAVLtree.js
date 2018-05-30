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
            key: n.key,
            value: n.value,
            height: n.height,
            left: copy(l),
            right: copy(r)
          };
  } else {
    return n;
  }
}

function create(l, x, d, r) {
  var hl = treeHeight(l);
  var hr = treeHeight(r);
  return {
          key: x,
          value: d,
          height: hl >= hr ? hl + 1 | 0 : hr + 1 | 0,
          left: l,
          right: r
        };
}

function singleton(x, d) {
  return {
          key: x,
          value: d,
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

function updateValue(n, newValue) {
  if (n.value === newValue) {
    return n;
  } else {
    return {
            key: n.key,
            value: newValue,
            height: n.height,
            left: n.left,
            right: n.right
          };
  }
}

function bal(l, x, d, r) {
  var hl = l !== null ? l.height : 0;
  var hr = r !== null ? r.height : 0;
  if (hl > (hr + 2 | 0)) {
    var ll = l.left;
    var lv = l.key;
    var ld = l.value;
    var lr = l.right;
    if (treeHeight(ll) >= treeHeight(lr)) {
      return create(ll, lv, ld, create(lr, x, d, r));
    } else {
      var lrl = lr.left;
      var lrv = lr.key;
      var lrd = lr.value;
      var lrr = lr.right;
      return create(create(ll, lv, ld, lrl), lrv, lrd, create(lrr, x, d, r));
    }
  } else if (hr > (hl + 2 | 0)) {
    var rl = r.left;
    var rv = r.key;
    var rd = r.value;
    var rr = r.right;
    if (treeHeight(rr) >= treeHeight(rl)) {
      return create(create(l, x, d, rl), rv, rd, rr);
    } else {
      var rll = rl.left;
      var rlv = rl.key;
      var rld = rl.value;
      var rlr = rl.right;
      return create(create(l, x, d, rll), rlv, rld, create(rlr, rv, rd, rr));
    }
  } else {
    return {
            key: x,
            value: d,
            height: hl >= hr ? hl + 1 | 0 : hr + 1 | 0,
            left: l,
            right: r
          };
  }
}

function minKey0Aux(_n) {
  while(true) {
    var n = _n;
    var match = n.left;
    if (match !== null) {
      _n = match;
      continue ;
    } else {
      return n.key;
    }
  };
}

function minKey(n) {
  if (n !== null) {
    return /* Some */[minKey0Aux(n)];
  } else {
    return /* None */0;
  }
}

function minKeyUndefined(n) {
  if (n !== null) {
    return minKey0Aux(n);
  } else {
    return undefined;
  }
}

function maxKey0Aux(_n) {
  while(true) {
    var n = _n;
    var match = n.right;
    if (match !== null) {
      _n = match;
      continue ;
    } else {
      return n.key;
    }
  };
}

function maxKey(n) {
  if (n !== null) {
    return /* Some */[maxKey0Aux(n)];
  } else {
    return /* None */0;
  }
}

function maxKeyUndefined(n) {
  if (n !== null) {
    return maxKey0Aux(n);
  } else {
    return undefined;
  }
}

function minKV0Aux(_n) {
  while(true) {
    var n = _n;
    var match = n.left;
    if (match !== null) {
      _n = match;
      continue ;
    } else {
      return /* tuple */[
              n.key,
              n.value
            ];
    }
  };
}

function minimum(n) {
  if (n !== null) {
    return /* Some */[minKV0Aux(n)];
  } else {
    return /* None */0;
  }
}

function minUndefined(n) {
  if (n !== null) {
    return minKV0Aux(n);
  } else {
    return undefined;
  }
}

function maxKV0Aux(_n) {
  while(true) {
    var n = _n;
    var match = n.right;
    if (match !== null) {
      _n = match;
      continue ;
    } else {
      return /* tuple */[
              n.key,
              n.value
            ];
    }
  };
}

function maximum(n) {
  if (n !== null) {
    return /* Some */[maxKV0Aux(n)];
  } else {
    return /* None */0;
  }
}

function maxUndefined(n) {
  if (n !== null) {
    return maxKV0Aux(n);
  } else {
    return undefined;
  }
}

function removeMinAuxWithRef(n, kr, vr) {
  var ln = n.left;
  var rn = n.right;
  var kn = n.key;
  var vn = n.value;
  if (ln !== null) {
    return bal(removeMinAuxWithRef(ln, kr, vr), kn, vn, rn);
  } else {
    kr[0] = kn;
    vr[0] = vn;
    return rn;
  }
}

function isEmpty(x) {
  return x === null;
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
      f(n.key, n.value);
      _n = n.right;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function forEach(n, f) {
  return forEachU(n, Curry.__2(f));
}

function mapU(n, f) {
  if (n !== null) {
    var newLeft = mapU(n.left, f);
    var newD = f(n.value);
    var newRight = mapU(n.right, f);
    return {
            key: n.key,
            value: newD,
            height: n.height,
            left: newLeft,
            right: newRight
          };
  } else {
    return null;
  }
}

function map(n, f) {
  return mapU(n, Curry.__1(f));
}

function mapWithKeyU(n, f) {
  if (n !== null) {
    var key = n.key;
    var newLeft = mapWithKeyU(n.left, f);
    var newD = f(key, n.value);
    var newRight = mapWithKeyU(n.right, f);
    return {
            key: key,
            value: newD,
            height: n.height,
            left: newLeft,
            right: newRight
          };
  } else {
    return null;
  }
}

function mapWithKey(n, f) {
  return mapWithKeyU(n, Curry.__2(f));
}

function reduceU(_m, _accu, f) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (m !== null) {
      var l = m.left;
      var v = m.key;
      var d = m.value;
      var r = m.right;
      _accu = f(reduceU(l, accu, f), v, d);
      _m = r;
      continue ;
    } else {
      return accu;
    }
  };
}

function reduce(m, accu, f) {
  return reduceU(m, accu, Curry.__3(f));
}

function everyU(_n, p) {
  while(true) {
    var n = _n;
    if (n !== null) {
      if (p(n.key, n.value) && everyU(n.left, p)) {
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
  return everyU(n, Curry.__2(p));
}

function someU(_n, p) {
  while(true) {
    var n = _n;
    if (n !== null) {
      if (p(n.key, n.value) || someU(n.left, p)) {
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
  return someU(n, Curry.__2(p));
}

function addMinElement(n, k, v) {
  if (n !== null) {
    return bal(addMinElement(n.left, k, v), n.key, n.value, n.right);
  } else {
    return singleton(k, v);
  }
}

function addMaxElement(n, k, v) {
  if (n !== null) {
    return bal(n.left, n.key, n.value, addMaxElement(n.right, k, v));
  } else {
    return singleton(k, v);
  }
}

function join(ln, v, d, rn) {
  if (ln !== null) {
    if (rn !== null) {
      var ll = ln.left;
      var lv = ln.key;
      var ld = ln.value;
      var lr = ln.right;
      var lh = ln.height;
      var rl = rn.left;
      var rv = rn.key;
      var rd = rn.value;
      var rr = rn.right;
      var rh = rn.height;
      if (lh > (rh + 2 | 0)) {
        return bal(ll, lv, ld, join(lr, v, d, rn));
      } else if (rh > (lh + 2 | 0)) {
        return bal(join(ln, v, d, rl), rv, rd, rr);
      } else {
        return create(ln, v, d, rn);
      }
    } else {
      return addMaxElement(ln, v, d);
    }
  } else {
    return addMinElement(rn, v, d);
  }
}

function concat(t1, t2) {
  if (t1 !== null) {
    if (t2 !== null) {
      var kr = [t2.key];
      var vr = [t2.value];
      var t2r = removeMinAuxWithRef(t2, kr, vr);
      return join(t1, kr[0], vr[0], t2r);
    } else {
      return t1;
    }
  } else {
    return t2;
  }
}

function concatOrJoin(t1, v, d, t2) {
  if (d) {
    return join(t1, v, d[0], t2);
  } else {
    return concat(t1, t2);
  }
}

function keepSharedU(n, p) {
  if (n !== null) {
    var v = n.key;
    var d = n.value;
    var newLeft = keepSharedU(n.left, p);
    var pvd = p(v, d);
    var newRight = keepSharedU(n.right, p);
    if (pvd) {
      return join(newLeft, v, d, newRight);
    } else {
      return concat(newLeft, newRight);
    }
  } else {
    return null;
  }
}

function keepShared(n, p) {
  return keepSharedU(n, Curry.__2(p));
}

function keepMapU(n, p) {
  if (n !== null) {
    var v = n.key;
    var d = n.value;
    var newLeft = keepMapU(n.left, p);
    var pvd = p(v, d);
    var newRight = keepMapU(n.right, p);
    if (pvd) {
      return join(newLeft, v, pvd[0], newRight);
    } else {
      return concat(newLeft, newRight);
    }
  } else {
    return null;
  }
}

function keepMap(n, p) {
  return keepMapU(n, Curry.__2(p));
}

function partitionSharedU(n, p) {
  if (n !== null) {
    var key = n.key;
    var value = n.value;
    var match = partitionSharedU(n.left, p);
    var lf = match[1];
    var lt = match[0];
    var pvd = p(key, value);
    var match$1 = partitionSharedU(n.right, p);
    var rf = match$1[1];
    var rt = match$1[0];
    if (pvd) {
      return /* tuple */[
              join(lt, key, value, rt),
              concat(lf, rf)
            ];
    } else {
      return /* tuple */[
              concat(lt, rt),
              join(lf, key, value, rf)
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
  return partitionSharedU(n, Curry.__2(p));
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
      var l = n.left;
      var r = n.right;
      var k = n.key;
      var v = n.value;
      _accu = /* :: */[
        /* tuple */[
          k,
          v
        ],
        toListAux(r, accu)
      ];
      _n = l;
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
        throw new Error("File \"belt_internalAVLtree.ml\", line 369, characters 6-12");
      }
      checkInvariantInternal(l);
      _v = r;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function fillArrayKey(_n, _i, arr) {
  while(true) {
    var i = _i;
    var n = _n;
    var l = n.left;
    var v = n.key;
    var r = n.right;
    var next = l !== null ? fillArrayKey(l, i, arr) : i;
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

function fillArrayValue(_n, _i, arr) {
  while(true) {
    var i = _i;
    var n = _n;
    var l = n.left;
    var r = n.right;
    var next = l !== null ? fillArrayValue(l, i, arr) : i;
    arr[next] = n.value;
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

function fillArray(_n, _i, arr) {
  while(true) {
    var i = _i;
    var n = _n;
    var l = n.left;
    var v = n.key;
    var r = n.right;
    var next = l !== null ? fillArray(l, i, arr) : i;
    arr[next] = /* tuple */[
      v,
      n.value
    ];
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

function keysToArray(n) {
  if (n !== null) {
    var size = lengthNode(n);
    var v = new Array(size);
    fillArrayKey(n, 0, v);
    return v;
  } else {
    return /* array */[];
  }
}

function valuesToArray(n) {
  if (n !== null) {
    var size = lengthNode(n);
    var v = new Array(size);
    fillArrayValue(n, 0, v);
    return v;
  } else {
    return /* array */[];
  }
}

function fromSortedArrayRevAux(arr, off, len) {
  if (len > 3 || len < 0) {
    var nl = len / 2 | 0;
    var left = fromSortedArrayRevAux(arr, off, nl);
    var match = arr[off - nl | 0];
    var right = fromSortedArrayRevAux(arr, (off - nl | 0) - 1 | 0, (len - nl | 0) - 1 | 0);
    return create(left, match[0], match[1], right);
  } else {
    switch (len) {
      case 0 : 
          return null;
      case 1 : 
          var match$1 = arr[off];
          return singleton(match$1[0], match$1[1]);
      case 2 : 
          var match_000 = arr[off];
          var match_001 = arr[off - 1 | 0];
          var match$2 = match_001;
          var match$3 = match_000;
          return {
                  key: match$2[0],
                  value: match$2[1],
                  height: 2,
                  left: singleton(match$3[0], match$3[1]),
                  right: null
                };
      case 3 : 
          var match_000$1 = arr[off];
          var match_001$1 = arr[off - 1 | 0];
          var match_002 = arr[off - 2 | 0];
          var match$4 = match_002;
          var match$5 = match_001$1;
          var match$6 = match_000$1;
          return {
                  key: match$5[0],
                  value: match$5[1],
                  height: 2,
                  left: singleton(match$6[0], match$6[1]),
                  right: singleton(match$4[0], match$4[1])
                };
      
    }
  }
}

function fromSortedArrayAux(arr, off, len) {
  if (len > 3 || len < 0) {
    var nl = len / 2 | 0;
    var left = fromSortedArrayAux(arr, off, nl);
    var match = arr[off + nl | 0];
    var right = fromSortedArrayAux(arr, (off + nl | 0) + 1 | 0, (len - nl | 0) - 1 | 0);
    return create(left, match[0], match[1], right);
  } else {
    switch (len) {
      case 0 : 
          return null;
      case 1 : 
          var match$1 = arr[off];
          return singleton(match$1[0], match$1[1]);
      case 2 : 
          var match_000 = arr[off];
          var match_001 = arr[off + 1 | 0];
          var match$2 = match_001;
          var match$3 = match_000;
          return {
                  key: match$2[0],
                  value: match$2[1],
                  height: 2,
                  left: singleton(match$3[0], match$3[1]),
                  right: null
                };
      case 3 : 
          var match_000$1 = arr[off];
          var match_001$1 = arr[off + 1 | 0];
          var match_002 = arr[off + 2 | 0];
          var match$4 = match_002;
          var match$5 = match_001$1;
          var match$6 = match_000$1;
          return {
                  key: match$5[0],
                  value: match$5[1],
                  height: 2,
                  left: singleton(match$6[0], match$6[1]),
                  right: singleton(match$4[0], match$4[1])
                };
      
    }
  }
}

function fromSortedArrayUnsafe(arr) {
  return fromSortedArrayAux(arr, 0, arr.length);
}

function cmpU(s1, s2, kcmp, vcmp) {
  var len1 = size(s1);
  var len2 = size(s2);
  if (len1 === len2) {
    var _e1 = stackAllLeft(s1, /* [] */0);
    var _e2 = stackAllLeft(s2, /* [] */0);
    var kcmp$1 = kcmp;
    var vcmp$1 = vcmp;
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1 && e2) {
        var h2 = e2[0];
        var h1 = e1[0];
        var c = kcmp$1(h1.key, h2.key);
        if (c === 0) {
          var cx = vcmp$1(h1.value, h2.value);
          if (cx === 0) {
            _e2 = stackAllLeft(h2.right, e2[1]);
            _e1 = stackAllLeft(h1.right, e1[1]);
            continue ;
          } else {
            return cx;
          }
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

function cmp(s1, s2, kcmp, vcmp) {
  return cmpU(s1, s2, kcmp, Curry.__2(vcmp));
}

function eqU(s1, s2, kcmp, veq) {
  var len1 = size(s1);
  var len2 = size(s2);
  if (len1 === len2) {
    var _e1 = stackAllLeft(s1, /* [] */0);
    var _e2 = stackAllLeft(s2, /* [] */0);
    var kcmp$1 = kcmp;
    var veq$1 = veq;
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1 && e2) {
        var h2 = e2[0];
        var h1 = e1[0];
        if (kcmp$1(h1.key, h2.key) === 0 && veq$1(h1.value, h2.value)) {
          _e2 = stackAllLeft(h2.right, e2[1]);
          _e1 = stackAllLeft(h1.right, e1[1]);
          continue ;
        } else {
          return false;
        }
      } else {
        return true;
      }
    };
  } else {
    return false;
  }
}

function eq(s1, s2, kcmp, veq) {
  return eqU(s1, s2, kcmp, Curry.__2(veq));
}

function get(_n, x, cmp) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      var c = cmp(x, v);
      if (c === 0) {
        return /* Some */[n.value];
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
      var v = n.key;
      var c = cmp(x, v);
      if (c === 0) {
        return n.value;
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
      var v = n.key;
      var c = cmp(x, v);
      if (c === 0) {
        return n.value;
      } else {
        _n = c < 0 ? n.left : n.right;
        continue ;
      }
    } else {
      throw new Error("getExn0");
    }
  };
}

function getWithDefault(_n, x, def, cmp) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      var c = cmp(x, v);
      if (c === 0) {
        return n.value;
      } else {
        _n = c < 0 ? n.left : n.right;
        continue ;
      }
    } else {
      return def;
    }
  };
}

function has(_n, x, cmp) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      var c = cmp(x, v);
      if (c === 0) {
        return true;
      } else {
        _n = c < 0 ? n.left : n.right;
        continue ;
      }
    } else {
      return false;
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

function updateMutate(t, x, data, cmp) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(x, k);
    if (c === 0) {
      t.value = data;
      return t;
    } else {
      var l = t.left;
      var r = t.right;
      if (c < 0) {
        var ll = updateMutate(l, x, data, cmp);
        t.left = ll;
      } else {
        t.right = updateMutate(r, x, data, cmp);
      }
      return balMutate(t);
    }
  } else {
    return singleton(x, data);
  }
}

function fromArray(xs, cmp) {
  var len = xs.length;
  if (len === 0) {
    return null;
  } else {
    var next = Belt_SortArray.strictlySortedLengthU(xs, (function (param, param$1) {
            return cmp(param[0], param$1[0]) < 0;
          }));
    var result;
    if (next >= 0) {
      result = fromSortedArrayAux(xs, 0, next);
    } else {
      next = -next | 0;
      result = fromSortedArrayRevAux(xs, next - 1 | 0, next);
    }
    for(var i = next ,i_finish = len - 1 | 0; i <= i_finish; ++i){
      var match = xs[i];
      result = updateMutate(result, match[0], match[1], cmp);
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
    nt.key = n.key;
    return rn;
  }
}

var empty = null;

exports.copy = copy;
exports.create = create;
exports.bal = bal;
exports.singleton = singleton;
exports.updateValue = updateValue;
exports.minKey = minKey;
exports.minKeyUndefined = minKeyUndefined;
exports.maxKey = maxKey;
exports.maxKeyUndefined = maxKeyUndefined;
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
exports.mapU = mapU;
exports.map = map;
exports.mapWithKeyU = mapWithKeyU;
exports.mapWithKey = mapWithKey;
exports.reduceU = reduceU;
exports.reduce = reduce;
exports.everyU = everyU;
exports.every = every;
exports.someU = someU;
exports.some = some;
exports.join = join;
exports.concat = concat;
exports.concatOrJoin = concatOrJoin;
exports.keepSharedU = keepSharedU;
exports.keepShared = keepShared;
exports.keepMapU = keepMapU;
exports.keepMap = keepMap;
exports.partitionSharedU = partitionSharedU;
exports.partitionShared = partitionShared;
exports.lengthNode = lengthNode;
exports.size = size;
exports.toList = toList;
exports.checkInvariantInternal = checkInvariantInternal;
exports.fillArray = fillArray;
exports.toArray = toArray;
exports.keysToArray = keysToArray;
exports.valuesToArray = valuesToArray;
exports.fromSortedArrayAux = fromSortedArrayAux;
exports.fromSortedArrayRevAux = fromSortedArrayRevAux;
exports.fromSortedArrayUnsafe = fromSortedArrayUnsafe;
exports.cmpU = cmpU;
exports.cmp = cmp;
exports.eqU = eqU;
exports.eq = eq;
exports.get = get;
exports.getUndefined = getUndefined;
exports.getWithDefault = getWithDefault;
exports.getExn = getExn;
exports.has = has;
exports.fromArray = fromArray;
exports.updateMutate = updateMutate;
exports.balMutate = balMutate;
exports.removeMinAuxWithRootMutate = removeMinAuxWithRootMutate;
/* No side effect */
