

import * as Curry from "./curry.js";
import * as Caml_option from "./caml_option.js";
import * as Belt_SortArray from "./belt_SortArray.js";

function treeHeight(n) {
  if (n !== undefined) {
    return n.h;
  } else {
    return 0;
  }
}

function copy(n) {
  if (n !== undefined) {
    return {
            k: n.k,
            v: n.v,
            h: n.h,
            l: copy(n.l),
            r: copy(n.r)
          };
  } else {
    return n;
  }
}

function create(l, x, d, r) {
  var hl = treeHeight(l);
  var hr = treeHeight(r);
  return {
          k: x,
          v: d,
          h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0,
          l: l,
          r: r
        };
}

function singleton(x, d) {
  return {
          k: x,
          v: d,
          h: 1,
          l: undefined,
          r: undefined
        };
}

function heightGe(l, r) {
  if (r !== undefined) {
    if (l !== undefined) {
      return l.h >= r.h;
    } else {
      return false;
    }
  } else {
    return true;
  }
}

function updateValue(n, newValue) {
  if (n.v === newValue) {
    return n;
  } else {
    return {
            k: n.k,
            v: newValue,
            h: n.h,
            l: n.l,
            r: n.r
          };
  }
}

function bal(l, x, d, r) {
  var hl = l !== undefined ? l.h : 0;
  var hr = r !== undefined ? r.h : 0;
  if (hl > (hr + 2 | 0)) {
    var ll = l.l;
    var lr = l.r;
    if (treeHeight(ll) >= treeHeight(lr)) {
      return create(ll, l.k, l.v, create(lr, x, d, r));
    } else {
      return create(create(ll, l.k, l.v, lr.l), lr.k, lr.v, create(lr.r, x, d, r));
    }
  }
  if (hr <= (hl + 2 | 0)) {
    return {
            k: x,
            v: d,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0,
            l: l,
            r: r
          };
  }
  var rl = r.l;
  var rr = r.r;
  if (treeHeight(rr) >= treeHeight(rl)) {
    return create(create(l, x, d, rl), r.k, r.v, rr);
  } else {
    return create(create(l, x, d, rl.l), rl.k, rl.v, create(rl.r, r.k, r.v, rr));
  }
}

function minKey0Aux(_n) {
  while(true) {
    var n = _n;
    var n$1 = n.l;
    if (n$1 === undefined) {
      return n.k;
    }
    _n = n$1;
    continue ;
  };
}

function minKey(n) {
  if (n !== undefined) {
    return Caml_option.some(minKey0Aux(n));
  }
  
}

function minKeyUndefined(n) {
  if (n !== undefined) {
    return minKey0Aux(n);
  }
  
}

function maxKey0Aux(_n) {
  while(true) {
    var n = _n;
    var n$1 = n.r;
    if (n$1 === undefined) {
      return n.k;
    }
    _n = n$1;
    continue ;
  };
}

function maxKey(n) {
  if (n !== undefined) {
    return Caml_option.some(maxKey0Aux(n));
  }
  
}

function maxKeyUndefined(n) {
  if (n !== undefined) {
    return maxKey0Aux(n);
  }
  
}

function minKV0Aux(_n) {
  while(true) {
    var n = _n;
    var n$1 = n.l;
    if (n$1 === undefined) {
      return [
              n.k,
              n.v
            ];
    }
    _n = n$1;
    continue ;
  };
}

function minimum(n) {
  if (n !== undefined) {
    return minKV0Aux(n);
  }
  
}

function minUndefined(n) {
  if (n !== undefined) {
    return minKV0Aux(n);
  }
  
}

function maxKV0Aux(_n) {
  while(true) {
    var n = _n;
    var n$1 = n.r;
    if (n$1 === undefined) {
      return [
              n.k,
              n.v
            ];
    }
    _n = n$1;
    continue ;
  };
}

function maximum(n) {
  if (n !== undefined) {
    return maxKV0Aux(n);
  }
  
}

function maxUndefined(n) {
  if (n !== undefined) {
    return maxKV0Aux(n);
  }
  
}

function removeMinAuxWithRef(n, kr, vr) {
  var ln = n.l;
  if (ln !== undefined) {
    return bal(removeMinAuxWithRef(ln, kr, vr), n.k, n.v, n.r);
  } else {
    kr.contents = n.k;
    vr.contents = n.v;
    return n.r;
  }
}

function isEmpty(x) {
  return x === undefined;
}

function stackAllLeft(_v, _s) {
  while(true) {
    var s = _s;
    var v = _v;
    if (v === undefined) {
      return s;
    }
    _s = {
      hd: v,
      tl: s
    };
    _v = v.l;
    continue ;
  };
}

function findFirstByU(n, p) {
  if (n === undefined) {
    return ;
  }
  var left = findFirstByU(n.l, p);
  if (left !== undefined) {
    return left;
  }
  var v = n.k;
  var d = n.v;
  var pvd = p(v, d);
  if (pvd) {
    return [
            v,
            d
          ];
  }
  var right = findFirstByU(n.r, p);
  if (right !== undefined) {
    return right;
  }
  
}

function findFirstBy(n, p) {
  return findFirstByU(n, Curry.__2(p));
}

function forEachU(_n, f) {
  while(true) {
    var n = _n;
    if (n === undefined) {
      return ;
    }
    forEachU(n.l, f);
    f(n.k, n.v);
    _n = n.r;
    continue ;
  };
}

function forEach(n, f) {
  return forEachU(n, Curry.__2(f));
}

function mapU(n, f) {
  if (n === undefined) {
    return ;
  }
  var newLeft = mapU(n.l, f);
  var newD = f(n.v);
  var newRight = mapU(n.r, f);
  return {
          k: n.k,
          v: newD,
          h: n.h,
          l: newLeft,
          r: newRight
        };
}

function map(n, f) {
  return mapU(n, Curry.__1(f));
}

function mapWithKeyU(n, f) {
  if (n === undefined) {
    return ;
  }
  var key = n.k;
  var newLeft = mapWithKeyU(n.l, f);
  var newD = f(key, n.v);
  var newRight = mapWithKeyU(n.r, f);
  return {
          k: key,
          v: newD,
          h: n.h,
          l: newLeft,
          r: newRight
        };
}

function mapWithKey(n, f) {
  return mapWithKeyU(n, Curry.__2(f));
}

function reduceU(_m, _accu, f) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (m === undefined) {
      return accu;
    }
    var v = m.k;
    var d = m.v;
    var l = m.l;
    var r = m.r;
    _accu = f(reduceU(l, accu, f), v, d);
    _m = r;
    continue ;
  };
}

function reduce(m, accu, f) {
  return reduceU(m, accu, Curry.__3(f));
}

function everyU(_n, p) {
  while(true) {
    var n = _n;
    if (n === undefined) {
      return true;
    }
    if (!p(n.k, n.v)) {
      return false;
    }
    if (!everyU(n.l, p)) {
      return false;
    }
    _n = n.r;
    continue ;
  };
}

function every(n, p) {
  return everyU(n, Curry.__2(p));
}

function someU(_n, p) {
  while(true) {
    var n = _n;
    if (n === undefined) {
      return false;
    }
    if (p(n.k, n.v)) {
      return true;
    }
    if (someU(n.l, p)) {
      return true;
    }
    _n = n.r;
    continue ;
  };
}

function some(n, p) {
  return someU(n, Curry.__2(p));
}

function addMinElement(n, k, v) {
  if (n !== undefined) {
    return bal(addMinElement(n.l, k, v), n.k, n.v, n.r);
  } else {
    return singleton(k, v);
  }
}

function addMaxElement(n, k, v) {
  if (n !== undefined) {
    return bal(n.l, n.k, n.v, addMaxElement(n.r, k, v));
  } else {
    return singleton(k, v);
  }
}

function join(ln, v, d, rn) {
  if (ln === undefined) {
    return addMinElement(rn, v, d);
  }
  if (rn === undefined) {
    return addMaxElement(ln, v, d);
  }
  var lv = ln.k;
  var ld = ln.v;
  var lh = ln.h;
  var ll = ln.l;
  var lr = ln.r;
  var rv = rn.k;
  var rd = rn.v;
  var rh = rn.h;
  var rl = rn.l;
  var rr = rn.r;
  if (lh > (rh + 2 | 0)) {
    return bal(ll, lv, ld, join(lr, v, d, rn));
  } else if (rh > (lh + 2 | 0)) {
    return bal(join(ln, v, d, rl), rv, rd, rr);
  } else {
    return create(ln, v, d, rn);
  }
}

function concat(t1, t2) {
  if (t1 === undefined) {
    return t2;
  }
  if (t2 === undefined) {
    return t1;
  }
  var kr = {
    contents: t2.k
  };
  var vr = {
    contents: t2.v
  };
  var t2r = removeMinAuxWithRef(t2, kr, vr);
  return join(t1, kr.contents, vr.contents, t2r);
}

function concatOrJoin(t1, v, d, t2) {
  if (d !== undefined) {
    return join(t1, v, Caml_option.valFromOption(d), t2);
  } else {
    return concat(t1, t2);
  }
}

function keepSharedU(n, p) {
  if (n === undefined) {
    return ;
  }
  var v = n.k;
  var d = n.v;
  var newLeft = keepSharedU(n.l, p);
  var pvd = p(v, d);
  var newRight = keepSharedU(n.r, p);
  if (pvd) {
    return join(newLeft, v, d, newRight);
  } else {
    return concat(newLeft, newRight);
  }
}

function keepShared(n, p) {
  return keepSharedU(n, Curry.__2(p));
}

function keepMapU(n, p) {
  if (n === undefined) {
    return ;
  }
  var v = n.k;
  var d = n.v;
  var newLeft = keepMapU(n.l, p);
  var pvd = p(v, d);
  var newRight = keepMapU(n.r, p);
  if (pvd !== undefined) {
    return join(newLeft, v, Caml_option.valFromOption(pvd), newRight);
  } else {
    return concat(newLeft, newRight);
  }
}

function keepMap(n, p) {
  return keepMapU(n, Curry.__2(p));
}

function partitionSharedU(n, p) {
  if (n === undefined) {
    return [
            undefined,
            undefined
          ];
  }
  var key = n.k;
  var value = n.v;
  var match = partitionSharedU(n.l, p);
  var lf = match[1];
  var lt = match[0];
  var pvd = p(key, value);
  var match$1 = partitionSharedU(n.r, p);
  var rf = match$1[1];
  var rt = match$1[0];
  if (pvd) {
    return [
            join(lt, key, value, rt),
            concat(lf, rf)
          ];
  } else {
    return [
            concat(lt, rt),
            join(lf, key, value, rf)
          ];
  }
}

function partitionShared(n, p) {
  return partitionSharedU(n, Curry.__2(p));
}

function lengthNode(n) {
  var l = n.l;
  var r = n.r;
  var sizeL = l !== undefined ? lengthNode(l) : 0;
  var sizeR = r !== undefined ? lengthNode(r) : 0;
  return (1 + sizeL | 0) + sizeR | 0;
}

function size(n) {
  if (n !== undefined) {
    return lengthNode(n);
  } else {
    return 0;
  }
}

function toListAux(_n, _accu) {
  while(true) {
    var accu = _accu;
    var n = _n;
    if (n === undefined) {
      return accu;
    }
    var k = n.k;
    var v = n.v;
    var l = n.l;
    var r = n.r;
    _accu = {
      hd: [
        k,
        v
      ],
      tl: toListAux(r, accu)
    };
    _n = l;
    continue ;
  };
}

function toList(s) {
  return toListAux(s, /* [] */0);
}

function checkInvariantInternal(_v) {
  while(true) {
    var v = _v;
    if (v === undefined) {
      return ;
    }
    var l = v.l;
    var r = v.r;
    var diff = treeHeight(l) - treeHeight(r) | 0;
    if (!(diff <= 2 && diff >= -2)) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "belt_internalAVLtree.ml",
              373,
              4
            ],
            Error: new Error()
          };
    }
    checkInvariantInternal(l);
    _v = r;
    continue ;
  };
}

function fillArrayKey(_n, _i, arr) {
  while(true) {
    var i = _i;
    var n = _n;
    var v = n.k;
    var l = n.l;
    var r = n.r;
    var next = l !== undefined ? fillArrayKey(l, i, arr) : i;
    arr[next] = v;
    var rnext = next + 1 | 0;
    if (r === undefined) {
      return rnext;
    }
    _i = rnext;
    _n = r;
    continue ;
  };
}

function fillArrayValue(_n, _i, arr) {
  while(true) {
    var i = _i;
    var n = _n;
    var l = n.l;
    var r = n.r;
    var next = l !== undefined ? fillArrayValue(l, i, arr) : i;
    arr[next] = n.v;
    var rnext = next + 1 | 0;
    if (r === undefined) {
      return rnext;
    }
    _i = rnext;
    _n = r;
    continue ;
  };
}

function fillArray(_n, _i, arr) {
  while(true) {
    var i = _i;
    var n = _n;
    var l = n.l;
    var v = n.k;
    var r = n.r;
    var next = l !== undefined ? fillArray(l, i, arr) : i;
    arr[next] = [
      v,
      n.v
    ];
    var rnext = next + 1 | 0;
    if (r === undefined) {
      return rnext;
    }
    _i = rnext;
    _n = r;
    continue ;
  };
}

function toArray(n) {
  if (n === undefined) {
    return [];
  }
  var size = lengthNode(n);
  var v = new Array(size);
  fillArray(n, 0, v);
  return v;
}

function keysToArray(n) {
  if (n === undefined) {
    return [];
  }
  var size = lengthNode(n);
  var v = new Array(size);
  fillArrayKey(n, 0, v);
  return v;
}

function valuesToArray(n) {
  if (n === undefined) {
    return [];
  }
  var size = lengthNode(n);
  var v = new Array(size);
  fillArrayValue(n, 0, v);
  return v;
}

function fromSortedArrayRevAux(arr, off, len) {
  switch (len) {
    case 0 :
        return ;
    case 1 :
        var match = arr[off];
        return singleton(match[0], match[1]);
    case 2 :
        var match_0 = arr[off];
        var match_1 = arr[off - 1 | 0];
        var match$1 = match_1;
        var match$2 = match_0;
        return {
                k: match$1[0],
                v: match$1[1],
                h: 2,
                l: singleton(match$2[0], match$2[1]),
                r: undefined
              };
    case 3 :
        var match_0$1 = arr[off];
        var match_1$1 = arr[off - 1 | 0];
        var match_2 = arr[off - 2 | 0];
        var match$3 = match_2;
        var match$4 = match_1$1;
        var match$5 = match_0$1;
        return {
                k: match$4[0],
                v: match$4[1],
                h: 2,
                l: singleton(match$5[0], match$5[1]),
                r: singleton(match$3[0], match$3[1])
              };
    default:
      var nl = len / 2 | 0;
      var left = fromSortedArrayRevAux(arr, off, nl);
      var match$6 = arr[off - nl | 0];
      var right = fromSortedArrayRevAux(arr, (off - nl | 0) - 1 | 0, (len - nl | 0) - 1 | 0);
      return create(left, match$6[0], match$6[1], right);
  }
}

function fromSortedArrayAux(arr, off, len) {
  switch (len) {
    case 0 :
        return ;
    case 1 :
        var match = arr[off];
        return singleton(match[0], match[1]);
    case 2 :
        var match_0 = arr[off];
        var match_1 = arr[off + 1 | 0];
        var match$1 = match_1;
        var match$2 = match_0;
        return {
                k: match$1[0],
                v: match$1[1],
                h: 2,
                l: singleton(match$2[0], match$2[1]),
                r: undefined
              };
    case 3 :
        var match_0$1 = arr[off];
        var match_1$1 = arr[off + 1 | 0];
        var match_2 = arr[off + 2 | 0];
        var match$3 = match_2;
        var match$4 = match_1$1;
        var match$5 = match_0$1;
        return {
                k: match$4[0],
                v: match$4[1],
                h: 2,
                l: singleton(match$5[0], match$5[1]),
                r: singleton(match$3[0], match$3[1])
              };
    default:
      var nl = len / 2 | 0;
      var left = fromSortedArrayAux(arr, off, nl);
      var match$6 = arr[off + nl | 0];
      var right = fromSortedArrayAux(arr, (off + nl | 0) + 1 | 0, (len - nl | 0) - 1 | 0);
      return create(left, match$6[0], match$6[1], right);
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
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (!e1) {
        return 0;
      }
      if (!e2) {
        return 0;
      }
      var h2 = e2.hd;
      var h1 = e1.hd;
      var c = kcmp(h1.k, h2.k);
      if (c !== 0) {
        return c;
      }
      var cx = vcmp(h1.v, h2.v);
      if (cx !== 0) {
        return cx;
      }
      _e2 = stackAllLeft(h2.r, e2.tl);
      _e1 = stackAllLeft(h1.r, e1.tl);
      continue ;
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
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (!e1) {
        return true;
      }
      if (!e2) {
        return true;
      }
      var h2 = e2.hd;
      var h1 = e1.hd;
      if (!(kcmp(h1.k, h2.k) === 0 && veq(h1.v, h2.v))) {
        return false;
      }
      _e2 = stackAllLeft(h2.r, e2.tl);
      _e1 = stackAllLeft(h1.r, e1.tl);
      continue ;
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
    if (n === undefined) {
      return ;
    }
    var v = n.k;
    var c = cmp(x, v);
    if (c === 0) {
      return Caml_option.some(n.v);
    }
    _n = c < 0 ? n.l : n.r;
    continue ;
  };
}

function getUndefined(_n, x, cmp) {
  while(true) {
    var n = _n;
    if (n === undefined) {
      return ;
    }
    var v = n.k;
    var c = cmp(x, v);
    if (c === 0) {
      return n.v;
    }
    _n = c < 0 ? n.l : n.r;
    continue ;
  };
}

function getExn(_n, x, cmp) {
  while(true) {
    var n = _n;
    if (n !== undefined) {
      var v = n.k;
      var c = cmp(x, v);
      if (c === 0) {
        return n.v;
      }
      _n = c < 0 ? n.l : n.r;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function getWithDefault(_n, x, def, cmp) {
  while(true) {
    var n = _n;
    if (n === undefined) {
      return def;
    }
    var v = n.k;
    var c = cmp(x, v);
    if (c === 0) {
      return n.v;
    }
    _n = c < 0 ? n.l : n.r;
    continue ;
  };
}

function has(_n, x, cmp) {
  while(true) {
    var n = _n;
    if (n === undefined) {
      return false;
    }
    var v = n.k;
    var c = cmp(x, v);
    if (c === 0) {
      return true;
    }
    _n = c < 0 ? n.l : n.r;
    continue ;
  };
}

function rotateWithLeftChild(k2) {
  var k1 = k2.l;
  k2.l = k1.r;
  k1.r = k2;
  var hlk2 = treeHeight(k2.l);
  var hrk2 = treeHeight(k2.r);
  k2.h = (
    hlk2 > hrk2 ? hlk2 : hrk2
  ) + 1 | 0;
  var hlk1 = treeHeight(k1.l);
  var hk2 = k2.h;
  k1.h = (
    hlk1 > hk2 ? hlk1 : hk2
  ) + 1 | 0;
  return k1;
}

function rotateWithRightChild(k1) {
  var k2 = k1.r;
  k1.r = k2.l;
  k2.l = k1;
  var hlk1 = treeHeight(k1.l);
  var hrk1 = treeHeight(k1.r);
  k1.h = (
    hlk1 > hrk1 ? hlk1 : hrk1
  ) + 1 | 0;
  var hrk2 = treeHeight(k2.r);
  var hk1 = k1.h;
  k2.h = (
    hrk2 > hk1 ? hrk2 : hk1
  ) + 1 | 0;
  return k2;
}

function doubleWithLeftChild(k3) {
  var x = k3.l;
  var v = rotateWithRightChild(x);
  k3.l = v;
  return rotateWithLeftChild(k3);
}

function doubleWithRightChild(k2) {
  var x = k2.r;
  var v = rotateWithLeftChild(x);
  k2.r = v;
  return rotateWithRightChild(k2);
}

function heightUpdateMutate(t) {
  var hlt = treeHeight(t.l);
  var hrt = treeHeight(t.r);
  t.h = (
    hlt > hrt ? hlt : hrt
  ) + 1 | 0;
  return t;
}

function balMutate(nt) {
  var l = nt.l;
  var r = nt.r;
  var hl = treeHeight(l);
  var hr = treeHeight(r);
  if (hl > (2 + hr | 0)) {
    var ll = l.l;
    var lr = l.r;
    if (heightGe(ll, lr)) {
      return heightUpdateMutate(rotateWithLeftChild(nt));
    } else {
      return heightUpdateMutate(doubleWithLeftChild(nt));
    }
  }
  if (hr > (2 + hl | 0)) {
    var rl = r.l;
    var rr = r.r;
    if (heightGe(rr, rl)) {
      return heightUpdateMutate(rotateWithRightChild(nt));
    } else {
      return heightUpdateMutate(doubleWithRightChild(nt));
    }
  }
  nt.h = (
    hl > hr ? hl : hr
  ) + 1 | 0;
  return nt;
}

function updateMutate(t, x, data, cmp) {
  if (t === undefined) {
    return singleton(x, data);
  }
  var k = t.k;
  var c = cmp(x, k);
  if (c === 0) {
    t.v = data;
    return t;
  }
  var l = t.l;
  var r = t.r;
  if (c < 0) {
    var ll = updateMutate(l, x, data, cmp);
    t.l = ll;
  } else {
    t.r = updateMutate(r, x, data, cmp);
  }
  return balMutate(t);
}

function fromArray(xs, cmp) {
  var len = xs.length;
  if (len === 0) {
    return ;
  }
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
  for(var i = next; i < len; ++i){
    var match = xs[i];
    result = updateMutate(result, match[0], match[1], cmp);
  }
  return result;
}

function removeMinAuxWithRootMutate(nt, n) {
  var rn = n.r;
  var ln = n.l;
  if (ln !== undefined) {
    n.l = removeMinAuxWithRootMutate(nt, ln);
    return balMutate(n);
  } else {
    nt.k = n.k;
    nt.v = n.v;
    return rn;
  }
}

export {
  copy ,
  create ,
  bal ,
  singleton ,
  updateValue ,
  minKey ,
  minKeyUndefined ,
  maxKey ,
  maxKeyUndefined ,
  minimum ,
  minUndefined ,
  maximum ,
  maxUndefined ,
  removeMinAuxWithRef ,
  isEmpty ,
  stackAllLeft ,
  findFirstByU ,
  findFirstBy ,
  forEachU ,
  forEach ,
  mapU ,
  map ,
  mapWithKeyU ,
  mapWithKey ,
  reduceU ,
  reduce ,
  everyU ,
  every ,
  someU ,
  some ,
  join ,
  concat ,
  concatOrJoin ,
  keepSharedU ,
  keepShared ,
  keepMapU ,
  keepMap ,
  partitionSharedU ,
  partitionShared ,
  lengthNode ,
  size ,
  toList ,
  checkInvariantInternal ,
  fillArray ,
  toArray ,
  keysToArray ,
  valuesToArray ,
  fromSortedArrayAux ,
  fromSortedArrayRevAux ,
  fromSortedArrayUnsafe ,
  cmpU ,
  cmp ,
  eqU ,
  eq ,
  get ,
  getUndefined ,
  getWithDefault ,
  getExn ,
  has ,
  fromArray ,
  updateMutate ,
  balMutate ,
  removeMinAuxWithRootMutate ,
  
}
/* No side effect */
