'use strict';

var Bs_Sort = require("./bs_Sort.js");

function height(n) {
  if (n !== null) {
    return n.h;
  } else {
    return 0;
  }
}

function copy(n) {
  if (n !== null) {
    var l = n.left;
    var r = n.right;
    return {
            left: copy(l),
            key: n.key,
            value: n.value,
            right: copy(r),
            h: n.h
          };
  } else {
    return n;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return {
          left: l,
          key: x,
          value: d,
          right: r,
          h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function singleton0(x, d) {
  return {
          left: null,
          key: x,
          value: d,
          right: null,
          h: 1
        };
}

function heightGe(l, r) {
  if (r !== null) {
    if (l !== null) {
      return +(l.h >= r.h);
    } else {
      return /* false */0;
    }
  } else {
    return /* true */1;
  }
}

function updateKV(n, key, value) {
  return {
          left: n.left,
          key: key,
          value: value,
          right: n.right,
          h: n.h
        };
}

function bal(l, x, d, r) {
  var hl = l !== null ? l.h : 0;
  var hr = r !== null ? r.h : 0;
  if (hl > (hr + 2 | 0)) {
    var ll = l.left;
    var lv = l.key;
    var ld = l.value;
    var lr = l.right;
    if (height(ll) >= height(lr)) {
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
    if (height(rr) >= height(rl)) {
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
            left: l,
            key: x,
            value: d,
            right: r,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
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

function minKVOpt0(n) {
  if (n !== null) {
    return /* Some */[minKV0Aux(n)];
  } else {
    return /* None */0;
  }
}

function minKVNull0(n) {
  if (n !== null) {
    return minKV0Aux(n);
  } else {
    return null;
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

function maxKVOpt0(n) {
  if (n !== null) {
    return /* Some */[maxKV0Aux(n)];
  } else {
    return /* None */0;
  }
}

function maxKVNull0(n) {
  if (n !== null) {
    return maxKV0Aux(n);
  } else {
    return null;
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

var empty0 = null;

function isEmpty0(x) {
  if (x !== null) {
    return /* false */0;
  } else {
    return /* true */1;
  }
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

function iter0(_n, f) {
  while(true) {
    var n = _n;
    if (n !== null) {
      iter0(n.left, f);
      f(n.key, n.value);
      _n = n.right;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function map0(n, f) {
  if (n !== null) {
    var newLeft = map0(n.left, f);
    var newD = f(n.value);
    var newRight = map0(n.right, f);
    return {
            left: newLeft,
            key: n.key,
            value: newD,
            right: newRight,
            h: n.h
          };
  } else {
    return null;
  }
}

function mapi0(n, f) {
  if (n !== null) {
    var key = n.key;
    var newLeft = mapi0(n.left, f);
    var newD = f(key, n.value);
    var newRight = mapi0(n.right, f);
    return {
            left: newLeft,
            key: key,
            value: newD,
            right: newRight,
            h: n.h
          };
  } else {
    return null;
  }
}

function fold0(_m, _accu, f) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (m !== null) {
      var l = m.left;
      var v = m.key;
      var d = m.value;
      var r = m.right;
      _accu = f(fold0(l, accu, f), v, d);
      _m = r;
      continue ;
      
    } else {
      return accu;
    }
  };
}

function forAll0(_n, p) {
  while(true) {
    var n = _n;
    if (n !== null) {
      if (p(n.key, n.value)) {
        if (forAll0(n.left, p)) {
          _n = n.right;
          continue ;
          
        } else {
          return /* false */0;
        }
      } else {
        return /* false */0;
      }
    } else {
      return /* true */1;
    }
  };
}

function exists0(_n, p) {
  while(true) {
    var n = _n;
    if (n !== null) {
      if (p(n.key, n.value)) {
        return /* true */1;
      } else if (exists0(n.left, p)) {
        return /* true */1;
      } else {
        _n = n.right;
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function addMinElement(n, k, v) {
  if (n !== null) {
    return bal(addMinElement(n.left, k, v), n.key, n.value, n.right);
  } else {
    return singleton0(k, v);
  }
}

function addMaxElement(n, k, v) {
  if (n !== null) {
    return bal(n.left, n.key, n.value, addMaxElement(n.right, k, v));
  } else {
    return singleton0(k, v);
  }
}

function join(ln, v, d, rn) {
  if (ln !== null) {
    if (rn !== null) {
      var ll = ln.left;
      var lv = ln.key;
      var ld = ln.value;
      var lr = ln.right;
      var lh = ln.h;
      var rl = rn.left;
      var rv = rn.key;
      var rd = rn.value;
      var rr = rn.right;
      var rh = rn.h;
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

function filterShared0(p, n) {
  if (n !== null) {
    var v = n.key;
    var d = n.value;
    var newLeft = filterShared0(p, n.left);
    var pvd = p(v, d);
    var newRight = filterShared0(p, n.right);
    if (pvd) {
      return join(newLeft, v, d, newRight);
    } else {
      return concat(newLeft, newRight);
    }
  } else {
    return n;
  }
}

function filterMap0(n, p) {
  if (n !== null) {
    var v = n.key;
    var d = n.value;
    var newLeft = filterMap0(n.left, p);
    var pvd = p(v, d);
    var newRight = filterMap0(n.right, p);
    if (pvd) {
      return join(newLeft, v, pvd[0], newRight);
    } else {
      return concat(newLeft, newRight);
    }
  } else {
    return null;
  }
}

function partitionShared0(p, n) {
  if (n !== null) {
    var key = n.key;
    var value = n.value;
    var match = partitionShared0(p, n.left);
    var lf = match[1];
    var lt = match[0];
    var pvd = p(key, value);
    var match$1 = partitionShared0(p, n.right);
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

function lengthNode(n) {
  var l = n.left;
  var r = n.right;
  var sizeL = l !== null ? lengthNode(l) : 0;
  var sizeR = r !== null ? lengthNode(r) : 0;
  return (1 + sizeL | 0) + sizeR | 0;
}

function length0(n) {
  if (n !== null) {
    return lengthNode(n);
  } else {
    return 0;
  }
}

function toListAux(_accu, _n) {
  while(true) {
    var n = _n;
    var accu = _accu;
    if (n !== null) {
      _n = n.left;
      _accu = /* :: */[
        /* tuple */[
          n.key,
          n.value
        ],
        toListAux(accu, n.right)
      ];
      continue ;
      
    } else {
      return accu;
    }
  };
}

function toList0(s) {
  return toListAux(/* [] */0, s);
}

function checkInvariant(_v) {
  while(true) {
    var v = _v;
    if (v !== null) {
      var l = v.left;
      var r = v.right;
      var diff = height(l) - height(r) | 0;
      if (diff <= 2) {
        if (diff >= -2) {
          if (checkInvariant(l)) {
            _v = r;
            continue ;
            
          } else {
            return /* false */0;
          }
        } else {
          return /* false */0;
        }
      } else {
        return /* false */0;
      }
    } else {
      return /* true */1;
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

function toArray0(n) {
  if (n !== null) {
    var size = lengthNode(n);
    var v = new Array(size);
    fillArray(n, 0, v);
    return v;
  } else {
    return /* array */[];
  }
}

function keysToArray0(n) {
  if (n !== null) {
    var size = lengthNode(n);
    var v = new Array(size);
    fillArrayKey(n, 0, v);
    return v;
  } else {
    return /* array */[];
  }
}

function valuesToArray0(n) {
  if (n !== null) {
    var size = lengthNode(n);
    var v = new Array(size);
    fillArrayValue(n, 0, v);
    return v;
  } else {
    return /* array */[];
  }
}

function ofSortedArrayRevAux(arr, off, len) {
  if (len > 3 || len < 0) {
    var nl = len / 2 | 0;
    var left = ofSortedArrayRevAux(arr, off, nl);
    var match = arr[off - nl | 0];
    var right = ofSortedArrayRevAux(arr, (off - nl | 0) - 1 | 0, (len - nl | 0) - 1 | 0);
    return create(left, match[0], match[1], right);
  } else {
    switch (len) {
      case 0 : 
          return empty0;
      case 1 : 
          var match$1 = arr[off];
          return singleton0(match$1[0], match$1[1]);
      case 2 : 
          var match_000 = arr[off];
          var match_001 = arr[off - 1 | 0];
          var match$2 = match_001;
          var match$3 = match_000;
          return {
                  left: singleton0(match$3[0], match$3[1]),
                  key: match$2[0],
                  value: match$2[1],
                  right: empty0,
                  h: 2
                };
      case 3 : 
          var match_000$1 = arr[off];
          var match_001$1 = arr[off - 1 | 0];
          var match_002 = arr[off - 2 | 0];
          var match$4 = match_002;
          var match$5 = match_001$1;
          var match$6 = match_000$1;
          return {
                  left: singleton0(match$6[0], match$6[1]),
                  key: match$5[0],
                  value: match$5[1],
                  right: singleton0(match$4[0], match$4[1]),
                  h: 2
                };
      
    }
  }
}

function ofSortedArrayAux(arr, off, len) {
  if (len > 3 || len < 0) {
    var nl = len / 2 | 0;
    var left = ofSortedArrayAux(arr, off, nl);
    var match = arr[off + nl | 0];
    var right = ofSortedArrayAux(arr, (off + nl | 0) + 1 | 0, (len - nl | 0) - 1 | 0);
    return create(left, match[0], match[1], right);
  } else {
    switch (len) {
      case 0 : 
          return empty0;
      case 1 : 
          var match$1 = arr[off];
          return singleton0(match$1[0], match$1[1]);
      case 2 : 
          var match_000 = arr[off];
          var match_001 = arr[off + 1 | 0];
          var match$2 = match_001;
          var match$3 = match_000;
          return {
                  left: singleton0(match$3[0], match$3[1]),
                  key: match$2[0],
                  value: match$2[1],
                  right: empty0,
                  h: 2
                };
      case 3 : 
          var match_000$1 = arr[off];
          var match_001$1 = arr[off + 1 | 0];
          var match_002 = arr[off + 2 | 0];
          var match$4 = match_002;
          var match$5 = match_001$1;
          var match$6 = match_000$1;
          return {
                  left: singleton0(match$6[0], match$6[1]),
                  key: match$5[0],
                  value: match$5[1],
                  right: singleton0(match$4[0], match$4[1]),
                  h: 2
                };
      
    }
  }
}

function ofSortedArrayUnsafe0(arr) {
  return ofSortedArrayAux(arr, 0, arr.length);
}

function cmp0(s1, s2, kcmp, vcmp) {
  var len1 = length0(s1);
  var len2 = length0(s2);
  if (len1 === len2) {
    var _e1 = stackAllLeft(s1, /* [] */0);
    var _e2 = stackAllLeft(s2, /* [] */0);
    var kcmp$1 = kcmp;
    var vcmp$1 = vcmp;
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1) {
        if (e2) {
          var h2 = e2[0];
          var h1 = e1[0];
          var c = kcmp$1(h1.key, h2.key);
          if (c) {
            return c;
          } else {
            var cx = vcmp$1(h1.value, h2.value);
            if (cx) {
              return cx;
            } else {
              _e2 = stackAllLeft(h2.right, e2[1]);
              _e1 = stackAllLeft(h1.right, e1[1]);
              continue ;
              
            }
          }
        } else {
          return 0;
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

function eq0(s1, s2, kcmp, vcmp) {
  var len1 = length0(s1);
  var len2 = length0(s2);
  if (len1 === len2) {
    var _e1 = stackAllLeft(s1, /* [] */0);
    var _e2 = stackAllLeft(s2, /* [] */0);
    var kcmp$1 = kcmp;
    var vcmp$1 = vcmp;
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1) {
        if (e2) {
          var h2 = e2[0];
          var h1 = e1[0];
          if (kcmp$1(h1.key, h2.key) === 0 && vcmp$1(h1.value, h2.value)) {
            _e2 = stackAllLeft(h2.right, e2[1]);
            _e1 = stackAllLeft(h1.right, e1[1]);
            continue ;
            
          } else {
            return /* false */0;
          }
        } else {
          return /* true */1;
        }
      } else {
        return /* true */1;
      }
    };
  } else {
    return /* false */0;
  }
}

function findOpt0(_n, x, cmp) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      var c = cmp(x, v);
      if (c) {
        _n = c < 0 ? n.left : n.right;
        continue ;
        
      } else {
        return /* Some */[n.value];
      }
    } else {
      return /* None */0;
    }
  };
}

function findNull0(_n, x, cmp) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      var c = cmp(x, v);
      if (c) {
        _n = c < 0 ? n.left : n.right;
        continue ;
        
      } else {
        return n.value;
      }
    } else {
      return null;
    }
  };
}

function findWithDefault0(_n, x, def, cmp) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      var c = cmp(x, v);
      if (c) {
        _n = c < 0 ? n.left : n.right;
        continue ;
        
      } else {
        return n.value;
      }
    } else {
      return def;
    }
  };
}

function mem0(_n, x, cmp) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      var c = cmp(x, v);
      if (c) {
        _n = c < 0 ? n.left : n.right;
        continue ;
        
      } else {
        return /* true */1;
      }
    } else {
      return /* false */0;
    }
  };
}

function rotateWithLeftChild(k2) {
  var k1 = k2.left;
  k2.left = k1.right;
  k1.right = k2;
  var hlk2 = height(k2.left);
  var hrk2 = height(k2.right);
  k2.h = (
    hlk2 > hrk2 ? hlk2 : hrk2
  ) + 1 | 0;
  var hlk1 = height(k1.left);
  var hk2 = k2.h;
  k1.h = (
    hlk1 > hk2 ? hlk1 : hk2
  ) + 1 | 0;
  return k1;
}

function rotateWithRightChild(k1) {
  var k2 = k1.right;
  k1.right = k2.left;
  k2.left = k1;
  var hlk1 = height(k1.left);
  var hrk1 = height(k1.right);
  k1.h = (
    hlk1 > hrk1 ? hlk1 : hrk1
  ) + 1 | 0;
  var hrk2 = height(k2.right);
  var hk1 = k1.h;
  k2.h = (
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
  var hlt = height(t.left);
  var hrt = height(t.right);
  t.h = (
    hlt > hrt ? hlt : hrt
  ) + 1 | 0;
  return t;
}

function balMutate(nt) {
  var l = nt.left;
  var r = nt.right;
  var hl = height(l);
  var hr = height(r);
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
    nt.h = (
      hl > hr ? hl : hr
    ) + 1 | 0;
    return nt;
  }
}

function addMutate(cmp, t, x, data) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(x, k);
    if (c) {
      var l = t.left;
      var r = t.right;
      if (c < 0) {
        var ll = addMutate(cmp, l, x, data);
        t.left = ll;
      } else {
        t.right = addMutate(cmp, r, x, data);
      }
      return balMutate(t);
    } else {
      t.key = x;
      t.value = data;
      return t;
    }
  } else {
    return singleton0(x, data);
  }
}

function ofArray0(cmp, xs) {
  var len = xs.length;
  if (len) {
    var next = Bs_Sort.strictlySortedLength(xs, (function (param, param$1) {
            return +(cmp(param[0], param$1[0]) < 0);
          }));
    var result;
    if (next >= 0) {
      result = ofSortedArrayAux(xs, 0, next);
    } else {
      next = -next | 0;
      result = ofSortedArrayRevAux(xs, next - 1 | 0, next);
    }
    for(var i = next ,i_finish = len - 1 | 0; i <= i_finish; ++i){
      var match = xs[i];
      result = addMutate(cmp, result, match[0], match[1]);
    }
    return result;
  } else {
    return empty0;
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

exports.copy = copy;
exports.create = create;
exports.bal = bal;
exports.singleton0 = singleton0;
exports.updateKV = updateKV;
exports.minKVOpt0 = minKVOpt0;
exports.minKVNull0 = minKVNull0;
exports.maxKVOpt0 = maxKVOpt0;
exports.maxKVNull0 = maxKVNull0;
exports.removeMinAuxWithRef = removeMinAuxWithRef;
exports.empty0 = empty0;
exports.isEmpty0 = isEmpty0;
exports.stackAllLeft = stackAllLeft;
exports.iter0 = iter0;
exports.map0 = map0;
exports.mapi0 = mapi0;
exports.fold0 = fold0;
exports.forAll0 = forAll0;
exports.exists0 = exists0;
exports.join = join;
exports.concat = concat;
exports.concatOrJoin = concatOrJoin;
exports.filterShared0 = filterShared0;
exports.filterMap0 = filterMap0;
exports.partitionShared0 = partitionShared0;
exports.lengthNode = lengthNode;
exports.length0 = length0;
exports.toList0 = toList0;
exports.checkInvariant = checkInvariant;
exports.fillArray = fillArray;
exports.toArray0 = toArray0;
exports.keysToArray0 = keysToArray0;
exports.valuesToArray0 = valuesToArray0;
exports.ofSortedArrayAux = ofSortedArrayAux;
exports.ofSortedArrayRevAux = ofSortedArrayRevAux;
exports.ofSortedArrayUnsafe0 = ofSortedArrayUnsafe0;
exports.cmp0 = cmp0;
exports.eq0 = eq0;
exports.findOpt0 = findOpt0;
exports.findNull0 = findNull0;
exports.findWithDefault0 = findWithDefault0;
exports.mem0 = mem0;
exports.ofArray0 = ofArray0;
exports.addMutate = addMutate;
exports.balMutate = balMutate;
exports.removeMinAuxWithRootMutate = removeMinAuxWithRootMutate;
/* No side effect */
