'use strict';


function height(n) {
  if (n !== null) {
    return n.h;
  } else {
    return 0;
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

function add_minBinding(k, v, n) {
  if (n !== null) {
    var l = n.left;
    var x = n.key;
    var d = n.value;
    var r = n.right;
    return bal(add_minBinding(k, v, l), x, d, r);
  } else {
    return singleton0(k, v);
  }
}

function add_maxBinding(k, v, n) {
  if (n !== null) {
    var l = n.left;
    var x = n.key;
    var d = n.value;
    var r = n.right;
    return bal(l, x, d, add_maxBinding(k, v, r));
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
      return add_maxBinding(v, d, ln);
    }
  } else {
    return add_minBinding(v, d, rn);
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

function concat_or_join(t1, v, d, t2) {
  if (d) {
    return join(t1, v, d[0], t2);
  } else {
    return concat(t1, t2);
  }
}

function filter0(p, n) {
  if (n !== null) {
    var l = n.left;
    var v = n.key;
    var d = n.value;
    var r = n.right;
    var l$prime = filter0(p, l);
    var pvd = p(v, d);
    var r$prime = filter0(p, r);
    if (pvd) {
      return join(l$prime, v, d, r$prime);
    } else {
      return concat(l$prime, r$prime);
    }
  } else {
    return n;
  }
}

function partition0(p, n) {
  if (n !== null) {
    var l = n.left;
    var v = n.key;
    var d = n.value;
    var r = n.right;
    var match = partition0(p, l);
    var lf = match[1];
    var lt = match[0];
    var pvd = p(v, d);
    var match$1 = partition0(p, r);
    var rf = match$1[1];
    var rt = match$1[0];
    if (pvd) {
      return /* tuple */[
              join(lt, v, d, rt),
              concat(lf, rf)
            ];
    } else {
      return /* tuple */[
              concat(lt, rt),
              join(lf, v, d, rf)
            ];
    }
  } else {
    return /* tuple */[
            null,
            null
          ];
  }
}

function lengthAux(n) {
  var l = n.left;
  var r = n.right;
  var sizeL = l !== null ? lengthAux(l) : 0;
  var sizeR = r !== null ? lengthAux(r) : 0;
  return (1 + sizeL | 0) + sizeR | 0;
}

function length0(n) {
  if (n !== null) {
    return lengthAux(n);
  } else {
    return 0;
  }
}

function bindings_aux(_accu, _n) {
  while(true) {
    var n = _n;
    var accu = _accu;
    if (n !== null) {
      var l = n.left;
      var v = n.key;
      var d = n.value;
      var r = n.right;
      _n = l;
      _accu = /* :: */[
        /* tuple */[
          v,
          d
        ],
        bindings_aux(accu, r)
      ];
      continue ;
      
    } else {
      return accu;
    }
  };
}

function bindings0(s) {
  return bindings_aux(/* [] */0, s);
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

var empty0 = null;

exports.height = height;
exports.create = create;
exports.singleton0 = singleton0;
exports.bal = bal;
exports.empty0 = empty0;
exports.isEmpty0 = isEmpty0;
exports.minKVOpt0 = minKVOpt0;
exports.minKVNull0 = minKVNull0;
exports.maxKVOpt0 = maxKVOpt0;
exports.removeMinAuxWithRef = removeMinAuxWithRef;
exports.iter0 = iter0;
exports.map0 = map0;
exports.mapi0 = mapi0;
exports.fold0 = fold0;
exports.forAll0 = forAll0;
exports.exists0 = exists0;
exports.join = join;
exports.concat = concat;
exports.concat_or_join = concat_or_join;
exports.filter0 = filter0;
exports.partition0 = partition0;
exports.stackAllLeft = stackAllLeft;
exports.lengthAux = lengthAux;
exports.length0 = length0;
exports.bindings_aux = bindings_aux;
exports.bindings0 = bindings0;
exports.checkInvariant = checkInvariant;
/* No side effect */
