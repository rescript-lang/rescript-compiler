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
    if (l !== null) {
      var ll = l.left;
      var lv = l.key;
      var ld = l.value;
      var lr = l.right;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      } else if (lr !== null) {
        var lrl = lr.left;
        var lrv = lr.key;
        var lrd = lr.value;
        var lrr = lr.right;
        return create(create(ll, lv, ld, lrl), lrv, lrd, create(lrr, x, d, r));
      } else {
        return /* assert false */0;
      }
    } else {
      return /* assert false */0;
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r !== null) {
      var rl = r.left;
      var rv = r.key;
      var rd = r.value;
      var rr = r.right;
      if (height(rr) >= height(rl)) {
        return create(create(l, x, d, rl), rv, rd, rr);
      } else if (rl !== null) {
        var rll = rl.left;
        var rlv = rl.key;
        var rld = rl.value;
        var rlr = rl.right;
        return create(create(l, x, d, rll), rlv, rld, create(rlr, rv, rd, rr));
      } else {
        return /* assert false */0;
      }
    } else {
      return /* assert false */0;
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

function isEmpty0(x) {
  if (x !== null) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

function minBindingAux(_n) {
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

function minBinding0(n) {
  if (n !== null) {
    return /* Some */[minBindingAux(n)];
  } else {
    return /* None */0;
  }
}

function maxBindingAux(_n) {
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

function maxBinding0(n) {
  if (n !== null) {
    return /* Some */[maxBindingAux(n)];
  } else {
    return /* None */0;
  }
}

function removeMinAux(n) {
  var ln = n.left;
  var rn = n.right;
  if (ln !== null) {
    return bal(removeMinAux(ln), n.key, n.value, rn);
  } else {
    return rn;
  }
}

function merge(t1, t2) {
  if (t1 !== null) {
    if (t2 !== null) {
      var match = minBindingAux(t2);
      return bal(t1, match[0], match[1], removeMinAux(t2));
    } else {
      return t1;
    }
  } else {
    return t2;
  }
}

function iter0(f, _n) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var l = n.left;
      var v = n.key;
      var d = n.value;
      var r = n.right;
      iter0(f, l);
      f(v, d);
      _n = r;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function map0(f, n) {
  if (n !== null) {
    var l = n.left;
    var v = n.key;
    var d = n.value;
    var r = n.right;
    var h = n.h;
    var l$prime = map0(f, l);
    var d$prime = f(d);
    var r$prime = map0(f, r);
    return {
            left: l$prime,
            key: v,
            value: d$prime,
            right: r$prime,
            h: h
          };
  } else {
    return null;
  }
}

function mapi0(f, n) {
  if (n !== null) {
    var l = n.left;
    var v = n.key;
    var d = n.value;
    var r = n.right;
    var h = n.h;
    var l$prime = mapi0(f, l);
    var d$prime = f(v, d);
    var r$prime = mapi0(f, r);
    return {
            left: l$prime,
            key: v,
            value: d$prime,
            right: r$prime,
            h: h
          };
  } else {
    return null;
  }
}

function fold0(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (m !== null) {
      var l = m.left;
      var v = m.key;
      var d = m.value;
      var r = m.right;
      _accu = f(v, d, fold0(f, l, accu));
      _m = r;
      continue ;
      
    } else {
      return accu;
    }
  };
}

function forAll0(p, _n) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var l = n.left;
      var v = n.key;
      var d = n.value;
      var r = n.right;
      if (p(v, d)) {
        if (forAll0(p, l)) {
          _n = r;
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

function exists0(p, _n) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var l = n.left;
      var v = n.key;
      var d = n.value;
      var r = n.right;
      if (p(v, d)) {
        return /* true */1;
      } else if (exists0(p, l)) {
        return /* true */1;
      } else {
        _n = r;
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
      var match = minBindingAux(t2);
      return join(t1, match[0], match[1], removeMinAux(t2));
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

function cons_enum(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (m !== null) {
      var l = m.left;
      var v = m.key;
      var d = m.value;
      var r = m.right;
      _e = /* More */[
        v,
        d,
        r,
        e
      ];
      _m = l;
      continue ;
      
    } else {
      return e;
    }
  };
}

function cardinalAux(n) {
  var l = n.left;
  var r = n.right;
  var sizeL = l !== null ? cardinalAux(l) : 0;
  var sizeR = r !== null ? cardinalAux(r) : 0;
  return (1 + sizeL | 0) + sizeR | 0;
}

function cardinal0(n) {
  if (n !== null) {
    return cardinalAux(n);
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
exports.minBindingAux = minBindingAux;
exports.minBinding0 = minBinding0;
exports.maxBindingAux = maxBindingAux;
exports.maxBinding0 = maxBinding0;
exports.removeMinAux = removeMinAux;
exports.merge = merge;
exports.iter0 = iter0;
exports.map0 = map0;
exports.mapi0 = mapi0;
exports.fold0 = fold0;
exports.forAll0 = forAll0;
exports.exists0 = exists0;
exports.add_minBinding = add_minBinding;
exports.add_maxBinding = add_maxBinding;
exports.join = join;
exports.concat = concat;
exports.concat_or_join = concat_or_join;
exports.filter0 = filter0;
exports.partition0 = partition0;
exports.cons_enum = cons_enum;
exports.cardinalAux = cardinalAux;
exports.cardinal0 = cardinal0;
exports.bindings_aux = bindings_aux;
exports.bindings0 = bindings0;
exports.checkInvariant = checkInvariant;
/* No side effect */
