'use strict';


function height(n) {
  if (n !== null) {
    return n.h;
  } else {
    return 0;
  }
}

function create(l, v, r) {
  var hl = l !== null ? l.h : 0;
  var hr = r !== null ? r.h : 0;
  return {
          left: l,
          key: v,
          right: r,
          h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
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

function bal(l, v, r) {
  var hl = l !== null ? l.h : 0;
  var hr = r !== null ? r.h : 0;
  if (hl > (hr + 2 | 0)) {
    if (l !== null) {
      var ll = l.left;
      var lv = l.key;
      var lr = l.right;
      if (heightGe(ll, lr)) {
        return create(ll, lv, create(lr, v, r));
      } else if (lr !== null) {
        var lrl = lr.left;
        var lrv = lr.key;
        var lrr = lr.right;
        return create(create(ll, lv, lrl), lrv, create(lrr, v, r));
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
      var rr = r.right;
      if (heightGe(rr, rl)) {
        return create(create(l, v, rl), rv, rr);
      } else if (rl !== null) {
        var rll = rl.left;
        var rlv = rl.key;
        var rlr = rl.right;
        return create(create(l, v, rll), rlv, create(rlr, rv, rr));
      } else {
        return /* assert false */0;
      }
    } else {
      return /* assert false */0;
    }
  } else {
    return {
            left: l,
            key: v,
            right: r,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
}

function singleton0(x) {
  return {
          left: null,
          key: x,
          right: null,
          h: 1
        };
}

function add_min_element(v, n) {
  if (n !== null) {
    return bal(add_min_element(v, n.left), n.key, n.right);
  } else {
    return singleton0(v);
  }
}

function add_max_element(v, n) {
  if (n !== null) {
    return bal(n.left, n.key, add_max_element(v, n.right));
  } else {
    return singleton0(v);
  }
}

function join(ln, v, rn) {
  if (ln !== null) {
    if (rn !== null) {
      var lh = ln.h;
      var rh = rn.h;
      if (lh > (rh + 2 | 0)) {
        return bal(ln.left, ln.key, join(ln.right, v, rn));
      } else if (rh > (lh + 2 | 0)) {
        return bal(join(ln, v, rn.left), rn.key, rn.right);
      } else {
        return create(ln, v, rn);
      }
    } else {
      return add_max_element(v, ln);
    }
  } else {
    return add_min_element(v, rn);
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
      return n.key;
    }
  };
}

function min0(n) {
  if (n !== null) {
    return /* Some */[min0Aux(n)];
  } else {
    return /* None */0;
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
      return n.key;
    }
  };
}

function max0(n) {
  if (n !== null) {
    return /* Some */[max0Aux(n)];
  } else {
    return /* None */0;
  }
}

function removeMinAux(n) {
  var rn = n.right;
  var ln = n.left;
  if (ln !== null) {
    return bal(removeMinAux(ln), n.key, rn);
  } else {
    return rn;
  }
}

function merge(t1, t2) {
  if (t1 !== null) {
    if (t2 !== null) {
      return bal(t1, min0Aux(t2), removeMinAux(t2));
    } else {
      return t1;
    }
  } else {
    return t2;
  }
}

function concat(t1, t2) {
  if (t1 !== null) {
    if (t2 !== null) {
      return join(t1, min0Aux(t2), removeMinAux(t2));
    } else {
      return t1;
    }
  } else {
    return t2;
  }
}

function isEmpty0(n) {
  if (n !== null) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

function cons_enum(_s, _e) {
  while(true) {
    var e = _e;
    var s = _s;
    if (s !== null) {
      _e = /* More */[
        s.key,
        s.right,
        e
      ];
      _s = s.left;
      continue ;
      
    } else {
      return e;
    }
  };
}

function iter0(f, _n) {
  while(true) {
    var n = _n;
    if (n !== null) {
      iter0(f, n.left);
      f(n.key);
      _n = n.right;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function fold0(f, _s, _accu) {
  while(true) {
    var accu = _accu;
    var s = _s;
    if (s !== null) {
      _accu = f(s.key, fold0(f, s.left, accu));
      _s = s.right;
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
      if (p(n.key)) {
        if (forAll0(p, n.left)) {
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

function exists0(p, _n) {
  while(true) {
    var n = _n;
    if (n !== null) {
      if (p(n.key)) {
        return /* true */1;
      } else if (exists0(p, n.left)) {
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

function filter0(p, n) {
  if (n !== null) {
    var l$prime = filter0(p, n.left);
    var v = n.key;
    var pv = p(v);
    var r$prime = filter0(p, n.right);
    if (pv) {
      return join(l$prime, v, r$prime);
    } else {
      return concat(l$prime, r$prime);
    }
  } else {
    return null;
  }
}

function partition0(p, n) {
  if (n !== null) {
    var match = partition0(p, n.left);
    var lf = match[1];
    var lt = match[0];
    var v = n.key;
    var pv = p(v);
    var match$1 = partition0(p, n.right);
    var rf = match$1[1];
    var rt = match$1[0];
    if (pv) {
      return /* tuple */[
              join(lt, v, rt),
              concat(lf, rf)
            ];
    } else {
      return /* tuple */[
              concat(lt, rt),
              join(lf, v, rf)
            ];
    }
  } else {
    return /* tuple */[
            null,
            null
          ];
  }
}

function cardinalAux(n) {
  var l = n.left;
  var r = n.right;
  var sizeL = l !== null ? cardinalAux(l) : 0;
  var sizeR = r !== null ? cardinalAux(r) : 0;
  return (1 + sizeL | 0) + sizeR | 0;
}

function length0(n) {
  if (n !== null) {
    return cardinalAux(n);
  } else {
    return 0;
  }
}

function elements_aux(_accu, _n) {
  while(true) {
    var n = _n;
    var accu = _accu;
    if (n !== null) {
      _n = n.left;
      _accu = /* :: */[
        n.key,
        elements_aux(accu, n.right)
      ];
      continue ;
      
    } else {
      return accu;
    }
  };
}

function elements0(s) {
  return elements_aux(/* [] */0, s);
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

function fillArray(_n, _i, arr) {
  while(true) {
    var i = _i;
    var n = _n;
    var l = n.left;
    var v = n.key;
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

function toArray0(n) {
  if (n !== null) {
    var size = cardinalAux(n);
    var v = new Array(size);
    fillArray(n, 0, v);
    return v;
  } else {
    return /* array */[];
  }
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

function removeMinAuxMutate(n) {
  var rn = n.right;
  var ln = n.left;
  if (ln !== null) {
    n.left = removeMinAuxMutate(ln);
    return balMutate(n);
  } else {
    return rn;
  }
}

function removeMinAuxMutateWithRoot(nt, n) {
  var rn = n.right;
  var ln = n.left;
  if (ln !== null) {
    n.left = removeMinAuxMutateWithRoot(nt, ln);
    return balMutate(n);
  } else {
    nt.key = n.key;
    return rn;
  }
}

var empty0 = null;

exports.height = height;
exports.create = create;
exports.heightGe = heightGe;
exports.bal = bal;
exports.singleton0 = singleton0;
exports.add_min_element = add_min_element;
exports.add_max_element = add_max_element;
exports.join = join;
exports.min0Aux = min0Aux;
exports.min0 = min0;
exports.max0Aux = max0Aux;
exports.max0 = max0;
exports.removeMinAux = removeMinAux;
exports.merge = merge;
exports.concat = concat;
exports.empty0 = empty0;
exports.isEmpty0 = isEmpty0;
exports.cons_enum = cons_enum;
exports.iter0 = iter0;
exports.fold0 = fold0;
exports.forAll0 = forAll0;
exports.exists0 = exists0;
exports.filter0 = filter0;
exports.partition0 = partition0;
exports.cardinalAux = cardinalAux;
exports.length0 = length0;
exports.elements_aux = elements_aux;
exports.elements0 = elements0;
exports.checkInvariant = checkInvariant;
exports.fillArray = fillArray;
exports.toArray0 = toArray0;
exports.rotateWithLeftChild = rotateWithLeftChild;
exports.rotateWithRightChild = rotateWithRightChild;
exports.doubleWithLeftChild = doubleWithLeftChild;
exports.doubleWithRightChild = doubleWithRightChild;
exports.heightUpdateMutate = heightUpdateMutate;
exports.balMutate = balMutate;
exports.removeMinAuxMutate = removeMinAuxMutate;
exports.removeMinAuxMutateWithRoot = removeMinAuxMutateWithRoot;
/* No side effect */
