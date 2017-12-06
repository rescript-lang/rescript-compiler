'use strict';


function height(param) {
  if (param) {
    return param[3];
  } else {
    return 0;
  }
}

function create(l, v, r) {
  var hl = l ? l[3] : 0;
  var hr = r ? r[3] : 0;
  return /* Node */[
          l,
          v,
          r,
          hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function bal(l, v, r) {
  var hl = l ? l[3] : 0;
  var hr = r ? r[3] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[2];
      var lv = l[1];
      var ll = l[0];
      if (height(ll) >= height(lr)) {
        return create(ll, lv, create(lr, v, r));
      } else if (lr) {
        return create(create(ll, lv, lr[0]), lr[1], create(lr[2], v, r));
      } else {
        return /* assert false */0;
      }
    } else {
      return /* assert false */0;
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r) {
      var rr = r[2];
      var rv = r[1];
      var rl = r[0];
      if (height(rr) >= height(rl)) {
        return create(create(l, v, rl), rv, rr);
      } else if (rl) {
        return create(create(l, v, rl[0]), rl[1], create(rl[2], rv, rr));
      } else {
        return /* assert false */0;
      }
    } else {
      return /* assert false */0;
    }
  } else {
    return /* Node */[
            l,
            v,
            r,
            hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function singleton0(x) {
  return /* Node */[
          /* Empty */0,
          x,
          /* Empty */0,
          1
        ];
}

function add_min_element(v, param) {
  if (param) {
    return bal(add_min_element(v, param[0]), param[1], param[2]);
  } else {
    return singleton0(v);
  }
}

function add_max_element(v, param) {
  if (param) {
    return bal(param[0], param[1], add_max_element(v, param[2]));
  } else {
    return singleton0(v);
  }
}

function join(l, v, r) {
  if (l) {
    if (r) {
      var rh = r[3];
      var lh = l[3];
      if (lh > (rh + 2 | 0)) {
        return bal(l[0], l[1], join(l[2], v, r));
      } else if (rh > (lh + 2 | 0)) {
        return bal(join(l, v, r[0]), r[1], r[2]);
      } else {
        return create(l, v, r);
      }
    } else {
      return add_max_element(v, l);
    }
  } else {
    return add_min_element(v, r);
  }
}

function min0(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param[0];
      if (l) {
        _param = l;
        continue ;
        
      } else {
        return /* Some */[param[1]];
      }
    } else {
      return /* None */0;
    }
  };
}

function max0(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var r = param[2];
      if (r) {
        _param = r;
        continue ;
        
      } else {
        return /* Some */[param[1]];
      }
    } else {
      return /* None */0;
    }
  };
}

function min_eltAssert0(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param[0];
      if (l) {
        _param = l;
        continue ;
        
      } else {
        return param[1];
      }
    } else {
      return /* assert false */0;
    }
  };
}

function remove_min_elt(param) {
  if (param) {
    var l = param[0];
    if (l) {
      return bal(remove_min_elt(l), param[1], param[2]);
    } else {
      return param[2];
    }
  } else {
    return /* assert false */0;
  }
}

function merge(t1, t2) {
  if (t1) {
    if (t2) {
      return bal(t1, min_eltAssert0(t2), remove_min_elt(t2));
    } else {
      return t1;
    }
  } else {
    return t2;
  }
}

function concat(t1, t2) {
  if (t1) {
    if (t2) {
      return join(t1, min_eltAssert0(t2), remove_min_elt(t2));
    } else {
      return t1;
    }
  } else {
    return t2;
  }
}

function isEmpty0(param) {
  if (param) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

function cons_enum(_s, _e) {
  while(true) {
    var e = _e;
    var s = _s;
    if (s) {
      _e = /* More */[
        s[1],
        s[2],
        e
      ];
      _s = s[0];
      continue ;
      
    } else {
      return e;
    }
  };
}

function iter0(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      iter0(f, param[0]);
      f(param[1]);
      _param = param[2];
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
    if (s) {
      _accu = f(s[1], fold0(f, s[0], accu));
      _s = s[2];
      continue ;
      
    } else {
      return accu;
    }
  };
}

function forAll0(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (p(param[1])) {
        if (forAll0(p, param[0])) {
          _param = param[2];
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

function exists0(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (p(param[1])) {
        return /* true */1;
      } else if (exists0(p, param[0])) {
        return /* true */1;
      } else {
        _param = param[2];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function filter0(p, param) {
  if (param) {
    var v = param[1];
    var l$prime = filter0(p, param[0]);
    var pv = p(v);
    var r$prime = filter0(p, param[2]);
    if (pv) {
      return join(l$prime, v, r$prime);
    } else {
      return concat(l$prime, r$prime);
    }
  } else {
    return /* Empty */0;
  }
}

function partition0(p, param) {
  if (param) {
    var v = param[1];
    var match = partition0(p, param[0]);
    var lf = match[1];
    var lt = match[0];
    var pv = p(v);
    var match$1 = partition0(p, param[2]);
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
            /* Empty */0,
            /* Empty */0
          ];
  }
}

function cardinal0(param) {
  if (param) {
    return (cardinal0(param[0]) + 1 | 0) + cardinal0(param[2]) | 0;
  } else {
    return 0;
  }
}

function elements_aux(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[0];
      _accu = /* :: */[
        param[1],
        elements_aux(accu, param[2])
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

var empty0 = /* Empty */0;

exports.height          = height;
exports.create          = create;
exports.bal             = bal;
exports.singleton0      = singleton0;
exports.add_min_element = add_min_element;
exports.add_max_element = add_max_element;
exports.join            = join;
exports.min0            = min0;
exports.max0            = max0;
exports.min_eltAssert0  = min_eltAssert0;
exports.remove_min_elt  = remove_min_elt;
exports.merge           = merge;
exports.concat          = concat;
exports.empty0          = empty0;
exports.isEmpty0        = isEmpty0;
exports.cons_enum       = cons_enum;
exports.iter0           = iter0;
exports.fold0           = fold0;
exports.forAll0         = forAll0;
exports.exists0         = exists0;
exports.filter0         = filter0;
exports.partition0      = partition0;
exports.cardinal0       = cardinal0;
exports.elements_aux    = elements_aux;
exports.elements0       = elements0;
/* No side effect */
