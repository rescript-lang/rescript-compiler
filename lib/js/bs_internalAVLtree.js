'use strict';


function height(param) {
  if (param) {
    return param[4];
  } else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* Node */[
          l,
          x,
          d,
          r,
          hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function singleton0(x, d) {
  return /* Node */[
          /* Empty */0,
          x,
          d,
          /* Empty */0,
          1
        ];
}

function bal(l, x, d, r) {
  var hl = l ? l[4] : 0;
  var hr = r ? r[4] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[3];
      var ld = l[2];
      var lv = l[1];
      var ll = l[0];
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      } else if (lr) {
        return create(create(ll, lv, ld, lr[0]), lr[1], lr[2], create(lr[3], x, d, r));
      } else {
        return /* assert false */0;
      }
    } else {
      return /* assert false */0;
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r) {
      var rr = r[3];
      var rd = r[2];
      var rv = r[1];
      var rl = r[0];
      if (height(rr) >= height(rl)) {
        return create(create(l, x, d, rl), rv, rd, rr);
      } else if (rl) {
        return create(create(l, x, d, rl[0]), rl[1], rl[2], create(rl[3], rv, rd, rr));
      } else {
        return /* assert false */0;
      }
    } else {
      return /* assert false */0;
    }
  } else {
    return /* Node */[
            l,
            x,
            d,
            r,
            hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function isEmpty0(param) {
  if (param) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

function minBinding0(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param[0];
      if (l) {
        _param = l;
        continue ;
        
      } else {
        return /* Some */[/* tuple */[
                  param[1],
                  param[2]
                ]];
      }
    } else {
      return /* None */0;
    }
  };
}

function maxBinding0(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var r = param[3];
      if (r) {
        _param = r;
        continue ;
        
      } else {
        return /* Some */[/* tuple */[
                  param[1],
                  param[2]
                ]];
      }
    } else {
      return /* None */0;
    }
  };
}

function minBindingAssert0(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param[0];
      if (l) {
        _param = l;
        continue ;
        
      } else {
        return /* tuple */[
                param[1],
                param[2]
              ];
      }
    } else {
      return /* assert false */0;
    }
  };
}

function remove_minBinding(param) {
  if (param) {
    var l = param[0];
    if (l) {
      return bal(remove_minBinding(l), param[1], param[2], param[3]);
    } else {
      return param[3];
    }
  } else {
    return /* assert false */0;
  }
}

function merge(t1, t2) {
  if (t1) {
    if (t2) {
      var match = minBindingAssert0(t2);
      return bal(t1, match[0], match[1], remove_minBinding(t2));
    } else {
      return t1;
    }
  } else {
    return t2;
  }
}

function iter0(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      iter0(f, param[0]);
      f(param[1], param[2]);
      _param = param[3];
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function map0(f, param) {
  if (param) {
    var l$prime = map0(f, param[0]);
    var d$prime = f(param[2]);
    var r$prime = map0(f, param[3]);
    return /* Node */[
            l$prime,
            param[1],
            d$prime,
            r$prime,
            param[4]
          ];
  } else {
    return /* Empty */0;
  }
}

function mapi0(f, param) {
  if (param) {
    var v = param[1];
    var l$prime = mapi0(f, param[0]);
    var d$prime = f(v, param[2]);
    var r$prime = mapi0(f, param[3]);
    return /* Node */[
            l$prime,
            v,
            d$prime,
            r$prime,
            param[4]
          ];
  } else {
    return /* Empty */0;
  }
}

function fold0(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (m) {
      _accu = f(m[1], m[2], fold0(f, m[0], accu));
      _m = m[3];
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
      if (p(param[1], param[2])) {
        if (forAll0(p, param[0])) {
          _param = param[3];
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
      if (p(param[1], param[2])) {
        return /* true */1;
      } else if (exists0(p, param[0])) {
        return /* true */1;
      } else {
        _param = param[3];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function add_minBinding(k, v, param) {
  if (param) {
    return bal(add_minBinding(k, v, param[0]), param[1], param[2], param[3]);
  } else {
    return singleton0(k, v);
  }
}

function add_maxBinding(k, v, param) {
  if (param) {
    return bal(param[0], param[1], param[2], add_maxBinding(k, v, param[3]));
  } else {
    return singleton0(k, v);
  }
}

function join(l, v, d, r) {
  if (l) {
    if (r) {
      var rh = r[4];
      var lh = l[4];
      if (lh > (rh + 2 | 0)) {
        return bal(l[0], l[1], l[2], join(l[3], v, d, r));
      } else if (rh > (lh + 2 | 0)) {
        return bal(join(l, v, d, r[0]), r[1], r[2], r[3]);
      } else {
        return create(l, v, d, r);
      }
    } else {
      return add_maxBinding(v, d, l);
    }
  } else {
    return add_minBinding(v, d, r);
  }
}

function concat(t1, t2) {
  if (t1) {
    if (t2) {
      var match = minBindingAssert0(t2);
      return join(t1, match[0], match[1], remove_minBinding(t2));
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

function filter0(p, param) {
  if (param) {
    var d = param[2];
    var v = param[1];
    var l$prime = filter0(p, param[0]);
    var pvd = p(v, d);
    var r$prime = filter0(p, param[3]);
    if (pvd) {
      return join(l$prime, v, d, r$prime);
    } else {
      return concat(l$prime, r$prime);
    }
  } else {
    return /* Empty */0;
  }
}

function partition0(p, param) {
  if (param) {
    var d = param[2];
    var v = param[1];
    var match = partition0(p, param[0]);
    var lf = match[1];
    var lt = match[0];
    var pvd = p(v, d);
    var match$1 = partition0(p, param[3]);
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
            /* Empty */0,
            /* Empty */0
          ];
  }
}

function cons_enum(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (m) {
      _e = /* More */[
        m[1],
        m[2],
        m[3],
        e
      ];
      _m = m[0];
      continue ;
      
    } else {
      return e;
    }
  };
}

function cardinal0(param) {
  if (param) {
    return (cardinal0(param[0]) + 1 | 0) + cardinal0(param[3]) | 0;
  } else {
    return 0;
  }
}

function bindings_aux(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[0];
      _accu = /* :: */[
        /* tuple */[
          param[1],
          param[2]
        ],
        bindings_aux(accu, param[3])
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

var empty0 = /* Empty */0;

exports.height            = height;
exports.create            = create;
exports.singleton0        = singleton0;
exports.bal               = bal;
exports.empty0            = empty0;
exports.isEmpty0          = isEmpty0;
exports.minBinding0       = minBinding0;
exports.maxBinding0       = maxBinding0;
exports.minBindingAssert0 = minBindingAssert0;
exports.remove_minBinding = remove_minBinding;
exports.merge             = merge;
exports.iter0             = iter0;
exports.map0              = map0;
exports.mapi0             = mapi0;
exports.fold0             = fold0;
exports.forAll0           = forAll0;
exports.exists0           = exists0;
exports.add_minBinding    = add_minBinding;
exports.add_maxBinding    = add_maxBinding;
exports.join              = join;
exports.concat            = concat;
exports.concat_or_join    = concat_or_join;
exports.filter0           = filter0;
exports.partition0        = partition0;
exports.cons_enum         = cons_enum;
exports.cardinal0         = cardinal0;
exports.bindings_aux      = bindings_aux;
exports.bindings0         = bindings0;
/* No side effect */
