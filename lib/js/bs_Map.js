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

function add0(cmp, x, data, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        return bal(add0(cmp, x, data, l), v, d, r);
      } else {
        return bal(l, v, d, add0(cmp, x, data, r));
      }
    } else {
      return /* Node */[
              l,
              x,
              data,
              r,
              param[4]
            ];
    }
  } else {
    return /* Node */[
            /* Empty */0,
            x,
            data,
            /* Empty */0,
            1
          ];
  }
}

function findOpt0(cmp, x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = cmp(x, param[1]);
      if (c) {
        _param = c < 0 ? param[0] : param[3];
        continue ;
        
      } else {
        return /* Some */[param[2]];
      }
    } else {
      return /* None */0;
    }
  };
}

function findAssert0(cmp, x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = cmp(x, param[1]);
      if (c) {
        _param = c < 0 ? param[0] : param[3];
        continue ;
        
      } else {
        return param[2];
      }
    } else {
      throw new Error("Not_found");
    }
  };
}

function findWithDefault0(cmp, def, x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = cmp(x, param[1]);
      if (c) {
        _param = c < 0 ? param[0] : param[3];
        continue ;
        
      } else {
        return param[2];
      }
    } else {
      return def;
    }
  };
}

function mem0(cmp, x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = cmp(x, param[1]);
      if (c) {
        _param = c < 0 ? param[0] : param[3];
        continue ;
        
      } else {
        return /* true */1;
      }
    } else {
      return /* false */0;
    }
  };
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

function remove0(cmp, x, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        return bal(remove0(cmp, x, l), v, d, r);
      } else {
        return bal(l, v, d, remove0(cmp, x, r));
      }
    } else {
      var t1 = l;
      var t2 = r;
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
  } else {
    return /* Empty */0;
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

function split0(cmp, x, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        var match = split0(cmp, x, l);
        return /* tuple */[
                match[0],
                match[1],
                join(match[2], v, d, r)
              ];
      } else {
        var match$1 = split0(cmp, x, r);
        return /* tuple */[
                join(l, v, d, match$1[0]),
                match$1[1],
                match$1[2]
              ];
      }
    } else {
      return /* tuple */[
              l,
              /* Some */[d],
              r
            ];
    }
  } else {
    return /* tuple */[
            /* Empty */0,
            /* None */0,
            /* Empty */0
          ];
  }
}

function merge0(cmp, f, s1, s2) {
  var exit = 0;
  if (s1) {
    var v1 = s1[1];
    if (s1[4] >= height(s2)) {
      var match = split0(cmp, v1, s2);
      return concat_or_join(merge0(cmp, f, s1[0], match[0]), v1, f(v1, /* Some */[s1[2]], match[1]), merge0(cmp, f, s1[3], match[2]));
    } else {
      exit = 1;
    }
  } else if (s2) {
    exit = 1;
  } else {
    return /* Empty */0;
  }
  if (exit === 1) {
    if (s2) {
      var v2 = s2[1];
      var match$1 = split0(cmp, v2, s1);
      return concat_or_join(merge0(cmp, f, match$1[0], s2[0]), v2, f(v2, match$1[1], /* Some */[s2[2]]), merge0(cmp, f, match$1[2], s2[3]));
    } else {
      return /* assert false */0;
    }
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

function compare0(keycmp, cmp, m1, m2) {
  var _e1 = cons_enum(m1, /* End */0);
  var _e2 = cons_enum(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var c = keycmp(e1[0], e2[0]);
        if (c !== 0) {
          return c;
        } else {
          var c$1 = cmp(e1[1], e2[1]);
          if (c$1 !== 0) {
            return c$1;
          } else {
            _e2 = cons_enum(e2[2], e2[3]);
            _e1 = cons_enum(e1[2], e1[3]);
            continue ;
            
          }
        }
      } else {
        return 1;
      }
    } else if (e2) {
      return -1;
    } else {
      return 0;
    }
  };
}

function equal0(keycmp, cmp, m1, m2) {
  var _e1 = cons_enum(m1, /* End */0);
  var _e2 = cons_enum(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        if (keycmp(e1[0], e2[0])) {
          return /* false */0;
        } else if (cmp(e1[1], e2[1])) {
          _e2 = cons_enum(e2[2], e2[3]);
          _e1 = cons_enum(e1[2], e1[3]);
          continue ;
          
        } else {
          return /* false */0;
        }
      } else {
        return /* false */0;
      }
    } else if (e2) {
      return /* false */0;
    } else {
      return /* true */1;
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

function empty(cmp) {
  return /* record */[
          /* cmp */cmp,
          /* data : Empty */0
        ];
}

function isEmpty(map) {
  return isEmpty0(map[/* data */1]);
}

function singleton(cmp, k, v) {
  return /* record */[
          /* cmp */cmp,
          /* data */singleton0(k, v)
        ];
}

function iter(f, map) {
  return iter0(f, map[/* data */1]);
}

function fold(f, map) {
  var partial_arg = map[/* data */1];
  return (function (param) {
      return fold0(f, partial_arg, param);
    });
}

function forAll(f, map) {
  return forAll0(f, map[/* data */1]);
}

function exists(f, map) {
  return exists0(f, map[/* data */1]);
}

function filter(f, map) {
  return /* record */[
          /* cmp */map[/* cmp */0],
          /* data */filter0(f, map[/* data */1])
        ];
}

function partition(p, map) {
  var match = partition0(p, map[/* data */1]);
  var map_cmp = map[/* cmp */0];
  return /* tuple */[
          /* record */[
            /* cmp */map_cmp,
            /* data */match[0]
          ],
          /* record */[
            /* cmp */map_cmp,
            /* data */match[1]
          ]
        ];
}

function cardinal(map) {
  return cardinal0(map[/* data */1]);
}

function bindings(map) {
  return bindings0(map[/* data */1]);
}

function minBinding(map) {
  return minBinding0(map[/* data */1]);
}

function maxBinding(map) {
  return maxBinding0(map[/* data */1]);
}

function map(f, m) {
  var m_cmp = m[/* cmp */0];
  return /* record */[
          /* cmp */m_cmp,
          /* data */map0(f, m[/* data */1])
        ];
}

function mapi(f, m) {
  var m_cmp = m[/* cmp */0];
  return /* record */[
          /* cmp */m_cmp,
          /* data */mapi0(f, m[/* data */1])
        ];
}

function add(key, data, map) {
  var map_cmp = map[/* cmp */0];
  return /* record */[
          /* cmp */map_cmp,
          /* data */add0(map_cmp[/* cmp */0], key, data, map[/* data */1])
        ];
}

function findOpt(x, map) {
  var X = map[/* cmp */0];
  return findOpt0(X[/* cmp */0], x, map[/* data */1]);
}

function findAssert(x, map) {
  var X = map[/* cmp */0];
  return findAssert0(X[/* cmp */0], x, map[/* data */1]);
}

function findWithDefault(def, x, map) {
  var X = map[/* cmp */0];
  return findWithDefault0(X[/* cmp */0], def, x, map[/* data */1]);
}

function mem(x, map) {
  var X = map[/* cmp */0];
  return mem0(X[/* cmp */0], x, map[/* data */1]);
}

function remove(x, map) {
  var map_cmp = map[/* cmp */0];
  return /* record */[
          /* cmp */map_cmp,
          /* data */remove0(map_cmp[/* cmp */0], x, map[/* data */1])
        ];
}

function split(x, map) {
  var map_cmp = map[/* cmp */0];
  var match = split0(map_cmp[/* cmp */0], x, map[/* data */1]);
  return /* tuple */[
          /* record */[
            /* cmp */map_cmp,
            /* data */match[0]
          ],
          match[1],
          /* record */[
            /* cmp */map_cmp,
            /* data */match[2]
          ]
        ];
}

function merge(f, s1, s2) {
  var s1_cmp = s1[/* cmp */0];
  return /* record */[
          /* cmp */s1_cmp,
          /* data */merge0(s1_cmp[/* cmp */0], f, s1[/* data */1], s2[/* data */1])
        ];
}

function compare(cmp, m1, m2) {
  var X = m1[/* cmp */0];
  return compare0(X[/* cmp */0], cmp, m1[/* data */1], m2[/* data */1]);
}

function equal(cmp, m1, m2) {
  var X = m1[/* cmp */0];
  return equal0(X[/* cmp */0], cmp, m1[/* data */1], m2[/* data */1]);
}

var empty0 = /* Empty */0;

exports.empty0           = empty0;
exports.empty            = empty;
exports.isEmpty0         = isEmpty0;
exports.isEmpty          = isEmpty;
exports.mem0             = mem0;
exports.mem              = mem;
exports.add0             = add0;
exports.add              = add;
exports.singleton0       = singleton0;
exports.singleton        = singleton;
exports.remove0          = remove0;
exports.remove           = remove;
exports.merge0           = merge0;
exports.merge            = merge;
exports.compare0         = compare0;
exports.compare          = compare;
exports.equal0           = equal0;
exports.equal            = equal;
exports.iter0            = iter0;
exports.iter             = iter;
exports.fold0            = fold0;
exports.fold             = fold;
exports.forAll0          = forAll0;
exports.forAll           = forAll;
exports.exists0          = exists0;
exports.exists           = exists;
exports.filter0          = filter0;
exports.filter           = filter;
exports.partition0       = partition0;
exports.partition        = partition;
exports.cardinal0        = cardinal0;
exports.cardinal         = cardinal;
exports.bindings0        = bindings0;
exports.bindings         = bindings;
exports.minBinding0      = minBinding0;
exports.minBinding       = minBinding;
exports.maxBinding0      = maxBinding0;
exports.maxBinding       = maxBinding;
exports.split0           = split0;
exports.split            = split;
exports.findOpt0         = findOpt0;
exports.findOpt          = findOpt;
exports.findAssert0      = findAssert0;
exports.findAssert       = findAssert;
exports.findWithDefault0 = findWithDefault0;
exports.findWithDefault  = findWithDefault;
exports.map0             = map0;
exports.map              = map;
exports.mapi0            = mapi0;
exports.mapi             = mapi;
/* No side effect */
