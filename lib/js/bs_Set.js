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

function add0(cmp, x, t) {
  if (t) {
    var r = t[2];
    var v = t[1];
    var l = t[0];
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        return bal(add0(cmp, x, l), v, r);
      } else {
        return bal(l, v, add0(cmp, x, r));
      }
    } else {
      return t;
    }
  } else {
    return /* Node */[
            /* Empty */0,
            x,
            /* Empty */0,
            1
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

function split0(cmp, x, param) {
  if (param) {
    var r = param[2];
    var v = param[1];
    var l = param[0];
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        var match = split0(cmp, x, l);
        return /* tuple */[
                match[0],
                match[1],
                join(match[2], v, r)
              ];
      } else {
        var match$1 = split0(cmp, x, r);
        return /* tuple */[
                join(l, v, match$1[0]),
                match$1[1],
                match$1[2]
              ];
      }
    } else {
      return /* tuple */[
              l,
              /* true */1,
              r
            ];
    }
  } else {
    return /* tuple */[
            /* Empty */0,
            /* false */0,
            /* Empty */0
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

function mem0(cmp, x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = cmp(x, param[1]);
      if (c) {
        _param = c < 0 ? param[0] : param[2];
        continue ;
        
      } else {
        return /* true */1;
      }
    } else {
      return /* false */0;
    }
  };
}

function remove0(cmp, x, param) {
  if (param) {
    var r = param[2];
    var v = param[1];
    var l = param[0];
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        return bal(remove0(cmp, x, l), v, r);
      } else {
        return bal(l, v, remove0(cmp, x, r));
      }
    } else {
      var t1 = l;
      var t2 = r;
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
  } else {
    return /* Empty */0;
  }
}

function union0(cmp, s1, s2) {
  if (s1) {
    if (s2) {
      var h2 = s2[3];
      var v2 = s2[1];
      var h1 = s1[3];
      var v1 = s1[1];
      if (h1 >= h2) {
        if (h2 === 1) {
          return add0(cmp, v2, s1);
        } else {
          var match = split0(cmp, v1, s2);
          return join(union0(cmp, s1[0], match[0]), v1, union0(cmp, s1[2], match[2]));
        }
      } else if (h1 === 1) {
        return add0(cmp, v1, s2);
      } else {
        var match$1 = split0(cmp, v2, s1);
        return join(union0(cmp, match$1[0], s2[0]), v2, union0(cmp, match$1[2], s2[2]));
      }
    } else {
      return s1;
    }
  } else {
    return s2;
  }
}

function inter0(cmp, s1, s2) {
  if (s1) {
    if (s2) {
      var r1 = s1[2];
      var v1 = s1[1];
      var l1 = s1[0];
      var match = split0(cmp, v1, s2);
      var l2 = match[0];
      if (match[1] !== 0) {
        return join(inter0(cmp, l1, l2), v1, inter0(cmp, r1, match[2]));
      } else {
        return concat(inter0(cmp, l1, l2), inter0(cmp, r1, match[2]));
      }
    } else {
      return /* Empty */0;
    }
  } else {
    return /* Empty */0;
  }
}

function diff0(cmp, s1, s2) {
  if (s1) {
    if (s2) {
      var r1 = s1[2];
      var v1 = s1[1];
      var l1 = s1[0];
      var match = split0(cmp, v1, s2);
      var l2 = match[0];
      if (match[1] !== 0) {
        return concat(diff0(cmp, l1, l2), diff0(cmp, r1, match[2]));
      } else {
        return join(diff0(cmp, l1, l2), v1, diff0(cmp, r1, match[2]));
      }
    } else {
      return s1;
    }
  } else {
    return /* Empty */0;
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

function cmp0(cmp, s1, s2) {
  var cmp$1 = cmp;
  var _e1 = cons_enum(s1, /* End */0);
  var _e2 = cons_enum(s2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var c = cmp$1(e1[0], e2[0]);
        if (c !== 0) {
          return c;
        } else {
          _e2 = cons_enum(e2[1], e2[2]);
          _e1 = cons_enum(e1[1], e1[2]);
          continue ;
          
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

function eq0(cmp, s1, s2) {
  return +(cmp0(cmp, s1, s2) === 0);
}

function subset0(cmp, _s1, _s2) {
  while(true) {
    var s2 = _s2;
    var s1 = _s1;
    if (s1) {
      if (s2) {
        var r2 = s2[2];
        var l2 = s2[0];
        var r1 = s1[2];
        var v1 = s1[1];
        var l1 = s1[0];
        var c = cmp(v1, s2[1]);
        if (c) {
          if (c < 0) {
            if (subset0(cmp, /* Node */[
                    l1,
                    v1,
                    /* Empty */0,
                    0
                  ], l2)) {
              _s1 = r1;
              continue ;
              
            } else {
              return /* false */0;
            }
          } else if (subset0(cmp, /* Node */[
                  /* Empty */0,
                  v1,
                  r1,
                  0
                ], r2)) {
            _s1 = l1;
            continue ;
            
          } else {
            return /* false */0;
          }
        } else if (subset0(cmp, l1, l2)) {
          _s2 = r2;
          _s1 = r1;
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

function findOpt0(cmp, x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[1];
      var c = cmp(x, v);
      if (c) {
        _param = c < 0 ? param[0] : param[2];
        continue ;
        
      } else {
        return /* Some */[v];
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
      var v = param[1];
      var c = cmp(x, v);
      if (c) {
        _param = c < 0 ? param[0] : param[2];
        continue ;
        
      } else {
        return v;
      }
    } else {
      throw new Error("Not_found");
    }
  };
}

function empty(cmp) {
  return /* record */[
          /* cmp */cmp,
          /* data : Empty */0
        ];
}

function isEmpty(m) {
  return isEmpty0(m[/* data */1]);
}

function mem(e, m) {
  var M = m[/* cmp */0];
  return mem0(M[/* cmp */0], e, m[/* data */1]);
}

function add(e, m) {
  var m_cmp = m[/* cmp */0];
  return /* record */[
          /* cmp */m_cmp,
          /* data */add0(m_cmp[/* cmp */0], e, m[/* data */1])
        ];
}

function singleton(cmp, e) {
  return /* record */[
          /* cmp */cmp,
          /* data */singleton0(e)
        ];
}

function remove(e, m) {
  var m_cmp = m[/* cmp */0];
  return /* record */[
          /* cmp */m_cmp,
          /* data */remove0(m_cmp[/* cmp */0], e, m[/* data */1])
        ];
}

function union(m, n) {
  var m_cmp = m[/* cmp */0];
  return /* record */[
          /* cmp */m_cmp,
          /* data */union0(m_cmp[/* cmp */0], m[/* data */1], n[/* data */1])
        ];
}

function inter(m, n) {
  var m_cmp = m[/* cmp */0];
  return /* record */[
          /* cmp */m_cmp,
          /* data */inter0(m_cmp[/* cmp */0], m[/* data */1], n[/* data */1])
        ];
}

function diff(m, n) {
  var m_cmp = m[/* cmp */0];
  return /* record */[
          /* cmp */m_cmp,
          /* data */diff0(m_cmp[/* cmp */0], m[/* data */1], n[/* data */1])
        ];
}

function cmp(m, n) {
  var m_cmp = m[/* cmp */0];
  return cmp0(m_cmp[/* cmp */0], m[/* data */1], n[/* data */1]);
}

function eq(m, n) {
  var m_cmp = m[/* cmp */0];
  return eq0(m_cmp[/* cmp */0], m[/* data */1], n[/* data */1]);
}

function subset(m, n) {
  var m_cmp = m[/* cmp */0];
  return subset0(m_cmp[/* cmp */0], m[/* data */1], n[/* data */1]);
}

function iter(f, m) {
  return iter0(f, m[/* data */1]);
}

function fold(f, m, acc) {
  return fold0(f, m[/* data */1], acc);
}

function forAll(f, m) {
  return forAll0(f, m[/* data */1]);
}

function exists(f, m) {
  return exists0(f, m[/* data */1]);
}

function filter(f, m) {
  return /* record */[
          /* cmp */m[/* cmp */0],
          /* data */filter0(f, m[/* data */1])
        ];
}

function partition(f, m) {
  var match = partition0(f, m[/* data */1]);
  var cmp = m[/* cmp */0];
  return /* tuple */[
          /* record */[
            /* cmp */cmp,
            /* data */match[0]
          ],
          /* record */[
            /* cmp */cmp,
            /* data */match[1]
          ]
        ];
}

function cardinal(m) {
  return cardinal0(m[/* data */1]);
}

function elements(m) {
  return elements0(m[/* data */1]);
}

function min(m) {
  return min0(m[/* data */1]);
}

function max(m) {
  return max0(m[/* data */1]);
}

function split(e, m) {
  var m_cmp = m[/* cmp */0];
  var match = split0(m_cmp[/* cmp */0], e, m[/* data */1]);
  return /* tuple */[
          /* record */[
            /* cmp */m_cmp,
            /* data */match[0]
          ],
          match[1],
          /* record */[
            /* cmp */m_cmp,
            /* data */match[2]
          ]
        ];
}

function findOpt(e, m) {
  var m_cmp = m[/* cmp */0];
  return findOpt0(m_cmp[/* cmp */0], e, m[/* data */1]);
}

function findAssert(e, m) {
  var m_cmp = m[/* cmp */0];
  return findAssert0(m_cmp[/* cmp */0], e, m[/* data */1]);
}

var empty0 = /* Empty */0;

exports.empty0      = empty0;
exports.empty       = empty;
exports.isEmpty0    = isEmpty0;
exports.isEmpty     = isEmpty;
exports.mem0        = mem0;
exports.mem         = mem;
exports.add0        = add0;
exports.add         = add;
exports.singleton0  = singleton0;
exports.singleton   = singleton;
exports.remove0     = remove0;
exports.remove      = remove;
exports.union0      = union0;
exports.union       = union;
exports.inter0      = inter0;
exports.inter       = inter;
exports.diff0       = diff0;
exports.diff        = diff;
exports.cmp0        = cmp0;
exports.cmp         = cmp;
exports.eq0         = eq0;
exports.eq          = eq;
exports.subset0     = subset0;
exports.subset      = subset;
exports.iter0       = iter0;
exports.iter        = iter;
exports.fold0       = fold0;
exports.fold        = fold;
exports.forAll0     = forAll0;
exports.forAll      = forAll;
exports.exists0     = exists0;
exports.exists      = exists;
exports.filter0     = filter0;
exports.filter      = filter;
exports.partition0  = partition0;
exports.partition   = partition;
exports.cardinal0   = cardinal0;
exports.cardinal    = cardinal;
exports.elements0   = elements0;
exports.elements    = elements;
exports.min0        = min0;
exports.min         = min;
exports.max0        = max0;
exports.max         = max;
exports.split0      = split0;
exports.split       = split;
exports.findOpt0    = findOpt0;
exports.findOpt     = findOpt;
exports.findAssert0 = findAssert0;
exports.findAssert  = findAssert;
/* No side effect */
