'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");

function add0(cmp, x, t) {
  if (t) {
    var r = t[2];
    var v = t[1];
    var l = t[0];
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        return Bs_internalAVLset.bal(add0(cmp, x, l), v, r);
      } else {
        return Bs_internalAVLset.bal(l, v, add0(cmp, x, r));
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
                Bs_internalAVLset.join(match[2], v, r)
              ];
      } else {
        var match$1 = split0(cmp, x, r);
        return /* tuple */[
                Bs_internalAVLset.join(l, v, match$1[0]),
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
        return Bs_internalAVLset.bal(remove0(cmp, x, l), v, r);
      } else {
        return Bs_internalAVLset.bal(l, v, remove0(cmp, x, r));
      }
    } else {
      return Bs_internalAVLset.merge(l, r);
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
          return Bs_internalAVLset.join(union0(cmp, s1[0], match[0]), v1, union0(cmp, s1[2], match[2]));
        }
      } else if (h1 === 1) {
        return add0(cmp, v1, s2);
      } else {
        var match$1 = split0(cmp, v2, s1);
        return Bs_internalAVLset.join(union0(cmp, match$1[0], s2[0]), v2, union0(cmp, match$1[2], s2[2]));
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
        return Bs_internalAVLset.join(inter0(cmp, l1, l2), v1, inter0(cmp, r1, match[2]));
      } else {
        return Bs_internalAVLset.concat(inter0(cmp, l1, l2), inter0(cmp, r1, match[2]));
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
        return Bs_internalAVLset.concat(diff0(cmp, l1, l2), diff0(cmp, r1, match[2]));
      } else {
        return Bs_internalAVLset.join(diff0(cmp, l1, l2), v1, diff0(cmp, r1, match[2]));
      }
    } else {
      return s1;
    }
  } else {
    return /* Empty */0;
  }
}

function cmp0(cmp, s1, s2) {
  var cmp$1 = cmp;
  var _e1 = Bs_internalAVLset.cons_enum(s1, /* End */0);
  var _e2 = Bs_internalAVLset.cons_enum(s2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var c = cmp$1(e1[0], e2[0]);
        if (c !== 0) {
          return c;
        } else {
          _e2 = Bs_internalAVLset.cons_enum(e2[1], e2[2]);
          _e1 = Bs_internalAVLset.cons_enum(e1[1], e1[2]);
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
  if (m[/* data */1]) {
    return /* false */0;
  } else {
    return /* true */1;
  }
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
          /* data */Bs_internalAVLset.singleton0(e)
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
  return Bs_internalAVLset.iter0(f, m[/* data */1]);
}

function fold(f, m, acc) {
  return Bs_internalAVLset.fold0(f, m[/* data */1], acc);
}

function forAll(f, m) {
  return Bs_internalAVLset.forAll0(f, m[/* data */1]);
}

function exists(f, m) {
  return Bs_internalAVLset.exists0(f, m[/* data */1]);
}

function filter(f, m) {
  return /* record */[
          /* cmp */m[/* cmp */0],
          /* data */Bs_internalAVLset.filter0(f, m[/* data */1])
        ];
}

function partition(f, m) {
  var match = Bs_internalAVLset.partition0(f, m[/* data */1]);
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
  return Bs_internalAVLset.cardinal0(m[/* data */1]);
}

function elements(m) {
  return Bs_internalAVLset.elements0(m[/* data */1]);
}

function min(m) {
  return Bs_internalAVLset.min0(m[/* data */1]);
}

function max(m) {
  return Bs_internalAVLset.max0(m[/* data */1]);
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

var isEmpty0 = Bs_internalAVLset.isEmpty0;

var singleton0 = Bs_internalAVLset.singleton0;

var iter0 = Bs_internalAVLset.iter0;

var fold0 = Bs_internalAVLset.fold0;

var forAll0 = Bs_internalAVLset.forAll0;

var exists0 = Bs_internalAVLset.exists0;

var filter0 = Bs_internalAVLset.filter0;

var partition0 = Bs_internalAVLset.partition0;

var cardinal0 = Bs_internalAVLset.cardinal0;

var elements0 = Bs_internalAVLset.elements0;

var min0 = Bs_internalAVLset.min0;

var max0 = Bs_internalAVLset.max0;

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
