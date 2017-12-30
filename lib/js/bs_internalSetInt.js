'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");

function add(t, x) {
  if (t !== null) {
    var v = t.key;
    if (x === v) {
      return t;
    } else if (x < v) {
      return Bs_internalAVLset.bal(add(t.left, x), v, t.right);
    } else {
      return Bs_internalAVLset.bal(t.left, v, add(t.right, x));
    }
  } else {
    return {
            left: null,
            key: x,
            right: null,
            h: 1
          };
  }
}

function splitAux(x, n) {
  var l = n.left;
  var v = n.key;
  var r = n.right;
  if (x === v) {
    return /* tuple */[
            l,
            /* true */1,
            r
          ];
  } else if (x < v) {
    if (l !== null) {
      var match = splitAux(x, l);
      return /* tuple */[
              match[0],
              match[1],
              Bs_internalAVLset.join(match[2], v, r)
            ];
    } else {
      return /* tuple */[
              null,
              /* false */0,
              n
            ];
    }
  } else if (r !== null) {
    var match$1 = splitAux(x, r);
    return /* tuple */[
            Bs_internalAVLset.join(l, v, match$1[0]),
            match$1[1],
            match$1[2]
          ];
  } else {
    return /* tuple */[
            n,
            /* false */0,
            null
          ];
  }
}

function split(x, t) {
  if (t !== null) {
    return splitAux(x, t);
  } else {
    return /* tuple */[
            null,
            /* false */0,
            null
          ];
  }
}

function mem(_t, x) {
  while(true) {
    var t = _t;
    if (t !== null) {
      var v = t.key;
      if (x === v) {
        return /* true */1;
      } else {
        _t = x < v ? t.left : t.right;
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function remove(t, x) {
  if (t !== null) {
    var l = t.left;
    var v = t.key;
    var r = t.right;
    if (x === v) {
      return Bs_internalAVLset.merge(l, r);
    } else if (x < v) {
      return Bs_internalAVLset.bal(remove(l, x), v, r);
    } else {
      return Bs_internalAVLset.bal(l, v, remove(r, x));
    }
  } else {
    return t;
  }
}

function union(s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var h1 = s1.h;
      var h2 = s2.h;
      if (h1 >= h2) {
        if (h2 === 1) {
          return add(s1, s2.key);
        } else {
          var l1 = s1.left;
          var v1 = s1.key;
          var r1 = s1.right;
          var match = splitAux(v1, s2);
          return Bs_internalAVLset.join(union(l1, match[0]), v1, union(r1, match[2]));
        }
      } else if (h1 === 1) {
        return add(s2, s1.key);
      } else {
        var l2 = s2.left;
        var v2 = s2.key;
        var r2 = s2.right;
        var match$1 = splitAux(v2, s1);
        return Bs_internalAVLset.join(union(match$1[0], l2), v2, union(match$1[2], r2));
      }
    } else {
      return s1;
    }
  } else {
    return s2;
  }
}

function inter(s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.key;
      var r1 = s1.right;
      var match = splitAux(v1, s2);
      var l2 = match[0];
      if (match[1] !== 0) {
        return Bs_internalAVLset.join(inter(l1, l2), v1, inter(r1, match[2]));
      } else {
        return Bs_internalAVLset.concat(inter(l1, l2), inter(r1, match[2]));
      }
    } else {
      return s2;
    }
  } else {
    return s1;
  }
}

function diff(s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.key;
      var r1 = s1.right;
      var match = splitAux(v1, s2);
      var l2 = match[0];
      if (match[1] !== 0) {
        return Bs_internalAVLset.concat(diff(l1, l2), diff(r1, match[2]));
      } else {
        return Bs_internalAVLset.join(diff(l1, l2), v1, diff(r1, match[2]));
      }
    } else {
      return s1;
    }
  } else {
    return s1;
  }
}

function compare_aux(_e1, _e2) {
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var v2 = e2[0];
        var v1 = e1[0];
        if (v1 !== v2) {
          if (v1 < v2) {
            return -1;
          } else {
            return 1;
          }
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

function cmp(s1, s2) {
  return compare_aux(Bs_internalAVLset.cons_enum(s1, /* End */0), Bs_internalAVLset.cons_enum(s2, /* End */0));
}

function eq_aux(_e1, _e2) {
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        if (e1[0] === e2[0]) {
          _e2 = Bs_internalAVLset.cons_enum(e2[1], e2[2]);
          _e1 = Bs_internalAVLset.cons_enum(e1[1], e1[2]);
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

function eq(s1, s2) {
  return eq_aux(Bs_internalAVLset.cons_enum(s1, /* End */0), Bs_internalAVLset.cons_enum(s2, /* End */0));
}

function subset(_s1, _s2) {
  while(true) {
    var s2 = _s2;
    var s1 = _s1;
    if (s1 !== null) {
      if (s2 !== null) {
        var l1 = s1.left;
        var v1 = s1.key;
        var r1 = s1.right;
        var l2 = s2.left;
        var v2 = s2.key;
        var r2 = s2.right;
        if (v1 === v2) {
          if (subset(l1, l2)) {
            _s2 = r2;
            _s1 = r1;
            continue ;
            
          } else {
            return /* false */0;
          }
        } else if (v1 < v2) {
          if (subset({
                  left: l1,
                  key: v1,
                  right: null,
                  h: 0
                }, l2)) {
            _s1 = r1;
            continue ;
            
          } else {
            return /* false */0;
          }
        } else if (subset({
                left: null,
                key: v1,
                right: r1,
                h: 0
              }, r2)) {
          _s1 = l1;
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

function findOpt(x, _n) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      if (x === v) {
        return /* Some */[v];
      } else {
        _n = x < v ? n.left : n.right;
        continue ;
        
      }
    } else {
      return /* None */0;
    }
  };
}

function findAssert(x, _n) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      if (x === v) {
        return /* Some */[v];
      } else {
        _n = x < v ? n.left : n.right;
        continue ;
        
      }
    } else {
      throw new Error("Not_found");
    }
  };
}

function addMutate(t, x) {
  if (t !== null) {
    var k = t.key;
    if (x === k) {
      return t;
    } else {
      var l = t.left;
      var r = t.right;
      if (x < k) {
        t.left = addMutate(l, x);
      } else {
        t.right = addMutate(r, x);
      }
      return Bs_internalAVLset.balMutate(t);
    }
  } else {
    return {
            left: null,
            key: x,
            right: null,
            h: 1
          };
  }
}

function removeMutateAux(nt, x) {
  var k = nt.key;
  if (x === k) {
    var l = nt.left;
    var r = nt.right;
    if (l !== null) {
      if (r !== null) {
        nt.right = Bs_internalAVLset.removeMinAuxMutateWithRoot(nt, r);
        return Bs_internalAVLset.balMutate(nt);
      } else {
        return l;
      }
    } else if (r !== null) {
      return r;
    } else {
      return l;
    }
  } else if (x < k) {
    var match = nt.left;
    if (match !== null) {
      nt.left = removeMutateAux(match, x);
      return Bs_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  } else {
    var match$1 = nt.right;
    if (match$1 !== null) {
      nt.right = removeMutateAux(match$1, x);
      return Bs_internalAVLset.balMutate(nt);
    } else {
      return nt;
    }
  }
}

function removeMutate(nt, x) {
  if (nt !== null) {
    return removeMutateAux(nt, x);
  } else {
    return nt;
  }
}

function addArrayMutate(t, xs) {
  var v = t;
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    v = addMutate(v, xs[i]);
  }
  return v;
}

function ofArray(xs) {
  var result = null;
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    result = addMutate(result, xs[i]);
  }
  return result;
}

var N = 0;

var A = 0;

exports.N = N;
exports.A = A;
exports.add = add;
exports.splitAux = splitAux;
exports.split = split;
exports.mem = mem;
exports.remove = remove;
exports.union = union;
exports.inter = inter;
exports.diff = diff;
exports.compare_aux = compare_aux;
exports.cmp = cmp;
exports.eq_aux = eq_aux;
exports.eq = eq;
exports.subset = subset;
exports.findOpt = findOpt;
exports.findAssert = findAssert;
exports.addMutate = addMutate;
exports.removeMutateAux = removeMutateAux;
exports.removeMutate = removeMutate;
exports.addArrayMutate = addArrayMutate;
exports.ofArray = ofArray;
/* No side effect */
