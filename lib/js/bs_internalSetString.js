'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");

function add(t, x) {
  if (t !== null) {
    var v = t.key;
    if (x === v) {
      return t;
    } else {
      var l = t.left;
      var r = t.right;
      if (x < v) {
        var ll = add(l, x);
        if (ll === l) {
          return t;
        } else {
          return Bs_internalAVLset.bal(ll, v, r);
        }
      } else {
        var rr = add(r, x);
        if (rr === r) {
          return t;
        } else {
          return Bs_internalAVLset.bal(l, v, add(r, x));
        }
      }
    }
  } else {
    return Bs_internalAVLset.singleton0(x);
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
      if (l !== null) {
        if (r !== null) {
          var v$1 = [r.key];
          var r$1 = Bs_internalAVLset.removeMinAuxWithRef(r, v$1);
          return Bs_internalAVLset.bal(l, v$1[0], r$1);
        } else {
          return l;
        }
      } else {
        return r;
      }
    } else if (x < v) {
      var ll = remove(l, x);
      if (ll === l) {
        return t;
      } else {
        return Bs_internalAVLset.bal(ll, v, r);
      }
    } else {
      var rr = remove(r, x);
      if (rr === r) {
        return t;
      } else {
        return Bs_internalAVLset.bal(l, v, rr);
      }
    }
  } else {
    return t;
  }
}

function compareAux(_e1, _e2) {
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var h2 = e2[0];
        var h1 = e1[0];
        var k1 = h1.key;
        var k2 = h2.key;
        if (k1 === k2) {
          _e2 = Bs_internalAVLset.stackAllLeft(h2.right, e2[1]);
          _e1 = Bs_internalAVLset.stackAllLeft(h1.right, e1[1]);
          continue ;
          
        } else if (k1 < k2) {
          return -1;
        } else {
          return 1;
        }
      } else {
        return 0;
      }
    } else {
      return 0;
    }
  };
}

function cmp(s1, s2) {
  var len1 = Bs_internalAVLset.length0(s1);
  var len2 = Bs_internalAVLset.length0(s2);
  if (len1 === len2) {
    return compareAux(Bs_internalAVLset.stackAllLeft(s1, /* [] */0), Bs_internalAVLset.stackAllLeft(s2, /* [] */0));
  } else if (len1 < len2) {
    return -1;
  } else {
    return 1;
  }
}

function eq(s1, s2) {
  return +(cmp(s1, s2) === 0);
}

function splitAuxNoPivot(n, x) {
  var l = n.left;
  var v = n.key;
  var r = n.right;
  if (x === v) {
    return /* tuple */[
            l,
            r
          ];
  } else if (x < v) {
    if (l !== null) {
      var match = splitAuxNoPivot(l, x);
      return /* tuple */[
              match[0],
              Bs_internalAVLset.join(match[1], v, r)
            ];
    } else {
      return /* tuple */[
              null,
              n
            ];
    }
  } else if (r !== null) {
    var match$1 = splitAuxNoPivot(r, x);
    return /* tuple */[
            Bs_internalAVLset.join(l, v, match$1[0]),
            match$1[1]
          ];
  } else {
    return /* tuple */[
            n,
            null
          ];
  }
}

function splitAuxPivot(n, x, pres) {
  var l = n.left;
  var v = n.key;
  var r = n.right;
  if (x === v) {
    pres[0] = /* true */1;
    return /* tuple */[
            l,
            r
          ];
  } else if (x < v) {
    if (l !== null) {
      var match = splitAuxPivot(l, x, pres);
      return /* tuple */[
              match[0],
              Bs_internalAVLset.join(match[1], v, r)
            ];
    } else {
      return /* tuple */[
              null,
              n
            ];
    }
  } else if (r !== null) {
    var match$1 = splitAuxPivot(r, x, pres);
    return /* tuple */[
            Bs_internalAVLset.join(l, v, match$1[0]),
            match$1[1]
          ];
  } else {
    return /* tuple */[
            n,
            null
          ];
  }
}

function split(t, x) {
  if (t !== null) {
    var pres = [/* false */0];
    var match = splitAuxPivot(t, x, pres);
    return /* tuple */[
            match[0],
            pres[0],
            match[1]
          ];
  } else {
    return /* tuple */[
            null,
            /* false */0,
            null
          ];
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
          var match = splitAuxNoPivot(s2, v1);
          return Bs_internalAVLset.join(union(l1, match[0]), v1, union(r1, match[1]));
        }
      } else if (h1 === 1) {
        return add(s2, s1.key);
      } else {
        var l2 = s2.left;
        var v2 = s2.key;
        var r2 = s2.right;
        var match$1 = splitAuxNoPivot(s1, v2);
        return Bs_internalAVLset.join(union(match$1[0], l2), v2, union(match$1[1], r2));
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
      var pres = [/* false */0];
      var match = splitAuxPivot(s2, v1, pres);
      var ll = inter(l1, match[0]);
      var rr = inter(r1, match[1]);
      if (pres[0]) {
        return Bs_internalAVLset.join(ll, v1, rr);
      } else {
        return Bs_internalAVLset.concat(ll, rr);
      }
    } else {
      return null;
    }
  } else {
    return null;
  }
}

function diff(s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.key;
      var r1 = s1.right;
      var pres = [/* false */0];
      var match = splitAuxPivot(s2, v1, pres);
      var ll = diff(l1, match[0]);
      var rr = diff(r1, match[1]);
      if (pres[0]) {
        return Bs_internalAVLset.concat(ll, rr);
      } else {
        return Bs_internalAVLset.join(ll, v1, rr);
      }
    } else {
      return s1;
    }
  } else {
    return s1;
  }
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
          if (subset(Bs_internalAVLset.create(l1, v1, null), l2)) {
            _s1 = r1;
            continue ;
            
          } else {
            return /* false */0;
          }
        } else if (subset(Bs_internalAVLset.create(null, v1, r1), r2)) {
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

function findOpt(_n, x) {
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

function findNull(_n, x) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      if (x === v) {
        return v;
      } else {
        _n = x < v ? n.left : n.right;
        continue ;
        
      }
    } else {
      return null;
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
    return Bs_internalAVLset.singleton0(x);
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

function sortedLengthAux(xs, _prec, _acc, len) {
  while(true) {
    var acc = _acc;
    var prec = _prec;
    if (acc >= len) {
      return acc;
    } else {
      var v = xs[acc];
      if (v > prec) {
        _acc = acc + 1 | 0;
        _prec = v;
        continue ;
        
      } else {
        return acc;
      }
    }
  };
}

function ofArray(xs) {
  var len = xs.length;
  if (len) {
    var next = sortedLengthAux(xs, xs[0], 1, len);
    var result = Bs_internalAVLset.ofSortedArrayAux(xs, 0, next);
    for(var i = next ,i_finish = len - 1 | 0; i <= i_finish; ++i){
      result = addMutate(result, xs[i]);
    }
    return result;
  } else {
    return null;
  }
}

var N = 0;

var A = 0;

exports.N = N;
exports.A = A;
exports.add = add;
exports.mem = mem;
exports.remove = remove;
exports.compareAux = compareAux;
exports.cmp = cmp;
exports.eq = eq;
exports.splitAuxNoPivot = splitAuxNoPivot;
exports.splitAuxPivot = splitAuxPivot;
exports.split = split;
exports.union = union;
exports.inter = inter;
exports.diff = diff;
exports.subset = subset;
exports.findOpt = findOpt;
exports.findNull = findNull;
exports.addMutate = addMutate;
exports.removeMutateAux = removeMutateAux;
exports.removeMutate = removeMutate;
exports.addArrayMutate = addArrayMutate;
exports.sortedLengthAux = sortedLengthAux;
exports.ofArray = ofArray;
/* No side effect */
