'use strict';

var Bs_internalAVLset = require("./bs_internalAVLset.js");

function add0(cmp, t, x) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(x, k);
    if (c) {
      var l = t.left;
      var r = t.right;
      if (c < 0) {
        var ll = add0(cmp, l, x);
        if (ll === l) {
          return t;
        } else {
          return Bs_internalAVLset.bal(ll, k, r);
        }
      } else {
        var rr = add0(cmp, r, x);
        if (rr === r) {
          return t;
        } else {
          return Bs_internalAVLset.bal(l, k, rr);
        }
      }
    } else {
      return t;
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

function splitAux(cmp, n, x) {
  var l = n.left;
  var v = n.key;
  var r = n.right;
  var c = cmp(x, v);
  if (c) {
    if (c < 0) {
      if (l !== null) {
        var match = splitAux(cmp, l, x);
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
      var match$1 = splitAux(cmp, r, x);
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
  } else {
    return /* tuple */[
            l,
            /* true */1,
            r
          ];
  }
}

function split0(cmp, t, x) {
  if (t !== null) {
    return splitAux(cmp, t, x);
  } else {
    return /* tuple */[
            null,
            /* false */0,
            null
          ];
  }
}

function mem0(cmp, _t, x) {
  while(true) {
    var t = _t;
    if (t !== null) {
      var v = t.key;
      var c = cmp(x, v);
      if (c) {
        _t = c < 0 ? t.left : t.right;
        continue ;
        
      } else {
        return /* true */1;
      }
    } else {
      return /* false */0;
    }
  };
}

function remove0(cmp, t, x) {
  if (t !== null) {
    var l = t.left;
    var v = t.key;
    var r = t.right;
    var c = cmp(x, v);
    if (c) {
      if (c < 0) {
        var ll = remove0(cmp, l, x);
        if (ll === l) {
          return t;
        } else {
          return Bs_internalAVLset.bal(ll, v, r);
        }
      } else {
        var rr = remove0(cmp, r, x);
        if (rr === r) {
          return t;
        } else {
          return Bs_internalAVLset.bal(l, v, rr);
        }
      }
    } else {
      return Bs_internalAVLset.merge(l, r);
    }
  } else {
    return t;
  }
}

function addArray0(cmp, h, arr) {
  var len = arr.length;
  var v = h;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var key = arr[i];
    v = add0(cmp, v, key);
  }
  return v;
}

function removeArray0(h, arr, cmp) {
  var len = arr.length;
  var v = h;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var key = arr[i];
    v = remove0(cmp, v, key);
  }
  return v;
}

function union0(cmp, s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var h1 = s1.h;
      var h2 = s2.h;
      if (h1 >= h2) {
        if (h2 === 1) {
          return add0(cmp, s1, s2.key);
        } else {
          var l1 = s1.left;
          var v1 = s1.key;
          var r1 = s1.right;
          var match = split0(cmp, s2, v1);
          return Bs_internalAVLset.join(union0(cmp, l1, match[0]), v1, union0(cmp, r1, match[2]));
        }
      } else if (h1 === 1) {
        return add0(cmp, s2, s1.key);
      } else {
        var l2 = s2.left;
        var v2 = s2.key;
        var r2 = s2.right;
        var match$1 = split0(cmp, s1, v2);
        return Bs_internalAVLset.join(union0(cmp, match$1[0], l2), v2, union0(cmp, match$1[2], r2));
      }
    } else {
      return s1;
    }
  } else {
    return s2;
  }
}

function inter0(cmp, s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.key;
      var r1 = s1.right;
      var match = splitAux(cmp, s2, v1);
      var l2 = match[0];
      if (match[1] !== 0) {
        return Bs_internalAVLset.join(inter0(cmp, l1, l2), v1, inter0(cmp, r1, match[2]));
      } else {
        return Bs_internalAVLset.concat(inter0(cmp, l1, l2), inter0(cmp, r1, match[2]));
      }
    } else {
      return s2;
    }
  } else {
    return s1;
  }
}

function diff0(cmp, s1, s2) {
  if (s1 !== null) {
    if (s2 !== null) {
      var l1 = s1.left;
      var v1 = s1.key;
      var r1 = s1.right;
      var match = splitAux(cmp, s2, v1);
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
    return s1;
  }
}

function compareAux(cmp, _e1, _e2) {
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var c = cmp(e1[0], e2[0]);
        if (c !== 0) {
          return c;
        } else {
          _e2 = Bs_internalAVLset.toEnum(e2[1], e2[2]);
          _e1 = Bs_internalAVLset.toEnum(e1[1], e1[2]);
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

function cmp0(cmp, s1, s2) {
  return compareAux(cmp, Bs_internalAVLset.toEnum(s1, /* End */0), Bs_internalAVLset.toEnum(s2, /* End */0));
}

function eq0(cmp, s1, s2) {
  return +(cmp0(cmp, s1, s2) === 0);
}

function subset0(cmp, _s1, _s2) {
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
        var c = cmp(v1, v2);
        if (c) {
          if (c < 0) {
            if (subset0(cmp, {
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
          } else if (subset0(cmp, {
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

function findOpt0(cmp, _n, x) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      var c = cmp(x, v);
      if (c) {
        _n = c < 0 ? n.left : n.right;
        continue ;
        
      } else {
        return /* Some */[v];
      }
    } else {
      return /* None */0;
    }
  };
}

function findNull0(cmp, _n, x) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      var c = cmp(x, v);
      if (c) {
        _n = c < 0 ? n.left : n.right;
        continue ;
        
      } else {
        return v;
      }
    } else {
      return null;
    }
  };
}

function addMutate(cmp, t, x) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(x, k);
    if (c) {
      var l = t.left;
      var r = t.right;
      if (c < 0) {
        var ll = addMutate(cmp, l, x);
        t.left = ll;
      } else {
        t.right = addMutate(cmp, r, x);
      }
      return Bs_internalAVLset.balMutate(t);
    } else {
      return t;
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

function removeMutateAux(cmp, nt, x) {
  var k = nt.key;
  var c = cmp(x, k);
  if (c) {
    if (c < 0) {
      var match = nt.left;
      if (match !== null) {
        nt.left = removeMutateAux(cmp, match, x);
        return Bs_internalAVLset.balMutate(nt);
      } else {
        return nt;
      }
    } else {
      var match$1 = nt.right;
      if (match$1 !== null) {
        nt.right = removeMutateAux(cmp, match$1, x);
        return Bs_internalAVLset.balMutate(nt);
      } else {
        return nt;
      }
    }
  } else {
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
  }
}

function sortedLengthAux(cmp, xs, _prec, _acc, len) {
  while(true) {
    var acc = _acc;
    var prec = _prec;
    if (acc >= len) {
      return acc;
    } else {
      var v = xs[acc];
      if (cmp(v, prec) >= 0) {
        _acc = acc + 1 | 0;
        _prec = v;
        continue ;
        
      } else {
        return acc;
      }
    }
  };
}

function ofArray0(cmp, xs) {
  var len = xs.length;
  if (len) {
    var next = sortedLengthAux(cmp, xs, xs[0], 1, len);
    var result = Bs_internalAVLset.ofSortedArrayAux(xs, 0, next);
    for(var i = next ,i_finish = len - 1 | 0; i <= i_finish; ++i){
      result = addMutate(cmp, result, xs[i]);
    }
    return result;
  } else {
    return Bs_internalAVLset.empty0;
  }
}

function addArrayMutate(t, xs, cmp) {
  var v = t;
  for(var i = 0 ,i_finish = xs.length - 1 | 0; i <= i_finish; ++i){
    v = addMutate(cmp, v, xs[i]);
  }
  return v;
}

function addMutateCheckAux(t, x, added, cmp) {
  if (t !== null) {
    var k = t.key;
    var c = cmp(x, k);
    if (c) {
      var l = t.left;
      var r = t.right;
      if (c < 0) {
        var ll = addMutateCheckAux(l, x, added, cmp);
        t.left = ll;
      } else {
        t.right = addMutateCheckAux(r, x, added, cmp);
      }
      return Bs_internalAVLset.balMutate(t);
    } else {
      return t;
    }
  } else {
    added[0] = /* true */1;
    return {
            left: null,
            key: x,
            right: null,
            h: 1
          };
  }
}

function removeArrayMutateAux(_t, xs, _i, len, cmp) {
  while(true) {
    var i = _i;
    var t = _t;
    if (i < len) {
      var ele = xs[i];
      var u = removeMutateAux(cmp, t, ele);
      if (u !== null) {
        _i = i + 1 | 0;
        _t = u;
        continue ;
        
      } else {
        return Bs_internalAVLset.empty0;
      }
    } else {
      return t;
    }
  };
}

function removeArrayMutate(t, xs, cmp) {
  if (t !== null) {
    var len = xs.length;
    return removeArrayMutateAux(t, xs, 0, len, cmp);
  } else {
    return t;
  }
}

function removeMutateCheckAux(nt, x, removed, cmp) {
  var k = nt.key;
  var c = cmp(x, k);
  if (c) {
    if (c < 0) {
      var match = nt.left;
      if (match !== null) {
        nt.left = removeMutateCheckAux(match, x, removed, cmp);
        return Bs_internalAVLset.balMutate(nt);
      } else {
        return nt;
      }
    } else {
      var match$1 = nt.right;
      if (match$1 !== null) {
        nt.right = removeMutateCheckAux(match$1, x, removed, cmp);
        return Bs_internalAVLset.balMutate(nt);
      } else {
        return nt;
      }
    }
  } else {
    removed[0] = /* true */1;
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
  }
}

var N = 0;

var B = 0;

var A = 0;

exports.N = N;
exports.B = B;
exports.A = A;
exports.add0 = add0;
exports.splitAux = splitAux;
exports.split0 = split0;
exports.mem0 = mem0;
exports.remove0 = remove0;
exports.addArray0 = addArray0;
exports.removeArray0 = removeArray0;
exports.union0 = union0;
exports.inter0 = inter0;
exports.diff0 = diff0;
exports.compareAux = compareAux;
exports.cmp0 = cmp0;
exports.eq0 = eq0;
exports.subset0 = subset0;
exports.findOpt0 = findOpt0;
exports.findNull0 = findNull0;
exports.addMutate = addMutate;
exports.removeMutateAux = removeMutateAux;
exports.sortedLengthAux = sortedLengthAux;
exports.ofArray0 = ofArray0;
exports.addArrayMutate = addArrayMutate;
exports.addMutateCheckAux = addMutateCheckAux;
exports.removeArrayMutateAux = removeArrayMutateAux;
exports.removeArrayMutate = removeArrayMutate;
exports.removeMutateCheckAux = removeMutateCheckAux;
/* No side effect */
