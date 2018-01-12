'use strict';

var Curry = require("./curry.js");
var Belt_SortArray = require("./belt_SortArray.js");
var Caml_primitive = require("./caml_primitive.js");
var Belt_internalAVLtree = require("./belt_internalAVLtree.js");

function add(t, x, data) {
  if (t !== null) {
    var k = t.key;
    if (x === k) {
      return Belt_internalAVLtree.updateValue(t, data);
    } else {
      var v = t.value;
      if (x < k) {
        return Belt_internalAVLtree.bal(add(t.left, x, data), k, v, t.right);
      } else {
        return Belt_internalAVLtree.bal(t.left, k, v, add(t.right, x, data));
      }
    }
  } else {
    return Belt_internalAVLtree.singleton(x, data);
  }
}

function get(_n, x) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      if (x === v) {
        return /* Some */[n.value];
      } else {
        _n = x < v ? n.left : n.right;
        continue ;
      }
    } else {
      return /* None */0;
    }
  };
}

function getUndefined(_n, x) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      if (x === v) {
        return n.value;
      } else {
        _n = x < v ? n.left : n.right;
        continue ;
      }
    } else {
      return undefined;
    }
  };
}

function getExn(_n, x) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      if (x === v) {
        return n.value;
      } else {
        _n = x < v ? n.left : n.right;
        continue ;
      }
    } else {
      throw new Error("getExn");
    }
  };
}

function getWithDefault(_n, x, def) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      if (x === v) {
        return n.value;
      } else {
        _n = x < v ? n.left : n.right;
        continue ;
      }
    } else {
      return def;
    }
  };
}

function has(_n, x) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      if (x === v) {
        return true;
      } else {
        _n = x < v ? n.left : n.right;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function remove(n, x) {
  if (n !== null) {
    var l = n.left;
    var v = n.key;
    var r = n.right;
    if (x === v) {
      if (l !== null) {
        if (r !== null) {
          var kr = [r.key];
          var vr = [r.value];
          var r$1 = Belt_internalAVLtree.removeMinAuxWithRef(r, kr, vr);
          return Belt_internalAVLtree.bal(l, kr[0], vr[0], r$1);
        } else {
          return l;
        }
      } else {
        return r;
      }
    } else if (x < v) {
      return Belt_internalAVLtree.bal(remove(l, x), v, n.value, r);
    } else {
      return Belt_internalAVLtree.bal(l, v, n.value, remove(r, x));
    }
  } else {
    return n;
  }
}

function splitAux(x, n) {
  var l = n.left;
  var v = n.key;
  var d = n.value;
  var r = n.right;
  if (x === v) {
    return /* tuple */[
            l,
            /* Some */[d],
            r
          ];
  } else if (x < v) {
    if (l !== null) {
      var match = splitAux(x, l);
      return /* tuple */[
              match[0],
              match[1],
              Belt_internalAVLtree.join(match[2], v, d, r)
            ];
    } else {
      return /* tuple */[
              Belt_internalAVLtree.empty,
              /* None */0,
              n
            ];
    }
  } else if (r !== null) {
    var match$1 = splitAux(x, r);
    return /* tuple */[
            Belt_internalAVLtree.join(l, v, d, match$1[0]),
            match$1[1],
            match$1[2]
          ];
  } else {
    return /* tuple */[
            n,
            /* None */0,
            Belt_internalAVLtree.empty
          ];
  }
}

function split(x, n) {
  if (n !== null) {
    return splitAux(x, n);
  } else {
    return /* tuple */[
            Belt_internalAVLtree.empty,
            /* None */0,
            Belt_internalAVLtree.empty
          ];
  }
}

function mergeU(s1, s2, f) {
  var exit = 0;
  if (s1 !== null) {
    if (s1.height >= (
        s2 !== null ? s2.height : 0
      )) {
      var l1 = s1.left;
      var v1 = s1.key;
      var d1 = s1.value;
      var r1 = s1.right;
      var match = split(v1, s2);
      return Belt_internalAVLtree.concatOrJoin(mergeU(l1, match[0], f), v1, f(v1, /* Some */[d1], match[1]), mergeU(r1, match[2], f));
    } else {
      exit = 1;
    }
  } else if (s2 !== null) {
    exit = 1;
  } else {
    return Belt_internalAVLtree.empty;
  }
  if (exit === 1) {
    if (s2 !== null) {
      var l2 = s2.left;
      var v2 = s2.key;
      var d2 = s2.value;
      var r2 = s2.right;
      var match$1 = split(v2, s1);
      return Belt_internalAVLtree.concatOrJoin(mergeU(match$1[0], l2, f), v2, f(v2, match$1[1], /* Some */[d2]), mergeU(match$1[2], r2, f));
    } else {
      return /* assert false */0;
    }
  }
  
}

function merge(s1, s2, f) {
  return mergeU(s1, s2, Curry.__3(f));
}

function compareAux(_e1, _e2, vcmp) {
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1 && e2) {
      var h2 = e2[0];
      var h1 = e1[0];
      var c = Caml_primitive.caml_int_compare(h1.key, h2.key);
      if (c === 0) {
        var cx = vcmp(h1.value, h2.value);
        if (cx === 0) {
          _e2 = Belt_internalAVLtree.stackAllLeft(h2.right, e2[1]);
          _e1 = Belt_internalAVLtree.stackAllLeft(h1.right, e1[1]);
          continue ;
        } else {
          return cx;
        }
      } else {
        return c;
      }
    } else {
      return 0;
    }
  };
}

function cmpU(s1, s2, cmp) {
  var len1 = Belt_internalAVLtree.size(s1);
  var len2 = Belt_internalAVLtree.size(s2);
  if (len1 === len2) {
    return compareAux(Belt_internalAVLtree.stackAllLeft(s1, /* [] */0), Belt_internalAVLtree.stackAllLeft(s2, /* [] */0), cmp);
  } else if (len1 < len2) {
    return -1;
  } else {
    return 1;
  }
}

function cmp(s1, s2, f) {
  return cmpU(s1, s2, Curry.__2(f));
}

function eqAux(_e1, _e2, eq) {
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1 && e2) {
      var h2 = e2[0];
      var h1 = e1[0];
      if (h1.key === h2.key && eq(h1.value, h2.value)) {
        _e2 = Belt_internalAVLtree.stackAllLeft(h2.right, e2[1]);
        _e1 = Belt_internalAVLtree.stackAllLeft(h1.right, e1[1]);
        continue ;
      } else {
        return false;
      }
    } else {
      return true;
    }
  };
}

function eqU(s1, s2, eq) {
  var len1 = Belt_internalAVLtree.size(s1);
  var len2 = Belt_internalAVLtree.size(s2);
  if (len1 === len2) {
    return eqAux(Belt_internalAVLtree.stackAllLeft(s1, /* [] */0), Belt_internalAVLtree.stackAllLeft(s2, /* [] */0), eq);
  } else {
    return false;
  }
}

function eq(s1, s2, f) {
  return eqU(s1, s2, Curry.__2(f));
}

function addMutate(t, x, data) {
  if (t !== null) {
    var k = t.key;
    if (x === k) {
      t.key = x;
      t.value = data;
      return t;
    } else {
      var l = t.left;
      var r = t.right;
      if (x < k) {
        var ll = addMutate(l, x, data);
        t.left = ll;
      } else {
        t.right = addMutate(r, x, data);
      }
      return Belt_internalAVLtree.balMutate(t);
    }
  } else {
    return Belt_internalAVLtree.singleton(x, data);
  }
}

function fromArray(xs) {
  var len = xs.length;
  if (len === 0) {
    return Belt_internalAVLtree.empty;
  } else {
    var next = Belt_SortArray.strictlySortedLengthU(xs, (function (param, param$1) {
            return param[0] < param$1[0];
          }));
    var result;
    if (next >= 0) {
      result = Belt_internalAVLtree.fromSortedArrayAux(xs, 0, next);
    } else {
      next = -next | 0;
      result = Belt_internalAVLtree.fromSortedArrayRevAux(xs, next - 1 | 0, next);
    }
    for(var i = next ,i_finish = len - 1 | 0; i <= i_finish; ++i){
      var match = xs[i];
      result = addMutate(result, match[0], match[1]);
    }
    return result;
  }
}

var N = 0;

var A = 0;

var S = 0;

exports.N = N;
exports.A = A;
exports.S = S;
exports.add = add;
exports.get = get;
exports.getUndefined = getUndefined;
exports.getExn = getExn;
exports.getWithDefault = getWithDefault;
exports.has = has;
exports.remove = remove;
exports.splitAux = splitAux;
exports.split = split;
exports.mergeU = mergeU;
exports.merge = merge;
exports.compareAux = compareAux;
exports.cmpU = cmpU;
exports.cmp = cmp;
exports.eqAux = eqAux;
exports.eqU = eqU;
exports.eq = eq;
exports.addMutate = addMutate;
exports.fromArray = fromArray;
/* No side effect */
