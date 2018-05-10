'use strict';

var Belt_SortArrayInt = require("./belt_SortArrayInt.js");
var Belt_internalAVLset = require("./belt_internalAVLset.js");

function has(_t, x) {
  while(true) {
    var t = _t;
    if (t !== null) {
      var v = t.value;
      if (x === v) {
        return true;
      } else {
        _t = x < v ? t.left : t.right;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function compareAux(_e1, _e2) {
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1 && e2) {
      var h2 = e2[0];
      var h1 = e1[0];
      var k1 = h1.value;
      var k2 = h2.value;
      if (k1 === k2) {
        _e2 = Belt_internalAVLset.stackAllLeft(h2.right, e2[1]);
        _e1 = Belt_internalAVLset.stackAllLeft(h1.right, e1[1]);
        continue ;
      } else if (k1 < k2) {
        return -1;
      } else {
        return 1;
      }
    } else {
      return 0;
    }
  };
}

function cmp(s1, s2) {
  var len1 = Belt_internalAVLset.size(s1);
  var len2 = Belt_internalAVLset.size(s2);
  if (len1 === len2) {
    return compareAux(Belt_internalAVLset.stackAllLeft(s1, /* [] */0), Belt_internalAVLset.stackAllLeft(s2, /* [] */0));
  } else if (len1 < len2) {
    return -1;
  } else {
    return 1;
  }
}

function eq(s1, s2) {
  return cmp(s1, s2) === 0;
}

function subset(_s1, _s2) {
  while(true) {
    var s2 = _s2;
    var s1 = _s1;
    if (s1 !== null) {
      if (s2 !== null) {
        var l1 = s1.left;
        var v1 = s1.value;
        var r1 = s1.right;
        var l2 = s2.left;
        var v2 = s2.value;
        var r2 = s2.right;
        if (v1 === v2) {
          if (subset(l1, l2)) {
            _s2 = r2;
            _s1 = r1;
            continue ;
          } else {
            return false;
          }
        } else if (v1 < v2) {
          if (subset(Belt_internalAVLset.create(l1, v1, Belt_internalAVLset.empty), l2)) {
            _s1 = r1;
            continue ;
          } else {
            return false;
          }
        } else if (subset(Belt_internalAVLset.create(Belt_internalAVLset.empty, v1, r1), r2)) {
          _s1 = l1;
          continue ;
        } else {
          return false;
        }
      } else {
        return false;
      }
    } else {
      return true;
    }
  };
}

function get(_n, x) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.value;
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

function getUndefined(_n, x) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.value;
      if (x === v) {
        return v;
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
      var v = n.value;
      if (x === v) {
        return v;
      } else {
        _n = x < v ? n.left : n.right;
        continue ;
      }
    } else {
      throw new Error("getExn");
    }
  };
}

function addMutate(t, x) {
  if (t !== null) {
    var k = t.value;
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
      return Belt_internalAVLset.balMutate(t);
    }
  } else {
    return Belt_internalAVLset.singleton(x);
  }
}

function fromArray(xs) {
  var len = xs.length;
  if (len === 0) {
    return Belt_internalAVLset.empty;
  } else {
    var next = Belt_SortArrayInt.strictlySortedLength(xs);
    var result;
    if (next >= 0) {
      result = Belt_internalAVLset.fromSortedArrayAux(xs, 0, next);
    } else {
      next = -next | 0;
      result = Belt_internalAVLset.fromSortedArrayRevAux(xs, next - 1 | 0, next);
    }
    for(var i = next ,i_finish = len - 1 | 0; i <= i_finish; ++i){
      result = addMutate(result, xs[i]);
    }
    return result;
  }
}

var S = 0;

var N = 0;

var A = 0;

exports.S = S;
exports.N = N;
exports.A = A;
exports.has = has;
exports.compareAux = compareAux;
exports.cmp = cmp;
exports.eq = eq;
exports.subset = subset;
exports.get = get;
exports.getUndefined = getUndefined;
exports.getExn = getExn;
exports.addMutate = addMutate;
exports.fromArray = fromArray;
/* No side effect */
