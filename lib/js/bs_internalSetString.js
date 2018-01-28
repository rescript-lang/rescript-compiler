'use strict';

var Bs_SortString = require("./bs_SortString.js");
var Bs_internalAVLset = require("./bs_internalAVLset.js");

function has(_t, x) {
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
  var len1 = Bs_internalAVLset.size(s1);
  var len2 = Bs_internalAVLset.size(s2);
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
          if (subset(Bs_internalAVLset.create(l1, v1, Bs_internalAVLset.empty), l2)) {
            _s1 = r1;
            continue ;
            
          } else {
            return /* false */0;
          }
        } else if (subset(Bs_internalAVLset.create(Bs_internalAVLset.empty, v1, r1), r2)) {
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

function get(_n, x) {
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

function getUndefined(_n, x) {
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
    return Bs_internalAVLset.singleton(x);
  }
}

function ofArray(xs) {
  var len = xs.length;
  if (len) {
    var next = Bs_SortString.strictlySortedLength(xs);
    var result;
    if (next >= 0) {
      result = Bs_internalAVLset.ofSortedArrayAux(xs, 0, next);
    } else {
      next = -next | 0;
      result = Bs_internalAVLset.ofSortedArrayRevAux(xs, next - 1 | 0, next);
    }
    for(var i = next ,i_finish = len - 1 | 0; i <= i_finish; ++i){
      result = addMutate(result, xs[i]);
    }
    return result;
  } else {
    return Bs_internalAVLset.empty;
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
exports.ofArray = ofArray;
/* No side effect */
