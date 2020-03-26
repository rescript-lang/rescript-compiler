'use strict';

var Curry = require("./curry.js");
var Caml_option = require("./caml_option.js");
var Belt_SortArray = require("./belt_SortArray.js");
var Caml_primitive = require("./caml_primitive.js");
var Belt_internalAVLtree = require("./belt_internalAVLtree.js");

function add(t, x, data) {
  if (t === null) {
    return Belt_internalAVLtree.singleton(x, data);
  }
  var k = t.key;
  if (x === k) {
    return Belt_internalAVLtree.updateValue(t, data);
  }
  var v = t.value;
  if (x < k) {
    return Belt_internalAVLtree.bal(add(t.left, x, data), k, v, t.right);
  } else {
    return Belt_internalAVLtree.bal(t.left, k, v, add(t.right, x, data));
  }
}

function get(_n, x) {
  while(true) {
    var n = _n;
    if (n === null) {
      return ;
    }
    var v = n.key;
    if (x === v) {
      return Caml_option.some(n.value);
    }
    _n = x < v ? n.left : n.right;
    continue ;
  };
}

function getUndefined(_n, x) {
  while(true) {
    var n = _n;
    if (n === null) {
      return ;
    }
    var v = n.key;
    if (x === v) {
      return n.value;
    }
    _n = x < v ? n.left : n.right;
    continue ;
  };
}

function getExn(_n, x) {
  while(true) {
    var n = _n;
    if (n !== null) {
      var v = n.key;
      if (x === v) {
        return n.value;
      }
      _n = x < v ? n.left : n.right;
      continue ;
    }
    throw new Error("getExn");
  };
}

function getWithDefault(_n, x, def) {
  while(true) {
    var n = _n;
    if (n === null) {
      return def;
    }
    var v = n.key;
    if (x === v) {
      return n.value;
    }
    _n = x < v ? n.left : n.right;
    continue ;
  };
}

function has(_n, x) {
  while(true) {
    var n = _n;
    if (n === null) {
      return false;
    }
    var v = n.key;
    if (x === v) {
      return true;
    }
    _n = x < v ? n.left : n.right;
    continue ;
  };
}

function remove(n, x) {
  if (n === null) {
    return n;
  }
  var l = n.left;
  var v = n.key;
  var r = n.right;
  if (x !== v) {
    if (x < v) {
      return Belt_internalAVLtree.bal(remove(l, x), v, n.value, r);
    } else {
      return Belt_internalAVLtree.bal(l, v, n.value, remove(r, x));
    }
  }
  if (l === null) {
    return r;
  }
  if (r === null) {
    return l;
  }
  var kr = {
    contents: r.key
  };
  var vr = {
    contents: r.value
  };
  var r$1 = Belt_internalAVLtree.removeMinAuxWithRef(r, kr, vr);
  return Belt_internalAVLtree.bal(l, kr.contents, vr.contents, r$1);
}

function splitAux(x, n) {
  var l = n.left;
  var v = n.key;
  var d = n.value;
  var r = n.right;
  if (x === v) {
    return /* tuple */[
            l,
            Caml_option.some(d),
            r
          ];
  }
  if (x < v) {
    if (l === null) {
      return /* tuple */[
              null,
              undefined,
              n
            ];
    }
    var match = splitAux(x, l);
    return /* tuple */[
            match[0],
            match[1],
            Belt_internalAVLtree.join(match[2], v, d, r)
          ];
  }
  if (r === null) {
    return /* tuple */[
            n,
            undefined,
            null
          ];
  }
  var match$1 = splitAux(x, r);
  return /* tuple */[
          Belt_internalAVLtree.join(l, v, d, match$1[0]),
          match$1[1],
          match$1[2]
        ];
}

function split(x, n) {
  if (n !== null) {
    return splitAux(x, n);
  } else {
    return /* tuple */[
            null,
            undefined,
            null
          ];
  }
}

function mergeU(s1, s2, f) {
  if (s1 !== null) {
    if (s1.height >= (
        s2 !== null ? s2.height : 0
      )) {
      var l1 = s1.left;
      var v1 = s1.key;
      var d1 = s1.value;
      var r1 = s1.right;
      var match = split(v1, s2);
      return Belt_internalAVLtree.concatOrJoin(mergeU(l1, match[0], f), v1, f(v1, Caml_option.some(d1), match[1]), mergeU(r1, match[2], f));
    }
    
  } else if (s2 === null) {
    return null;
  }
  if (s2 === null) {
    return /* assert false */0;
  }
  var l2 = s2.left;
  var v2 = s2.key;
  var d2 = s2.value;
  var r2 = s2.right;
  var match$1 = split(v2, s1);
  return Belt_internalAVLtree.concatOrJoin(mergeU(match$1[0], l2, f), v2, f(v2, match$1[1], Caml_option.some(d2)), mergeU(match$1[2], r2, f));
}

function merge(s1, s2, f) {
  return mergeU(s1, s2, Curry.__3(f));
}

function compareAux(_e1, _e2, vcmp) {
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (!e1) {
      return 0;
    }
    if (!e2) {
      return 0;
    }
    var h2 = e2[0];
    var h1 = e1[0];
    var c = Caml_primitive.caml_string_compare(h1.key, h2.key);
    if (c !== 0) {
      return c;
    }
    var cx = vcmp(h1.value, h2.value);
    if (cx !== 0) {
      return cx;
    }
    _e2 = Belt_internalAVLtree.stackAllLeft(h2.right, e2[1]);
    _e1 = Belt_internalAVLtree.stackAllLeft(h1.right, e1[1]);
    continue ;
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
    if (!e1) {
      return true;
    }
    if (!e2) {
      return true;
    }
    var h2 = e2[0];
    var h1 = e1[0];
    if (!(h1.key === h2.key && eq(h1.value, h2.value))) {
      return false;
    }
    _e2 = Belt_internalAVLtree.stackAllLeft(h2.right, e2[1]);
    _e1 = Belt_internalAVLtree.stackAllLeft(h1.right, e1[1]);
    continue ;
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
  if (t === null) {
    return Belt_internalAVLtree.singleton(x, data);
  }
  var k = t.key;
  if (x === k) {
    t.key = x;
    t.value = data;
    return t;
  }
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

function fromArray(xs) {
  var len = xs.length;
  if (len === 0) {
    return null;
  }
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

var N = /* alias */0;

var A = /* alias */0;

var S = /* alias */0;

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
