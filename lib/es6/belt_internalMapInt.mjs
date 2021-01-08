

import * as Curry from "./curry.mjs";
import * as Caml_option from "./caml_option.mjs";
import * as Belt_SortArray from "./belt_SortArray.mjs";
import * as Caml_primitive from "./caml_primitive.mjs";
import * as Belt_internalAVLtree from "./belt_internalAVLtree.mjs";

function add(t, x, data) {
  if (t === undefined) {
    return Belt_internalAVLtree.singleton(x, data);
  }
  var k = t.k;
  if (x === k) {
    return Belt_internalAVLtree.updateValue(t, data);
  }
  var v = t.v;
  if (x < k) {
    return Belt_internalAVLtree.bal(add(t.l, x, data), k, v, t.r);
  } else {
    return Belt_internalAVLtree.bal(t.l, k, v, add(t.r, x, data));
  }
}

function get(_n, x) {
  while(true) {
    var n = _n;
    if (n === undefined) {
      return ;
    }
    var v = n.k;
    if (x === v) {
      return Caml_option.some(n.v);
    }
    _n = x < v ? n.l : n.r;
    continue ;
  };
}

function getUndefined(_n, x) {
  while(true) {
    var n = _n;
    if (n === undefined) {
      return ;
    }
    var v = n.k;
    if (x === v) {
      return n.v;
    }
    _n = x < v ? n.l : n.r;
    continue ;
  };
}

function getExn(_n, x) {
  while(true) {
    var n = _n;
    if (n !== undefined) {
      var v = n.k;
      if (x === v) {
        return n.v;
      }
      _n = x < v ? n.l : n.r;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function getWithDefault(_n, x, def) {
  while(true) {
    var n = _n;
    if (n === undefined) {
      return def;
    }
    var v = n.k;
    if (x === v) {
      return n.v;
    }
    _n = x < v ? n.l : n.r;
    continue ;
  };
}

function has(_n, x) {
  while(true) {
    var n = _n;
    if (n === undefined) {
      return false;
    }
    var v = n.k;
    if (x === v) {
      return true;
    }
    _n = x < v ? n.l : n.r;
    continue ;
  };
}

function remove(n, x) {
  if (n === undefined) {
    return n;
  }
  var v = n.k;
  var l = n.l;
  var r = n.r;
  if (x !== v) {
    if (x < v) {
      return Belt_internalAVLtree.bal(remove(l, x), v, n.v, r);
    } else {
      return Belt_internalAVLtree.bal(l, v, n.v, remove(r, x));
    }
  }
  if (l === undefined) {
    return r;
  }
  if (r === undefined) {
    return l;
  }
  var kr = {
    contents: r.k
  };
  var vr = {
    contents: r.v
  };
  var r$1 = Belt_internalAVLtree.removeMinAuxWithRef(r, kr, vr);
  return Belt_internalAVLtree.bal(l, kr.contents, vr.contents, r$1);
}

function splitAux(x, n) {
  var v = n.k;
  var d = n.v;
  var l = n.l;
  var r = n.r;
  if (x === v) {
    return [
            l,
            Caml_option.some(d),
            r
          ];
  }
  if (x < v) {
    if (l === undefined) {
      return [
              undefined,
              undefined,
              n
            ];
    }
    var match = splitAux(x, l);
    return [
            match[0],
            match[1],
            Belt_internalAVLtree.join(match[2], v, d, r)
          ];
  }
  if (r === undefined) {
    return [
            n,
            undefined,
            undefined
          ];
  }
  var match$1 = splitAux(x, r);
  return [
          Belt_internalAVLtree.join(l, v, d, match$1[0]),
          match$1[1],
          match$1[2]
        ];
}

function split(x, n) {
  if (n !== undefined) {
    return splitAux(x, n);
  } else {
    return [
            undefined,
            undefined,
            undefined
          ];
  }
}

function mergeU(s1, s2, f) {
  if (s1 !== undefined) {
    if (s1.h >= (
        s2 !== undefined ? s2.h : 0
      )) {
      var v1 = s1.k;
      var d1 = s1.v;
      var l1 = s1.l;
      var r1 = s1.r;
      var match = split(v1, s2);
      return Belt_internalAVLtree.concatOrJoin(mergeU(l1, match[0], f), v1, f(v1, Caml_option.some(d1), match[1]), mergeU(r1, match[2], f));
    }
    
  } else if (s2 === undefined) {
    return ;
  }
  var v2 = s2.k;
  var d2 = s2.v;
  var l2 = s2.l;
  var r2 = s2.r;
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
    var h2 = e2.hd;
    var h1 = e1.hd;
    var c = Caml_primitive.caml_int_compare(h1.k, h2.k);
    if (c !== 0) {
      return c;
    }
    var cx = vcmp(h1.v, h2.v);
    if (cx !== 0) {
      return cx;
    }
    _e2 = Belt_internalAVLtree.stackAllLeft(h2.r, e2.tl);
    _e1 = Belt_internalAVLtree.stackAllLeft(h1.r, e1.tl);
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
    var h2 = e2.hd;
    var h1 = e1.hd;
    if (!(h1.k === h2.k && eq(h1.v, h2.v))) {
      return false;
    }
    _e2 = Belt_internalAVLtree.stackAllLeft(h2.r, e2.tl);
    _e1 = Belt_internalAVLtree.stackAllLeft(h1.r, e1.tl);
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
  if (t === undefined) {
    return Belt_internalAVLtree.singleton(x, data);
  }
  var k = t.k;
  if (x === k) {
    t.k = x;
    t.v = data;
    return t;
  }
  var l = t.l;
  var r = t.r;
  if (x < k) {
    var ll = addMutate(l, x, data);
    t.l = ll;
  } else {
    t.r = addMutate(r, x, data);
  }
  return Belt_internalAVLtree.balMutate(t);
}

function fromArray(xs) {
  var len = xs.length;
  if (len === 0) {
    return ;
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
  for(var i = next; i < len; ++i){
    var match = xs[i];
    result = addMutate(result, match[0], match[1]);
  }
  return result;
}

var N;

var A;

var S;

export {
  N ,
  A ,
  S ,
  add ,
  get ,
  getUndefined ,
  getExn ,
  getWithDefault ,
  has ,
  remove ,
  splitAux ,
  split ,
  mergeU ,
  merge ,
  compareAux ,
  cmpU ,
  cmp ,
  eqAux ,
  eqU ,
  eq ,
  addMutate ,
  fromArray ,
  
}
/* No side effect */
