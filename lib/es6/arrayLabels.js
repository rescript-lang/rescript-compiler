

import * as Curry from "./curry.js";
import * as Caml_obj from "./caml_obj.js";
import * as Caml_array from "./caml_array.js";
import * as Caml_exceptions from "./caml_exceptions.js";
import * as Caml_js_exceptions from "./caml_js_exceptions.js";

var make_float = Caml_array.make_float;

var Floatarray = {};

function init(l, f) {
  if (l === 0) {
    return [];
  }
  if (l < 0) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Array.init",
          Error: new Error()
        };
  }
  var res = Caml_array.make(l, Curry._1(f, 0));
  for(var i = 1; i < l; ++i){
    res[i] = Curry._1(f, i);
  }
  return res;
}

function make_matrix(sx, sy, init) {
  var res = Caml_array.make(sx, []);
  for(var x = 0; x < sx; ++x){
    res[x] = Caml_array.make(sy, init);
  }
  return res;
}

function copy(a) {
  var l = a.length;
  if (l === 0) {
    return [];
  } else {
    return Caml_array.sub(a, 0, l);
  }
}

function append(a1, a2) {
  var l1 = a1.length;
  if (l1 === 0) {
    return copy(a2);
  } else if (a2.length === 0) {
    return Caml_array.sub(a1, 0, l1);
  } else {
    return a1.concat(a2);
  }
}

function sub(a, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (a.length - len | 0)) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Array.sub",
          Error: new Error()
        };
  }
  return Caml_array.sub(a, ofs, len);
}

function fill(a, ofs, len, v) {
  if (ofs < 0 || len < 0 || ofs > (a.length - len | 0)) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Array.fill",
          Error: new Error()
        };
  }
  for(var i = ofs ,i_finish = ofs + len | 0; i < i_finish; ++i){
    a[i] = v;
  }
}

function blit(a1, ofs1, a2, ofs2, len) {
  if (len < 0 || ofs1 < 0 || ofs1 > (a1.length - len | 0) || ofs2 < 0 || ofs2 > (a2.length - len | 0)) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Array.blit",
          Error: new Error()
        };
  }
  Caml_array.blit(a1, ofs1, a2, ofs2, len);
}

function iter(f, a) {
  for(var i = 0 ,i_finish = a.length; i < i_finish; ++i){
    Curry._1(f, a[i]);
  }
}

function iter2(f, a, b) {
  if (a.length !== b.length) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Array.iter2: arrays must have the same length",
          Error: new Error()
        };
  }
  for(var i = 0 ,i_finish = a.length; i < i_finish; ++i){
    Curry._2(f, a[i], b[i]);
  }
}

function map(f, a) {
  var l = a.length;
  if (l === 0) {
    return [];
  }
  var r = Caml_array.make(l, Curry._1(f, a[0]));
  for(var i = 1; i < l; ++i){
    r[i] = Curry._1(f, a[i]);
  }
  return r;
}

function map2(f, a, b) {
  var la = a.length;
  var lb = b.length;
  if (la !== lb) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Array.map2: arrays must have the same length",
          Error: new Error()
        };
  }
  if (la === 0) {
    return [];
  }
  var r = Caml_array.make(la, Curry._2(f, a[0], b[0]));
  for(var i = 1; i < la; ++i){
    r[i] = Curry._2(f, a[i], b[i]);
  }
  return r;
}

function iteri(f, a) {
  for(var i = 0 ,i_finish = a.length; i < i_finish; ++i){
    Curry._2(f, i, a[i]);
  }
}

function mapi(f, a) {
  var l = a.length;
  if (l === 0) {
    return [];
  }
  var r = Caml_array.make(l, Curry._2(f, 0, a[0]));
  for(var i = 1; i < l; ++i){
    r[i] = Curry._2(f, i, a[i]);
  }
  return r;
}

function to_list(a) {
  var _i = a.length - 1 | 0;
  var _res = /* [] */0;
  while(true) {
    var res = _res;
    var i = _i;
    if (i < 0) {
      return res;
    }
    _res = {
      hd: a[i],
      tl: res
    };
    _i = i - 1 | 0;
    continue ;
  };
}

function list_length(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (!param) {
      return accu;
    }
    _param = param.tl;
    _accu = accu + 1 | 0;
    continue ;
  };
}

function of_list(param) {
  if (!param) {
    return [];
  }
  var a = Caml_array.make(list_length(0, param), param.hd);
  var _i = 1;
  var _param = param.tl;
  while(true) {
    var param$1 = _param;
    var i = _i;
    if (!param$1) {
      return a;
    }
    a[i] = param$1.hd;
    _param = param$1.tl;
    _i = i + 1 | 0;
    continue ;
  };
}

function fold_left(f, x, a) {
  var r = x;
  for(var i = 0 ,i_finish = a.length; i < i_finish; ++i){
    r = Curry._2(f, r, a[i]);
  }
  return r;
}

function fold_right(f, a, x) {
  var r = x;
  for(var i = a.length - 1 | 0; i >= 0; --i){
    r = Curry._2(f, a[i], r);
  }
  return r;
}

function exists(p, a) {
  var n = a.length;
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === n) {
      return false;
    }
    if (Curry._1(p, a[i])) {
      return true;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function for_all(p, a) {
  var n = a.length;
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === n) {
      return true;
    }
    if (!Curry._1(p, a[i])) {
      return false;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function mem(x, a) {
  var n = a.length;
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === n) {
      return false;
    }
    if (Caml_obj.equal(a[i], x)) {
      return true;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function memq(x, a) {
  var n = a.length;
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === n) {
      return false;
    }
    if (x === a[i]) {
      return true;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

var Bottom = /* @__PURE__ */Caml_exceptions.create("ArrayLabels.Bottom");

function sort(cmp, a) {
  var maxson = function (l, i) {
    var i31 = ((i + i | 0) + i | 0) + 1 | 0;
    var x = i31;
    if ((i31 + 2 | 0) < l) {
      if (Curry._2(cmp, Caml_array.get(a, i31), Caml_array.get(a, i31 + 1 | 0)) < 0) {
        x = i31 + 1 | 0;
      }
      if (Curry._2(cmp, Caml_array.get(a, x), Caml_array.get(a, i31 + 2 | 0)) < 0) {
        x = i31 + 2 | 0;
      }
      return x;
    }
    if ((i31 + 1 | 0) < l && Curry._2(cmp, Caml_array.get(a, i31), Caml_array.get(a, i31 + 1 | 0)) < 0) {
      return i31 + 1 | 0;
    }
    if (i31 < l) {
      return i31;
    }
    throw {
          RE_EXN_ID: Bottom,
          _1: i,
          Error: new Error()
        };
  };
  var trickle = function (l, i, e) {
    try {
      var _i = i;
      while(true) {
        var i$1 = _i;
        var j = maxson(l, i$1);
        if (Curry._2(cmp, Caml_array.get(a, j), e) <= 0) {
          return Caml_array.set(a, i$1, e);
        }
        Caml_array.set(a, i$1, Caml_array.get(a, j));
        _i = j;
        continue ;
      };
    }
    catch (raw_i){
      var i$2 = Caml_js_exceptions.internalToOCamlException(raw_i);
      if (i$2.RE_EXN_ID === Bottom) {
        return Caml_array.set(a, i$2._1, e);
      }
      throw i$2;
    }
  };
  var bubble = function (l, i) {
    try {
      var _i = i;
      while(true) {
        var i$1 = _i;
        var j = maxson(l, i$1);
        Caml_array.set(a, i$1, Caml_array.get(a, j));
        _i = j;
        continue ;
      };
    }
    catch (raw_i){
      var i$2 = Caml_js_exceptions.internalToOCamlException(raw_i);
      if (i$2.RE_EXN_ID === Bottom) {
        return i$2._1;
      }
      throw i$2;
    }
  };
  var trickleup = function (_i, e) {
    while(true) {
      var i = _i;
      var father = (i - 1 | 0) / 3 | 0;
      if (i === father) {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "arrayLabels.res",
                321,
                4
              ],
              Error: new Error()
            };
      }
      if (Curry._2(cmp, Caml_array.get(a, father), e) >= 0) {
        return Caml_array.set(a, i, e);
      }
      Caml_array.set(a, i, Caml_array.get(a, father));
      if (father <= 0) {
        return Caml_array.set(a, 0, e);
      }
      _i = father;
      continue ;
    };
  };
  var l = a.length;
  for(var i = ((l + 1 | 0) / 3 | 0) - 1 | 0; i >= 0; --i){
    trickle(l, i, Caml_array.get(a, i));
  }
  for(var i$1 = l - 1 | 0; i$1 >= 2; --i$1){
    var e = Caml_array.get(a, i$1);
    Caml_array.set(a, i$1, Caml_array.get(a, 0));
    trickleup(bubble(i$1, 0), e);
  }
  if (l <= 1) {
    return ;
  }
  var e$1 = Caml_array.get(a, 1);
  Caml_array.set(a, 1, Caml_array.get(a, 0));
  Caml_array.set(a, 0, e$1);
}

function stable_sort(cmp, a) {
  var merge = function (src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) {
    var src1r = src1ofs + src1len | 0;
    var src2r = src2ofs + src2len | 0;
    var _i1 = src1ofs;
    var _s1 = Caml_array.get(a, src1ofs);
    var _i2 = src2ofs;
    var _s2 = Caml_array.get(src2, src2ofs);
    var _d = dstofs;
    while(true) {
      var d = _d;
      var s2 = _s2;
      var i2 = _i2;
      var s1 = _s1;
      var i1 = _i1;
      if (Curry._2(cmp, s1, s2) <= 0) {
        Caml_array.set(dst, d, s1);
        var i1$1 = i1 + 1 | 0;
        if (i1$1 >= src1r) {
          return blit(src2, i2, dst, d + 1 | 0, src2r - i2 | 0);
        }
        _d = d + 1 | 0;
        _s1 = Caml_array.get(a, i1$1);
        _i1 = i1$1;
        continue ;
      }
      Caml_array.set(dst, d, s2);
      var i2$1 = i2 + 1 | 0;
      if (i2$1 >= src2r) {
        return blit(a, i1, dst, d + 1 | 0, src1r - i1 | 0);
      }
      _d = d + 1 | 0;
      _s2 = Caml_array.get(src2, i2$1);
      _i2 = i2$1;
      continue ;
    };
  };
  var isortto = function (srcofs, dst, dstofs, len) {
    for(var i = 0; i < len; ++i){
      var e = Caml_array.get(a, srcofs + i | 0);
      var j = (dstofs + i | 0) - 1 | 0;
      while(j >= dstofs && Curry._2(cmp, Caml_array.get(dst, j), e) > 0) {
        Caml_array.set(dst, j + 1 | 0, Caml_array.get(dst, j));
        j = j - 1 | 0;
      };
      Caml_array.set(dst, j + 1 | 0, e);
    }
  };
  var sortto = function (srcofs, dst, dstofs, len) {
    if (len <= 5) {
      return isortto(srcofs, dst, dstofs, len);
    }
    var l1 = len / 2 | 0;
    var l2 = len - l1 | 0;
    sortto(srcofs + l1 | 0, dst, dstofs + l1 | 0, l2);
    sortto(srcofs, a, srcofs + l2 | 0, l1);
    merge(srcofs + l2 | 0, l1, dst, dstofs + l1 | 0, l2, dst, dstofs);
  };
  var l = a.length;
  if (l <= 5) {
    return isortto(0, a, 0, l);
  }
  var l1 = l / 2 | 0;
  var l2 = l - l1 | 0;
  var t = Caml_array.make(l2, Caml_array.get(a, 0));
  sortto(l1, t, 0, l2);
  sortto(0, a, l2, l1);
  merge(l2, l1, t, 0, l2, a, 0);
}

var create_matrix = make_matrix;

var concat = Caml_array.concat;

var fast_sort = stable_sort;

export {
  init ,
  make_matrix ,
  create_matrix ,
  append ,
  concat ,
  sub ,
  copy ,
  fill ,
  blit ,
  to_list ,
  of_list ,
  iter ,
  map ,
  iteri ,
  mapi ,
  fold_left ,
  fold_right ,
  iter2 ,
  map2 ,
  exists ,
  for_all ,
  mem ,
  memq ,
  make_float ,
  sort ,
  stable_sort ,
  fast_sort ,
  Floatarray ,
}
/* No side effect */
