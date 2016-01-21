// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Pervasives      = require("./pervasives");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Caml_array      = require("../runtime/caml_array");

function init(l, f) {
  if (l) {
    if (l < 0) {
      return Pervasives.invalid_arg("Array.init");
    }
    else {
      var res = Caml_array.caml_make_vect(l, f(0));
      for(var i = 1 ,i_finish = -1 + l; i<= i_finish; ++i){
        res[i] = f(i);
      }
      return res;
    }
  }
  else {
    return /* array */[];
  }
}

function make_matrix(sx, sy, init) {
  var res = Caml_array.caml_make_vect(sx, /* array */[]);
  for(var x = 0 ,x_finish = -1 + sx; x<= x_finish; ++x){
    res[x] = Caml_array.caml_make_vect(sy, init);
  }
  return res;
}

function copy(a) {
  var l = a.length;
  if (l) {
    return Caml_array.caml_array_sub(a, 0, l);
  }
  else {
    return /* array */[];
  }
}

function append(a1, a2) {
  var l1 = a1.length;
  if (l1) {
    if (a2.length) {
      return a1.concat(a2);
    }
    else {
      return Caml_array.caml_array_sub(a1, 0, l1);
    }
  }
  else {
    return copy(a2);
  }
}

function sub(a, ofs, len) {
  if (len < 0 || ofs > a.length - len) {
    return Pervasives.invalid_arg("Array.sub");
  }
  else {
    return Caml_array.caml_array_sub(a, ofs, len);
  }
}

function fill(a, ofs, len, v) {
  if (ofs < 0 || len < 0 || ofs > a.length - len) {
    return Pervasives.invalid_arg("Array.fill");
  }
  else {
    for(var i = ofs ,i_finish = ofs + len - 1; i<= i_finish; ++i){
      a[i] = v;
    }
    return /* () */0;
  }
}

function blit(a1, ofs1, a2, ofs2, len) {
  if (len < 0 || ofs1 < 0 || ofs1 > a1.length - len || ofs2 < 0 || ofs2 > a2.length - len) {
    return Pervasives.invalid_arg("Array.blit");
  }
  else {
    return Caml_array.caml_array_blit(a1, ofs1, a2, ofs2, len);
  }
}

function iter(f, a) {
  for(var i = 0 ,i_finish = a.length - 1; i<= i_finish; ++i){
    f(a[i]);
  }
  return /* () */0;
}

function map(f, a) {
  var l = a.length;
  if (l) {
    var r = Caml_array.caml_make_vect(l, f(a[0]));
    for(var i = 1 ,i_finish = l - 1; i<= i_finish; ++i){
      r[i] = f(a[i]);
    }
    return r;
  }
  else {
    return /* array */[];
  }
}

function iteri(f, a) {
  for(var i = 0 ,i_finish = a.length - 1; i<= i_finish; ++i){
    f(i, a[i]);
  }
  return /* () */0;
}

function mapi(f, a) {
  var l = a.length;
  if (l) {
    var r = Caml_array.caml_make_vect(l, f(0, a[0]));
    for(var i = 1 ,i_finish = l - 1; i<= i_finish; ++i){
      r[i] = f(i, a[i]);
    }
    return r;
  }
  else {
    return /* array */[];
  }
}

function to_list(a) {
  var _i = a.length - 1;
  var _res = /* [] */0;
  while(true) {
    var res = _res;
    var i = _i;
    if (i < 0) {
      return res;
    }
    else {
      _res = [
        /* :: */0,
        a[i],
        res
      ];
      _i = i - 1;
    }
  };
}

function list_length(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[2];
      _accu = 1 + accu;
    }
    else {
      return accu;
    }
  };
}

function of_list(l) {
  if (l) {
    var a = Caml_array.caml_make_vect(list_length(0, l), l[1]);
    var _i = 1;
    var _param = l[2];
    while(true) {
      var param = _param;
      var i = _i;
      if (param) {
        a[i] = param[1];
        _param = param[2];
        _i = i + 1;
      }
      else {
        return a;
      }
    };
  }
  else {
    return /* array */[];
  }
}

function fold_left(f, x, a) {
  var r = x;
  for(var i = 0 ,i_finish = a.length - 1; i<= i_finish; ++i){
    r = f(r, a[i]);
  }
  return r;
}

function fold_right(f, a, x) {
  var r = x;
  for(var i = a.length - 1; i>= 0; --i){
    r = f(a[i], r);
  }
  return r;
}

var Bottom = [
  248,
  "Array.Bottom",
  ++ Caml_exceptions.caml_oo_last_id
];

function sort(cmp, a) {
  var maxson = function (l, i) {
    var i31 = i + i + i + 1;
    var x = i31;
    if (i31 + 2 < l) {
      if (cmp(a[i31], a[i31 + 1]) < 0) {
        x = i31 + 1;
      }
      if (cmp(a[x], a[i31 + 2]) < 0) {
        x = i31 + 2;
      }
      return x;
    }
    else {
      if (i31 + 1 < l && cmp(a[i31], a[i31 + 1]) < 0) {
        return i31 + 1;
      }
      else {
        if (i31 < l) {
          return i31;
        }
        else {
          throw [
                0,
                Bottom,
                i
              ];
        }
      }
    }
  };
  var trickle = function (l, i, e) {
    try {
      var l$1 = l;
      var _i = i;
      var e$1 = e;
      while(true) {
        var i$1 = _i;
        var j = maxson(l$1, i$1);
        if (cmp(a[j], e$1) > 0) {
          a[i$1] = a[j];
          _i = j;
        }
        else {
          a[i$1] = e$1;
          return /* () */0;
        }
      };
    }
    catch (exn){
      if (exn[1] === Bottom) {
        a[exn[2]] = e;
        return /* () */0;
      }
      else {
        throw exn;
      }
    }
  };
  var bubble = function (l, i) {
    try {
      var l$1 = l;
      var _i = i;
      while(true) {
        var i$1 = _i;
        var j = maxson(l$1, i$1);
        a[i$1] = a[j];
        _i = j;
      };
    }
    catch (exn){
      if (exn[1] === Bottom) {
        return exn[2];
      }
      else {
        throw exn;
      }
    }
  };
  var trickleup = function (_i, e) {
    while(true) {
      var i = _i;
      var father = (i - 1) / 3 | 0;
      if (i === father) {
        throw [
              0,
              Caml_exceptions.Assert_failure,
              [
                0,
                "array.ml",
                168,
                4
              ]
            ];
      }
      if (cmp(a[father], e) < 0) {
        a[i] = a[father];
        if (father > 0) {
          _i = father;
        }
        else {
          a[0] = e;
          return /* () */0;
        }
      }
      else {
        a[i] = e;
        return /* () */0;
      }
    };
  };
  var l = a.length;
  for(var i = ((l + 1) / 3 | 0) - 1; i>= 0; --i){
    trickle(l, i, a[i]);
  }
  for(var i$1 = l - 1; i$1>= 2; --i$1){
    var e = a[i$1];
    a[i$1] = a[0];
    trickleup(bubble(i$1, 0), e);
  }
  if (l > 1) {
    var e$1 = a[1];
    a[1] = a[0];
    a[0] = e$1;
    return /* () */0;
  }
  else {
    return 0;
  }
}

var cutoff = 5;

function stable_sort(cmp, a) {
  var merge = function (src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) {
    var src1r = src1ofs + src1len;
    var src2r = src2ofs + src2len;
    var _i1 = src1ofs;
    var _s1 = a[src1ofs];
    var _i2 = src2ofs;
    var _s2 = src2[src2ofs];
    var _d = dstofs;
    while(true) {
      var d = _d;
      var s2 = _s2;
      var i2 = _i2;
      var s1 = _s1;
      var i1 = _i1;
      if (cmp(s1, s2) <= 0) {
        dst[d] = s1;
        var i1$1 = i1 + 1;
        if (i1$1 < src1r) {
          _d = d + 1;
          _s1 = a[i1$1];
          _i1 = i1$1;
        }
        else {
          return blit(src2, i2, dst, d + 1, src2r - i2);
        }
      }
      else {
        dst[d] = s2;
        var i2$1 = i2 + 1;
        if (i2$1 < src2r) {
          _d = d + 1;
          _s2 = src2[i2$1];
          _i2 = i2$1;
        }
        else {
          return blit(a, i1, dst, d + 1, src1r - i1);
        }
      }
    };
  };
  var isortto = function (srcofs, dst, dstofs, len) {
    for(var i = 0 ,i_finish = len - 1; i<= i_finish; ++i){
      var e = a[srcofs + i];
      var j = dstofs + i - 1;
      while(j >= dstofs && cmp(dst[j], e) > 0) {
        dst[j + 1] = dst[j];
        -- j;
      };
      dst[j + 1] = e;
    }
    return /* () */0;
  };
  var sortto = function (srcofs, dst, dstofs, len) {
    if (len <= cutoff) {
      return isortto(srcofs, dst, dstofs, len);
    }
    else {
      var l1 = len / 2 | 0;
      var l2 = len - l1;
      sortto(srcofs + l1, dst, dstofs + l1, l2);
      sortto(srcofs, a, srcofs + l2, l1);
      return merge(srcofs + l2, l1, dst, dstofs + l1, l2, dst, dstofs);
    }
  };
  var l = a.length;
  if (l <= cutoff) {
    return isortto(0, a, 0, l);
  }
  else {
    var l1 = l / 2 | 0;
    var l2 = l - l1;
    var t = Caml_array.caml_make_vect(l2, a[0]);
    sortto(l1, t, 0, l2);
    sortto(0, a, l2, l1);
    return merge(l2, l1, t, 0, l2, a, 0);
  }
}

var create_matrix = make_matrix;

function concat(prim) {
  return Caml_array.caml_array_concat(prim);
}

var fast_sort = stable_sort;

exports.init          = init;
exports.make_matrix   = make_matrix;
exports.create_matrix = create_matrix;
exports.append        = append;
exports.concat        = concat;
exports.sub           = sub;
exports.copy          = copy;
exports.fill          = fill;
exports.blit          = blit;
exports.to_list       = to_list;
exports.of_list       = of_list;
exports.iter          = iter;
exports.map           = map;
exports.iteri         = iteri;
exports.mapi          = mapi;
exports.fold_left     = fold_left;
exports.fold_right    = fold_right;
exports.sort          = sort;
exports.stable_sort   = stable_sort;
exports.fast_sort     = fast_sort;
/* No side effect */
