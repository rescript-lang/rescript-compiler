'use strict';

var Js_math = require("./js_math.js");
var Caml_array = require("./caml_array.js");

function init(l, f) {
  if (l < 0) {
    throw new Error("File \"bs_Array.ml\", line 35, characters 4-10");
  }
  var res = new Array(l);
  for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
    res[i] = f(i);
  }
  return res;
}

function swapUnsafe(xs, i, j) {
  var tmp = xs[i];
  xs[i] = xs[j];
  xs[j] = tmp;
  return /* () */0;
}

function shuffleInPlace(xs) {
  var len = xs.length;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    swapUnsafe(xs, i, Js_math.random_int(i, len));
  }
  return /* () */0;
}

function makeMatrix(sx, sy, init) {
  if (!(sx >= 0 && sy >= 0)) {
    throw new Error("File \"bs_Array.ml\", line 55, characters 4-10");
  }
  var res = new Array(sx);
  for(var x = 0 ,x_finish = sx - 1 | 0; x <= x_finish; ++x){
    var initY = new Array(sy);
    for(var y = 0 ,y_finish = sy - 1 | 0; y <= y_finish; ++y){
      initY[y] = init;
    }
    res[x] = initY;
  }
  return res;
}

function copy(a) {
  var l = a.length;
  var v = new Array(l);
  for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
    v[i] = a[i];
  }
  return v;
}

function zip(xs, ys) {
  var lenx = xs.length;
  var leny = ys.length;
  var len = lenx < leny ? lenx : leny;
  var s = new Array(len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    s[i] = /* tuple */[
      xs[i],
      ys[i]
    ];
  }
  return s;
}

function append(a1, a2) {
  var l1 = a1.length;
  if (l1) {
    if (a2.length) {
      return a1.concat(a2);
    } else {
      return Caml_array.caml_array_sub(a1, 0, l1);
    }
  } else {
    return copy(a2);
  }
}

function sub(a, ofs, len) {
  if (len < 0 || ofs > (a.length - len | 0)) {
    throw new Error("Array.sub");
  } else {
    return Caml_array.caml_array_sub(a, ofs, len);
  }
}

function fill(a, ofs, len, v) {
  if (ofs < 0 || len < 0 || ofs > (a.length - len | 0)) {
    throw new Error("Array.fill");
  } else {
    for(var i = ofs ,i_finish = (ofs + len | 0) - 1 | 0; i <= i_finish; ++i){
      a[i] = v;
    }
    return /* () */0;
  }
}

function blit(a1, ofs1, a2, ofs2, len) {
  if (len < 0 || ofs1 < 0 || ofs1 > (a1.length - len | 0) || ofs2 < 0 || ofs2 > (a2.length - len | 0)) {
    throw new Error("Array.blit");
  } else {
    return Caml_array.caml_array_blit(a1, ofs1, a2, ofs2, len);
  }
}

function iter(a, f) {
  for(var i = 0 ,i_finish = a.length - 1 | 0; i <= i_finish; ++i){
    f(a[i]);
  }
  return /* () */0;
}

function map(a, f) {
  var l = a.length;
  var r = new Array(l);
  for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
    r[i] = f(a[i]);
  }
  return r;
}

function iteri(a, f) {
  for(var i = 0 ,i_finish = a.length - 1 | 0; i <= i_finish; ++i){
    f(i, a[i]);
  }
  return /* () */0;
}

function mapi(a, f) {
  var l = a.length;
  var r = new Array(l);
  for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
    r[i] = f(i, a[i]);
  }
  return r;
}

function toList(a) {
  var _i = a.length - 1 | 0;
  var _res = /* [] */0;
  while(true) {
    var res = _res;
    var i = _i;
    if (i < 0) {
      return res;
    } else {
      _res = /* :: */[
        a[i],
        res
      ];
      _i = i - 1 | 0;
      continue ;
      
    }
  };
}

function list_length(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[1];
      _accu = accu + 1 | 0;
      continue ;
      
    } else {
      return accu;
    }
  };
}

function fillAUx(arr, _i, _xs) {
  while(true) {
    var xs = _xs;
    var i = _i;
    if (xs) {
      arr[i] = xs[0];
      _xs = xs[1];
      _i = i + 1 | 0;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function ofList(xs) {
  var len = list_length(0, xs);
  var a = new Array(len);
  fillAUx(a, 0, xs);
  return a;
}

function foldLeft(a, x, f) {
  var r = x;
  for(var i = 0 ,i_finish = a.length - 1 | 0; i <= i_finish; ++i){
    r = f(r, a[i]);
  }
  return r;
}

function foldRight(a, x, f) {
  var r = x;
  for(var i = a.length - 1 | 0; i >= 0; --i){
    r = f(r, a[i]);
  }
  return r;
}

function forAll(arr, b) {
  var len = arr.length;
  var arr$1 = arr;
  var _i = 0;
  var b$1 = b;
  var len$1 = len;
  while(true) {
    var i = _i;
    if (i === len$1) {
      return /* true */1;
    } else if (b$1(arr$1[i])) {
      _i = i + 1 | 0;
      continue ;
      
    } else {
      return /* false */0;
    }
  };
}

var concat = Caml_array.caml_array_concat;

exports.init = init;
exports.shuffleInPlace = shuffleInPlace;
exports.zip = zip;
exports.makeMatrix = makeMatrix;
exports.append = append;
exports.concat = concat;
exports.sub = sub;
exports.copy = copy;
exports.fill = fill;
exports.blit = blit;
exports.toList = toList;
exports.ofList = ofList;
exports.iter = iter;
exports.map = map;
exports.iteri = iteri;
exports.mapi = mapi;
exports.foldLeft = foldLeft;
exports.foldRight = foldRight;
exports.forAll = forAll;
/* No side effect */
