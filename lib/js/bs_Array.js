'use strict';

var Js_math = require("./js_math.js");
var Caml_primitive = require("./caml_primitive.js");

function get(arr, i) {
  if (i >= 0 && i < arr.length) {
    return /* Some */[arr[i]];
  } else {
    return /* None */0;
  }
}

function getExn(arr, i) {
  if (!(i >= 0 && i < arr.length)) {
    throw new Error("File \"bs_Array.ml\", line 25, characters 6-12");
  }
  return arr[i];
}

function set(arr, i, v) {
  if (i >= 0 && i < arr.length) {
    arr[i] = v;
    return /* () */0;
  } else {
    return 0;
  }
}

function copy(a) {
  var l = a.length;
  var v = new Array(l);
  for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
    v[i] = a[i];
  }
  return v;
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

function shuffle(xs) {
  var result = copy(xs);
  shuffleInPlace(result);
  return result;
}

function reverseInPlace(xs) {
  var len = xs.length;
  var xs$1 = xs;
  var ofs = 0;
  var len$1 = len;
  for(var i = 0 ,i_finish = (len$1 / 2 | 0) - 1 | 0; i <= i_finish; ++i){
    swapUnsafe(xs$1, ofs + i | 0, ((ofs + len$1 | 0) - i | 0) - 1 | 0);
  }
  return /* () */0;
}

function reverse(xs) {
  var len = xs.length;
  var result = new Array(len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    result[i] = xs[(len - 1 | 0) - i | 0];
  }
  return result;
}

function make(l, f) {
  if (l <= 0) {
    return /* array */[];
  } else {
    var res = new Array(l);
    for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
      res[i] = f;
    }
    return res;
  }
}

function makeBy(l, f) {
  if (l <= 0) {
    return /* array */[];
  } else {
    var res = new Array(l);
    for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
      res[i] = f(i);
    }
    return res;
  }
}

function makeByAndShuffle(l, f) {
  var u = makeBy(l, f);
  shuffleInPlace(u);
  return u;
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

function zipBy(xs, ys, f) {
  var lenx = xs.length;
  var leny = ys.length;
  var len = lenx < leny ? lenx : leny;
  var s = new Array(len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    s[i] = f(xs[i], ys[i]);
  }
  return s;
}

function concat(a1, a2) {
  var l1 = a1.length;
  var l2 = a2.length;
  var a1a2 = new Array(l1 + l2 | 0);
  for(var i = 0 ,i_finish = l1 - 1 | 0; i <= i_finish; ++i){
    a1a2[i] = a1[i];
  }
  for(var i$1 = 0 ,i_finish$1 = l2 - 1 | 0; i$1 <= i_finish$1; ++i$1){
    a1a2[l1 + i$1 | 0] = a2[i$1];
  }
  return a1a2;
}

function concatMany(arrs) {
  var lenArrs = arrs.length;
  var totalLen = 0;
  for(var i = 0 ,i_finish = lenArrs - 1 | 0; i <= i_finish; ++i){
    totalLen = totalLen + arrs[i].length | 0;
  }
  var result = new Array(totalLen);
  totalLen = 0;
  for(var j = 0 ,j_finish = lenArrs - 1 | 0; j <= j_finish; ++j){
    var cur = arrs[j];
    for(var k = 0 ,k_finish = cur.length - 1 | 0; k <= k_finish; ++k){
      result[totalLen] = cur[k];
      totalLen = totalLen + 1 | 0;
    }
  }
  return result;
}

function slice(a, offset, len) {
  if (len <= 0) {
    return /* array */[];
  } else {
    var lena = a.length;
    var ofs = offset < 0 ? Caml_primitive.caml_int_max(lena + offset | 0, 0) : offset;
    var hasLen = lena - ofs | 0;
    var copyLength = hasLen < len ? hasLen : len;
    if (copyLength <= 0) {
      return /* array */[];
    } else {
      var result = new Array(copyLength);
      for(var i = 0 ,i_finish = copyLength - 1 | 0; i <= i_finish; ++i){
        result[i] = a[ofs + i | 0];
      }
      return result;
    }
  }
}

function fill(a, offset, len, v) {
  if (len > 0) {
    var lena = a.length;
    var ofs = offset < 0 ? Caml_primitive.caml_int_max(lena + offset | 0, 0) : offset;
    var hasLen = lena - ofs | 0;
    var fillLength = hasLen < len ? hasLen : len;
    if (fillLength > 0) {
      for(var i = ofs ,i_finish = (ofs + fillLength | 0) - 1 | 0; i <= i_finish; ++i){
        a[i] = v;
      }
      return /* () */0;
    } else {
      return 0;
    }
  } else {
    return 0;
  }
}

function blitUnsafe(a1, srcofs1, a2, srcofs2, blitLength) {
  if (srcofs2 <= srcofs1) {
    for(var j = 0 ,j_finish = blitLength - 1 | 0; j <= j_finish; ++j){
      a2[j + srcofs2 | 0] = a1[j + srcofs1 | 0];
    }
    return /* () */0;
  } else {
    for(var j$1 = blitLength - 1 | 0; j$1 >= 0; --j$1){
      a2[j$1 + srcofs2 | 0] = a1[j$1 + srcofs1 | 0];
    }
    return /* () */0;
  }
}

function blit(a1, ofs1, a2, ofs2, len) {
  if (len > 0) {
    var lena1 = a1.length;
    var lena2 = a2.length;
    var srcofs1 = ofs1 < 0 ? Caml_primitive.caml_int_max(lena1 + ofs1 | 0, 0) : ofs1;
    var srcofs2 = ofs2 < 0 ? Caml_primitive.caml_int_max(lena2 + ofs2 | 0, 0) : ofs2;
    var blitLength = Caml_primitive.caml_int_min(len, Caml_primitive.caml_int_min(lena1 - srcofs1 | 0, lena2 - srcofs2 | 0));
    if (srcofs2 <= srcofs1) {
      for(var j = 0 ,j_finish = blitLength - 1 | 0; j <= j_finish; ++j){
        a2[j + srcofs2 | 0] = a1[j + srcofs1 | 0];
      }
      return /* () */0;
    } else {
      for(var j$1 = blitLength - 1 | 0; j$1 >= 0; --j$1){
        a2[j$1 + srcofs2 | 0] = a1[j$1 + srcofs1 | 0];
      }
      return /* () */0;
    }
  } else {
    return 0;
  }
}

function forEach(a, f) {
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

function keep(a, f) {
  var l = a.length;
  var r = new Array(l);
  var j = 0;
  for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
    var v = a[i];
    if (f(v)) {
      r[j] = v;
      j = j + 1 | 0;
    }
    
  }
  r.length = j;
  return r;
}

function keepMap(a, f) {
  var l = a.length;
  var r = new Array(l);
  var j = 0;
  for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
    var v = a[i];
    var match = f(v);
    if (match) {
      r[j] = match[0];
      j = j + 1 | 0;
    }
    
  }
  r.length = j;
  return r;
}

function forEachWithIndex(a, f) {
  for(var i = 0 ,i_finish = a.length - 1 | 0; i <= i_finish; ++i){
    f(i, a[i]);
  }
  return /* () */0;
}

function mapWithIndex(a, f) {
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

function reduce(a, x, f) {
  var r = x;
  for(var i = 0 ,i_finish = a.length - 1 | 0; i <= i_finish; ++i){
    r = f(r, a[i]);
  }
  return r;
}

function reduceReverse(a, x, f) {
  var r = x;
  for(var i = a.length - 1 | 0; i >= 0; --i){
    r = f(r, a[i]);
  }
  return r;
}

function every(arr, b) {
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

function every2(a, b, p) {
  var lena = a.length;
  var lenb = b.length;
  if (lena !== lenb) {
    return /* false */0;
  } else {
    var arr1 = a;
    var arr2 = b;
    var _i = 0;
    var b$1 = p;
    var len = lena;
    while(true) {
      var i = _i;
      if (i === len) {
        return /* true */1;
      } else if (b$1(arr1[i], arr2[i])) {
        _i = i + 1 | 0;
        continue ;
        
      } else {
        return /* false */0;
      }
    };
  }
}

function cmp(a, b, p) {
  var lena = a.length;
  var lenb = b.length;
  if (lena > lenb) {
    return 1;
  } else if (lena < lenb) {
    return -1;
  } else {
    var arr1 = a;
    var arr2 = b;
    var _i = 0;
    var b$1 = p;
    var len = lena;
    while(true) {
      var i = _i;
      if (i === len) {
        return 0;
      } else {
        var c = b$1(arr1[i], arr2[i]);
        if (c) {
          return c;
        } else {
          _i = i + 1 | 0;
          continue ;
          
        }
      }
    };
  }
}

var eq = every2;

exports.get = get;
exports.set = set;
exports.getExn = getExn;
exports.shuffleInPlace = shuffleInPlace;
exports.shuffle = shuffle;
exports.reverseInPlace = reverseInPlace;
exports.reverse = reverse;
exports.make = make;
exports.makeBy = makeBy;
exports.makeByAndShuffle = makeByAndShuffle;
exports.zip = zip;
exports.concat = concat;
exports.concatMany = concatMany;
exports.slice = slice;
exports.copy = copy;
exports.fill = fill;
exports.blit = blit;
exports.blitUnsafe = blitUnsafe;
exports.toList = toList;
exports.ofList = ofList;
exports.forEach = forEach;
exports.map = map;
exports.zipBy = zipBy;
exports.keep = keep;
exports.keepMap = keepMap;
exports.forEachWithIndex = forEachWithIndex;
exports.mapWithIndex = mapWithIndex;
exports.reduce = reduce;
exports.reduceReverse = reduceReverse;
exports.every = every;
exports.every2 = every2;
exports.cmp = cmp;
exports.eq = eq;
/* No side effect */
