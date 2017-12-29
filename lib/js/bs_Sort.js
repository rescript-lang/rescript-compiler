'use strict';

var Caml_array = require("./caml_array.js");

function isSorted(a, cmp) {
  var len = a.length;
  if (len) {
    var a$1 = a;
    var _i = 0;
    var cmp$1 = cmp;
    var last_bound = len - 1 | 0;
    while(true) {
      var i = _i;
      if (i === last_bound) {
        return /* true */1;
      } else if (cmp$1(a$1[i], a$1[i + 1 | 0]) < 0) {
        _i = i + 1 | 0;
        continue ;
        
      } else {
        return /* false */0;
      }
    };
  } else {
    return /* true */1;
  }
}

function merge(src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs, cmp) {
  var src1r = src1ofs + src1len | 0;
  var src2r = src2ofs + src2len | 0;
  var _i1 = src1ofs;
  var _s1 = src[src1ofs];
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
      var i1$1 = i1 + 1 | 0;
      if (i1$1 < src1r) {
        _d = d + 1 | 0;
        _s1 = src[i1$1];
        _i1 = i1$1;
        continue ;
        
      } else {
        return Caml_array.caml_array_blit(src2, i2, dst, d + 1 | 0, src2r - i2 | 0);
      }
    } else {
      dst[d] = s2;
      var i2$1 = i2 + 1 | 0;
      if (i2$1 < src2r) {
        _d = d + 1 | 0;
        _s2 = src2[i2$1];
        _i2 = i2$1;
        continue ;
        
      } else {
        return Caml_array.caml_array_blit(src, i1, dst, d + 1 | 0, src1r - i1 | 0);
      }
    }
  };
}

function mergeInts(src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) {
  var src1r = src1ofs + src1len | 0;
  var src2r = src2ofs + src2len | 0;
  var _i1 = src1ofs;
  var _s1 = src[src1ofs];
  var _i2 = src2ofs;
  var _s2 = src2[src2ofs];
  var _d = dstofs;
  while(true) {
    var d = _d;
    var s2 = _s2;
    var i2 = _i2;
    var s1 = _s1;
    var i1 = _i1;
    if (s1 <= s2) {
      dst[d] = s1;
      var i1$1 = i1 + 1 | 0;
      if (i1$1 < src1r) {
        _d = d + 1 | 0;
        _s1 = src[i1$1];
        _i1 = i1$1;
        continue ;
        
      } else {
        return Caml_array.caml_array_blit(src2, i2, dst, d + 1 | 0, src2r - i2 | 0);
      }
    } else {
      dst[d] = s2;
      var i2$1 = i2 + 1 | 0;
      if (i2$1 < src2r) {
        _d = d + 1 | 0;
        _s2 = src2[i2$1];
        _i2 = i2$1;
        continue ;
        
      } else {
        return Caml_array.caml_array_blit(src, i1, dst, d + 1 | 0, src1r - i1 | 0);
      }
    }
  };
}

function mergeFloats(src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) {
  var src1r = src1ofs + src1len | 0;
  var src2r = src2ofs + src2len | 0;
  var _i1 = src1ofs;
  var _s1 = src[src1ofs];
  var _i2 = src2ofs;
  var _s2 = src2[src2ofs];
  var _d = dstofs;
  while(true) {
    var d = _d;
    var s2 = _s2;
    var i2 = _i2;
    var s1 = _s1;
    var i1 = _i1;
    if (s1 <= s2) {
      dst[d] = s1;
      var i1$1 = i1 + 1 | 0;
      if (i1$1 < src1r) {
        _d = d + 1 | 0;
        _s1 = src[i1$1];
        _i1 = i1$1;
        continue ;
        
      } else {
        return Caml_array.caml_array_blit(src2, i2, dst, d + 1 | 0, src2r - i2 | 0);
      }
    } else {
      dst[d] = s2;
      var i2$1 = i2 + 1 | 0;
      if (i2$1 < src2r) {
        _d = d + 1 | 0;
        _s2 = src2[i2$1];
        _i2 = i2$1;
        continue ;
        
      } else {
        return Caml_array.caml_array_blit(src, i1, dst, d + 1 | 0, src1r - i1 | 0);
      }
    }
  };
}

function insertionSort(src, srcofs, dst, dstofs, len, cmp) {
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var e = src[srcofs + i | 0];
    var j = (dstofs + i | 0) - 1 | 0;
    while(j >= dstofs && cmp(dst[j], e) > 0) {
      dst[j + 1 | 0] = dst[j];
      j = j - 1 | 0;
    };
    dst[j + 1 | 0] = e;
  }
  return /* () */0;
}

function insertionSortInts(src, srcofs, dst, dstofs, len) {
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var e = src[srcofs + i | 0];
    var j = (dstofs + i | 0) - 1 | 0;
    while(j >= dstofs && dst[j] > e) {
      dst[j + 1 | 0] = dst[j];
      j = j - 1 | 0;
    };
    dst[j + 1 | 0] = e;
  }
  return /* () */0;
}

function insertionSortFloats(src, srcofs, dst, dstofs, len) {
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var e = src[srcofs + i | 0];
    var j = (dstofs + i | 0) - 1 | 0;
    while(j >= dstofs && dst[j] > e) {
      dst[j + 1 | 0] = dst[j];
      j = j - 1 | 0;
    };
    dst[j + 1 | 0] = e;
  }
  return /* () */0;
}

function sortTo(src, srcofs, dst, dstofs, len, cmp) {
  if (len <= 5) {
    return insertionSort(src, srcofs, dst, dstofs, len, cmp);
  } else {
    var l1 = len / 2 | 0;
    var l2 = len - l1 | 0;
    sortTo(src, srcofs + l1 | 0, dst, dstofs + l1 | 0, l2, cmp);
    sortTo(src, srcofs, src, srcofs + l2 | 0, l1, cmp);
    return merge(src, srcofs + l2 | 0, l1, dst, dstofs + l1 | 0, l2, dst, dstofs, cmp);
  }
}

function sortToInts(src, srcofs, dst, dstofs, len) {
  if (len <= 5) {
    return insertionSortInts(src, srcofs, dst, dstofs, len);
  } else {
    var l1 = len / 2 | 0;
    var l2 = len - l1 | 0;
    sortToInts(src, srcofs + l1 | 0, dst, dstofs + l1 | 0, l2);
    sortToInts(src, srcofs, src, srcofs + l2 | 0, l1);
    return mergeInts(src, srcofs + l2 | 0, l1, dst, dstofs + l1 | 0, l2, dst, dstofs);
  }
}

function sortToFloats(src, srcofs, dst, dstofs, len) {
  if (len <= 5) {
    return insertionSortFloats(src, srcofs, dst, dstofs, len);
  } else {
    var l1 = len / 2 | 0;
    var l2 = len - l1 | 0;
    sortToFloats(src, srcofs + l1 | 0, dst, dstofs + l1 | 0, l2);
    sortToFloats(src, srcofs, src, srcofs + l2 | 0, l1);
    return mergeFloats(src, srcofs + l2 | 0, l1, dst, dstofs + l1 | 0, l2, dst, dstofs);
  }
}

function stableSortBy(a, cmp) {
  var l = a.length;
  if (l <= 5) {
    return insertionSort(a, 0, a, 0, l, cmp);
  } else {
    var l1 = l / 2 | 0;
    var l2 = l - l1 | 0;
    var t = new Array(l2);
    sortTo(a, l1, t, 0, l2, cmp);
    sortTo(a, 0, a, l2, l1, cmp);
    return merge(a, l2, l1, t, 0, l2, a, 0, cmp);
  }
}

function stableSortInts(a) {
  var l = a.length;
  if (l <= 5) {
    return insertionSortInts(a, 0, a, 0, l);
  } else {
    var l1 = l / 2 | 0;
    var l2 = l - l1 | 0;
    var t = new Array(l2);
    sortToInts(a, l1, t, 0, l2);
    sortToInts(a, 0, a, l2, l1);
    return mergeInts(a, l2, l1, t, 0, l2, a, 0);
  }
}

function stableSortFloats(a) {
  var l = a.length;
  if (l <= 5) {
    return insertionSortFloats(a, 0, a, 0, l);
  } else {
    var l1 = l / 2 | 0;
    var l2 = l - l1 | 0;
    var t = new Array(l2);
    sortToFloats(a, l1, t, 0, l2);
    sortToFloats(a, 0, a, l2, l1);
    return mergeFloats(a, l2, l1, t, 0, l2, a, 0);
  }
}

function sortByCont(xs, cmp) {
  xs.sort(cmp);
  return xs;
}

exports.isSorted = isSorted;
exports.stableSortBy = stableSortBy;
exports.stableSortInts = stableSortInts;
exports.stableSortFloats = stableSortFloats;
exports.sortByCont = sortByCont;
/* No side effect */
