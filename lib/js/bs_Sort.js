'use strict';

var Bs_Array = require("./bs_Array.js");

function sortedLengthAuxMore(xs, _prec, _acc, len, lt) {
  while(true) {
    var acc = _acc;
    var prec = _prec;
    if (acc >= len) {
      return acc;
    } else {
      var v = xs[acc];
      if (lt(v, prec)) {
        _acc = acc + 1 | 0;
        _prec = v;
        continue ;
        
      } else {
        return acc;
      }
    }
  };
}

function strictlySortedLength(xs, lt) {
  var len = xs.length;
  if (len === 0 || len === 1) {
    return len;
  } else {
    var x0 = xs[0];
    var x1 = xs[1];
    if (lt(x0, x1)) {
      var xs$1 = xs;
      var _prec = x1;
      var _acc = 2;
      var len$1 = len;
      var lt$1 = lt;
      while(true) {
        var acc = _acc;
        var prec = _prec;
        if (acc >= len$1) {
          return acc;
        } else {
          var v = xs$1[acc];
          if (lt$1(prec, v)) {
            _acc = acc + 1 | 0;
            _prec = v;
            continue ;
            
          } else {
            return acc;
          }
        }
      };
    } else if (lt(x1, x0)) {
      return -sortedLengthAuxMore(xs, x1, 2, len, lt) | 0;
    } else {
      return 1;
    }
  }
}

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
      } else if (cmp$1(a$1[i], a$1[i + 1 | 0]) <= 0) {
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
        return Bs_Array.blitUnsafe(src2, i2, dst, d + 1 | 0, src2r - i2 | 0);
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
        return Bs_Array.blitUnsafe(src, i1, dst, d + 1 | 0, src1r - i1 | 0);
      }
    }
  };
}

function union(src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs, cmp) {
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
    var c = cmp(s1, s2);
    if (c < 0) {
      dst[d] = s1;
      var i1$1 = i1 + 1 | 0;
      var d$1 = d + 1 | 0;
      if (i1$1 < src1r) {
        _d = d$1;
        _s1 = src[i1$1];
        _i1 = i1$1;
        continue ;
        
      } else {
        Bs_Array.blitUnsafe(src2, i2, dst, d$1, src2r - i2 | 0);
        return (d$1 + src2r | 0) - i2 | 0;
      }
    } else if (c) {
      dst[d] = s2;
      var i2$1 = i2 + 1 | 0;
      var d$2 = d + 1 | 0;
      if (i2$1 < src2r) {
        _d = d$2;
        _s2 = src2[i2$1];
        _i2 = i2$1;
        continue ;
        
      } else {
        Bs_Array.blitUnsafe(src, i1, dst, d$2, src1r - i1 | 0);
        return (d$2 + src1r | 0) - i1 | 0;
      }
    } else {
      dst[d] = s1;
      var i1$2 = i1 + 1 | 0;
      var i2$2 = i2 + 1 | 0;
      var d$3 = d + 1 | 0;
      if (i1$2 < src1r && i2$2 < src2r) {
        _d = d$3;
        _s2 = src2[i2$2];
        _i2 = i2$2;
        _s1 = src[i1$2];
        _i1 = i1$2;
        continue ;
        
      } else if (i1$2 === src1r) {
        Bs_Array.blitUnsafe(src2, i2$2, dst, d$3, src2r - i2$2 | 0);
        return (d$3 + src2r | 0) - i2$2 | 0;
      } else {
        Bs_Array.blitUnsafe(src, i1$2, dst, d$3, src1r - i1$2 | 0);
        return (d$3 + src1r | 0) - i1$2 | 0;
      }
    }
  };
}

function intersect(src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs, cmp) {
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
    var c = cmp(s1, s2);
    if (c < 0) {
      var i1$1 = i1 + 1 | 0;
      if (i1$1 < src1r) {
        _s1 = src[i1$1];
        _i1 = i1$1;
        continue ;
        
      } else {
        return d;
      }
    } else if (c) {
      var i2$1 = i2 + 1 | 0;
      if (i2$1 < src2r) {
        _s2 = src2[i2$1];
        _i2 = i2$1;
        continue ;
        
      } else {
        return d;
      }
    } else {
      dst[d] = s1;
      var i1$2 = i1 + 1 | 0;
      var i2$2 = i2 + 1 | 0;
      var d$1 = d + 1 | 0;
      if (i1$2 < src1r && i2$2 < src2r) {
        _d = d$1;
        _s2 = src2[i2$2];
        _i2 = i2$2;
        _s1 = src[i1$2];
        _i1 = i1$2;
        continue ;
        
      } else {
        return d$1;
      }
    }
  };
}

function diff(src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs, cmp) {
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
    var c = cmp(s1, s2);
    if (c < 0) {
      dst[d] = s1;
      var d$1 = d + 1 | 0;
      var i1$1 = i1 + 1 | 0;
      if (i1$1 < src1r) {
        _d = d$1;
        _s1 = src[i1$1];
        _i1 = i1$1;
        continue ;
        
      } else {
        return d$1;
      }
    } else if (c) {
      var i2$1 = i2 + 1 | 0;
      if (i2$1 < src2r) {
        _s2 = src2[i2$1];
        _i2 = i2$1;
        continue ;
        
      } else {
        Bs_Array.blitUnsafe(src, i1, dst, d, src1r - i1 | 0);
        return (d + src1r | 0) - i1 | 0;
      }
    } else {
      var i1$2 = i1 + 1 | 0;
      var i2$2 = i2 + 1 | 0;
      if (i1$2 < src1r && i2$2 < src2r) {
        _s2 = src2[i2$2];
        _i2 = i2$2;
        _s1 = src[i1$2];
        _i1 = i1$2;
        continue ;
        
      } else if (i1$2 === src1r) {
        return d;
      } else {
        Bs_Array.blitUnsafe(src, i1$2, dst, d, src1r - i1$2 | 0);
        return (d + src1r | 0) - i1$2 | 0;
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

function stableSortInPlaceBy(a, cmp) {
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

function stableSortBy(a, cmp) {
  var b = Bs_Array.copy(a);
  stableSortInPlaceBy(b, cmp);
  return b;
}

function binarySearchBy(sorted, key, cmp) {
  var len = sorted.length;
  if (len) {
    var lo = sorted[0];
    var c = cmp(key, lo);
    if (c < 0) {
      return -1;
    } else {
      var hi = sorted[len - 1 | 0];
      var c2 = cmp(key, hi);
      if (c2 > 0) {
        return -(len + 1 | 0) | 0;
      } else {
        var arr = sorted;
        var _lo = 0;
        var _hi = len - 1 | 0;
        var key$1 = key;
        var cmp$1 = cmp;
        while(true) {
          var hi$1 = _hi;
          var lo$1 = _lo;
          var mid = (lo$1 + hi$1 | 0) / 2 | 0;
          var midVal = arr[mid];
          var c$1 = cmp$1(key$1, midVal);
          if (c$1) {
            if (c$1 < 0) {
              if (hi$1 === mid) {
                if (cmp$1(arr[lo$1], key$1)) {
                  return -(hi$1 + 1 | 0) | 0;
                } else {
                  return lo$1;
                }
              } else {
                _hi = mid;
                continue ;
                
              }
            } else if (lo$1 === mid) {
              if (cmp$1(arr[hi$1], key$1)) {
                return -(hi$1 + 1 | 0) | 0;
              } else {
                return hi$1;
              }
            } else {
              _lo = mid;
              continue ;
              
            }
          } else {
            return mid;
          }
        };
      }
    }
  } else {
    return -1;
  }
}

exports.strictlySortedLength = strictlySortedLength;
exports.isSorted = isSorted;
exports.stableSortInPlaceBy = stableSortInPlaceBy;
exports.stableSortBy = stableSortBy;
exports.union = union;
exports.intersect = intersect;
exports.diff = diff;
exports.binarySearchBy = binarySearchBy;
/* No side effect */
