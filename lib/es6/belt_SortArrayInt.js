

import * as Belt_Array from "./belt_Array.js";

function sortedLengthAuxMore(xs, _prec, _acc, len) {
  while(true) {
    var acc = _acc;
    var prec = _prec;
    if (acc >= len) {
      return acc;
    } else {
      var v = xs[acc];
      if (prec > v) {
        _acc = acc + 1 | 0;
        _prec = v;
        continue ;
      } else {
        return acc;
      }
    }
  };
}

function strictlySortedLength(xs) {
  var len = xs.length;
  if (len === 0 || len === 1) {
    return len;
  } else {
    var x0 = xs[0];
    var x1 = xs[1];
    if (x0 < x1) {
      var xs$1 = xs;
      var _prec = x1;
      var _acc = 2;
      var len$1 = len;
      while(true) {
        var acc = _acc;
        var prec = _prec;
        if (acc >= len$1) {
          return acc;
        } else {
          var v = xs$1[acc];
          if (prec < v) {
            _acc = acc + 1 | 0;
            _prec = v;
            continue ;
          } else {
            return acc;
          }
        }
      };
    } else if (x0 > x1) {
      return -sortedLengthAuxMore(xs, x1, 2, len) | 0;
    } else {
      return 1;
    }
  }
}

function isSorted(a) {
  var len = a.length;
  if (len === 0) {
    return true;
  } else {
    var a$1 = a;
    var _i = 0;
    var last_bound = len - 1 | 0;
    while(true) {
      var i = _i;
      if (i === last_bound) {
        return true;
      } else if (a$1[i] <= a$1[i + 1 | 0]) {
        _i = i + 1 | 0;
        continue ;
      } else {
        return false;
      }
    };
  }
}

function merge(src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) {
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
        return Belt_Array.blitUnsafe(src2, i2, dst, d + 1 | 0, src2r - i2 | 0);
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
        return Belt_Array.blitUnsafe(src, i1, dst, d + 1 | 0, src1r - i1 | 0);
      }
    }
  };
}

function union(src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) {
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
    if (s1 < s2) {
      dst[d] = s1;
      var i1$1 = i1 + 1 | 0;
      var d$1 = d + 1 | 0;
      if (i1$1 < src1r) {
        _d = d$1;
        _s1 = src[i1$1];
        _i1 = i1$1;
        continue ;
      } else {
        Belt_Array.blitUnsafe(src2, i2, dst, d$1, src2r - i2 | 0);
        return (d$1 + src2r | 0) - i2 | 0;
      }
    } else if (s1 === s2) {
      dst[d] = s1;
      var i1$2 = i1 + 1 | 0;
      var i2$1 = i2 + 1 | 0;
      var d$2 = d + 1 | 0;
      if (i1$2 < src1r && i2$1 < src2r) {
        _d = d$2;
        _s2 = src2[i2$1];
        _i2 = i2$1;
        _s1 = src[i1$2];
        _i1 = i1$2;
        continue ;
      } else if (i1$2 === src1r) {
        Belt_Array.blitUnsafe(src2, i2$1, dst, d$2, src2r - i2$1 | 0);
        return (d$2 + src2r | 0) - i2$1 | 0;
      } else {
        Belt_Array.blitUnsafe(src, i1$2, dst, d$2, src1r - i1$2 | 0);
        return (d$2 + src1r | 0) - i1$2 | 0;
      }
    } else {
      dst[d] = s2;
      var i2$2 = i2 + 1 | 0;
      var d$3 = d + 1 | 0;
      if (i2$2 < src2r) {
        _d = d$3;
        _s2 = src2[i2$2];
        _i2 = i2$2;
        continue ;
      } else {
        Belt_Array.blitUnsafe(src, i1, dst, d$3, src1r - i1 | 0);
        return (d$3 + src1r | 0) - i1 | 0;
      }
    }
  };
}

function intersect(src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) {
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
    if (s1 < s2) {
      var i1$1 = i1 + 1 | 0;
      if (i1$1 < src1r) {
        _s1 = src[i1$1];
        _i1 = i1$1;
        continue ;
      } else {
        return d;
      }
    } else if (s1 === s2) {
      dst[d] = s1;
      var i1$2 = i1 + 1 | 0;
      var i2$1 = i2 + 1 | 0;
      var d$1 = d + 1 | 0;
      if (i1$2 < src1r && i2$1 < src2r) {
        _d = d$1;
        _s2 = src2[i2$1];
        _i2 = i2$1;
        _s1 = src[i1$2];
        _i1 = i1$2;
        continue ;
      } else {
        return d$1;
      }
    } else {
      var i2$2 = i2 + 1 | 0;
      if (i2$2 < src2r) {
        _s2 = src2[i2$2];
        _i2 = i2$2;
        continue ;
      } else {
        return d;
      }
    }
  };
}

function diff(src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) {
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
    if (s1 < s2) {
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
    } else if (s1 === s2) {
      var i1$2 = i1 + 1 | 0;
      var i2$1 = i2 + 1 | 0;
      if (i1$2 < src1r && i2$1 < src2r) {
        _s2 = src2[i2$1];
        _i2 = i2$1;
        _s1 = src[i1$2];
        _i1 = i1$2;
        continue ;
      } else if (i1$2 === src1r) {
        return d;
      } else {
        Belt_Array.blitUnsafe(src, i1$2, dst, d, src1r - i1$2 | 0);
        return (d + src1r | 0) - i1$2 | 0;
      }
    } else {
      var i2$2 = i2 + 1 | 0;
      if (i2$2 < src2r) {
        _s2 = src2[i2$2];
        _i2 = i2$2;
        continue ;
      } else {
        Belt_Array.blitUnsafe(src, i1, dst, d, src1r - i1 | 0);
        return (d + src1r | 0) - i1 | 0;
      }
    }
  };
}

function insertionSort(src, srcofs, dst, dstofs, len) {
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

function sortTo(src, srcofs, dst, dstofs, len) {
  if (len <= 5) {
    return insertionSort(src, srcofs, dst, dstofs, len);
  } else {
    var l1 = len / 2 | 0;
    var l2 = len - l1 | 0;
    sortTo(src, srcofs + l1 | 0, dst, dstofs + l1 | 0, l2);
    sortTo(src, srcofs, src, srcofs + l2 | 0, l1);
    return merge(src, srcofs + l2 | 0, l1, dst, dstofs + l1 | 0, l2, dst, dstofs);
  }
}

function stableSortInPlace(a) {
  var l = a.length;
  if (l <= 5) {
    return insertionSort(a, 0, a, 0, l);
  } else {
    var l1 = l / 2 | 0;
    var l2 = l - l1 | 0;
    var t = new Array(l2);
    sortTo(a, l1, t, 0, l2);
    sortTo(a, 0, a, l2, l1);
    return merge(a, l2, l1, t, 0, l2, a, 0);
  }
}

function stableSort(a) {
  var b = a.slice(0);
  stableSortInPlace(b);
  return b;
}

function binarySearch(sorted, key) {
  var len = sorted.length;
  if (len === 0) {
    return -1;
  } else {
    var lo = sorted[0];
    if (key < lo) {
      return -1;
    } else {
      var hi = sorted[len - 1 | 0];
      if (key > hi) {
        return -(len + 1 | 0) | 0;
      } else {
        var arr = sorted;
        var _lo = 0;
        var _hi = len - 1 | 0;
        var key$1 = key;
        while(true) {
          var hi$1 = _hi;
          var lo$1 = _lo;
          var mid = (lo$1 + hi$1 | 0) / 2 | 0;
          var midVal = arr[mid];
          if (key$1 === midVal) {
            return mid;
          } else if (key$1 < midVal) {
            if (hi$1 === mid) {
              if (arr[lo$1] === key$1) {
                return lo$1;
              } else {
                return -(hi$1 + 1 | 0) | 0;
              }
            } else {
              _hi = mid;
              continue ;
            }
          } else if (lo$1 === mid) {
            if (arr[hi$1] === key$1) {
              return hi$1;
            } else {
              return -(hi$1 + 1 | 0) | 0;
            }
          } else {
            _lo = mid;
            continue ;
          }
        };
      }
    }
  }
}

export {
  strictlySortedLength ,
  isSorted ,
  stableSortInPlace ,
  stableSort ,
  binarySearch ,
  union ,
  intersect ,
  diff ,
  
}
/* No side effect */
