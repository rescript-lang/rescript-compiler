

import * as Caml from "./caml.js";
import * as Caml_option from "./caml_option.js";

function get(arr, i) {
  if (i >= 0 && i < arr.length) {
    return Caml_option.some(arr[i]);
  }
  
}

function getExn(arr, i) {
  if (!(i >= 0 && i < arr.length)) {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "belt_Array.res",
          36,
          2
        ]
      }
    });
  }
  return arr[i];
}

function set(arr, i, v) {
  if (i >= 0 && i < arr.length) {
    arr[i] = v;
    return true;
  } else {
    return false;
  }
}

function setExn(arr, i, v) {
  if (!(i >= 0 && i < arr.length)) {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "belt_Array.res",
          49,
          2
        ]
      }
    });
  }
  arr[i] = v;
}

function swapUnsafe(xs, i, j) {
  let tmp = xs[i];
  xs[i] = xs[j];
  xs[j] = tmp;
}

function shuffleInPlace(xs) {
  let len = xs.length;
  let random_int = function (min, max) {
    return Math.floor(Math.random() * (max - min | 0)) + min | 0;
  };
  for (let i = 0; i < len; ++i) {
    swapUnsafe(xs, i, random_int(i, len));
  }
}

function shuffle(xs) {
  let result = xs.slice(0);
  shuffleInPlace(result);
  return result;
}

function reverseInPlace(xs) {
  let len = xs.length;
  let ofs = 0;
  for (let i = 0, i_finish = len / 2 | 0; i < i_finish; ++i) {
    swapUnsafe(xs, ofs + i | 0, ((ofs + len | 0) - i | 0) - 1 | 0);
  }
}

function reverse(xs) {
  let len = xs.length;
  let result = new Array(len);
  for (let i = 0; i < len; ++i) {
    result[i] = xs[(len - 1 | 0) - i | 0];
  }
  return result;
}

function make(l, f) {
  if (l <= 0) {
    return [];
  }
  let res = new Array(l);
  for (let i = 0; i < l; ++i) {
    res[i] = f;
  }
  return res;
}

function makeByU(l, f) {
  if (l <= 0) {
    return [];
  }
  let res = new Array(l);
  for (let i = 0; i < l; ++i) {
    res[i] = f(i);
  }
  return res;
}

function makeBy(l, f) {
  return makeByU(l, (function (a) {
    return f(a);
  }));
}

function makeByAndShuffleU(l, f) {
  let u = makeByU(l, f);
  shuffleInPlace(u);
  return u;
}

function makeByAndShuffle(l, f) {
  return makeByAndShuffleU(l, (function (a) {
    return f(a);
  }));
}

function range(start, finish) {
  let cut = finish - start | 0;
  if (cut < 0) {
    return [];
  }
  let arr = new Array(cut + 1 | 0);
  for (let i = 0; i <= cut; ++i) {
    arr[i] = start + i | 0;
  }
  return arr;
}

function rangeBy(start, finish, step) {
  let cut = finish - start | 0;
  if (cut < 0 || step <= 0) {
    return [];
  }
  let nb = (cut / step | 0) + 1 | 0;
  let arr = new Array(nb);
  let cur = start;
  for (let i = 0; i < nb; ++i) {
    arr[i] = cur;
    cur = cur + step | 0;
  }
  return arr;
}

function zip(xs, ys) {
  let lenx = xs.length;
  let leny = ys.length;
  let len = lenx < leny ? lenx : leny;
  let s = new Array(len);
  for (let i = 0; i < len; ++i) {
    s[i] = [
      xs[i],
      ys[i]
    ];
  }
  return s;
}

function zipByU(xs, ys, f) {
  let lenx = xs.length;
  let leny = ys.length;
  let len = lenx < leny ? lenx : leny;
  let s = new Array(len);
  for (let i = 0; i < len; ++i) {
    s[i] = f(xs[i], ys[i]);
  }
  return s;
}

function zipBy(xs, ys, f) {
  return zipByU(xs, ys, (function (a, b) {
    return f(a, b);
  }));
}

function concat(a1, a2) {
  let l1 = a1.length;
  let l2 = a2.length;
  let a1a2 = new Array(l1 + l2 | 0);
  for (let i = 0; i < l1; ++i) {
    a1a2[i] = a1[i];
  }
  for (let i$1 = 0; i$1 < l2; ++i$1) {
    a1a2[l1 + i$1 | 0] = a2[i$1];
  }
  return a1a2;
}

function concatMany(arrs) {
  let lenArrs = arrs.length;
  let totalLen = 0;
  for (let i = 0; i < lenArrs; ++i) {
    totalLen = totalLen + arrs[i].length | 0;
  }
  let result = new Array(totalLen);
  totalLen = 0;
  for (let j = 0; j < lenArrs; ++j) {
    let cur = arrs[j];
    for (let k = 0, k_finish = cur.length; k < k_finish; ++k) {
      result[totalLen] = cur[k];
      totalLen = totalLen + 1 | 0;
    }
  }
  return result;
}

function slice(a, offset, len) {
  if (len <= 0) {
    return [];
  }
  let lena = a.length;
  let ofs = offset < 0 ? Caml.int_max(lena + offset | 0, 0) : offset;
  let hasLen = lena - ofs | 0;
  let copyLength = hasLen < len ? hasLen : len;
  if (copyLength <= 0) {
    return [];
  }
  let result = new Array(copyLength);
  for (let i = 0; i < copyLength; ++i) {
    result[i] = a[ofs + i | 0];
  }
  return result;
}

function sliceToEnd(a, offset) {
  let lena = a.length;
  let ofs = offset < 0 ? Caml.int_max(lena + offset | 0, 0) : offset;
  let len = lena > ofs ? lena - ofs | 0 : 0;
  let result = new Array(len);
  for (let i = 0; i < len; ++i) {
    result[i] = a[ofs + i | 0];
  }
  return result;
}

function fill(a, offset, len, v) {
  if (len <= 0) {
    return;
  }
  let lena = a.length;
  let ofs = offset < 0 ? Caml.int_max(lena + offset | 0, 0) : offset;
  let hasLen = lena - ofs | 0;
  let fillLength = hasLen < len ? hasLen : len;
  if (fillLength <= 0) {
    return;
  }
  for (let i = ofs, i_finish = ofs + fillLength | 0; i < i_finish; ++i) {
    a[i] = v;
  }
}

function blitUnsafe(a1, srcofs1, a2, srcofs2, blitLength) {
  if (srcofs2 <= srcofs1) {
    for (let j = 0; j < blitLength; ++j) {
      a2[j + srcofs2 | 0] = a1[j + srcofs1 | 0];
    }
    return;
  }
  for (let j$1 = blitLength - 1 | 0; j$1 >= 0; --j$1) {
    a2[j$1 + srcofs2 | 0] = a1[j$1 + srcofs1 | 0];
  }
}

function blit(a1, ofs1, a2, ofs2, len) {
  let lena1 = a1.length;
  let lena2 = a2.length;
  let srcofs1 = ofs1 < 0 ? Caml.int_max(lena1 + ofs1 | 0, 0) : ofs1;
  let srcofs2 = ofs2 < 0 ? Caml.int_max(lena2 + ofs2 | 0, 0) : ofs2;
  let blitLength = Caml.int_min(len, Caml.int_min(lena1 - srcofs1 | 0, lena2 - srcofs2 | 0));
  if (srcofs2 <= srcofs1) {
    for (let j = 0; j < blitLength; ++j) {
      a2[j + srcofs2 | 0] = a1[j + srcofs1 | 0];
    }
    return;
  }
  for (let j$1 = blitLength - 1 | 0; j$1 >= 0; --j$1) {
    a2[j$1 + srcofs2 | 0] = a1[j$1 + srcofs1 | 0];
  }
}

function forEachU(a, f) {
  for (let i = 0, i_finish = a.length; i < i_finish; ++i) {
    f(a[i]);
  }
}

function forEach(a, f) {
  forEachU(a, (function (a) {
    f(a);
  }));
}

function mapU(a, f) {
  let l = a.length;
  let r = new Array(l);
  for (let i = 0; i < l; ++i) {
    r[i] = f(a[i]);
  }
  return r;
}

function map(a, f) {
  return mapU(a, (function (a) {
    return f(a);
  }));
}

function flatMapU(a, f) {
  return concatMany(mapU(a, f));
}

function flatMap(a, f) {
  return concatMany(mapU(a, (function (a) {
    return f(a);
  })));
}

function getByU(a, p) {
  let l = a.length;
  let i = 0;
  let r;
  while (r === undefined && i < l) {
    let v = a[i];
    if (p(v)) {
      r = Caml_option.some(v);
    }
    i = i + 1 | 0;
  };
  return r;
}

function getBy(a, p) {
  return getByU(a, (function (a) {
    return p(a);
  }));
}

function getIndexByU(a, p) {
  let l = a.length;
  let i = 0;
  let r;
  while (r === undefined && i < l) {
    let v = a[i];
    if (p(v)) {
      r = i;
    }
    i = i + 1 | 0;
  };
  return r;
}

function getIndexBy(a, p) {
  return getIndexByU(a, (function (a) {
    return p(a);
  }));
}

function keepU(a, f) {
  let l = a.length;
  let r = new Array(l);
  let j = 0;
  for (let i = 0; i < l; ++i) {
    let v = a[i];
    if (f(v)) {
      r[j] = v;
      j = j + 1 | 0;
    }
    
  }
  r.length = j;
  return r;
}

function keep(a, f) {
  return keepU(a, (function (a) {
    return f(a);
  }));
}

function keepWithIndexU(a, f) {
  let l = a.length;
  let r = new Array(l);
  let j = 0;
  for (let i = 0; i < l; ++i) {
    let v = a[i];
    if (f(v, i)) {
      r[j] = v;
      j = j + 1 | 0;
    }
    
  }
  r.length = j;
  return r;
}

function keepWithIndex(a, f) {
  return keepWithIndexU(a, (function (a, i) {
    return f(a, i);
  }));
}

function keepMapU(a, f) {
  let l = a.length;
  let r = new Array(l);
  let j = 0;
  for (let i = 0; i < l; ++i) {
    let v = a[i];
    let v$1 = f(v);
    if (v$1 !== undefined) {
      r[j] = Caml_option.valFromOption(v$1);
      j = j + 1 | 0;
    }
    
  }
  r.length = j;
  return r;
}

function keepMap(a, f) {
  return keepMapU(a, (function (a) {
    return f(a);
  }));
}

function forEachWithIndexU(a, f) {
  for (let i = 0, i_finish = a.length; i < i_finish; ++i) {
    f(i, a[i]);
  }
}

function forEachWithIndex(a, f) {
  forEachWithIndexU(a, (function (a, b) {
    f(a, b);
  }));
}

function mapWithIndexU(a, f) {
  let l = a.length;
  let r = new Array(l);
  for (let i = 0; i < l; ++i) {
    r[i] = f(i, a[i]);
  }
  return r;
}

function mapWithIndex(a, f) {
  return mapWithIndexU(a, (function (a, b) {
    return f(a, b);
  }));
}

function reduceU(a, x, f) {
  let r = x;
  for (let i = 0, i_finish = a.length; i < i_finish; ++i) {
    r = f(r, a[i]);
  }
  return r;
}

function reduce(a, x, f) {
  return reduceU(a, x, (function (a, b) {
    return f(a, b);
  }));
}

function reduceReverseU(a, x, f) {
  let r = x;
  for (let i = a.length - 1 | 0; i >= 0; --i) {
    r = f(r, a[i]);
  }
  return r;
}

function reduceReverse(a, x, f) {
  return reduceReverseU(a, x, (function (a, b) {
    return f(a, b);
  }));
}

function reduceReverse2U(a, b, x, f) {
  let r = x;
  let len = Caml.int_min(a.length, b.length);
  for (let i = len - 1 | 0; i >= 0; --i) {
    r = f(r, a[i], b[i]);
  }
  return r;
}

function reduceReverse2(a, b, x, f) {
  return reduceReverse2U(a, b, x, (function (a, b, c) {
    return f(a, b, c);
  }));
}

function reduceWithIndexU(a, x, f) {
  let r = x;
  for (let i = 0, i_finish = a.length; i < i_finish; ++i) {
    r = f(r, a[i], i);
  }
  return r;
}

function reduceWithIndex(a, x, f) {
  return reduceWithIndexU(a, x, (function (a, b, c) {
    return f(a, b, c);
  }));
}

function everyU(arr, b) {
  let len = arr.length;
  let _i = 0;
  while (true) {
    let i = _i;
    if (i === len) {
      return true;
    }
    if (!b(arr[i])) {
      return false;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function every(arr, f) {
  return everyU(arr, (function (b) {
    return f(b);
  }));
}

function someU(arr, b) {
  let len = arr.length;
  let _i = 0;
  while (true) {
    let i = _i;
    if (i === len) {
      return false;
    }
    if (b(arr[i])) {
      return true;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function some(arr, f) {
  return someU(arr, (function (b) {
    return f(b);
  }));
}

function everyAux2(arr1, arr2, _i, b, len) {
  while (true) {
    let i = _i;
    if (i === len) {
      return true;
    }
    if (!b(arr1[i], arr2[i])) {
      return false;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function every2U(a, b, p) {
  return everyAux2(a, b, 0, p, Caml.int_min(a.length, b.length));
}

function every2(a, b, p) {
  return every2U(a, b, (function (a, b) {
    return p(a, b);
  }));
}

function some2U(a, b, p) {
  let _i = 0;
  let len = Caml.int_min(a.length, b.length);
  while (true) {
    let i = _i;
    if (i === len) {
      return false;
    }
    if (p(a[i], b[i])) {
      return true;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function some2(a, b, p) {
  return some2U(a, b, (function (a, b) {
    return p(a, b);
  }));
}

function eqU(a, b, p) {
  let lena = a.length;
  let lenb = b.length;
  if (lena === lenb) {
    return everyAux2(a, b, 0, p, lena);
  } else {
    return false;
  }
}

function eq(a, b, p) {
  return eqU(a, b, (function (a, b) {
    return p(a, b);
  }));
}

function cmpU(a, b, p) {
  let lena = a.length;
  let lenb = b.length;
  if (lena > lenb) {
    return 1;
  } else if (lena < lenb) {
    return -1;
  } else {
    let _i = 0;
    while (true) {
      let i = _i;
      if (i === lena) {
        return 0;
      }
      let c = p(a[i], b[i]);
      if (c !== 0) {
        return c;
      }
      _i = i + 1 | 0;
      continue;
    };
  }
}

function cmp(a, b, p) {
  return cmpU(a, b, (function (a, b) {
    return p(a, b);
  }));
}

function partitionU(a, f) {
  let l = a.length;
  let i = 0;
  let j = 0;
  let a1 = new Array(l);
  let a2 = new Array(l);
  for (let ii = 0; ii < l; ++ii) {
    let v = a[ii];
    if (f(v)) {
      a1[i] = v;
      i = i + 1 | 0;
    } else {
      a2[j] = v;
      j = j + 1 | 0;
    }
  }
  a1.length = i;
  a2.length = j;
  return [
    a1,
    a2
  ];
}

function partition(a, f) {
  return partitionU(a, (function (x) {
    return f(x);
  }));
}

function unzip(a) {
  let l = a.length;
  let a1 = new Array(l);
  let a2 = new Array(l);
  for (let i = 0; i < l; ++i) {
    let match = a[i];
    a1[i] = match[0];
    a2[i] = match[1];
  }
  return [
    a1,
    a2
  ];
}

function joinWithU(a, sep, toString) {
  let l = a.length;
  if (l === 0) {
    return "";
  }
  let lastIndex = l - 1 | 0;
  let _i = 0;
  let _res = "";
  while (true) {
    let res = _res;
    let i = _i;
    if (i === lastIndex) {
      return res + toString(a[i]);
    }
    _res = res + (toString(a[i]) + sep);
    _i = i + 1 | 0;
    continue;
  };
}

function joinWith(a, sep, toString) {
  return joinWithU(a, sep, (function (x) {
    return toString(x);
  }));
}

function initU(n, f) {
  let v = new Array(n);
  for (let i = 0; i < n; ++i) {
    v[i] = f(i);
  }
  return v;
}

function init(n, f) {
  return initU(n, (function (i) {
    return f(i);
  }));
}

export {
  get,
  getExn,
  set,
  setExn,
  shuffleInPlace,
  shuffle,
  reverseInPlace,
  reverse,
  make,
  range,
  rangeBy,
  makeByU,
  makeBy,
  makeByAndShuffleU,
  makeByAndShuffle,
  zip,
  zipByU,
  zipBy,
  unzip,
  concat,
  concatMany,
  slice,
  sliceToEnd,
  fill,
  blit,
  blitUnsafe,
  forEachU,
  forEach,
  mapU,
  map,
  flatMapU,
  flatMap,
  getByU,
  getBy,
  getIndexByU,
  getIndexBy,
  keepU,
  keep,
  keepWithIndexU,
  keepWithIndex,
  keepMapU,
  keepMap,
  forEachWithIndexU,
  forEachWithIndex,
  mapWithIndexU,
  mapWithIndex,
  partitionU,
  partition,
  reduceU,
  reduce,
  reduceReverseU,
  reduceReverse,
  reduceReverse2U,
  reduceReverse2,
  reduceWithIndexU,
  reduceWithIndex,
  joinWithU,
  joinWith,
  someU,
  some,
  everyU,
  every,
  every2U,
  every2,
  some2U,
  some2,
  cmpU,
  cmp,
  eqU,
  eq,
  initU,
  init,
}
/* No side effect */
