'use strict';


function merge(order, l1, l2) {
  if (!l1) {
    return l2;
  }
  if (!l2) {
    return l1;
  }
  let h2 = l2.hd;
  let h1 = l1.hd;
  if (order(h1, h2)) {
    return {
      hd: h1,
      tl: merge(order, l1.tl, l2)
    };
  } else {
    return {
      hd: h2,
      tl: merge(order, l1, l2.tl)
    };
  }
}

function list(order, l) {
  let initlist = function (param) {
    if (!param) {
      return /* [] */0;
    }
    let match = param.tl;
    let e = param.hd;
    if (!match) {
      return {
        hd: {
          hd: e,
          tl: /* [] */0
        },
        tl: /* [] */0
      };
    }
    let e2 = match.hd;
    return {
      hd: order(e, e2) ? ({
          hd: e,
          tl: {
            hd: e2,
            tl: /* [] */0
          }
        }) : ({
          hd: e2,
          tl: {
            hd: e,
            tl: /* [] */0
          }
        }),
      tl: initlist(match.tl)
    };
  };
  let merge2 = function (param) {
    if (!param) {
      return param;
    }
    let match = param.tl;
    if (match) {
      return {
        hd: merge(order, param.hd, match.hd),
        tl: merge2(match.tl)
      };
    } else {
      return param;
    }
  };
  let _param = initlist(l);
  while (true) {
    let param = _param;
    if (!param) {
      return /* [] */0;
    }
    if (!param.tl) {
      return param.hd;
    }
    _param = merge2(param);
    continue;
  };
}

function swap(arr, i, j) {
  let tmp = arr[i];
  arr[i] = arr[j];
  arr[j] = tmp;
}

function array(cmp, arr) {
  let qsort = function (_lo, _hi) {
    while (true) {
      let hi = _hi;
      let lo = _lo;
      if ((hi - lo | 0) < 6) {
        return;
      }
      let mid = ((lo + hi | 0) >>> 1);
      if (cmp(arr[mid], arr[lo])) {
        swap(arr, mid, lo);
      }
      if (cmp(arr[hi], arr[mid])) {
        swap(arr, mid, hi);
        if (cmp(arr[mid], arr[lo])) {
          swap(arr, mid, lo);
        }
        
      }
      let pivot = arr[mid];
      let i = lo + 1 | 0;
      let j = hi - 1 | 0;
      if (!cmp(pivot, arr[hi]) || !cmp(arr[lo], pivot)) {
        throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "Sort.array"
          }
        });
      }
      while (i < j) {
        while (!cmp(pivot, arr[i])) {
          i = i + 1 | 0;
        };
        while (!cmp(arr[j], pivot)) {
          j = j - 1 | 0;
        };
        if (i < j) {
          swap(arr, i, j);
        }
        i = i + 1 | 0;
        j = j - 1 | 0;
      };
      if ((j - lo | 0) <= (hi - i | 0)) {
        qsort(lo, j);
        _lo = i;
        continue;
      }
      qsort(i, hi);
      _hi = j;
      continue;
    };
  };
  qsort(0, arr.length - 1 | 0);
  for (let i = 1, i_finish = arr.length; i < i_finish; ++i) {
    let val_i = arr[i];
    if (!cmp(arr[i - 1 | 0], val_i)) {
      arr[i] = arr[i - 1 | 0];
      let j = i - 1 | 0;
      while (j >= 1 && !cmp(arr[j - 1 | 0], val_i)) {
        arr[j] = arr[j - 1 | 0];
        j = j - 1 | 0;
      };
      arr[j] = val_i;
    }
    
  }
}

exports.list = list;
exports.array = array;
exports.merge = merge;
/* No side effect */
