

import * as Curry from "./curry.js";
import * as Caml_builtin_exceptions from "./caml_builtin_exceptions.js";

function merge(order, l1, l2) {
  if (!l1) {
    return l2;
  }
  if (!l2) {
    return l1;
  }
  var h2 = l2[0];
  var h1 = l1[0];
  if (Curry._2(order, h1, h2)) {
    return /* :: */[
            h1,
            merge(order, l1[1], l2)
          ];
  } else {
    return /* :: */[
            h2,
            merge(order, l1, l2[1])
          ];
  }
}

function list(order, l) {
  var initlist = function (param) {
    if (!param) {
      return /* [] */0;
    }
    var match = param[1];
    var e = param[0];
    if (!match) {
      return /* :: */[
              /* :: */[
                e,
                /* [] */0
              ],
              /* [] */0
            ];
    }
    var e2 = match[0];
    return /* :: */[
            Curry._2(order, e, e2) ? /* :: */[
                e,
                /* :: */[
                  e2,
                  /* [] */0
                ]
              ] : /* :: */[
                e2,
                /* :: */[
                  e,
                  /* [] */0
                ]
              ],
            initlist(match[1])
          ];
  };
  var merge2 = function (x) {
    if (!x) {
      return x;
    }
    var match = x[1];
    if (match) {
      return /* :: */[
              merge(order, x[0], match[0]),
              merge2(match[1])
            ];
    } else {
      return x;
    }
  };
  var _llist = initlist(l);
  while(true) {
    var llist = _llist;
    if (!llist) {
      return /* [] */0;
    }
    if (!llist[1]) {
      return llist[0];
    }
    _llist = merge2(llist);
    continue ;
  };
}

function swap(arr, i, j) {
  var tmp = arr[i];
  arr[i] = arr[j];
  arr[j] = tmp;
  
}

function array(cmp, arr) {
  var qsort = function (_lo, _hi) {
    while(true) {
      var hi = _hi;
      var lo = _lo;
      if ((hi - lo | 0) < 6) {
        return ;
      }
      var mid = ((lo + hi | 0) >>> 1);
      if (Curry._2(cmp, arr[mid], arr[lo])) {
        swap(arr, mid, lo);
      }
      if (Curry._2(cmp, arr[hi], arr[mid])) {
        swap(arr, mid, hi);
        if (Curry._2(cmp, arr[mid], arr[lo])) {
          swap(arr, mid, lo);
        }
        
      }
      var pivot = arr[mid];
      var i = lo + 1 | 0;
      var j = hi - 1 | 0;
      if (!Curry._2(cmp, pivot, arr[hi]) || !Curry._2(cmp, arr[lo], pivot)) {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Sort.array"
            ];
      }
      while(i < j) {
        while(!Curry._2(cmp, pivot, arr[i])) {
          i = i + 1 | 0;
        };
        while(!Curry._2(cmp, arr[j], pivot)) {
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
        continue ;
      }
      qsort(i, hi);
      _hi = j;
      continue ;
    };
  };
  qsort(0, arr.length - 1 | 0);
  for(var i = 1 ,i_finish = arr.length - 1 | 0; i <= i_finish; ++i){
    var val_i = arr[i];
    if (!Curry._2(cmp, arr[i - 1 | 0], val_i)) {
      arr[i] = arr[i - 1 | 0];
      var j = i - 1 | 0;
      while(j >= 1 && !Curry._2(cmp, arr[j - 1 | 0], val_i)) {
        arr[j] = arr[j - 1 | 0];
        j = j - 1 | 0;
      };
      arr[j] = val_i;
    }
    
  }
  
}

export {
  list ,
  array ,
  merge ,
  
}
/* No side effect */
