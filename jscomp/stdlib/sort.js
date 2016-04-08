// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_curry              = require("../runtime/caml_curry");

function merge(order, l1, l2) {
  if (l1) {
    var h1 = l1[0];
    if (l2) {
      var h2 = l2[0];
      if (Caml_curry.app2(order, h1, h2)) {
        return /* :: */[
                h1,
                merge(order, l1[1], l2)
              ];
      }
      else {
        return /* :: */[
                h2,
                merge(order, l1, l2[1])
              ];
      }
    }
    else {
      return l1;
    }
  }
  else {
    return l2;
  }
}

function list(order, l) {
  var initlist = function (param) {
    if (param) {
      var match = param[1];
      var e = param[0];
      if (match) {
        var e2 = match[0];
        return /* :: */[
                Caml_curry.app2(order, e, e2) ? /* :: */[
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
      }
      else {
        return /* :: */[
                /* :: */[
                  e,
                  /* [] */0
                ],
                /* [] */0
              ];
      }
    }
    else {
      return /* [] */0;
    }
  };
  var merge2 = function (x) {
    if (x) {
      var match = x[1];
      if (match) {
        return /* :: */[
                merge(order, x[0], match[0]),
                merge2(match[1])
              ];
      }
      else {
        return x;
      }
    }
    else {
      return x;
    }
  };
  var _llist = initlist(l);
  while(true) {
    var llist = _llist;
    if (llist) {
      if (llist[1]) {
        _llist = merge2(llist);
        continue ;
        
      }
      else {
        return llist[0];
      }
    }
    else {
      return /* [] */0;
    }
  };
}

function swap(arr, i, j) {
  var tmp = arr[i];
  arr[i] = arr[j];
  arr[j] = tmp;
  return /* () */0;
}

function array(cmp, arr) {
  var qsort = function (_lo, _hi) {
    while(true) {
      var hi = _hi;
      var lo = _lo;
      if (hi - lo >= 6) {
        var mid = ((lo + hi | 0) >>> 1);
        if (Caml_curry.app2(cmp, arr[mid], arr[lo])) {
          swap(arr, mid, lo);
        }
        if (Caml_curry.app2(cmp, arr[hi], arr[mid])) {
          swap(arr, mid, hi);
          if (Caml_curry.app2(cmp, arr[mid], arr[lo])) {
            swap(arr, mid, lo);
          }
          
        }
        var pivot = arr[mid];
        var i = lo + 1 | 0;
        var j = hi - 1;
        if (!Caml_curry.app2(cmp, pivot, arr[hi]) || !Caml_curry.app2(cmp, arr[lo], pivot)) {
          throw [
                Caml_builtin_exceptions.invalid_argument,
                "Sort.array"
              ];
        }
        while(i < j) {
          while(!Caml_curry.app2(cmp, pivot, arr[i])) {
            i = i + 1 | 0;
          };
          while(!Caml_curry.app2(cmp, arr[j], pivot)) {
            j = j - 1 | 0;
          };
          if (i < j) {
            swap(arr, i, j);
          }
          i = i + 1 | 0;
          j = j - 1 | 0;
        };
        if (j - lo <= hi - i) {
          qsort(lo, j);
          _lo = i;
          continue ;
          
        }
        else {
          qsort(i, hi);
          _hi = j;
          continue ;
          
        }
      }
      else {
        return 0;
      }
    };
  };
  qsort(0, arr.length - 1);
  for(var i = 1 ,i_finish = arr.length - 1; i<= i_finish; ++i){
    var val_i = arr[i];
    if (!Caml_curry.app2(cmp, arr[i - 1], val_i)) {
      arr[i] = arr[i - 1];
      var j = i - 1;
      while(j >= 1 && !Caml_curry.app2(cmp, arr[j - 1], val_i)) {
        arr[j] = arr[j - 1];
        j = j - 1 | 0;
      };
      arr[j] = val_i;
    }
    
  }
  return /* () */0;
}

exports.list  = list;
exports.array = array;
exports.merge = merge;
/* No side effect */
