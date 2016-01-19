// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_exceptions = require("../runtime/caml_exceptions");

function merge(order, l1, l2) {
  if (l1) {
    var h1 = l1[1];
    if (l2) {
      var h2 = l2[1];
      if (order(h1, h2)) {
        return [
                /* :: */0,
                h1,
                merge(order, l1[2], l2)
              ];
      }
      else {
        return [
                /* :: */0,
                h2,
                merge(order, l1, l2[2])
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
      var match = param[2];
      var e = param[1];
      if (match) {
        var e2 = match[1];
        return [
                /* :: */0,
                order(e, e2) ? [
                    /* :: */0,
                    e,
                    [
                      /* :: */0,
                      e2,
                      /* [] */0
                    ]
                  ] : [
                    /* :: */0,
                    e2,
                    [
                      /* :: */0,
                      e,
                      /* [] */0
                    ]
                  ],
                initlist(match[2])
              ];
      }
      else {
        return [
                /* :: */0,
                [
                  /* :: */0,
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
      var match = x[2];
      if (match) {
        return [
                /* :: */0,
                merge(order, x[1], match[1]),
                merge2(match[2])
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
  var mergeall = function (_llist) {
    while(/* true */1) {
      var llist = _llist;
      if (llist) {
        if (llist[2]) {
          _llist = merge2(llist);
        }
        else {
          return llist[1];
        }
      }
      else {
        return /* [] */0;
      }
    };
  };
  return mergeall(initlist(l));
}

function swap(arr, i, j) {
  var tmp = arr[i];
  arr[i] = arr[j];
  arr[j] = tmp;
  return /* () */0;
}

function array(cmp, arr) {
  var qsort = function (_lo, _hi) {
    while(/* true */1) {
      var hi = _hi;
      var lo = _lo;
      if (hi - lo >= 6) {
        var mid = (lo + hi >>> 1);
        if (cmp(arr[mid], arr[lo])) {
          swap(arr, mid, lo);
        }
        if (cmp(arr[hi], arr[mid])) {
          swap(arr, mid, hi);
          if (cmp(arr[mid], arr[lo])) {
            swap(arr, mid, lo);
          }
          
        }
        var pivot = arr[mid];
        var i = lo + 1;
        var j = hi - 1;
        if (!cmp(pivot, arr[hi]) || !cmp(arr[lo], pivot)) {
          throw [
                0,
                Caml_exceptions.Invalid_argument,
                "Sort.array"
              ];
        }
        while(i < j) {
          while(!cmp(pivot, arr[i])) {
            ++ i;
          };
          while(!cmp(arr[j], pivot)) {
            -- j;
          };
          if (i < j) {
            swap(arr, i, j);
          }
          ++ i;
          -- j;
        };
        if (j - lo <= hi - i) {
          qsort(lo, j);
          _lo = i;
        }
        else {
          qsort(i, hi);
          _hi = j;
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
    if (!cmp(arr[i - 1], val_i)) {
      arr[i] = arr[i - 1];
      var j = i - 1;
      while(j >= 1 && !cmp(arr[j - 1], val_i)) {
        arr[j] = arr[j - 1];
        -- j;
      };
      arr[j] = val_i;
    }
    
  }
  return /* () */0;
}

exports.list = list;
exports.array = array;
exports.merge = merge;
/* No side effect */
