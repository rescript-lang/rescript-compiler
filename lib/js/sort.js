'use strict';

var Curry = require("./curry.js");
var Pervasives = require("./pervasives.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function merge(order, l1, l2) {
  if (l1) {
    if (l2) {
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
    } else {
      return l1;
    }
  } else {
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
      } else {
        return /* :: */[
                /* :: */[
                  e,
                  /* [] */0
                ],
                /* [] */0
              ];
      }
    } else {
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
      } else {
        return x;
      }
    } else {
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
      } else {
        return llist[0];
      }
    } else {
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
      if ((hi - lo | 0) >= 6) {
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
        var i = /* record */{
          contents: lo + 1 | 0
        };
        var j = /* record */{
          contents: hi - 1 | 0
        };
        if (!Curry._2(cmp, pivot, arr[hi]) || !Curry._2(cmp, arr[lo], pivot)) {
          throw [
                Caml_builtin_exceptions.invalid_argument,
                "Sort.array"
              ];
        }
        while(i.contents < j.contents) {
          while(!Curry._2(cmp, pivot, arr[i.contents])) {
            Pervasives.incr(i);
          };
          while(!Curry._2(cmp, arr[j.contents], pivot)) {
            Pervasives.decr(j);
          };
          if (i.contents < j.contents) {
            swap(arr, i.contents, j.contents);
          }
          Pervasives.incr(i);
          Pervasives.decr(j);
        };
        if ((j.contents - lo | 0) <= (hi - i.contents | 0)) {
          qsort(lo, j.contents);
          _lo = i.contents;
          continue ;
        } else {
          qsort(i.contents, hi);
          _hi = j.contents;
          continue ;
        }
      } else {
        return 0;
      }
    };
  };
  qsort(0, arr.length - 1 | 0);
  for(var i = 1 ,i_finish = arr.length - 1 | 0; i <= i_finish; ++i){
    var val_i = arr[i];
    if (!Curry._2(cmp, arr[i - 1 | 0], val_i)) {
      arr[i] = arr[i - 1 | 0];
      var j = /* record */{
        contents: i - 1 | 0
      };
      while(j.contents >= 1 && !Curry._2(cmp, arr[j.contents - 1 | 0], val_i)) {
        arr[j.contents] = arr[j.contents - 1 | 0];
        Pervasives.decr(j);
      };
      arr[j.contents] = val_i;
    }
    
  }
  return /* () */0;
}

exports.list = list;
exports.array = array;
exports.merge = merge;
/* No side effect */
