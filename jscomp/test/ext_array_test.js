'use strict';

var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function reverse_range(a, i, len) {
  if (len === 0) {
    return /* () */0;
  } else {
    for(var k = 0 ,k_finish = (len - 1 | 0) / 2 | 0; k <= k_finish; ++k){
      var t = a[i + k | 0];
      a[i + k | 0] = a[((i + len | 0) - 1 | 0) - k | 0];
      a[((i + len | 0) - 1 | 0) - k | 0] = t;
    }
    return /* () */0;
  }
}

function reverse_in_place(a) {
  return reverse_range(a, 0, a.length);
}

function reverse(a) {
  var b_len = a.length;
  if (b_len === 0) {
    return /* array */[];
  } else {
    var b = $$Array.copy(a);
    for(var i = 0 ,i_finish = b_len - 1 | 0; i <= i_finish; ++i){
      b[i] = a[(b_len - 1 | 0) - i | 0];
    }
    return b;
  }
}

function reverse_of_list(l) {
  if (l) {
    var len = List.length(l);
    var a = Caml_array.caml_make_vect(len, l[0]);
    var _i = 0;
    var _param = l[1];
    while(true) {
      var param = _param;
      var i = _i;
      if (param) {
        a[(len - i | 0) - 2 | 0] = param[0];
        _param = param[1];
        _i = i + 1 | 0;
        continue ;
      } else {
        return a;
      }
    };
  } else {
    return /* array */[];
  }
}

function filter(f, a) {
  var arr_len = a.length;
  var _acc = /* [] */0;
  var _i = 0;
  while(true) {
    var i = _i;
    var acc = _acc;
    if (i === arr_len) {
      return reverse_of_list(acc);
    } else {
      var v = a[i];
      if (Curry._1(f, v)) {
        _i = i + 1 | 0;
        _acc = /* :: */[
          v,
          acc
        ];
        continue ;
      } else {
        _i = i + 1 | 0;
        continue ;
      }
    }
  };
}

function filter_map(f, a) {
  var arr_len = a.length;
  var _acc = /* [] */0;
  var _i = 0;
  while(true) {
    var i = _i;
    var acc = _acc;
    if (i === arr_len) {
      return reverse_of_list(acc);
    } else {
      var v = a[i];
      var match = Curry._1(f, v);
      _i = i + 1 | 0;
      if (match) {
        _acc = /* :: */[
          match[0],
          acc
        ];
        continue ;
      } else {
        continue ;
      }
    }
  };
}

function range(from, to_) {
  if (from > to_) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Ext_array_test.range"
        ];
  } else {
    return $$Array.init((to_ - from | 0) + 1 | 0, (function (i) {
                  return i + from | 0;
                }));
  }
}

function map2i(f, a, b) {
  var len = a.length;
  if (len !== b.length) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Ext_array_test.map2i"
        ];
  } else {
    return $$Array.mapi((function (i, a) {
                  return Curry._3(f, i, a, b[i]);
                }), a);
  }
}

function tolist_aux(a, f, _i, _res) {
  while(true) {
    var res = _res;
    var i = _i;
    if (i < 0) {
      return res;
    } else {
      var v = a[i];
      var match = Curry._1(f, v);
      _res = match ? /* :: */[
          match[0],
          res
        ] : res;
      _i = i - 1 | 0;
      continue ;
    }
  };
}

function to_list_map(f, a) {
  return tolist_aux(a, f, a.length - 1 | 0, /* [] */0);
}

function to_list_map_acc(f, a, acc) {
  return tolist_aux(a, f, a.length - 1 | 0, acc);
}

function of_list_map(f, a) {
  if (a) {
    var tl = a[1];
    var hd = Curry._1(f, a[0]);
    var len = List.length(tl) + 1 | 0;
    var arr = Caml_array.caml_make_vect(len, hd);
    var _i = 1;
    var _param = tl;
    while(true) {
      var param = _param;
      var i = _i;
      if (param) {
        arr[i] = Curry._1(f, param[0]);
        _param = param[1];
        _i = i + 1 | 0;
        continue ;
      } else {
        return arr;
      }
    };
  } else {
    return /* array */[];
  }
}

function rfind_with_index(arr, cmp, v) {
  var len = arr.length;
  var _i = len - 1 | 0;
  while(true) {
    var i = _i;
    if (i < 0 || Curry._2(cmp, arr[i], v)) {
      return i;
    } else {
      _i = i - 1 | 0;
      continue ;
    }
  };
}

function rfind_and_split(arr, cmp, v) {
  var i = rfind_with_index(arr, cmp, v);
  if (i < 0) {
    return /* No_split */-226265796;
  } else {
    return /* `Split */[
            345791162,
            /* tuple */[
              $$Array.sub(arr, 0, i),
              $$Array.sub(arr, i + 1 | 0, (arr.length - i | 0) - 1 | 0)
            ]
          ];
  }
}

function find_with_index(arr, cmp, v) {
  var len = arr.length;
  var _i = 0;
  var len$1 = len;
  while(true) {
    var i = _i;
    if (i >= len$1) {
      return -1;
    } else if (Curry._2(cmp, arr[i], v)) {
      return i;
    } else {
      _i = i + 1 | 0;
      continue ;
    }
  };
}

function find_and_split(arr, cmp, v) {
  var i = find_with_index(arr, cmp, v);
  if (i < 0) {
    return /* No_split */-226265796;
  } else {
    return /* `Split */[
            345791162,
            /* tuple */[
              $$Array.sub(arr, 0, i),
              $$Array.sub(arr, i + 1 | 0, (arr.length - i | 0) - 1 | 0)
            ]
          ];
  }
}

function exists(p, a) {
  var n = a.length;
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === n) {
      return /* false */0;
    } else if (Curry._1(p, a[i])) {
      return /* true */1;
    } else {
      _i = i + 1 | 0;
      continue ;
    }
  };
}

function is_empty(arr) {
  return +(arr.length === 0);
}

function unsafe_loop(_index, len, p, xs, ys) {
  while(true) {
    var index = _index;
    if (index >= len) {
      return /* true */1;
    } else if (Curry._2(p, xs[index], ys[index])) {
      _index = index + 1 | 0;
      continue ;
    } else {
      return /* false */0;
    }
  };
}

function for_all2_no_exn(p, xs, ys) {
  var len_xs = xs.length;
  var len_ys = ys.length;
  if (len_xs === len_ys) {
    return unsafe_loop(0, len_xs, p, xs, ys);
  } else {
    return /* false */0;
  }
}

exports.reverse_range = reverse_range;
exports.reverse_in_place = reverse_in_place;
exports.reverse = reverse;
exports.reverse_of_list = reverse_of_list;
exports.filter = filter;
exports.filter_map = filter_map;
exports.range = range;
exports.map2i = map2i;
exports.tolist_aux = tolist_aux;
exports.to_list_map = to_list_map;
exports.to_list_map_acc = to_list_map_acc;
exports.of_list_map = of_list_map;
exports.rfind_with_index = rfind_with_index;
exports.rfind_and_split = rfind_and_split;
exports.find_with_index = find_with_index;
exports.find_and_split = find_and_split;
exports.exists = exists;
exports.is_empty = is_empty;
exports.unsafe_loop = unsafe_loop;
exports.for_all2_no_exn = for_all2_no_exn;
/* No side effect */
