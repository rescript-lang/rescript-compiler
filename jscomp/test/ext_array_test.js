// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let List = require("../../lib/js/list.js");
let $$Array = require("../../lib/js/array.js");
let Caml_array = require("../../lib/js/caml_array.js");
let Caml_option = require("../../lib/js/caml_option.js");

function reverse_range(a, i, len) {
  if (len === 0) {
    return;
  }
  for(let k = 0 ,k_finish = (len - 1 | 0) / 2 | 0; k <= k_finish; ++k){
    let t = a[i + k | 0];
    a[i + k | 0] = a[((i + len | 0) - 1 | 0) - k | 0];
    a[((i + len | 0) - 1 | 0) - k | 0] = t;
  }
}

function reverse_in_place(a) {
  reverse_range(a, 0, a.length);
}

function reverse(a) {
  let b_len = a.length;
  if (b_len === 0) {
    return [];
  }
  let b = $$Array.copy(a);
  for(let i = 0; i < b_len; ++i){
    b[i] = a[(b_len - 1 | 0) - i | 0];
  }
  return b;
}

function reverse_of_list(x) {
  if (!x) {
    return [];
  }
  let len = List.length(x);
  let a = Caml_array.make(len, x.hd);
  let _i = 0;
  let _x = x.tl;
  while(true) {
    let x$1 = _x;
    let i = _i;
    if (!x$1) {
      return a;
    }
    a[(len - i | 0) - 2 | 0] = x$1.hd;
    _x = x$1.tl;
    _i = i + 1 | 0;
    continue;
  };
}

function filter(f, a) {
  let arr_len = a.length;
  let _acc = /* [] */0;
  let _i = 0;
  while(true) {
    let i = _i;
    let acc = _acc;
    if (i === arr_len) {
      return reverse_of_list(acc);
    }
    let v = a[i];
    if (f(v)) {
      _i = i + 1 | 0;
      _acc = {
        hd: v,
        tl: acc
      };
      continue;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function filter_map(f, a) {
  let arr_len = a.length;
  let _acc = /* [] */0;
  let _i = 0;
  while(true) {
    let i = _i;
    let acc = _acc;
    if (i === arr_len) {
      return reverse_of_list(acc);
    }
    let v = a[i];
    let v$1 = f(v);
    if (v$1 !== undefined) {
      _i = i + 1 | 0;
      _acc = {
        hd: Caml_option.valFromOption(v$1),
        tl: acc
      };
      continue;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function range(from, to_) {
  if (from <= to_) {
    return $$Array.init((to_ - from | 0) + 1 | 0, (function (i) {
      return i + from | 0;
    }));
  }
  throw new Error("Invalid_argument", {
        cause: {
          RE_EXN_ID: "Invalid_argument",
          _1: "Ext_array_test.range"
        }
      });
}

function map2i(f, a, b) {
  let len = a.length;
  if (len === b.length) {
    return $$Array.mapi((function (i, a) {
      return f(i, a, b[i]);
    }), a);
  }
  throw new Error("Invalid_argument", {
        cause: {
          RE_EXN_ID: "Invalid_argument",
          _1: "Ext_array_test.map2i"
        }
      });
}

function tolist_aux(a, f, _i, _res) {
  while(true) {
    let res = _res;
    let i = _i;
    if (i < 0) {
      return res;
    }
    let v = a[i];
    let v$1 = f(v);
    _res = v$1 !== undefined ? ({
        hd: Caml_option.valFromOption(v$1),
        tl: res
      }) : res;
    _i = i - 1 | 0;
    continue;
  };
}

function to_list_map(f, a) {
  return tolist_aux(a, f, a.length - 1 | 0, /* [] */0);
}

function to_list_map_acc(f, a, acc) {
  return tolist_aux(a, f, a.length - 1 | 0, acc);
}

function of_list_map(f, a) {
  if (!a) {
    return [];
  }
  let tl = a.tl;
  let hd = f(a.hd);
  let len = List.length(tl) + 1 | 0;
  let arr = Caml_array.make(len, hd);
  let _i = 1;
  let _x = tl;
  while(true) {
    let x = _x;
    let i = _i;
    if (!x) {
      return arr;
    }
    arr[i] = f(x.hd);
    _x = x.tl;
    _i = i + 1 | 0;
    continue;
  };
}

function rfind_with_index(arr, cmp, v) {
  let len = arr.length;
  let _i = len - 1 | 0;
  while(true) {
    let i = _i;
    if (i < 0) {
      return i;
    }
    if (cmp(arr[i], v)) {
      return i;
    }
    _i = i - 1 | 0;
    continue;
  };
}

function rfind_and_split(arr, cmp, v) {
  let i = rfind_with_index(arr, cmp, v);
  if (i < 0) {
    return "No_split";
  } else {
    return {
      NAME: "Split",
      VAL: [
        $$Array.sub(arr, 0, i),
        $$Array.sub(arr, i + 1 | 0, (arr.length - i | 0) - 1 | 0)
      ]
    };
  }
}

function find_with_index(arr, cmp, v) {
  let len = arr.length;
  let _i = 0;
  while(true) {
    let i = _i;
    if (i >= len) {
      return -1;
    }
    if (cmp(arr[i], v)) {
      return i;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function find_and_split(arr, cmp, v) {
  let i = find_with_index(arr, cmp, v);
  if (i < 0) {
    return "No_split";
  } else {
    return {
      NAME: "Split",
      VAL: [
        $$Array.sub(arr, 0, i),
        $$Array.sub(arr, i + 1 | 0, (arr.length - i | 0) - 1 | 0)
      ]
    };
  }
}

function exists(p, a) {
  let n = a.length;
  let _i = 0;
  while(true) {
    let i = _i;
    if (i === n) {
      return false;
    }
    if (p(a[i])) {
      return true;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function is_empty(arr) {
  return arr.length === 0;
}

function unsafe_loop(_index, len, p, xs, ys) {
  while(true) {
    let index = _index;
    if (index >= len) {
      return true;
    }
    if (!p(xs[index], ys[index])) {
      return false;
    }
    _index = index + 1 | 0;
    continue;
  };
}

function for_all2_no_exn(p, xs, ys) {
  let len_xs = xs.length;
  let len_ys = ys.length;
  if (len_xs === len_ys) {
    return unsafe_loop(0, len_xs, p, xs, ys);
  } else {
    return false;
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
