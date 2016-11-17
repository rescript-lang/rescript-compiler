'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");
var Curry                   = require("../../lib/js/curry");
var Caml_array              = require("../../lib/js/caml_array");
var $$Array                 = require("../../lib/js/array");
var List                    = require("../../lib/js/list");

function reverse_in_place(a) {
  var a$1 = a;
  var i = 0;
  var len = a.length;
  if (len) {
    for(var k = 0 ,k_finish = (len - 1 | 0) / 2 | 0; k <= k_finish; ++k){
      var t = a$1[i + k | 0];
      a$1[i + k | 0] = a$1[((i + len | 0) - 1 | 0) - k | 0];
      a$1[((i + len | 0) - 1 | 0) - k | 0] = t;
    }
    return /* () */0;
  }
  else {
    return /* () */0;
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
        
      }
      else {
        return a;
      }
    };
  }
  else {
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
    }
    else {
      var v = a[i];
      if (Curry._1(f, v)) {
        _i = i + 1 | 0;
        _acc = /* :: */[
          v,
          acc
        ];
        continue ;
        
      }
      else {
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
    }
    else {
      var v = a[i];
      var match = Curry._1(f, v);
      _i = i + 1 | 0;
      if (match) {
        _acc = /* :: */[
          match[0],
          acc
        ];
        continue ;
        
      }
      else {
        continue ;
        
      }
    }
  };
}

function range(from, to_) {
  if (from > to_) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Ext_array.range"
        ];
  }
  else {
    return $$Array.init((to_ - from | 0) + 1 | 0, function (i) {
                return i + from | 0;
              });
  }
}

function map2i(f, a, b) {
  var len = a.length;
  if (len !== b.length) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Ext_array.map2i"
        ];
  }
  else {
    return $$Array.mapi(function (i, a) {
                return Curry._3(f, i, a, b[i]);
              }, a);
  }
}

function to_list_map(f, a) {
  var _i = a.length - 1 | 0;
  var _res = /* [] */0;
  while(true) {
    var res = _res;
    var i = _i;
    if (i < 0) {
      return res;
    }
    else {
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

function rfind_with_index(arr, cmp, v) {
  var len = arr.length;
  var _i = len - 1 | 0;
  while(true) {
    var i = _i;
    if (i < 0) {
      return i;
    }
    else if (Curry._2(cmp, arr[i], v)) {
      return i;
    }
    else {
      _i = i - 1 | 0;
      continue ;
      
    }
  };
}

function rfind_and_split(arr, cmp, v) {
  var i = rfind_with_index(arr, cmp, v);
  if (i < 0) {
    return /* No_split */-226265796;
  }
  else {
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
    }
    else if (Curry._2(cmp, arr[i], v)) {
      return i;
    }
    else {
      _i = i + 1 | 0;
      continue ;
      
    }
  };
}

function find_and_split(arr, cmp, v) {
  var i = find_with_index(arr, cmp, v);
  if (i < 0) {
    return /* No_split */-226265796;
  }
  else {
    return /* `Split */[
            345791162,
            /* tuple */[
              $$Array.sub(arr, 0, i),
              $$Array.sub(arr, i + 1 | 0, (arr.length - i | 0) - 1 | 0)
            ]
          ];
  }
}

exports.reverse_in_place = reverse_in_place;
exports.reverse_of_list  = reverse_of_list;
exports.filter           = filter;
exports.filter_map       = filter_map;
exports.range            = range;
exports.map2i            = map2i;
exports.to_list_map      = to_list_map;
exports.rfind_with_index = rfind_with_index;
exports.rfind_and_split  = rfind_and_split;
exports.find_with_index  = find_with_index;
exports.find_and_split   = find_and_split;
/* No side effect */
