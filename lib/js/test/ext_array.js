// GENERATED CODE BY BUCKLESCRIPT VERSION 0.6.0 , PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../caml_builtin_exceptions");
var Curry                   = require("../curry");
var Caml_array              = require("../caml_array");
var $$Array                 = require("../array");
var List                    = require("../list");

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

exports.reverse_in_place = reverse_in_place;
exports.reverse_of_list  = reverse_of_list;
exports.filter           = filter;
exports.filter_map       = filter_map;
exports.range            = range;
exports.map2i            = map2i;
/* No side effect */
