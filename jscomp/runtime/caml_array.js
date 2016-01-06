// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

function caml_array_sub(x, offset, len) {
  var result = new Array(len);
  var j = 0;
  var i = offset;
  while(j < len) {
    result[j] = x[i];
    ++ j;
    ++ i;
  };
  return result;
}

function len(_acc, _l) {
  while(/* true */1) {
    var l = _l;
    var acc = _acc;
    if (l) {
      _l = l[2];
      _acc = l[1].length + acc;
    }
    else {
      return acc;
    }
  };
}

function fill(arr, _i, _l) {
  while(/* true */1) {
    var l = _l;
    var i = _i;
    if (l) {
      var x = l[1];
      var l$1 = x.length;
      var k = i;
      var j = 0;
      while(j < l$1) {
        arr[k] = x[j];
        ++ k;
        ++ j;
      };
      _l = l[2];
      _i = k;
    }
    else {
      return /* () */0;
    }
  };
}

function caml_array_concat(l) {
  var v = len(0, l);
  var result = new Array(v);
  fill(result, 0, l);
  return result;
}

function caml_make_vect(len, init) {
  var b = new Array(len);
  for(var i = 0 ,i_finish = len - 1; i<= i_finish; ++i){
    b[i] = init;
  }
  return b;
}

function caml_array_blit(a1, i1, a2, i2, len) {
  if (i2 <= i1) {
    for(var j = 0 ,j_finish = len - 1; j<= j_finish; ++j){
      a2[j + i2] = a1[j + i1];
    }
    return /* () */0;
  }
  else {
    for(var j$1 = len - 1; j$1>= 0; --j$1){
      a2[j$1 + i2] = a1[j$1 + i1];
    }
    return /* () */0;
  }
}

exports.caml_array_sub = caml_array_sub;
exports.caml_array_concat = caml_array_concat;
exports.caml_make_vect = caml_make_vect;
exports.caml_array_blit = caml_array_blit;
/* No side effect */
