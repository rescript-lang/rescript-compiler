'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function string_of_char(prim) {
  return String.fromCharCode(prim);
}

function caml_string_get(s, i) {
  if (i >= s.length || i < 0) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "index out of bounds"
        ];
  } else {
    return s.charCodeAt(i);
  }
}

function caml_create_string(len) {
  if (len < 0) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "String.create"
        ];
  } else {
    var result = new Array(len);
    for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
      result[i] = /* "\000" */0;
    }
    return result;
  }
}

function caml_fill_string(s, i, l, c) {
  if (l > 0) {
    for(var k = i ,k_finish = (l + i | 0) - 1 | 0; k <= k_finish; ++k){
      s[k] = c;
    }
    return /* () */0;
  } else {
    return 0;
  }
}

function caml_blit_string(s1, i1, s2, i2, len) {
  if (len > 0) {
    var off1 = s1.length - i1 | 0;
    if (len <= off1) {
      for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
        s2[i2 + i | 0] = s1.charCodeAt(i1 + i | 0);
      }
      return /* () */0;
    } else {
      for(var i$1 = 0 ,i_finish$1 = off1 - 1 | 0; i$1 <= i_finish$1; ++i$1){
        s2[i2 + i$1 | 0] = s1.charCodeAt(i1 + i$1 | 0);
      }
      for(var i$2 = off1 ,i_finish$2 = len - 1 | 0; i$2 <= i_finish$2; ++i$2){
        s2[i2 + i$2 | 0] = /* "\000" */0;
      }
      return /* () */0;
    }
  } else {
    return 0;
  }
}

function caml_blit_bytes(s1, i1, s2, i2, len) {
  if (len > 0) {
    if (s1 === s2) {
      var s1$1 = s1;
      var i1$1 = i1;
      var i2$1 = i2;
      var len$1 = len;
      if (i1$1 < i2$1) {
        var range_a = (s1$1.length - i2$1 | 0) - 1 | 0;
        var range_b = len$1 - 1 | 0;
        var range = range_a > range_b ? range_b : range_a;
        for(var j = range; j >= 0; --j){
          s1$1[i2$1 + j | 0] = s1$1[i1$1 + j | 0];
        }
        return /* () */0;
      } else if (i1$1 > i2$1) {
        var range_a$1 = (s1$1.length - i1$1 | 0) - 1 | 0;
        var range_b$1 = len$1 - 1 | 0;
        var range$1 = range_a$1 > range_b$1 ? range_b$1 : range_a$1;
        for(var k = 0; k <= range$1; ++k){
          s1$1[i2$1 + k | 0] = s1$1[i1$1 + k | 0];
        }
        return /* () */0;
      } else {
        return 0;
      }
    } else {
      var off1 = s1.length - i1 | 0;
      if (len <= off1) {
        for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
          s2[i2 + i | 0] = s1[i1 + i | 0];
        }
        return /* () */0;
      } else {
        for(var i$1 = 0 ,i_finish$1 = off1 - 1 | 0; i$1 <= i_finish$1; ++i$1){
          s2[i2 + i$1 | 0] = s1[i1 + i$1 | 0];
        }
        for(var i$2 = off1 ,i_finish$2 = len - 1 | 0; i$2 <= i_finish$2; ++i$2){
          s2[i2 + i$2 | 0] = /* "\000" */0;
        }
        return /* () */0;
      }
    }
  } else {
    return 0;
  }
}

function bytes_of_string(s) {
  var len = s.length;
  var res = new Array(len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    res[i] = s.charCodeAt(i);
  }
  return res;
}

function bytes_to_string(a) {
  var bytes = a;
  var i = 0;
  var len = a.length;
  var s = "";
  var s_len = len;
  if (i === 0 && len <= 4096 && len === bytes.length) {
    return String.fromCharCode.apply(null, bytes);
  } else {
    var offset = 0;
    while(s_len > 0) {
      var next = s_len < 1024 ? s_len : 1024;
      var tmp_bytes = new Array(next);
      caml_blit_bytes(bytes, offset, tmp_bytes, 0, next);
      s = s + String.fromCharCode.apply(null, tmp_bytes);
      s_len = s_len - next | 0;
      offset = offset + next | 0;
    };
    return s;
  }
}

function caml_string_of_char_array(chars) {
  var len = chars.length;
  var bytes = new Array(len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    bytes[i] = chars[i];
  }
  return bytes_to_string(bytes);
}

function caml_is_printable(c) {
  if (c > 31) {
    return c < 127;
  } else {
    return false;
  }
}

function caml_string_get16(s, i) {
  return s.charCodeAt(i) + (s.charCodeAt(i + 1 | 0) << 8) | 0;
}

function caml_string_get32(s, i) {
  return ((s.charCodeAt(i) + (s.charCodeAt(i + 1 | 0) << 8) | 0) + (s.charCodeAt(i + 2 | 0) << 16) | 0) + (s.charCodeAt(i + 3 | 0) << 24) | 0;
}

function get(s, i) {
  if (i < 0 || i >= s.length) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "index out of bounds"
        ];
  } else {
    return s.charCodeAt(i);
  }
}

exports.bytes_of_string = bytes_of_string;
exports.bytes_to_string = bytes_to_string;
exports.caml_is_printable = caml_is_printable;
exports.caml_string_of_char_array = caml_string_of_char_array;
exports.caml_string_get = caml_string_get;
exports.caml_create_string = caml_create_string;
exports.caml_fill_string = caml_fill_string;
exports.caml_blit_string = caml_blit_string;
exports.caml_blit_bytes = caml_blit_bytes;
exports.caml_string_get16 = caml_string_get16;
exports.caml_string_get32 = caml_string_get32;
exports.string_of_char = string_of_char;
exports.get = get;
/* No side effect */
