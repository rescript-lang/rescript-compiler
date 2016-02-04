// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
define(["./caml_exceptions"],
  function(Caml_exceptions){
    'use strict';
    function add(prim, prim$1) {
      return prim + prim$1;
    }
    
    function caml_string_get(s, i) {
      if (i >= s.length || i < 0) {
        throw [
              0,
              Caml_exceptions.Invalid_argument,
              "index out of bounds"
            ];
      }
      else {
        return s.charCodeAt(i);
      }
    }
    
    function caml_create_string(len) {
      if (len < 0) {
        throw [
              0,
              Caml_exceptions.Invalid_argument,
              "String.create"
            ];
      }
      else {
        return new Array(len);
      }
    }
    
    function caml_string_compare(s1, s2) {
      if (s1 === s2) {
        return 0;
      }
      else if (s1 < s2) {
        return -1;
      }
      else {
        return 1;
      }
    }
    
    function caml_fill_string(s, i, l, c) {
      if (l > 0) {
        for(var k = i ,k_finish = l + i - 1; k<= k_finish; ++k){
          s[k] = c;
        }
        return /* () */0;
      }
      else {
        return 0;
      }
    }
    
    function caml_blit_string(s1, i1, s2, i2, len) {
      if (len > 0) {
        var off1 = s1.length - i1;
        if (len <= off1) {
          for(var i = 0 ,i_finish = len - 1; i<= i_finish; ++i){
            s2[i2 + i] = s1.charCodeAt(i1 + i);
          }
          return /* () */0;
        }
        else {
          for(var i$1 = 0 ,i_finish$1 = off1 - 1; i$1<= i_finish$1; ++i$1){
            s2[i2 + i$1] = s1.charCodeAt(i1 + i$1);
          }
          for(var i$2 = off1 ,i_finish$2 = len - 1; i$2<= i_finish$2; ++i$2){
            s2[i2 + i$2] = /* "\000" */0;
          }
          return /* () */0;
        }
      }
      else {
        return 0;
      }
    }
    
    function caml_blit_bytes(s1, i1, s2, i2, len) {
      if (len > 0) {
        var off1 = s1.length - i1;
        if (len <= off1) {
          for(var i = 0 ,i_finish = len - 1; i<= i_finish; ++i){
            s2[i2 + i] = s1[i1 + i];
          }
          return /* () */0;
        }
        else {
          for(var i$1 = 0 ,i_finish$1 = off1 - 1; i$1<= i_finish$1; ++i$1){
            s2[i2 + i$1] = s1[i1 + i$1];
          }
          for(var i$2 = off1 ,i_finish$2 = len - 1; i$2<= i_finish$2; ++i$2){
            s2[i2 + i$2] = /* "\000" */0;
          }
          return /* () */0;
        }
      }
      else {
        return 0;
      }
    }
    
    function bytes_of_string(s) {
      var len = s.length;
      var res = new Array(len);
      for(var i = 0 ,i_finish = len - 1; i<= i_finish; ++i){
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
      var seg = 1024;
      if (i === 0 && len <= 4 * seg && len === bytes.length) {
        return String.fromCharCode.apply(null,bytes);
      }
      else {
        var offset = 0;
        while(s_len > 0) {
          var next = s_len < 1024 ? s_len : seg;
          var tmp_bytes = new Array(next);
          caml_blit_bytes(bytes, offset, tmp_bytes, 0, next);
          s = s + String.fromCharCode.apply(null,tmp_bytes);
          s_len -= next;
          offset += next;
        };
        return s;
      }
    }
    
    function caml_string_of_char_array(chars) {
      var len = chars.length;
      var bytes = new Array(len);
      for(var i = 0 ,i_finish = len - 1; i<= i_finish; ++i){
        bytes[i] = chars[i];
      }
      return bytes_to_string(bytes);
    }
    
    function caml_is_printable(c) {
      var code = c;
      return +(code > 31 && code < 127);
    }
    return {
      add : add, 
      bytes_of_string : bytes_of_string, 
      bytes_to_string : bytes_to_string, 
      caml_is_printable : caml_is_printable, 
      caml_string_of_char_array : caml_string_of_char_array, 
      caml_string_get : caml_string_get, 
      caml_string_compare : caml_string_compare, 
      caml_create_string : caml_create_string, 
      caml_fill_string : caml_fill_string, 
      caml_blit_string : caml_blit_string, 
      caml_blit_bytes : caml_blit_bytes
    }
  })
/* No side effect */
