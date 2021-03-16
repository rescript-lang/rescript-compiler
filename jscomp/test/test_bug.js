'use strict';

var Bytes = require("../../lib/js/bytes.js");
var Test_char = require("./test_char.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");

function escaped(s) {
  var n = 0;
  for(var i = 0 ,i_finish = s.length; i < i_finish; ++i){
    var c = s[i];
    var tmp;
    var exit = 0;
    if (c >= 14) {
      if (c !== 34 && c !== 92) {
        exit = 1;
      } else {
        tmp = 2;
      }
    } else if (c >= 11) {
      if (c >= 13) {
        tmp = 2;
      } else {
        exit = 1;
      }
    } else if (c >= 8) {
      tmp = 2;
    } else {
      exit = 1;
    }
    if (exit === 1) {
      tmp = Test_char.caml_is_printable(c) ? 1 : 4;
    }
    n = n + tmp | 0;
  }
  if (n === s.length) {
    return Bytes.copy(s);
  }
  var s$p = Caml_bytes.caml_create_bytes(n);
  n = 0;
  for(var i$1 = 0 ,i_finish$1 = s.length; i$1 < i_finish$1; ++i$1){
    var c$1 = s[i$1];
    var exit$1 = 0;
    if (c$1 > 92 || c$1 < 34) {
      if (c$1 >= 14) {
        exit$1 = 1;
      } else {
        switch (c$1) {
          case 8 :
              s$p[n] = /* '\\' */92;
              n = n + 1 | 0;
              s$p[n] = /* 'b' */98;
              break;
          case 9 :
              s$p[n] = /* '\\' */92;
              n = n + 1 | 0;
              s$p[n] = /* 't' */116;
              break;
          case 10 :
              s$p[n] = /* '\\' */92;
              n = n + 1 | 0;
              s$p[n] = /* 'n' */110;
              break;
          case 0 :
          case 1 :
          case 2 :
          case 3 :
          case 4 :
          case 5 :
          case 6 :
          case 7 :
          case 11 :
          case 12 :
              exit$1 = 1;
              break;
          case 13 :
              s$p[n] = /* '\\' */92;
              n = n + 1 | 0;
              s$p[n] = /* 'r' */114;
              break;
          
        }
      }
    } else if (c$1 > 91 || c$1 < 35) {
      s$p[n] = /* '\\' */92;
      n = n + 1 | 0;
      s$p[n] = c$1;
    } else {
      exit$1 = 1;
    }
    if (exit$1 === 1) {
      if (Test_char.caml_is_printable(c$1)) {
        s$p[n] = c$1;
      } else {
        s$p[n] = /* '\\' */92;
        n = n + 1 | 0;
        s$p[n] = 48 + (c$1 / 100 | 0) | 0;
        n = n + 1 | 0;
        s$p[n] = 48 + (c$1 / 10 | 0) % 10 | 0;
        n = n + 1 | 0;
        s$p[n] = 48 + c$1 % 10 | 0;
      }
    }
    n = n + 1 | 0;
  }
  return s$p;
}

exports.escaped = escaped;
/* No side effect */
