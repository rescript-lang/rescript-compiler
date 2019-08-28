'use strict';

var Bytes = require("../../lib/js/bytes.js");
var Caml_char = require("../../lib/js/caml_char.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Pervasives = require("../../lib/js/pervasives.js");

function escaped(s) {
  var n = /* record */{
    contents: 0
  };
  for(var i = 0 ,i_finish = s.length - 1 | 0; i <= i_finish; ++i){
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
      tmp = Caml_char.caml_is_printable(c) ? 1 : 4;
    }
    n.contents = n.contents + tmp | 0;
  }
  if (n.contents === s.length) {
    return Bytes.copy(s);
  } else {
    var s$prime = Caml_bytes.caml_create_bytes(n.contents);
    n.contents = 0;
    for(var i$1 = 0 ,i_finish$1 = s.length - 1 | 0; i$1 <= i_finish$1; ++i$1){
      var c$1 = s[i$1];
      var exit$1 = 0;
      var switcher = c$1 - 34 | 0;
      if (switcher > 58 || switcher < 0) {
        if (switcher >= -20) {
          exit$1 = 1;
        } else {
          switch (switcher + 34 | 0) {
            case 8 :
                s$prime[n.contents] = /* "\\" */92;
                Pervasives.incr(n);
                s$prime[n.contents] = /* "b" */98;
                break;
            case 9 :
                s$prime[n.contents] = /* "\\" */92;
                Pervasives.incr(n);
                s$prime[n.contents] = /* "t" */116;
                break;
            case 10 :
                s$prime[n.contents] = /* "\\" */92;
                Pervasives.incr(n);
                s$prime[n.contents] = /* "n" */110;
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
                s$prime[n.contents] = /* "\\" */92;
                Pervasives.incr(n);
                s$prime[n.contents] = /* "r" */114;
                break;
            
          }
        }
      } else if (switcher > 57 || switcher < 1) {
        s$prime[n.contents] = /* "\\" */92;
        Pervasives.incr(n);
        s$prime[n.contents] = c$1;
      } else {
        exit$1 = 1;
      }
      if (exit$1 === 1) {
        if (Caml_char.caml_is_printable(c$1)) {
          s$prime[n.contents] = c$1;
        } else {
          s$prime[n.contents] = /* "\\" */92;
          Pervasives.incr(n);
          s$prime[n.contents] = 48 + (c$1 / 100 | 0) | 0;
          Pervasives.incr(n);
          s$prime[n.contents] = 48 + (c$1 / 10 | 0) % 10 | 0;
          Pervasives.incr(n);
          s$prime[n.contents] = 48 + c$1 % 10 | 0;
        }
      }
      Pervasives.incr(n);
    }
    return s$prime;
  }
}

exports.escaped = escaped;
/* No side effect */
