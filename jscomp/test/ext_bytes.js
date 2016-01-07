// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Bytes = require("../stdlib/bytes");
var Caml_string = require("../runtime/caml_string");

function escaped(s) {
  var n = 0;
  for(var i = 0 ,i_finish = s.length - 1; i<= i_finish; ++i){
    var match = s[i];
    var $js;
    if (match >= 32) {
      var switcher = -34 + match;
      $js = !(58 < (switcher >>> 0)) ? (
          56 < (-1 + switcher >>> 0) ? 2 : 1
        ) : (
          switcher >= 93 ? 4 : 1
        );
    }
    else {
      $js = match >= 11 ? (
          match !== 13 ? 4 : 2
        ) : (
          match >= 8 ? 2 : 4
        );
    }
    n += $js;
  }
  if (n === s.length) {
    return Bytes.copy(s);
  }
  else {
    var s$prime = Caml_string.caml_create_string(n);
    n = 0;
    for(var i$1 = 0 ,i_finish$1 = s.length - 1; i$1<= i_finish$1; ++i$1){
      var c = s[i$1];
      /* initialize */var exit = 0;
      if (c >= 35) {
        c !== 92 ? (
            c >= 127 ? (exit = 4) : (s$prime[n] = c)
          ) : (exit = 2);
      }
      else {
        if (c >= 32) {
          c >= 34 ? (exit = 2) : (s$prime[n] = c);
        }
        else {
          if (c >= 14) {
            exit = 4;
          }
          else {
            switch (c) {
              case 8 : 
                  s$prime[n] = /* "\\" */92;
                  ++ n;
                  s$prime[n] = /* "b" */98;
                  break;
              case 9 : 
                  s$prime[n] = /* "\\" */92;
                  ++ n;
                  s$prime[n] = /* "t" */116;
                  break;
              case 10 : 
                  s$prime[n] = /* "\\" */92;
                  ++ n;
                  s$prime[n] = /* "n" */110;
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
                  exit = 4;
                  break;
              case 13 : 
                  s$prime[n] = /* "\\" */92;
                  ++ n;
                  s$prime[n] = /* "r" */114;
                  break;
              
            }
          }
        }
      }
      switch (exit) {
        case 4 : 
            s$prime[n] = /* "\\" */92;
            ++ n;
            s$prime[n] = 48 + (c / 100 | 0);
            ++ n;
            s$prime[n] = 48 + (c / 10 | 0) % 10;
            ++ n;
            s$prime[n] = 48 + c % 10;
            break;
        case 2 : 
            s$prime[n] = /* "\\" */92;
            ++ n;
            s$prime[n] = c;
            break;
        
      }
      ++ n;
    }
    return s$prime;
  }
}

exports.escaped = escaped;
/* No side effect */
