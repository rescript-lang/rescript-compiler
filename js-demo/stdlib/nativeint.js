// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
define(["../runtime/caml_format","./sys","../runtime/caml_primitive"],
  function(Caml_format,Sys,Caml_primitive){
    'use strict';
    function succ(n) {
      return n + 1;
    }
    
    function pred(n) {
      return n - 1;
    }
    
    function abs(n) {
      if (n >= 0) {
        return n;
      }
      else {
        return -n;
      }
    }
    
    var size = Sys.word_size;
    
    var min_int = -9007199254740991;
    
    var max_int = 9007199254740991;
    
    function lognot(n) {
      return n ^ -1;
    }
    
    function to_string(n) {
      return Caml_format.caml_nativeint_format("%d", n);
    }
    
    function compare(x, y) {
      return Caml_primitive.caml_nativeint_compare(x, y);
    }
    
    var zero = 0;
    
    var one = 1;
    
    var minus_one = -1;
    return {
      zero : zero, 
      one : one, 
      minus_one : minus_one, 
      succ : succ, 
      pred : pred, 
      abs : abs, 
      size : size, 
      max_int : max_int, 
      min_int : min_int, 
      lognot : lognot, 
      to_string : to_string, 
      compare : compare
    }
  })
/* No side effect */
