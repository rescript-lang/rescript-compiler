// Generated CODE, PLEASE EDIT WITH CARE
'use strict';
define(["../runtime/caml_format","../runtime/caml_primitive"],
  function(Caml_format,Caml_primitive){
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
    
    function lognot(n) {
      return n ^ -1;
    }
    
    function to_string(n) {
      return Caml_format.caml_int32_format("%d", n);
    }
    
    function compare(x, y) {
      return Caml_primitive.caml_int32_compare(x, y);
    }
    
    var zero = 0;
    
    var one = 1;
    
    var minus_one = -1;
    
    var max_int = 2147483647;
    
    var min_int = -2147483648;
    return {
      zero : zero, 
      one : one, 
      minus_one : minus_one, 
      succ : succ, 
      pred : pred, 
      abs : abs, 
      max_int : max_int, 
      min_int : min_int, 
      lognot : lognot, 
      to_string : to_string, 
      compare : compare
    }
  })
/* No side effect */
