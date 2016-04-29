// GENERATED CODE BY BUCKLESCRIPT VERSION 0.3 , PLEASE EDIT WITH CARE
'use strict';
define(["exports", "../runtime/caml_builtin_exceptions", "../runtime/caml_string"],
  function(exports, Caml_builtin_exceptions, Caml_string){
    'use strict';
    function chr(n) {
      if (n < 0 || n > 255) {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Char.chr"
            ];
      }
      else {
        return n;
      }
    }
    
    function escaped(c) {
      var exit = 0;
      if (c !== 39) {
        if (c !== 92) {
          if (c >= 14) {
            exit = 1;
          }
          else {
            switch (c) {
              case 8 : 
                  return "\\b";
              case 9 : 
                  return "\\t";
              case 10 : 
                  return "\\n";
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
                  exit = 1;
                  break;
              case 13 : 
                  return "\\r";
              
            }
          }
        }
        else {
          return "\\\\";
        }
      }
      else {
        return "\\'";
      }
      if (exit === 1) {
        if (Caml_string.caml_is_printable(c)) {
          return Caml_string.caml_string_of_char_array(/* int array */[c]);
        }
        else {
          var n = c;
          return Caml_string.caml_string_of_char_array(/* int array */[
                      /* "\\" */92,
                      48 + (n / 100 | 0) | 0,
                      48 + (n / 10 | 0) % 10 | 0,
                      48 + n % 10 | 0
                    ]);
        }
      }
      
    }
    
    function lowercase(c) {
      if (c >= /* "A" */65 && c <= /* "Z" */90 || c >= /* "\192" */192 && c <= /* "\214" */214 || c >= /* "\216" */216 && c <= /* "\222" */222) {
        return c + 32 | 0;
      }
      else {
        return c;
      }
    }
    
    function uppercase(c) {
      if (c >= /* "a" */97 && c <= /* "z" */122 || c >= /* "\224" */224 && c <= /* "\246" */246 || c >= /* "\248" */248 && c <= /* "\254" */254) {
        return c - 32 | 0;
      }
      else {
        return c;
      }
    }
    
    function compare(c1, c2) {
      return c1 - c2 | 0;
    }
    
    exports.chr       = chr;
    exports.escaped   = escaped;
    exports.lowercase = lowercase;
    exports.uppercase = uppercase;
    exports.compare   = compare;
    
  })
/* No side effect */
