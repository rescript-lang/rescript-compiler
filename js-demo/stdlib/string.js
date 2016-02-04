// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
define(["./bytes","./list","../runtime/caml_string"],
  function(Bytes,List,Caml_string){
    'use strict';
    var bts = Bytes.unsafe_to_string;
    
    var bos = Bytes.unsafe_of_string;
    
    function make(n, c) {
      return bts(Bytes.make(n, c));
    }
    
    function init(n, f) {
      return bts(Bytes.init(n, f));
    }
    
    function copy(s) {
      return bts(Bytes.copy(bos(s)));
    }
    
    function sub(s, ofs, len) {
      return bts(Bytes.sub(bos(s), ofs, len));
    }
    
    function concat(sep, l) {
      if (l) {
        var hd = l[1];
        var num = [
          0,
          0
        ];
        var len = [
          0,
          0
        ];
        List.iter(function (s) {
              ++ num[1];
              len[1] += s.length;
              return /* () */0;
            }, l);
        var r = Caml_string.caml_create_string(len[1] + sep.length * (num[1] - 1));
        Caml_string.caml_blit_string(hd, 0, r, 0, hd.length);
        var pos = [
          0,
          hd.length
        ];
        List.iter(function (s) {
              Caml_string.caml_blit_string(sep, 0, r, pos[1], sep.length);
              pos[1] += sep.length;
              Caml_string.caml_blit_string(s, 0, r, pos[1], s.length);
              pos[1] += s.length;
              return /* () */0;
            }, l[2]);
        return Bytes.unsafe_to_string(r);
      }
      else {
        return "";
      }
    }
    
    function iter(f, s) {
      return Bytes.iter(f, bos(s));
    }
    
    function iteri(f, s) {
      return Bytes.iteri(f, bos(s));
    }
    
    function map(f, s) {
      return bts(Bytes.map(f, bos(s)));
    }
    
    function mapi(f, s) {
      return bts(Bytes.mapi(f, bos(s)));
    }
    
    function is_space(param) {
      var switcher = param - 9;
      if (switcher > 4 || switcher < 0) {
        if (switcher !== 23) {
          return /* false */0;
        }
        else {
          return /* true */1;
        }
      }
      else if (switcher !== 2) {
        return /* true */1;
      }
      else {
        return /* false */0;
      }
    }
    
    function trim(s) {
      if (s === "" || !(is_space(s.charCodeAt(0)) || is_space(s.charCodeAt(s.length - 1)))) {
        return s;
      }
      else {
        return bts(Bytes.trim(bos(s)));
      }
    }
    
    function escaped(s) {
      var needs_escape = function (_i) {
        while(true) {
          var i = _i;
          if (i >= s.length) {
            return /* false */0;
          }
          else {
            var c = s.charCodeAt(i);
            var exit = 0;
            if (c >= 14) {
              if (c !== 34) {
                if (c !== 92) {
                  exit = 1;
                }
                else {
                  return /* true */1;
                }
              }
              else {
                return /* true */1;
              }
            }
            else if (c >= 11) {
              if (c >= 13) {
                return /* true */1;
              }
              else {
                exit = 1;
              }
            }
            else if (c >= 8) {
              return /* true */1;
            }
            else {
              exit = 1;
            }
            if (exit === 1) {
              if (Caml_string.caml_is_printable(c)) {
                _i = i + 1;
              }
              else {
                return /* true */1;
              }
            }
            
          }
        };
      };
      if (needs_escape(0)) {
        return bts(Bytes.escaped(bos(s)));
      }
      else {
        return s;
      }
    }
    
    function index(s, c) {
      return Bytes.index(bos(s), c);
    }
    
    function rindex(s, c) {
      return Bytes.rindex(bos(s), c);
    }
    
    function index_from(s, i, c) {
      return Bytes.index_from(bos(s), i, c);
    }
    
    function rindex_from(s, i, c) {
      return Bytes.rindex_from(bos(s), i, c);
    }
    
    function contains(s, c) {
      return Bytes.contains(bos(s), c);
    }
    
    function contains_from(s, i, c) {
      return Bytes.contains_from(bos(s), i, c);
    }
    
    function rcontains_from(s, i, c) {
      return Bytes.rcontains_from(bos(s), i, c);
    }
    
    function uppercase(s) {
      return bts(Bytes.uppercase(bos(s)));
    }
    
    function lowercase(s) {
      return bts(Bytes.lowercase(bos(s)));
    }
    
    function capitalize(s) {
      return bts(Bytes.capitalize(bos(s)));
    }
    
    function uncapitalize(s) {
      return bts(Bytes.uncapitalize(bos(s)));
    }
    
    function compare(x, y) {
      return Caml_string.caml_string_compare(x, y);
    }
    
    var fill = Bytes.fill;
    
    var blit = Bytes.blit_string;
    return {
      make : make, 
      init : init, 
      copy : copy, 
      sub : sub, 
      fill : fill, 
      blit : blit, 
      concat : concat, 
      iter : iter, 
      iteri : iteri, 
      map : map, 
      mapi : mapi, 
      trim : trim, 
      escaped : escaped, 
      index : index, 
      rindex : rindex, 
      index_from : index_from, 
      rindex_from : rindex_from, 
      contains : contains, 
      contains_from : contains_from, 
      rcontains_from : rcontains_from, 
      uppercase : uppercase, 
      lowercase : lowercase, 
      capitalize : capitalize, 
      uncapitalize : uncapitalize, 
      compare : compare
    }
  })
/* No side effect */
