// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Bytes       = require("./bytes");
var Caml_curry  = require("../runtime/caml_curry");
var List        = require("./list");
var Caml_string = require("../runtime/caml_string");

var bts = Bytes.unsafe_to_string;

var bos = Bytes.unsafe_of_string;

function make(n, c) {
  return Caml_curry.app1(bts, Bytes.make(n, c));
}

function init(n, f) {
  return Caml_curry.app1(bts, Bytes.init(n, f));
}

function copy(s) {
  return Caml_curry.app1(bts, Bytes.copy(Caml_curry.app1(bos, s)));
}

function sub(s, ofs, len) {
  return Caml_curry.app1(bts, Bytes.sub(Caml_curry.app1(bos, s), ofs, len));
}

function concat(sep, l) {
  if (l) {
    var hd = l[0];
    var num = [0];
    var len = [0];
    List.iter(function (s) {
          ++ num[0];
          len[0] += s.length;
          return /* () */0;
        }, l);
    var r = Caml_string.caml_create_string(len[0] + sep.length * (num[0] - 1));
    Caml_string.caml_blit_string(hd, 0, r, 0, hd.length);
    var pos = [hd.length];
    List.iter(function (s) {
          Caml_string.caml_blit_string(sep, 0, r, pos[0], sep.length);
          pos[0] += sep.length;
          Caml_string.caml_blit_string(s, 0, r, pos[0], s.length);
          pos[0] += s.length;
          return /* () */0;
        }, l[1]);
    return Caml_string.bytes_to_string(r);
  }
  else {
    return "";
  }
}

function iter(f, s) {
  return Bytes.iter(f, Caml_curry.app1(bos, s));
}

function iteri(f, s) {
  return Bytes.iteri(f, Caml_curry.app1(bos, s));
}

function map(f, s) {
  return Caml_curry.app1(bts, Bytes.map(f, Caml_curry.app1(bos, s)));
}

function mapi(f, s) {
  return Caml_curry.app1(bts, Bytes.mapi(f, Caml_curry.app1(bos, s)));
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
    return Caml_curry.app1(bts, Bytes.trim(Caml_curry.app1(bos, s)));
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
            continue ;
            
          }
          else {
            return /* true */1;
          }
        }
        
      }
    };
  };
  if (needs_escape(0)) {
    return Caml_curry.app1(bts, Bytes.escaped(Caml_curry.app1(bos, s)));
  }
  else {
    return s;
  }
}

function index(s, c) {
  return Bytes.index(Caml_curry.app1(bos, s), c);
}

function rindex(s, c) {
  return Bytes.rindex(Caml_curry.app1(bos, s), c);
}

function index_from(s, i, c) {
  return Bytes.index_from(Caml_curry.app1(bos, s), i, c);
}

function rindex_from(s, i, c) {
  return Bytes.rindex_from(Caml_curry.app1(bos, s), i, c);
}

function contains(s, c) {
  return Bytes.contains(Caml_curry.app1(bos, s), c);
}

function contains_from(s, i, c) {
  return Bytes.contains_from(Caml_curry.app1(bos, s), i, c);
}

function rcontains_from(s, i, c) {
  return Bytes.rcontains_from(Caml_curry.app1(bos, s), i, c);
}

function uppercase(s) {
  return Caml_curry.app1(bts, Bytes.uppercase(Caml_curry.app1(bos, s)));
}

function lowercase(s) {
  return Caml_curry.app1(bts, Bytes.lowercase(Caml_curry.app1(bos, s)));
}

function capitalize(s) {
  return Caml_curry.app1(bts, Bytes.capitalize(Caml_curry.app1(bos, s)));
}

function uncapitalize(s) {
  return Caml_curry.app1(bts, Bytes.uncapitalize(Caml_curry.app1(bos, s)));
}

function compare(x, y) {
  return Caml_string.caml_string_compare(x, y);
}

var fill = Bytes.fill;

var blit = Bytes.blit_string;

exports.make           = make;
exports.init           = init;
exports.copy           = copy;
exports.sub            = sub;
exports.fill           = fill;
exports.blit           = blit;
exports.concat         = concat;
exports.iter           = iter;
exports.iteri          = iteri;
exports.map            = map;
exports.mapi           = mapi;
exports.trim           = trim;
exports.escaped        = escaped;
exports.index          = index;
exports.rindex         = rindex;
exports.index_from     = index_from;
exports.rindex_from    = rindex_from;
exports.contains       = contains;
exports.contains_from  = contains_from;
exports.rcontains_from = rcontains_from;
exports.uppercase      = uppercase;
exports.lowercase      = lowercase;
exports.capitalize     = capitalize;
exports.uncapitalize   = uncapitalize;
exports.compare        = compare;
/* No side effect */
