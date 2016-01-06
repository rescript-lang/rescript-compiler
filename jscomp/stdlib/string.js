// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Bytes = require("./bytes");
var List = require("./list");
var Caml_string = require("../runtime/caml_string");

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
  var switcher = -9 + param;
  return 4 < (switcher >>> 0) ? (
            switcher !== 23 ? /* false */0 : /* true */1
          ) : (
            switcher !== 2 ? /* true */1 : /* false */0
          );
}

function trim(s) {
  return s === "" ? s : (
            is_space(s.charCodeAt(0)) || is_space(s.charCodeAt(s.length - 1)) ? bts(Bytes.trim(bos(s))) : s
          );
}

function escaped(s) {
  var needs_escape = function (_i) {
    while(/* true */1) {
      var i = _i;
      if (i >= s.length) {
        return /* false */0;
      }
      else {
        var c = s.charCodeAt(i);
        /* initialize */var exit = 0;
        if (c >= 14) {
          if (c !== 34) {
            if (c !== 92) {
              exit = 15;
            }
            else {
              return /* true */1;
            }
          }
          else {
            return /* true */1;
          }
        }
        else {
          if (c >= 11) {
            if (c >= 13) {
              return /* true */1;
            }
            else {
              exit = 15;
            }
          }
          else {
            if (c >= 8) {
              return /* true */1;
            }
            else {
              exit = 15;
            }
          }
        }
        if (exit === 15) {
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
  return needs_escape(0) ? bts(Bytes.escaped(bos(s))) : s;
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

exports.make = make;
exports.init = init;
exports.copy = copy;
exports.sub = sub;
exports.fill = fill;
exports.blit = blit;
exports.concat = concat;
exports.iter = iter;
exports.iteri = iteri;
exports.map = map;
exports.mapi = mapi;
exports.trim = trim;
exports.escaped = escaped;
exports.index = index;
exports.rindex = rindex;
exports.index_from = index_from;
exports.rindex_from = rindex_from;
exports.contains = contains;
exports.contains_from = contains_from;
exports.rcontains_from = rcontains_from;
exports.uppercase = uppercase;
exports.lowercase = lowercase;
exports.capitalize = capitalize;
exports.uncapitalize = uncapitalize;
exports.compare = compare;
/* No side effect */
