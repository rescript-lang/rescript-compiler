// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Pervasives = require("./pervasives");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Char = require("./char");
var Caml_primitive = require("../runtime/caml_primitive");
var Caml_string = require("../runtime/caml_string");
var List = require("./list");

function make(n, c) {
  var s = Caml_string.caml_create_string(n);
  Caml_string.caml_fill_string(s, 0, n, c);
  return s;
}

function init(n, f) {
  var s = Caml_string.caml_create_string(n);
  for(var i = 0 ,i_finish = n - 1; i<= i_finish; ++i){
    s[i] = f(i);
  }
  return s;
}

var empty = [];

function copy(s) {
  var len = s.length;
  var r = Caml_string.caml_create_string(len);
  Caml_string.caml_blit_bytes(s, 0, r, 0, len);
  return r;
}

function to_string(b) {
  return Caml_string.bytes_to_string(copy(b));
}

function of_string(s) {
  return copy(Caml_string.bytes_of_string(s));
}

function sub(s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > s.length - len) {
    return Pervasives.invalid_arg("String.sub / Bytes.sub");
  }
  else {
    var r = Caml_string.caml_create_string(len);
    Caml_string.caml_blit_bytes(s, ofs, r, 0, len);
    return r;
  }
}

function sub_string(b, ofs, len) {
  return Caml_string.bytes_to_string(sub(b, ofs, len));
}

function extend(s, left, right) {
  var len = s.length + left + right;
  var r = Caml_string.caml_create_string(len);
  var match = left < 0 ? [
      /* tuple */0,
      -left,
      0
    ] : [
      /* tuple */0,
      0,
      left
    ];
  var dstoff = match[2];
  var srcoff = match[1];
  var cpylen = Pervasives.min(s.length - srcoff, len - dstoff);
  if (cpylen > 0) {
    Caml_string.caml_blit_bytes(s, srcoff, r, dstoff, cpylen);
  }
  return r;
}

function fill(s, ofs, len, c) {
  return ofs < 0 || len < 0 || ofs > s.length - len ? Pervasives.invalid_arg("String.fill / Bytes.fill") : Caml_string.caml_fill_string(s, ofs, len, c);
}

function blit(s1, ofs1, s2, ofs2, len) {
  return len < 0 || ofs1 < 0 || ofs1 > s1.length - len || ofs2 < 0 || ofs2 > s2.length - len ? Pervasives.invalid_arg("Bytes.blit") : Caml_string.caml_blit_bytes(s1, ofs1, s2, ofs2, len);
}

function blit_string(s1, ofs1, s2, ofs2, len) {
  return len < 0 || ofs1 < 0 || ofs1 > s1.length - len || ofs2 < 0 || ofs2 > s2.length - len ? Pervasives.invalid_arg("String.blit / Bytes.blit_string") : Caml_string.caml_blit_string(s1, ofs1, s2, ofs2, len);
}

function iter(f, a) {
  for(var i = 0 ,i_finish = a.length - 1; i<= i_finish; ++i){
    f(a[i]);
  }
  return /* () */0;
}

function iteri(f, a) {
  for(var i = 0 ,i_finish = a.length - 1; i<= i_finish; ++i){
    f(i, a[i]);
  }
  return /* () */0;
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
    Caml_string.caml_blit_bytes(hd, 0, r, 0, hd.length);
    var pos = [
      0,
      hd.length
    ];
    List.iter(function (s) {
          Caml_string.caml_blit_bytes(sep, 0, r, pos[1], sep.length);
          pos[1] += sep.length;
          Caml_string.caml_blit_bytes(s, 0, r, pos[1], s.length);
          pos[1] += s.length;
          return /* () */0;
        }, l[2]);
    return r;
  }
  else {
    return empty;
  }
}

var cat = Caml_string.bytes_cat;

function is_space(param) {
  var switcher = -9 + param;
  return 4 < (switcher >>> 0) ? (
            switcher !== 23 ? /* false */0 : /* true */1
          ) : (
            switcher !== 2 ? /* true */1 : /* false */0
          );
}

function trim(s) {
  var len = s.length;
  var i = 0;
  while(i < len && is_space(s[i])) {
    ++ i;
  };
  var j = len - 1;
  while(j >= i && is_space(s[j])) {
    -- j;
  };
  return j >= i ? sub(s, i, j - i + 1) : empty;
}

function escaped(s) {
  var n = 0;
  for(var i = 0 ,i_finish = s.length - 1; i<= i_finish; ++i){
    var c = s[i];
    var $js;
    /* initialize */var exit = 0;
    c >= 14 ? (
        c !== 34 ? (
            c !== 92 ? (exit = 30) : ($js = 2)
          ) : ($js = 2)
      ) : (
        c >= 11 ? (
            c >= 13 ? ($js = 2) : (exit = 30)
          ) : (
            c >= 8 ? ($js = 2) : (exit = 30)
          )
      );
    if (exit === 30) {
      $js = Caml_string.caml_is_printable(c) ? 1 : 4;
    }
    n += $js;
  }
  if (n === s.length) {
    return copy(s);
  }
  else {
    var s$prime = Caml_string.caml_create_string(n);
    n = 0;
    for(var i$1 = 0 ,i_finish$1 = s.length - 1; i$1<= i_finish$1; ++i$1){
      var c$1 = s[i$1];
      /* initialize */var exit$1 = 0;
      var switcher = -34 + c$1;
      if (!(58 < (switcher >>> 0))) {
        if (56 < (-1 + switcher >>> 0)) {
          s$prime[n] = /* "\\" */92;
          ++ n;
          s$prime[n] = c$1;
        }
        else {
          exit$1 = 27;
        }
      }
      else {
        if (switcher >= -20) {
          exit$1 = 27;
        }
        else {
          switch (34 + switcher) {
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
                exit$1 = 27;
                break;
            case 13 : 
                s$prime[n] = /* "\\" */92;
                ++ n;
                s$prime[n] = /* "r" */114;
                break;
            
          }
        }
      }
      if (exit$1 === 27) {
        if (Caml_string.caml_is_printable(c$1)) {
          s$prime[n] = c$1;
        }
        else {
          s$prime[n] = /* "\\" */92;
          ++ n;
          s$prime[n] = 48 + (c$1 / 100 | 0);
          ++ n;
          s$prime[n] = 48 + (c$1 / 10 | 0) % 10;
          ++ n;
          s$prime[n] = 48 + c$1 % 10;
        }
      }
      ++ n;
    }
    return s$prime;
  }
}

function map(f, s) {
  var l = s.length;
  if (l) {
    var r = Caml_string.caml_create_string(l);
    for(var i = 0 ,i_finish = l - 1; i<= i_finish; ++i){
      r[i] = f(s[i]);
    }
    return r;
  }
  else {
    return s;
  }
}

function mapi(f, s) {
  var l = s.length;
  if (l) {
    var r = Caml_string.caml_create_string(l);
    for(var i = 0 ,i_finish = l - 1; i<= i_finish; ++i){
      r[i] = f(i, s[i]);
    }
    return r;
  }
  else {
    return s;
  }
}

function uppercase(s) {
  return map(Char.uppercase, s);
}

function lowercase(s) {
  return map(Char.lowercase, s);
}

function apply1(f, s) {
  if (s.length) {
    var r = copy(s);
    r[0] = f(s[0]);
    return r;
  }
  else {
    return s;
  }
}

function capitalize(s) {
  return apply1(Char.uppercase, s);
}

function uncapitalize(s) {
  return apply1(Char.lowercase, s);
}

function index_rec(s, lim, _i, c) {
  while(/* true */1) {
    var i = _i;
    if (i >= lim) {
      throw Caml_exceptions.Not_found;
    }
    else {
      if (s[i] === c) {
        return i;
      }
      else {
        _i = i + 1;
      }
    }
  };
}

function index(s, c) {
  return index_rec(s, s.length, 0, c);
}

function index_from(s, i, c) {
  var l = s.length;
  return i < 0 || i > l ? Pervasives.invalid_arg("String.index_from / Bytes.index_from") : index_rec(s, l, i, c);
}

function rindex_rec(s, _i, c) {
  while(/* true */1) {
    var i = _i;
    if (i < 0) {
      throw Caml_exceptions.Not_found;
    }
    else {
      if (s[i] === c) {
        return i;
      }
      else {
        _i = i - 1;
      }
    }
  };
}

function rindex(s, c) {
  return rindex_rec(s, s.length - 1, c);
}

function rindex_from(s, i, c) {
  return i < -1 || i >= s.length ? Pervasives.invalid_arg("String.rindex_from / Bytes.rindex_from") : rindex_rec(s, i, c);
}

function contains_from(s, i, c) {
  var l = s.length;
  if (i < 0 || i > l) {
    return Pervasives.invalid_arg("String.contains_from / Bytes.contains_from");
  }
  else {
    try {
      index_rec(s, l, i, c);
      return /* true */1;
    }
    catch (exn){
      if (exn === Caml_exceptions.Not_found) {
        return /* false */0;
      }
      else {
        throw exn;
      }
    }
  }
}

function contains(s, c) {
  return contains_from(s, 0, c);
}

function rcontains_from(s, i, c) {
  if (i < 0 || i >= s.length) {
    return Pervasives.invalid_arg("String.rcontains_from / Bytes.rcontains_from");
  }
  else {
    try {
      rindex_rec(s, i, c);
      return /* true */1;
    }
    catch (exn){
      if (exn === Caml_exceptions.Not_found) {
        return /* false */0;
      }
      else {
        throw exn;
      }
    }
  }
}

function compare(x, y) {
  return Caml_primitive.caml_compare(x, y);
}

function unsafe_to_string(prim) {
  return Caml_string.bytes_to_string(prim);
}

function unsafe_of_string(prim) {
  return Caml_string.bytes_of_string(prim);
}

exports.make = make;
exports.init = init;
exports.empty = empty;
exports.copy = copy;
exports.of_string = of_string;
exports.to_string = to_string;
exports.sub = sub;
exports.sub_string = sub_string;
exports.extend = extend;
exports.fill = fill;
exports.blit = blit;
exports.blit_string = blit_string;
exports.concat = concat;
exports.cat = cat;
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
exports.unsafe_to_string = unsafe_to_string;
exports.unsafe_of_string = unsafe_of_string;
/* No side effect */
