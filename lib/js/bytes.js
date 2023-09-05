'use strict';

var Caml = require("./caml.js");
var Char = require("./char.js");
var Curry = require("./curry.js");
var Caml_bytes = require("./caml_bytes.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");

function unsafe_fill(s, i, l, c) {
  if (l <= 0) {
    return ;
  }
  for(var k = i ,k_finish = l + i | 0; k < k_finish; ++k){
    s[k] = c;
  }
}

function unsafe_blit(s1, i1, s2, i2, len) {
  if (len <= 0) {
    return ;
  }
  if (s1 === s2) {
    if (i1 < i2) {
      var range_a = (s1.length - i2 | 0) - 1 | 0;
      var range_b = len - 1 | 0;
      var range = range_a > range_b ? range_b : range_a;
      for(var j = range; j >= 0; --j){
        s1[i2 + j | 0] = s1[i1 + j | 0];
      }
      return ;
    }
    if (i1 <= i2) {
      return ;
    }
    var range_a$1 = (s1.length - i1 | 0) - 1 | 0;
    var range_b$1 = len - 1 | 0;
    var range$1 = range_a$1 > range_b$1 ? range_b$1 : range_a$1;
    for(var k = 0; k <= range$1; ++k){
      s1[i2 + k | 0] = s1[i1 + k | 0];
    }
    return ;
  }
  var off1 = s1.length - i1 | 0;
  if (len <= off1) {
    for(var i = 0; i < len; ++i){
      s2[i2 + i | 0] = s1[i1 + i | 0];
    }
    return ;
  }
  for(var i$1 = 0; i$1 < off1; ++i$1){
    s2[i2 + i$1 | 0] = s1[i1 + i$1 | 0];
  }
  for(var i$2 = off1; i$2 < len; ++i$2){
    s2[i2 + i$2 | 0] = /* '\000' */0;
  }
}

function make(n, c) {
  var s = Caml_bytes.create(n);
  unsafe_fill(s, 0, n, c);
  return s;
}

function init(n, f) {
  var s = Caml_bytes.create(n);
  for(var i = 0; i < n; ++i){
    s[i] = Curry._1(f, i);
  }
  return s;
}

var empty = [];

function copy(s) {
  var len = s.length;
  var r = Caml_bytes.create(len);
  unsafe_blit(s, 0, r, 0, len);
  return r;
}

function to_string(a) {
  var i = 0;
  var len = a.length;
  var s = "";
  var s_len = len;
  if (i === 0 && len <= 4096 && len === a.length) {
    return String.fromCharCode.apply(null, a);
  }
  var offset = 0;
  while(s_len > 0) {
    var next = s_len < 1024 ? s_len : 1024;
    var tmp_bytes = new Array(next);
    for(var k = 0; k < next; ++k){
      tmp_bytes[k] = a[k + offset | 0];
    }
    s = s + String.fromCharCode.apply(null, tmp_bytes);
    s_len = s_len - next | 0;
    offset = offset + next | 0;
  };
  return s;
}

function of_string(s) {
  var len = s.length;
  var res = new Array(len);
  for(var i = 0; i < len; ++i){
    res[i] = s.codePointAt(i);
  }
  return res;
}

function sub(s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "String.sub / Bytes.sub",
          Error: new Error()
        };
  }
  var r = Caml_bytes.create(len);
  unsafe_blit(s, ofs, r, 0, len);
  return r;
}

function sub_string(b, ofs, len) {
  return to_string(sub(b, ofs, len));
}

function $plus$plus(a, b) {
  var c = a + b | 0;
  var match = a < 0;
  var match$1 = b < 0;
  var match$2 = c < 0;
  if (match) {
    if (!match$1) {
      return c;
    }
    if (match$2) {
      return c;
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Bytes.extend",
          Error: new Error()
        };
  }
  if (match$1) {
    return c;
  }
  if (match$2) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Bytes.extend",
          Error: new Error()
        };
  }
  return c;
}

function extend(s, left, right) {
  var len = $plus$plus($plus$plus(s.length, left), right);
  var r = Caml_bytes.create(len);
  var match = left < 0 ? [
      -left | 0,
      0
    ] : [
      0,
      left
    ];
  var dstoff = match[1];
  var srcoff = match[0];
  var cpylen = Caml.int_min(s.length - srcoff | 0, len - dstoff | 0);
  if (cpylen > 0) {
    unsafe_blit(s, srcoff, r, dstoff, cpylen);
  }
  return r;
}

function fill(s, ofs, len, c) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "String.fill / Bytes.fill",
          Error: new Error()
        };
  }
  unsafe_fill(s, ofs, len, c);
}

function blit(s1, ofs1, s2, ofs2, len) {
  if (len < 0 || ofs1 < 0 || ofs1 > (s1.length - len | 0) || ofs2 < 0 || ofs2 > (s2.length - len | 0)) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Bytes.blit",
          Error: new Error()
        };
  }
  unsafe_blit(s1, ofs1, s2, ofs2, len);
}

function blit_string(s1, ofs1, s2, ofs2, len) {
  if (len < 0 || ofs1 < 0 || ofs1 > (s1.length - len | 0) || ofs2 < 0 || ofs2 > (s2.length - len | 0)) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "String.blit / Bytes.blit_string",
          Error: new Error()
        };
  }
  if (len <= 0) {
    return ;
  }
  var off1 = s1.length - ofs1 | 0;
  if (len <= off1) {
    for(var i = 0; i < len; ++i){
      s2[ofs2 + i | 0] = s1.codePointAt(ofs1 + i | 0);
    }
    return ;
  }
  for(var i$1 = 0; i$1 < off1; ++i$1){
    s2[ofs2 + i$1 | 0] = s1.codePointAt(ofs1 + i$1 | 0);
  }
  for(var i$2 = off1; i$2 < len; ++i$2){
    s2[ofs2 + i$2 | 0] = /* '\000' */0;
  }
}

function iter(f, a) {
  for(var i = 0 ,i_finish = a.length; i < i_finish; ++i){
    Curry._1(f, a[i]);
  }
}

function iteri(f, a) {
  for(var i = 0 ,i_finish = a.length; i < i_finish; ++i){
    Curry._2(f, i, a[i]);
  }
}

function ensure_ge(x, y) {
  if (x >= y) {
    return x;
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Bytes.concat",
        Error: new Error()
      };
}

function sum_lengths(_acc, seplen, _param) {
  while(true) {
    var param = _param;
    var acc = _acc;
    if (!param) {
      return acc;
    }
    var tl = param.tl;
    var hd = param.hd;
    if (!tl) {
      return hd.length + acc | 0;
    }
    _param = tl;
    _acc = ensure_ge((hd.length + seplen | 0) + acc | 0, acc);
    continue ;
  };
}

function concat(sep, param) {
  if (!param) {
    return empty;
  }
  var seplen = sep.length;
  var dst = Caml_bytes.create(sum_lengths(0, seplen, param));
  var _pos = 0;
  var _param = param;
  while(true) {
    var param$1 = _param;
    var pos = _pos;
    if (!param$1) {
      return dst;
    }
    var tl = param$1.tl;
    var hd = param$1.hd;
    if (tl) {
      unsafe_blit(hd, 0, dst, pos, hd.length);
      unsafe_blit(sep, 0, dst, pos + hd.length | 0, seplen);
      _param = tl;
      _pos = (pos + hd.length | 0) + seplen | 0;
      continue ;
    }
    unsafe_blit(hd, 0, dst, pos, hd.length);
    return dst;
  };
}

function cat(s1, s2) {
  var l1 = s1.length;
  var l2 = s2.length;
  var r = Caml_bytes.create(l1 + l2 | 0);
  unsafe_blit(s1, 0, r, 0, l1);
  unsafe_blit(s2, 0, r, l1, l2);
  return r;
}

function is_space(param) {
  if (param > 13 || param < 9) {
    return param === 32;
  } else {
    return param !== 11;
  }
}

function trim(s) {
  var len = s.length;
  var i = 0;
  while(i < len && is_space(s[i])) {
    i = i + 1 | 0;
  };
  var j = len - 1 | 0;
  while(j >= i && is_space(s[j])) {
    j = j - 1 | 0;
  };
  if (j >= i) {
    return sub(s, i, (j - i | 0) + 1 | 0);
  } else {
    return empty;
  }
}

function escaped(s) {
  var n = 0;
  for(var i = 0 ,i_finish = s.length; i < i_finish; ++i){
    var match = s[i];
    n = n + (
      match >= 32 ? (
          match > 92 || match < 34 ? (
              match >= 127 ? 4 : 1
            ) : (
              match > 91 || match < 35 ? 2 : 1
            )
        ) : (
          match >= 11 ? (
              match !== 13 ? 4 : 2
            ) : (
              match >= 8 ? 2 : 4
            )
        )
    ) | 0;
  }
  if (n === s.length) {
    return copy(s);
  }
  var s$p = Caml_bytes.create(n);
  n = 0;
  for(var i$1 = 0 ,i_finish$1 = s.length; i$1 < i_finish$1; ++i$1){
    var c = s[i$1];
    var exit = 0;
    if (c >= 35) {
      if (c !== 92) {
        if (c >= 127) {
          exit = 1;
        } else {
          s$p[n] = c;
        }
      } else {
        exit = 2;
      }
    } else if (c >= 32) {
      if (c >= 34) {
        exit = 2;
      } else {
        s$p[n] = c;
      }
    } else if (c >= 14) {
      exit = 1;
    } else {
      switch (c) {
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
            exit = 1;
            break;
        case 13 :
            s$p[n] = /* '\\' */92;
            n = n + 1 | 0;
            s$p[n] = /* 'r' */114;
            break;
        
      }
    }
    switch (exit) {
      case 1 :
          s$p[n] = /* '\\' */92;
          n = n + 1 | 0;
          s$p[n] = 48 + (c / 100 | 0) | 0;
          n = n + 1 | 0;
          s$p[n] = 48 + (c / 10 | 0) % 10 | 0;
          n = n + 1 | 0;
          s$p[n] = 48 + c % 10 | 0;
          break;
      case 2 :
          s$p[n] = /* '\\' */92;
          n = n + 1 | 0;
          s$p[n] = c;
          break;
      
    }
    n = n + 1 | 0;
  }
  return s$p;
}

function map(f, s) {
  var l = s.length;
  if (l === 0) {
    return s;
  }
  var r = Caml_bytes.create(l);
  for(var i = 0; i < l; ++i){
    r[i] = Curry._1(f, s[i]);
  }
  return r;
}

function mapi(f, s) {
  var l = s.length;
  if (l === 0) {
    return s;
  }
  var r = Caml_bytes.create(l);
  for(var i = 0; i < l; ++i){
    r[i] = Curry._2(f, i, s[i]);
  }
  return r;
}

function uppercase_ascii(s) {
  return map(Char.uppercase_ascii, s);
}

function lowercase_ascii(s) {
  return map(Char.lowercase_ascii, s);
}

function apply1(f, s) {
  if (s.length === 0) {
    return s;
  }
  var r = copy(s);
  r[0] = Curry._1(f, s[0]);
  return r;
}

function capitalize_ascii(s) {
  return apply1(Char.uppercase_ascii, s);
}

function uncapitalize_ascii(s) {
  return apply1(Char.lowercase_ascii, s);
}

function index_rec(s, lim, _i, c) {
  while(true) {
    var i = _i;
    if (i >= lim) {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    if (s[i] === c) {
      return i;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function index(s, c) {
  return index_rec(s, s.length, 0, c);
}

function index_rec_opt(s, lim, _i, c) {
  while(true) {
    var i = _i;
    if (i >= lim) {
      return ;
    }
    if (s[i] === c) {
      return i;
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function index_opt(s, c) {
  return index_rec_opt(s, s.length, 0, c);
}

function index_from(s, i, c) {
  var l = s.length;
  if (i < 0 || i > l) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "String.index_from / Bytes.index_from",
          Error: new Error()
        };
  }
  return index_rec(s, l, i, c);
}

function index_from_opt(s, i, c) {
  var l = s.length;
  if (i < 0 || i > l) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "String.index_from_opt / Bytes.index_from_opt",
          Error: new Error()
        };
  }
  return index_rec_opt(s, l, i, c);
}

function rindex_rec(s, _i, c) {
  while(true) {
    var i = _i;
    if (i < 0) {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    if (s[i] === c) {
      return i;
    }
    _i = i - 1 | 0;
    continue ;
  };
}

function rindex(s, c) {
  return rindex_rec(s, s.length - 1 | 0, c);
}

function rindex_from(s, i, c) {
  if (i < -1 || i >= s.length) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "String.rindex_from / Bytes.rindex_from",
          Error: new Error()
        };
  }
  return rindex_rec(s, i, c);
}

function rindex_rec_opt(s, _i, c) {
  while(true) {
    var i = _i;
    if (i < 0) {
      return ;
    }
    if (s[i] === c) {
      return i;
    }
    _i = i - 1 | 0;
    continue ;
  };
}

function rindex_opt(s, c) {
  return rindex_rec_opt(s, s.length - 1 | 0, c);
}

function rindex_from_opt(s, i, c) {
  if (i < -1 || i >= s.length) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "String.rindex_from_opt / Bytes.rindex_from_opt",
          Error: new Error()
        };
  }
  return rindex_rec_opt(s, i, c);
}

function contains_from(s, i, c) {
  var l = s.length;
  if (i < 0 || i > l) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "String.contains_from / Bytes.contains_from",
          Error: new Error()
        };
  }
  try {
    index_rec(s, l, i, c);
    return true;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return false;
    }
    throw exn;
  }
}

function contains(s, c) {
  return contains_from(s, 0, c);
}

function rcontains_from(s, i, c) {
  if (i < 0 || i >= s.length) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "String.rcontains_from / Bytes.rcontains_from",
          Error: new Error()
        };
  }
  try {
    rindex_rec(s, i, c);
    return true;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return false;
    }
    throw exn;
  }
}

var compare = Caml_bytes.bytes_compare;

var equal = Caml_bytes.bytes_equal;

var unsafe_to_string = to_string;

var unsafe_of_string = of_string;

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
exports.index_opt = index_opt;
exports.rindex = rindex;
exports.rindex_opt = rindex_opt;
exports.index_from = index_from;
exports.index_from_opt = index_from_opt;
exports.rindex_from = rindex_from;
exports.rindex_from_opt = rindex_from_opt;
exports.contains = contains;
exports.contains_from = contains_from;
exports.rcontains_from = rcontains_from;
exports.uppercase_ascii = uppercase_ascii;
exports.lowercase_ascii = lowercase_ascii;
exports.capitalize_ascii = capitalize_ascii;
exports.uncapitalize_ascii = uncapitalize_ascii;
exports.compare = compare;
exports.equal = equal;
exports.unsafe_to_string = unsafe_to_string;
exports.unsafe_of_string = unsafe_of_string;
/* No side effect */
