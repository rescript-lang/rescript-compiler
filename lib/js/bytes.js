'use strict';

let Caml = require("./caml.js");
let Char = require("./char.js");
let Caml_bytes = require("./caml_bytes.js");
let Caml_js_exceptions = require("./caml_js_exceptions.js");

function unsafe_fill(s, i, l, c) {
  if (l <= 0) {
    return;
  }
  for (let k = i, k_finish = l + i | 0; k < k_finish; ++k) {
    s[k] = c;
  }
}

function unsafe_blit(s1, i1, s2, i2, len) {
  if (len <= 0) {
    return;
  }
  if (s1 === s2) {
    if (i1 < i2) {
      let range_a = (s1.length - i2 | 0) - 1 | 0;
      let range_b = len - 1 | 0;
      let range = range_a > range_b ? range_b : range_a;
      for (let j = range; j >= 0; --j) {
        s1[i2 + j | 0] = s1[i1 + j | 0];
      }
      return;
    }
    if (i1 <= i2) {
      return;
    }
    let range_a$1 = (s1.length - i1 | 0) - 1 | 0;
    let range_b$1 = len - 1 | 0;
    let range$1 = range_a$1 > range_b$1 ? range_b$1 : range_a$1;
    for (let k = 0; k <= range$1; ++k) {
      s1[i2 + k | 0] = s1[i1 + k | 0];
    }
    return;
  }
  let off1 = s1.length - i1 | 0;
  if (len <= off1) {
    for (let i = 0; i < len; ++i) {
      s2[i2 + i | 0] = s1[i1 + i | 0];
    }
    return;
  }
  for (let i$1 = 0; i$1 < off1; ++i$1) {
    s2[i2 + i$1 | 0] = s1[i1 + i$1 | 0];
  }
  for (let i$2 = off1; i$2 < len; ++i$2) {
    s2[i2 + i$2 | 0] = /* '\000' */0;
  }
}

function make(n, c) {
  let s = Caml_bytes.create(n);
  unsafe_fill(s, 0, n, c);
  return s;
}

function init(n, f) {
  let s = Caml_bytes.create(n);
  for (let i = 0; i < n; ++i) {
    s[i] = f(i);
  }
  return s;
}

let empty = [];

function copy(s) {
  let len = s.length;
  let r = Caml_bytes.create(len);
  unsafe_blit(s, 0, r, 0, len);
  return r;
}

function to_string(a) {
  let i = 0;
  let len = a.length;
  let s = "";
  let s_len = len;
  if (i === 0 && len <= 4096 && len === a.length) {
    return String.fromCharCode.apply(null, a);
  }
  let offset = 0;
  while (s_len > 0) {
    let next = s_len < 1024 ? s_len : 1024;
    let tmp_bytes = new Array(next);
    for (let k = 0; k < next; ++k) {
      tmp_bytes[k] = a[k + offset | 0];
    }
    s = s + String.fromCharCode.apply(null, tmp_bytes);
    s_len = s_len - next | 0;
    offset = offset + next | 0;
  };
  return s;
}

function of_string(s) {
  let len = s.length;
  let res = new Array(len);
  for (let i = 0; i < len; ++i) {
    res[i] = s.codePointAt(i);
  }
  return res;
}

function sub(s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "String.sub / Bytes.sub"
    });
  }
  let r = Caml_bytes.create(len);
  unsafe_blit(s, ofs, r, 0, len);
  return r;
}

function sub_string(b, ofs, len) {
  return to_string(sub(b, ofs, len));
}

function $plus$plus(a, b) {
  let c = a + b | 0;
  let match = a < 0;
  let match$1 = b < 0;
  let match$2 = c < 0;
  if (match) {
    if (!match$1) {
      return c;
    }
    if (match$2) {
      return c;
    }
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "Bytes.extend"
    });
  }
  if (match$1) {
    return c;
  }
  if (match$2) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "Bytes.extend"
    });
  }
  return c;
}

function extend(s, left, right) {
  let len = $plus$plus($plus$plus(s.length, left), right);
  let r = Caml_bytes.create(len);
  let match = left < 0 ? [
      -left | 0,
      0
    ] : [
      0,
      left
    ];
  let dstoff = match[1];
  let srcoff = match[0];
  let cpylen = Caml.int_min(s.length - srcoff | 0, len - dstoff | 0);
  if (cpylen > 0) {
    unsafe_blit(s, srcoff, r, dstoff, cpylen);
  }
  return r;
}

function fill(s, ofs, len, c) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "String.fill / Bytes.fill"
    });
  }
  unsafe_fill(s, ofs, len, c);
}

function blit(s1, ofs1, s2, ofs2, len) {
  if (len < 0 || ofs1 < 0 || ofs1 > (s1.length - len | 0) || ofs2 < 0 || ofs2 > (s2.length - len | 0)) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "Bytes.blit"
    });
  }
  unsafe_blit(s1, ofs1, s2, ofs2, len);
}

function blit_string(s1, ofs1, s2, ofs2, len) {
  if (len < 0 || ofs1 < 0 || ofs1 > (s1.length - len | 0) || ofs2 < 0 || ofs2 > (s2.length - len | 0)) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "String.blit / Bytes.blit_string"
    });
  }
  if (len <= 0) {
    return;
  }
  let off1 = s1.length - ofs1 | 0;
  if (len <= off1) {
    for (let i = 0; i < len; ++i) {
      s2[ofs2 + i | 0] = s1.codePointAt(ofs1 + i | 0);
    }
    return;
  }
  for (let i$1 = 0; i$1 < off1; ++i$1) {
    s2[ofs2 + i$1 | 0] = s1.codePointAt(ofs1 + i$1 | 0);
  }
  for (let i$2 = off1; i$2 < len; ++i$2) {
    s2[ofs2 + i$2 | 0] = /* '\000' */0;
  }
}

function iter(f, a) {
  for (let i = 0, i_finish = a.length; i < i_finish; ++i) {
    f(a[i]);
  }
}

function iteri(f, a) {
  for (let i = 0, i_finish = a.length; i < i_finish; ++i) {
    f(i, a[i]);
  }
}

function ensure_ge(x, y) {
  if (x >= y) {
    return x;
  }
  throw Caml_js_exceptions.internalFromExtension({
    RE_EXN_ID: "Invalid_argument",
    _1: "Bytes.concat"
  });
}

function sum_lengths(_acc, seplen, _param) {
  while (true) {
    let param = _param;
    let acc = _acc;
    if (!param) {
      return acc;
    }
    let tl = param.tl;
    let hd = param.hd;
    if (!tl) {
      return hd.length + acc | 0;
    }
    _param = tl;
    _acc = ensure_ge((hd.length + seplen | 0) + acc | 0, acc);
    continue;
  };
}

function concat(sep, param) {
  if (!param) {
    return empty;
  }
  let seplen = sep.length;
  let dst = Caml_bytes.create(sum_lengths(0, seplen, param));
  let _pos = 0;
  let _param = param;
  while (true) {
    let param$1 = _param;
    let pos = _pos;
    if (!param$1) {
      return dst;
    }
    let tl = param$1.tl;
    let hd = param$1.hd;
    if (tl) {
      unsafe_blit(hd, 0, dst, pos, hd.length);
      unsafe_blit(sep, 0, dst, pos + hd.length | 0, seplen);
      _param = tl;
      _pos = (pos + hd.length | 0) + seplen | 0;
      continue;
    }
    unsafe_blit(hd, 0, dst, pos, hd.length);
    return dst;
  };
}

function cat(s1, s2) {
  let l1 = s1.length;
  let l2 = s2.length;
  let r = Caml_bytes.create(l1 + l2 | 0);
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
  let len = s.length;
  let i = 0;
  while (i < len && is_space(s[i])) {
    i = i + 1 | 0;
  };
  let j = len - 1 | 0;
  while (j >= i && is_space(s[j])) {
    j = j - 1 | 0;
  };
  if (j >= i) {
    return sub(s, i, (j - i | 0) + 1 | 0);
  } else {
    return empty;
  }
}

function escaped(s) {
  let n = 0;
  for (let i = 0, i_finish = s.length; i < i_finish; ++i) {
    let match = s[i];
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
  let s$p = Caml_bytes.create(n);
  n = 0;
  for (let i$1 = 0, i_finish$1 = s.length; i$1 < i_finish$1; ++i$1) {
    let c = s[i$1];
    let exit = 0;
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
  let l = s.length;
  if (l === 0) {
    return s;
  }
  let r = Caml_bytes.create(l);
  for (let i = 0; i < l; ++i) {
    r[i] = f(s[i]);
  }
  return r;
}

function mapi(f, s) {
  let l = s.length;
  if (l === 0) {
    return s;
  }
  let r = Caml_bytes.create(l);
  for (let i = 0; i < l; ++i) {
    r[i] = f(i, s[i]);
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
  let r = copy(s);
  r[0] = f(s[0]);
  return r;
}

function capitalize_ascii(s) {
  return apply1(Char.uppercase_ascii, s);
}

function uncapitalize_ascii(s) {
  return apply1(Char.lowercase_ascii, s);
}

function index_rec(s, lim, _i, c) {
  while (true) {
    let i = _i;
    if (i >= lim) {
      throw Caml_js_exceptions.internalMakeExn("Not_found");
    }
    if (s[i] === c) {
      return i;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function index(s, c) {
  return index_rec(s, s.length, 0, c);
}

function index_rec_opt(s, lim, _i, c) {
  while (true) {
    let i = _i;
    if (i >= lim) {
      return;
    }
    if (s[i] === c) {
      return i;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function index_opt(s, c) {
  return index_rec_opt(s, s.length, 0, c);
}

function index_from(s, i, c) {
  let l = s.length;
  if (i < 0 || i > l) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "String.index_from / Bytes.index_from"
    });
  }
  return index_rec(s, l, i, c);
}

function index_from_opt(s, i, c) {
  let l = s.length;
  if (i < 0 || i > l) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "String.index_from_opt / Bytes.index_from_opt"
    });
  }
  return index_rec_opt(s, l, i, c);
}

function rindex_rec(s, _i, c) {
  while (true) {
    let i = _i;
    if (i < 0) {
      throw Caml_js_exceptions.internalMakeExn("Not_found");
    }
    if (s[i] === c) {
      return i;
    }
    _i = i - 1 | 0;
    continue;
  };
}

function rindex(s, c) {
  return rindex_rec(s, s.length - 1 | 0, c);
}

function rindex_from(s, i, c) {
  if (i < -1 || i >= s.length) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "String.rindex_from / Bytes.rindex_from"
    });
  }
  return rindex_rec(s, i, c);
}

function rindex_rec_opt(s, _i, c) {
  while (true) {
    let i = _i;
    if (i < 0) {
      return;
    }
    if (s[i] === c) {
      return i;
    }
    _i = i - 1 | 0;
    continue;
  };
}

function rindex_opt(s, c) {
  return rindex_rec_opt(s, s.length - 1 | 0, c);
}

function rindex_from_opt(s, i, c) {
  if (i < -1 || i >= s.length) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "String.rindex_from_opt / Bytes.rindex_from_opt"
    });
  }
  return rindex_rec_opt(s, i, c);
}

function contains_from(s, i, c) {
  let l = s.length;
  if (i < 0 || i > l) {
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "String.contains_from / Bytes.contains_from"
    });
  }
  try {
    index_rec(s, l, i, c);
    return true;
  } catch (raw_exn) {
    let exn = Caml_js_exceptions.internalAnyToExn(raw_exn);
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
    throw Caml_js_exceptions.internalFromExtension({
      RE_EXN_ID: "Invalid_argument",
      _1: "String.rcontains_from / Bytes.rcontains_from"
    });
  }
  try {
    rindex_rec(s, i, c);
    return true;
  } catch (raw_exn) {
    let exn = Caml_js_exceptions.internalAnyToExn(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return false;
    }
    throw exn;
  }
}

let compare = Caml_bytes.bytes_compare;

let equal = Caml_bytes.bytes_equal;

let unsafe_to_string = to_string;

let unsafe_of_string = of_string;

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
