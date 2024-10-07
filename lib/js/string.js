'use strict';

let Char = require("./char.js");
let $$Array = require("./array.js");
let Primitive_exceptions = require("./primitive_exceptions.js");

function apply1(f, bytes) {
  if (bytes.length === 0) {
    return bytes;
  }
  let r = bytes.slice();
  r[0] = f(bytes[0]);
  return r;
}

function concat(sep, xs) {
  return $$Array.of_list(xs).join(sep);
}

function bos(str) {
  return $$Array.map(str => str.codePointAt(0), Array.from(str));
}

function make(len, ch) {
  return String.fromCodePoint(ch).repeat(len);
}

function init(len, f) {
  return $$Array.init(len, i => String.fromCodePoint(f(i))).join("");
}

function sub(s, ofs, len) {
  return String.fromCodePoint(...$$Array.sub(bos(s), ofs, len));
}

function iter(f, s) {
  for (let i = 0, i_finish = s.length; i < i_finish; ++i) {
    f(s.codePointAt(i));
  }
}

function iteri(f, s) {
  for (let i = 0, i_finish = s.length; i < i_finish; ++i) {
    f(i, s.codePointAt(i));
  }
}

function map(f, s) {
  return String.fromCodePoint(...$$Array.map(f, bos(s)));
}

function mapi(f, s) {
  return String.fromCodePoint(...$$Array.mapi(f, bos(s)));
}

function escaped(s) {
  let needs_escape = _i => {
    while (true) {
      let i = _i;
      if (i >= s.length) {
        return false;
      }
      let match = s.codePointAt(i);
      if (match < 32) {
        return true;
      }
      if (match > 92 || match < 34) {
        if (match >= 127) {
          return true;
        }
        _i = i + 1 | 0;
        continue;
      }
      if (match > 91 || match < 35) {
        return true;
      }
      _i = i + 1 | 0;
      continue;
    };
  };
  if (!needs_escape(0)) {
    return s;
  }
  let bytes = bos(s);
  return $$Array.map(Char.escaped, bytes).join("");
}

function index_rec(s, lim, _i, c) {
  while (true) {
    let i = _i;
    if (i >= lim) {
      throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
    }
    if (s.codePointAt(i) === c) {
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
    if (s.codePointAt(i) === c) {
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
    throw {
      RE_EXN_ID: "Invalid_argument",
      _1: "String.index_from / Bytes.index_from",
      Error: new Error()
    };
  }
  return index_rec(s, l, i, c);
}

function index_from_opt(s, i, c) {
  let l = s.length;
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
  while (true) {
    let i = _i;
    if (i < 0) {
      throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
    }
    if (s.codePointAt(i) === c) {
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
    throw {
      RE_EXN_ID: "Invalid_argument",
      _1: "String.rindex_from / Bytes.rindex_from",
      Error: new Error()
    };
  }
  return rindex_rec(s, i, c);
}

function rindex_rec_opt(s, _i, c) {
  while (true) {
    let i = _i;
    if (i < 0) {
      return;
    }
    if (s.codePointAt(i) === c) {
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
    throw {
      RE_EXN_ID: "Invalid_argument",
      _1: "String.rindex_from_opt / Bytes.rindex_from_opt",
      Error: new Error()
    };
  }
  return rindex_rec_opt(s, i, c);
}

function contains_from(s, i, c) {
  let l = s.length;
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
  } catch (raw_exn) {
    let exn = Primitive_exceptions.internalToException(raw_exn);
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
  } catch (raw_exn) {
    let exn = Primitive_exceptions.internalToException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return false;
    }
    throw exn;
  }
}

function uppercase_ascii(s) {
  let bytes = bos(s);
  return String.fromCodePoint(...$$Array.map(Char.uppercase_ascii, bytes));
}

function lowercase_ascii(s) {
  let bytes = bos(s);
  return String.fromCodePoint(...$$Array.map(Char.lowercase_ascii, bytes));
}

function capitalize_ascii(s) {
  let bytes = bos(s);
  return String.fromCodePoint(...apply1(Char.uppercase_ascii, bytes));
}

function uncapitalize_ascii(s) {
  let bytes = bos(s);
  return String.fromCodePoint(...apply1(Char.lowercase_ascii, bytes));
}

function split_on_char(sep, s) {
  let r = /* [] */0;
  let j = s.length;
  for (let i = s.length - 1 | 0; i >= 0; --i) {
    if (s.codePointAt(i) === sep) {
      r = {
        hd: sub(s, i + 1 | 0, (j - i | 0) - 1 | 0),
        tl: r
      };
      j = i;
    }
    
  }
  return {
    hd: sub(s, 0, j),
    tl: r
  };
}

exports.make = make;
exports.init = init;
exports.sub = sub;
exports.concat = concat;
exports.iter = iter;
exports.iteri = iteri;
exports.map = map;
exports.mapi = mapi;
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
exports.split_on_char = split_on_char;
/* No side effect */
