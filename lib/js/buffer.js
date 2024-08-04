'use strict';

let Bytes = require("./bytes.js");
let $$String = require("./string.js");
let Caml_bytes = require("./caml_bytes.js");
let Caml_string = require("./caml_string.js");

function create(n) {
  let n$1 = n < 1 ? 1 : n;
  let s = Caml_bytes.create(n$1);
  return {
    buffer: s,
    position: 0,
    length: n$1,
    initial_buffer: s
  };
}

function contents(b) {
  return Bytes.sub_string(b.buffer, 0, b.position);
}

function to_bytes(b) {
  return Bytes.sub(b.buffer, 0, b.position);
}

function sub(b, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (b.position - len | 0)) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "Buffer.sub"
      }
    });
  }
  return Bytes.sub_string(b.buffer, ofs, len);
}

function blit(src, srcoff, dst, dstoff, len) {
  if (len < 0 || srcoff < 0 || srcoff > (src.position - len | 0) || dstoff < 0 || dstoff > (dst.length - len | 0)) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "Buffer.blit"
      }
    });
  }
  Bytes.blit(src.buffer, srcoff, dst, dstoff, len);
}

function nth(b, ofs) {
  if (ofs < 0 || ofs >= b.position) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "Buffer.nth"
      }
    });
  }
  return b.buffer[ofs];
}

function length(b) {
  return b.position;
}

function clear(b) {
  b.position = 0;
}

function reset(b) {
  b.position = 0;
  b.buffer = b.initial_buffer;
  b.length = b.buffer.length;
}

function resize(b, more) {
  let len = b.length;
  let new_len = len;
  while ((b.position + more | 0) > new_len) {
    new_len = (new_len << 1);
  };
  let new_buffer = Caml_bytes.create(new_len);
  Bytes.blit(b.buffer, 0, new_buffer, 0, b.position);
  b.buffer = new_buffer;
  b.length = new_len;
}

function add_char(b, c) {
  let pos = b.position;
  if (pos >= b.length) {
    resize(b, 1);
  }
  b.buffer[pos] = c;
  b.position = pos + 1 | 0;
}

function add_utf_8_uchar(b, u) {
  let u$1 = u;
  if (u$1 < 0) {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "buffer.res",
          98,
          18
        ]
      }
    });
  }
  if (u$1 <= 127) {
    return add_char(b, u$1);
  }
  if (u$1 <= 2047) {
    let pos = b.position;
    if ((pos + 2 | 0) > b.length) {
      resize(b, 2);
    }
    b.buffer[pos] = 192 | (u$1 >>> 6);
    b.buffer[pos + 1 | 0] = 128 | u$1 & 63;
    b.position = pos + 2 | 0;
    return;
  }
  if (u$1 <= 65535) {
    let pos$1 = b.position;
    if ((pos$1 + 3 | 0) > b.length) {
      resize(b, 3);
    }
    b.buffer[pos$1] = 224 | (u$1 >>> 12);
    b.buffer[pos$1 + 1 | 0] = 128 | (u$1 >>> 6) & 63;
    b.buffer[pos$1 + 2 | 0] = 128 | u$1 & 63;
    b.position = pos$1 + 3 | 0;
    return;
  }
  if (u$1 <= 1114111) {
    let pos$2 = b.position;
    if ((pos$2 + 4 | 0) > b.length) {
      resize(b, 4);
    }
    b.buffer[pos$2] = 240 | (u$1 >>> 18);
    b.buffer[pos$2 + 1 | 0] = 128 | (u$1 >>> 12) & 63;
    b.buffer[pos$2 + 2 | 0] = 128 | (u$1 >>> 6) & 63;
    b.buffer[pos$2 + 3 | 0] = 128 | u$1 & 63;
    b.position = pos$2 + 4 | 0;
    return;
  }
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "buffer.res",
        127,
        9
      ]
    }
  });
}

function add_utf_16be_uchar(b, u) {
  let u$1 = u;
  if (u$1 < 0) {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "buffer.res",
          132,
          18
        ]
      }
    });
  }
  if (u$1 <= 65535) {
    let pos = b.position;
    if ((pos + 2 | 0) > b.length) {
      resize(b, 2);
    }
    b.buffer[pos] = (u$1 >>> 8);
    b.buffer[pos + 1 | 0] = u$1 & 255;
    b.position = pos + 2 | 0;
    return;
  }
  if (u$1 <= 1114111) {
    let u$p = u$1 - 65536 | 0;
    let hi = 55296 | (u$p >>> 10);
    let lo = 56320 | u$p & 1023;
    let pos$1 = b.position;
    if ((pos$1 + 4 | 0) > b.length) {
      resize(b, 4);
    }
    b.buffer[pos$1] = (hi >>> 8);
    b.buffer[pos$1 + 1 | 0] = hi & 255;
    b.buffer[pos$1 + 2 | 0] = (lo >>> 8);
    b.buffer[pos$1 + 3 | 0] = lo & 255;
    b.position = pos$1 + 4 | 0;
    return;
  }
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "buffer.res",
        154,
        9
      ]
    }
  });
}

function add_utf_16le_uchar(b, u) {
  let u$1 = u;
  if (u$1 < 0) {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "buffer.res",
          159,
          18
        ]
      }
    });
  }
  if (u$1 <= 65535) {
    let pos = b.position;
    if ((pos + 2 | 0) > b.length) {
      resize(b, 2);
    }
    b.buffer[pos] = u$1 & 255;
    b.buffer[pos + 1 | 0] = (u$1 >>> 8);
    b.position = pos + 2 | 0;
    return;
  }
  if (u$1 <= 1114111) {
    let u$p = u$1 - 65536 | 0;
    let hi = 55296 | (u$p >>> 10);
    let lo = 56320 | u$p & 1023;
    let pos$1 = b.position;
    if ((pos$1 + 4 | 0) > b.length) {
      resize(b, 4);
    }
    b.buffer[pos$1] = hi & 255;
    b.buffer[pos$1 + 1 | 0] = (hi >>> 8);
    b.buffer[pos$1 + 2 | 0] = lo & 255;
    b.buffer[pos$1 + 3 | 0] = (lo >>> 8);
    b.position = pos$1 + 4 | 0;
    return;
  }
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "buffer.res",
        181,
        9
      ]
    }
  });
}

function add_substring(b, s, offset, len) {
  if (offset < 0 || len < 0 || offset > (s.length - len | 0)) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "Buffer.add_substring/add_subbytes"
      }
    });
  }
  let new_position = b.position + len | 0;
  if (new_position > b.length) {
    resize(b, len);
  }
  Bytes.blit_string(s, offset, b.buffer, b.position, len);
  b.position = new_position;
}

function add_subbytes(b, s, offset, len) {
  add_substring(b, Bytes.unsafe_to_string(s), offset, len);
}

function add_string(b, s) {
  let len = s.length;
  let new_position = b.position + len | 0;
  if (new_position > b.length) {
    resize(b, len);
  }
  Bytes.blit_string(s, 0, b.buffer, b.position, len);
  b.position = new_position;
}

function add_bytes(b, s) {
  add_string(b, Bytes.unsafe_to_string(s));
}

function add_buffer(b, bs) {
  add_subbytes(b, bs.buffer, 0, bs.position);
}

function closing(param) {
  if (param === 40) {
    return /* ')' */41;
  }
  if (param === 123) {
    return /* '}' */125;
  }
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "buffer.res",
        216,
        9
      ]
    }
  });
}

function advance_to_closing(opening, closing, k, s, start) {
  let _k = k;
  let _i = start;
  let lim = s.length;
  while (true) {
    let i = _i;
    let k$1 = _k;
    if (i >= lim) {
      throw new Error("Not_found", {
        cause: {
          RE_EXN_ID: "Not_found"
        }
      });
    }
    if (Caml_string.get(s, i) === opening) {
      _i = i + 1 | 0;
      _k = k$1 + 1 | 0;
      continue;
    }
    if (Caml_string.get(s, i) === closing) {
      if (k$1 === 0) {
        return i;
      }
      _i = i + 1 | 0;
      _k = k$1 - 1 | 0;
      continue;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function advance_to_non_alpha(s, start) {
  let _i = start;
  let lim = s.length;
  while (true) {
    let i = _i;
    if (i >= lim) {
      return lim;
    }
    let match = Caml_string.get(s, i);
    if (match >= 91) {
      if (match >= 97) {
        if (match >= 123) {
          return i;
        }
        
      } else if (match !== 95) {
        return i;
      }
      
    } else if (match >= 58) {
      if (match < 65) {
        return i;
      }
      
    } else if (match < 48) {
      return i;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function find_ident(s, start, lim) {
  if (start >= lim) {
    throw new Error("Not_found", {
      cause: {
        RE_EXN_ID: "Not_found"
      }
    });
  }
  let c = Caml_string.get(s, start);
  if (c !== 40 && c !== 123) {
    let stop = advance_to_non_alpha(s, start + 1 | 0);
    return [
      $$String.sub(s, start, stop - start | 0),
      stop
    ];
  }
  let new_start = start + 1 | 0;
  let stop$1 = advance_to_closing(c, closing(c), 0, s, new_start);
  return [
    $$String.sub(s, new_start, (stop$1 - start | 0) - 1 | 0),
    stop$1 + 1 | 0
  ];
}

function add_substitute(b, f, s) {
  let lim = s.length;
  let _previous = /* ' ' */32;
  let _i = 0;
  while (true) {
    let i = _i;
    let previous = _previous;
    if (i >= lim) {
      if (previous === /* '\\' */92) {
        return add_char(b, previous);
      } else {
        return;
      }
    }
    let current = Caml_string.get(s, i);
    if (current !== 36) {
      if (previous === /* '\\' */92) {
        add_char(b, /* '\\' */92);
        add_char(b, current);
        _i = i + 1 | 0;
        _previous = /* ' ' */32;
        continue;
      }
      if (current !== 92) {
        add_char(b, current);
        _i = i + 1 | 0;
        _previous = current;
        continue;
      }
      _i = i + 1 | 0;
      _previous = current;
      continue;
    }
    if (previous === /* '\\' */92) {
      add_char(b, current);
      _i = i + 1 | 0;
      _previous = /* ' ' */32;
      continue;
    }
    let j = i + 1 | 0;
    let match = find_ident(s, j, lim);
    add_string(b, f(match[0]));
    _i = match[1];
    _previous = /* ' ' */32;
    continue;
  };
}

function truncate(b, len) {
  if (len < 0 || len > b.position) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "Buffer.truncate"
      }
    });
  }
  b.position = len;
}

exports.create = create;
exports.contents = contents;
exports.to_bytes = to_bytes;
exports.sub = sub;
exports.blit = blit;
exports.nth = nth;
exports.length = length;
exports.clear = clear;
exports.reset = reset;
exports.add_char = add_char;
exports.add_utf_8_uchar = add_utf_8_uchar;
exports.add_utf_16le_uchar = add_utf_16le_uchar;
exports.add_utf_16be_uchar = add_utf_16be_uchar;
exports.add_string = add_string;
exports.add_bytes = add_bytes;
exports.add_substring = add_substring;
exports.add_subbytes = add_subbytes;
exports.add_substitute = add_substitute;
exports.add_buffer = add_buffer;
exports.truncate = truncate;
/* No side effect */
