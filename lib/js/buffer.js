'use strict';

var Bytes = require("./bytes.js");
var Curry = require("./curry.js");
var $$String = require("./string.js");
var Caml_bytes = require("./caml_bytes.js");
var Pervasives = require("./pervasives.js");
var Caml_string = require("./caml_string.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function create(n) {
  var n$1 = n < 1 ? 1 : n;
  var s = Caml_bytes.caml_create_bytes(n$1);
  return /* record */[
          /* buffer */s,
          /* position */0,
          /* length */n$1,
          /* initial_buffer */s
        ];
}

function contents(b) {
  return Bytes.sub_string(b[/* buffer */0], 0, b[/* position */1]);
}

function to_bytes(b) {
  return Bytes.sub(b[/* buffer */0], 0, b[/* position */1]);
}

function sub(b, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (b[/* position */1] - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Buffer.sub"
        ];
  }
  return Bytes.sub_string(b[/* buffer */0], ofs, len);
}

function blit(src, srcoff, dst, dstoff, len) {
  if (len < 0 || srcoff < 0 || srcoff > (src[/* position */1] - len | 0) || dstoff < 0 || dstoff > (dst.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Buffer.blit"
        ];
  }
  return Caml_bytes.caml_blit_bytes(src[/* buffer */0], srcoff, dst, dstoff, len);
}

function nth(b, ofs) {
  if (ofs < 0 || ofs >= b[/* position */1]) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Buffer.nth"
        ];
  }
  return b[/* buffer */0][ofs];
}

function length(b) {
  return b[/* position */1];
}

function clear(b) {
  b[/* position */1] = 0;
  return /* () */0;
}

function reset(b) {
  b[/* position */1] = 0;
  b[/* buffer */0] = b[/* initial_buffer */3];
  b[/* length */2] = b[/* buffer */0].length;
  return /* () */0;
}

function resize(b, more) {
  var len = b[/* length */2];
  var new_len = len;
  while((b[/* position */1] + more | 0) > new_len) {
    new_len = (new_len << 1);
  };
  var new_buffer = Caml_bytes.caml_create_bytes(new_len);
  Bytes.blit(b[/* buffer */0], 0, new_buffer, 0, b[/* position */1]);
  b[/* buffer */0] = new_buffer;
  b[/* length */2] = new_len;
  return /* () */0;
}

function add_char(b, c) {
  var pos = b[/* position */1];
  if (pos >= b[/* length */2]) {
    resize(b, 1);
  }
  b[/* buffer */0][pos] = c;
  b[/* position */1] = pos + 1 | 0;
  return /* () */0;
}

function add_utf_8_uchar(b, u) {
  var u$1 = u;
  if (u$1 < 0) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "buffer.ml",
            90,
            19
          ]
        ];
  }
  if (u$1 <= 127) {
    return add_char(b, u$1);
  } else if (u$1 <= 2047) {
    var pos = b[/* position */1];
    if ((pos + 2 | 0) > b[/* length */2]) {
      resize(b, 2);
    }
    b[/* buffer */0][pos] = 192 | (u$1 >>> 6);
    b[/* buffer */0][pos + 1 | 0] = 128 | u$1 & 63;
    b[/* position */1] = pos + 2 | 0;
    return /* () */0;
  } else if (u$1 <= 65535) {
    var pos$1 = b[/* position */1];
    if ((pos$1 + 3 | 0) > b[/* length */2]) {
      resize(b, 3);
    }
    b[/* buffer */0][pos$1] = 224 | (u$1 >>> 12);
    b[/* buffer */0][pos$1 + 1 | 0] = 128 | (u$1 >>> 6) & 63;
    b[/* buffer */0][pos$1 + 2 | 0] = 128 | u$1 & 63;
    b[/* position */1] = pos$1 + 3 | 0;
    return /* () */0;
  } else if (u$1 <= 1114111) {
    var pos$2 = b[/* position */1];
    if ((pos$2 + 4 | 0) > b[/* length */2]) {
      resize(b, 4);
    }
    b[/* buffer */0][pos$2] = 240 | (u$1 >>> 18);
    b[/* buffer */0][pos$2 + 1 | 0] = 128 | (u$1 >>> 12) & 63;
    b[/* buffer */0][pos$2 + 2 | 0] = 128 | (u$1 >>> 6) & 63;
    b[/* buffer */0][pos$2 + 3 | 0] = 128 | u$1 & 63;
    b[/* position */1] = pos$2 + 4 | 0;
    return /* () */0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "buffer.ml",
            123,
            8
          ]
        ];
  }
}

function add_utf_16be_uchar(b, u) {
  var u$1 = u;
  if (u$1 < 0) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "buffer.ml",
            126,
            19
          ]
        ];
  }
  if (u$1 <= 65535) {
    var pos = b[/* position */1];
    if ((pos + 2 | 0) > b[/* length */2]) {
      resize(b, 2);
    }
    b[/* buffer */0][pos] = (u$1 >>> 8);
    b[/* buffer */0][pos + 1 | 0] = u$1 & 255;
    b[/* position */1] = pos + 2 | 0;
    return /* () */0;
  } else if (u$1 <= 1114111) {
    var u$prime = u$1 - 65536 | 0;
    var hi = 55296 | (u$prime >>> 10);
    var lo = 56320 | u$prime & 1023;
    var pos$1 = b[/* position */1];
    if ((pos$1 + 4 | 0) > b[/* length */2]) {
      resize(b, 4);
    }
    b[/* buffer */0][pos$1] = (hi >>> 8);
    b[/* buffer */0][pos$1 + 1 | 0] = hi & 255;
    b[/* buffer */0][pos$1 + 2 | 0] = (lo >>> 8);
    b[/* buffer */0][pos$1 + 3 | 0] = lo & 255;
    b[/* position */1] = pos$1 + 4 | 0;
    return /* () */0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "buffer.ml",
            144,
            8
          ]
        ];
  }
}

function add_utf_16le_uchar(b, u) {
  var u$1 = u;
  if (u$1 < 0) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "buffer.ml",
            147,
            19
          ]
        ];
  }
  if (u$1 <= 65535) {
    var pos = b[/* position */1];
    if ((pos + 2 | 0) > b[/* length */2]) {
      resize(b, 2);
    }
    b[/* buffer */0][pos] = u$1 & 255;
    b[/* buffer */0][pos + 1 | 0] = (u$1 >>> 8);
    b[/* position */1] = pos + 2 | 0;
    return /* () */0;
  } else if (u$1 <= 1114111) {
    var u$prime = u$1 - 65536 | 0;
    var hi = 55296 | (u$prime >>> 10);
    var lo = 56320 | u$prime & 1023;
    var pos$1 = b[/* position */1];
    if ((pos$1 + 4 | 0) > b[/* length */2]) {
      resize(b, 4);
    }
    b[/* buffer */0][pos$1] = hi & 255;
    b[/* buffer */0][pos$1 + 1 | 0] = (hi >>> 8);
    b[/* buffer */0][pos$1 + 2 | 0] = lo & 255;
    b[/* buffer */0][pos$1 + 3 | 0] = (lo >>> 8);
    b[/* position */1] = pos$1 + 4 | 0;
    return /* () */0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "buffer.ml",
            165,
            8
          ]
        ];
  }
}

function add_substring(b, s, offset, len) {
  if (offset < 0 || len < 0 || offset > (s.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Buffer.add_substring/add_subbytes"
        ];
  }
  var new_position = b[/* position */1] + len | 0;
  if (new_position > b[/* length */2]) {
    resize(b, len);
  }
  Bytes.blit_string(s, offset, b[/* buffer */0], b[/* position */1], len);
  b[/* position */1] = new_position;
  return /* () */0;
}

function add_subbytes(b, s, offset, len) {
  return add_substring(b, Caml_bytes.bytes_to_string(s), offset, len);
}

function add_string(b, s) {
  var len = s.length;
  var new_position = b[/* position */1] + len | 0;
  if (new_position > b[/* length */2]) {
    resize(b, len);
  }
  Bytes.blit_string(s, 0, b[/* buffer */0], b[/* position */1], len);
  b[/* position */1] = new_position;
  return /* () */0;
}

function add_bytes(b, s) {
  return add_string(b, Caml_bytes.bytes_to_string(s));
}

function add_buffer(b, bs) {
  return add_subbytes(b, bs[/* buffer */0], 0, bs[/* position */1]);
}

function add_channel(b, ic, len) {
  if (len < 0) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Buffer.add_channel"
        ];
  }
  if ((b[/* position */1] + len | 0) > b[/* length */2]) {
    resize(b, len);
  }
  var b$1 = b;
  var ic$1 = ic;
  var _len = len;
  while(true) {
    var len$1 = _len;
    if (len$1 > 0) {
      var n = Pervasives.input(ic$1, b$1[/* buffer */0], b$1[/* position */1], len$1);
      b$1[/* position */1] = b$1[/* position */1] + n | 0;
      if (n === 0) {
        throw Caml_builtin_exceptions.end_of_file;
      }
      _len = len$1 - n | 0;
      continue ;
    } else {
      return 0;
    }
  };
}

function output_buffer(oc, b) {
  return Pervasives.output(oc, b[/* buffer */0], 0, b[/* position */1]);
}

function closing(param) {
  if (param !== 40) {
    if (param !== 123) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            /* tuple */[
              "buffer.ml",
              216,
              9
            ]
          ];
    } else {
      return /* "}" */125;
    }
  } else {
    return /* ")" */41;
  }
}

function advance_to_closing(opening, closing, k, s, start) {
  var _k = k;
  var _i = start;
  var lim = s.length;
  while(true) {
    var i = _i;
    var k$1 = _k;
    if (i >= lim) {
      throw Caml_builtin_exceptions.not_found;
    }
    if (Caml_string.get(s, i) === opening) {
      _i = i + 1 | 0;
      _k = k$1 + 1 | 0;
      continue ;
    } else if (Caml_string.get(s, i) === closing) {
      if (k$1 === 0) {
        return i;
      } else {
        _i = i + 1 | 0;
        _k = k$1 - 1 | 0;
        continue ;
      }
    } else {
      _i = i + 1 | 0;
      continue ;
    }
  };
}

function advance_to_non_alpha(s, start) {
  var _i = start;
  var lim = s.length;
  while(true) {
    var i = _i;
    if (i >= lim) {
      return lim;
    } else {
      var match = Caml_string.get(s, i);
      var exit = 0;
      if (match >= 91) {
        if (match >= 97) {
          if (match >= 123) {
            return i;
          } else {
            exit = 1;
          }
        } else if (match !== 95) {
          return i;
        } else {
          exit = 1;
        }
      } else if (match >= 58) {
        if (match >= 65) {
          exit = 1;
        } else {
          return i;
        }
      } else if (match >= 48) {
        exit = 1;
      } else {
        return i;
      }
      if (exit === 1) {
        _i = i + 1 | 0;
        continue ;
      }
      
    }
  };
}

function find_ident(s, start, lim) {
  if (start >= lim) {
    throw Caml_builtin_exceptions.not_found;
  }
  var c = Caml_string.get(s, start);
  var exit = 0;
  if (c !== 40 && c !== 123) {
    var stop = advance_to_non_alpha(s, start + 1 | 0);
    return /* tuple */[
            $$String.sub(s, start, stop - start | 0),
            stop
          ];
  } else {
    exit = 1;
  }
  if (exit === 1) {
    var new_start = start + 1 | 0;
    var stop$1 = advance_to_closing(c, closing(c), 0, s, new_start);
    return /* tuple */[
            $$String.sub(s, new_start, (stop$1 - start | 0) - 1 | 0),
            stop$1 + 1 | 0
          ];
  }
  
}

function add_substitute(b, f, s) {
  var lim = s.length;
  var _previous = /* " " */32;
  var _i = 0;
  while(true) {
    var i = _i;
    var previous = _previous;
    if (i < lim) {
      var current = Caml_string.get(s, i);
      if (current !== 36) {
        if (previous === /* "\\" */92) {
          add_char(b, /* "\\" */92);
          add_char(b, current);
          _i = i + 1 | 0;
          _previous = /* " " */32;
          continue ;
        } else if (current !== 92) {
          add_char(b, current);
          _i = i + 1 | 0;
          _previous = current;
          continue ;
        } else {
          _i = i + 1 | 0;
          _previous = current;
          continue ;
        }
      } else if (previous === /* "\\" */92) {
        add_char(b, current);
        _i = i + 1 | 0;
        _previous = /* " " */32;
        continue ;
      } else {
        var j = i + 1 | 0;
        var match = find_ident(s, j, lim);
        add_string(b, Curry._1(f, match[0]));
        _i = match[1];
        _previous = /* " " */32;
        continue ;
      }
    } else if (previous === /* "\\" */92) {
      return add_char(b, previous);
    } else {
      return 0;
    }
  };
}

function truncate(b, len) {
  if (len < 0 || len > b[/* position */1]) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Buffer.truncate"
        ];
  }
  b[/* position */1] = len;
  return /* () */0;
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
exports.add_channel = add_channel;
exports.output_buffer = output_buffer;
exports.truncate = truncate;
/* No side effect */
