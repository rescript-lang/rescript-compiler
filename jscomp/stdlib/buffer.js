// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Bytes                   = require("./bytes");
var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Pervasives              = require("./pervasives");
var Sys                     = require("./sys");
var $$String                = require("./string");
var Caml_curry              = require("../runtime/caml_curry");
var Caml_string             = require("../runtime/caml_string");

function create(n) {
  var n$1 = n < 1 ? 1 : n;
  var n$2 = n$1 > Sys.max_string_length ? Sys.max_string_length : n$1;
  var s = Caml_string.caml_create_string(n$2);
  return /* record */[
          s,
          0,
          n$2,
          s
        ];
}

function contents(b) {
  return Bytes.sub_string(b[0], 0, b[1]);
}

function to_bytes(b) {
  return Bytes.sub(b[0], 0, b[1]);
}

function sub(b, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > b[1] - len) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Buffer.sub"
        ];
  }
  else {
    return Bytes.sub_string(b[0], ofs, len);
  }
}

function blit(src, srcoff, dst, dstoff, len) {
  if (len < 0 || srcoff < 0 || srcoff > src[1] - len || dstoff < 0 || dstoff > dst.length - len) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Buffer.blit"
        ];
  }
  else {
    return Bytes.blit(src[0], srcoff, dst, dstoff, len);
  }
}

function nth(b, ofs) {
  if (ofs < 0 || ofs >= b[1]) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Buffer.nth"
        ];
  }
  else {
    return b[0][ofs];
  }
}

function length(b) {
  return b[1];
}

function clear(b) {
  b[1] = 0;
  return /* () */0;
}

function reset(b) {
  b[1] = 0;
  b[0] = b[3];
  b[2] = b[0].length;
  return /* () */0;
}

function resize(b, more) {
  var len = b[2];
  var new_len = len;
  while(b[1] + more > new_len) {
    new_len = 2 * new_len;
  };
  if (new_len > Sys.max_string_length) {
    if (b[1] + more <= Sys.max_string_length) {
      new_len = Sys.max_string_length;
    }
    else {
      throw [
            Caml_builtin_exceptions.failure,
            "Buffer.add: cannot grow buffer"
          ];
    }
  }
  var new_buffer = Caml_string.caml_create_string(new_len);
  Bytes.blit(b[0], 0, new_buffer, 0, b[1]);
  b[0] = new_buffer;
  b[2] = new_len;
  return /* () */0;
}

function add_char(b, c) {
  var pos = b[1];
  if (pos >= b[2]) {
    resize(b, 1);
  }
  b[0][pos] = c;
  b[1] = pos + 1;
  return /* () */0;
}

function add_substring(b, s, offset, len) {
  if (offset < 0 || len < 0 || offset + len > s.length) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Buffer.add_substring/add_subbytes"
        ];
  }
  var new_position = b[1] + len;
  if (new_position > b[2]) {
    resize(b, len);
  }
  Bytes.blit_string(s, offset, b[0], b[1], len);
  b[1] = new_position;
  return /* () */0;
}

function add_subbytes(b, s, offset, len) {
  return add_substring(b, Caml_string.bytes_to_string(s), offset, len);
}

function add_string(b, s) {
  var len = s.length;
  var new_position = b[1] + len;
  if (new_position > b[2]) {
    resize(b, len);
  }
  Bytes.blit_string(s, 0, b[0], b[1], len);
  b[1] = new_position;
  return /* () */0;
}

function add_bytes(b, s) {
  return add_string(b, Caml_string.bytes_to_string(s));
}

function add_buffer(b, bs) {
  return add_subbytes(b, bs[0], 0, bs[1]);
}

function add_channel(b, ic, len) {
  if (len < 0 || len > Sys.max_string_length) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Buffer.add_channel"
        ];
  }
  if (b[1] + len > b[2]) {
    resize(b, len);
  }
  Pervasives.really_input(ic, b[0], b[1], len);
  b[1] += len;
  return /* () */0;
}

function output_buffer(oc, b) {
  return Pervasives.output(oc, b[0], 0, b[1]);
}

function closing(param) {
  if (param !== 40) {
    if (param !== 123) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "buffer.ml",
              115,
              9
            ]
          ];
    }
    else {
      return /* "}" */125;
    }
  }
  else {
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
    else if (s.charCodeAt(i) === opening) {
      _i = i + 1;
      _k = k$1 + 1;
      continue ;
      
    }
    else if (s.charCodeAt(i) === closing) {
      if (k$1) {
        _i = i + 1;
        _k = k$1 - 1;
        continue ;
        
      }
      else {
        return i;
      }
    }
    else {
      _i = i + 1;
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
    }
    else {
      var match = s.charCodeAt(i);
      var exit = 0;
      if (match >= 91) {
        if (match >= 97) {
          if (match >= 123) {
            return i;
          }
          else {
            exit = 1;
          }
        }
        else if (match !== 95) {
          return i;
        }
        else {
          exit = 1;
        }
      }
      else if (match >= 58) {
        if (match >= 65) {
          exit = 1;
        }
        else {
          return i;
        }
      }
      else if (match >= 48) {
        exit = 1;
      }
      else {
        return i;
      }
      if (exit === 1) {
        _i = i + 1;
        continue ;
        
      }
      
    }
  };
}

function find_ident(s, start, lim) {
  if (start >= lim) {
    throw Caml_builtin_exceptions.not_found;
  }
  else {
    var c = s.charCodeAt(start);
    var exit = 0;
    if (c !== 40) {
      if (c !== 123) {
        var stop = advance_to_non_alpha(s, start + 1);
        return /* tuple */[
                $$String.sub(s, start, stop - start),
                stop
              ];
      }
      else {
        exit = 1;
      }
    }
    else {
      exit = 1;
    }
    if (exit === 1) {
      var new_start = start + 1;
      var stop$1 = advance_to_closing(c, closing(c), 0, s, new_start);
      return /* tuple */[
              $$String.sub(s, new_start, stop$1 - start - 1),
              stop$1 + 1
            ];
    }
    
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
      var current = s.charCodeAt(i);
      if (current !== 36) {
        if (previous === /* "\\" */92) {
          add_char(b, /* "\\" */92);
          add_char(b, current);
          _i = i + 1;
          _previous = /* " " */32;
          continue ;
          
        }
        else if (current !== 92) {
          add_char(b, current);
          _i = i + 1;
          _previous = current;
          continue ;
          
        }
        else {
          _i = i + 1;
          _previous = current;
          continue ;
          
        }
      }
      else if (previous === /* "\\" */92) {
        add_char(b, current);
        _i = i + 1;
        _previous = /* " " */32;
        continue ;
        
      }
      else {
        var j = i + 1;
        var match = find_ident(s, j, lim);
        add_string(b, Caml_curry.app1(f, match[0]));
        _i = match[1];
        _previous = /* " " */32;
        continue ;
        
      }
    }
    else if (previous === /* "\\" */92) {
      return add_char(b, previous);
    }
    else {
      return 0;
    }
  };
}

exports.create         = create;
exports.contents       = contents;
exports.to_bytes       = to_bytes;
exports.sub            = sub;
exports.blit           = blit;
exports.nth            = nth;
exports.length         = length;
exports.clear          = clear;
exports.reset          = reset;
exports.add_char       = add_char;
exports.add_string     = add_string;
exports.add_bytes      = add_bytes;
exports.add_substring  = add_substring;
exports.add_subbytes   = add_subbytes;
exports.add_substitute = add_substitute;
exports.add_buffer     = add_buffer;
exports.add_channel    = add_channel;
exports.output_buffer  = output_buffer;
/* No side effect */
