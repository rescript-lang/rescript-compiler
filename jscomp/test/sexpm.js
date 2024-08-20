// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Sys = require("../../lib/js/sys.js");
let Caml = require("../../lib/js/caml.js");
let Char = require("../../lib/js/char.js");
let List = require("../../lib/js/list.js");
let Bytes = require("../../lib/js/bytes.js");
let Buffer = require("../../lib/js/buffer.js");
let $$String = require("../../lib/js/string.js");
let Caml_bytes = require("../../lib/js/caml_bytes.js");
let Pervasives = require("../../lib/js/pervasives.js");
let Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function _must_escape(s) {
  try {
    for (let i = 0, i_finish = s.length; i < i_finish; ++i) {
      let c = s.codePointAt(i);
      let exit = 0;
      if (c >= 42) {
        if (c !== 59) {
          if (c !== 92) {
            exit = 1;
          } else {
            throw new Error(Pervasives.Exit, {
              cause: {
                RE_EXN_ID: Pervasives.Exit
              }
            });
          }
        } else {
          throw new Error(Pervasives.Exit, {
            cause: {
              RE_EXN_ID: Pervasives.Exit
            }
          });
        }
      } else if (c >= 11) {
        if (c >= 32) {
          switch (c) {
            case 33 :
            case 35 :
            case 36 :
            case 37 :
            case 38 :
            case 39 :
              exit = 1;
              break;
            case 32 :
            case 34 :
            case 40 :
            case 41 :
              throw new Error(Pervasives.Exit, {
                cause: {
                  RE_EXN_ID: Pervasives.Exit
                }
              });
          }
        } else {
          exit = 1;
        }
      } else {
        if (c >= 9) {
          throw new Error(Pervasives.Exit, {
            cause: {
              RE_EXN_ID: Pervasives.Exit
            }
          });
        }
        exit = 1;
      }
      if (exit === 1 && c > 127) {
        throw new Error(Pervasives.Exit, {
          cause: {
            RE_EXN_ID: Pervasives.Exit
          }
        });
      }
      
    }
    return false;
  } catch (raw_exn) {
    let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === Pervasives.Exit) {
      return true;
    }
    throw exn;
  }
}

function to_buf(b, t) {
  if (t.NAME === "List") {
    let l = t.VAL;
    if (l) {
      if (l.tl) {
        Buffer.add_char(b, /* '(' */40);
        List.iteri((function (i, t$p) {
          if (i > 0) {
            Buffer.add_char(b, /* ' ' */32);
          }
          to_buf(b, t$p);
        }), l);
        return Buffer.add_char(b, /* ')' */41);
      } else {
        Buffer.add_string(b, "(");
        to_buf(b, l.hd);
        return Buffer.add_string(b, ")");
      }
    } else {
      return Buffer.add_string(b, "()");
    }
  }
  let s = t.VAL;
  if (_must_escape(s)) {
    return Buffer.add_string(b, "\"" + ($$String.escaped(s) + "\""));
  } else {
    return Buffer.add_string(b, s);
  }
}

function to_string(t) {
  let b = Buffer.create(128);
  to_buf(b, t);
  return Buffer.contents(b);
}

function make(bufsizeOpt, refill) {
  let bufsize = bufsizeOpt !== undefined ? bufsizeOpt : 1024;
  let bufsize$1 = Caml.int_min(bufsize > 16 ? bufsize : 16, Sys.max_string_length);
  return {
    buf: Caml_bytes.create(bufsize$1),
    refill: refill,
    atom: Buffer.create(32),
    i: 0,
    len: 0,
    line: 1,
    col: 1
  };
}

function _is_digit(c) {
  if (/* '0' */48 <= c) {
    return c <= /* '9' */57;
  } else {
    return false;
  }
}

function _refill(t, k_succ, k_fail) {
  let n = t.refill(t.buf, 0, t.buf.length);
  t.i = 0;
  t.len = n;
  if (n === 0) {
    return k_fail(t);
  } else {
    return k_succ(t);
  }
}

function _get(t) {
  if (t.i >= t.len) {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "sexpm.res",
          111,
          4
        ]
      }
    });
  }
  let c = Caml_bytes.get(t.buf, t.i);
  t.i = t.i + 1 | 0;
  if (c === /* '\n' */10) {
    t.col = 1;
    t.line = t.line + 1 | 0;
  } else {
    t.col = t.col + 1 | 0;
  }
  return c;
}

function _error(param, param$1) {
  let line = param.line;
  let col = param.col;
  let b = Buffer.create(32);
  Buffer.add_string(b, "at " + (line + (", " + (col + ": "))));
  Buffer.add_string(b, param$1);
  let msg$p = Buffer.contents(b);
  return {
    NAME: "Error",
    VAL: msg$p
  };
}

function _error_eof(t) {
  return _error(t, "unexpected end of input");
}

function expr(k, t) {
  while (true) {
    if (t.i === t.len) {
      return _refill(t, (function (extra) {
        return expr(k, extra);
      }), _error_eof);
    }
    let c = _get(t);
    if (c >= 11) {
      if (c !== 32) {
        return expr_starting_with(c, k, t);
      }
      continue;
    }
    if (c < 9) {
      return expr_starting_with(c, k, t);
    }
    continue;
  };
}

function expr_starting_with(c, k, t) {
  if (c >= 42) {
    if (c === 59) {
      return skip_comment((function (param, param$1) {
        return expr(k, t);
      }), t);
    }
    if (c === 92) {
      return _error(t, "unexpected '\\'");
    }
    
  } else if (c >= 11) {
    if (c >= 32) {
      switch (c) {
        case 32 :
          throw new Error("Assert_failure", {
            cause: {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "sexpm.res",
                152,
                27
              ]
            }
          });
        case 34 :
          return quoted(k, t);
        case 33 :
        case 35 :
        case 36 :
        case 37 :
        case 38 :
        case 39 :
          break;
        case 40 :
          return expr_list(/* [] */0, k, t);
        case 41 :
          return _error(t, "unexpected ')'");
      }
    }
    
  } else if (c >= 9) {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "sexpm.res",
          152,
          27
        ]
      }
    });
  }
  Buffer.add_char(t.atom, c);
  return atom(k, t);
}

function expr_list(acc, k, t) {
  while (true) {
    if (t.i === t.len) {
      return _refill(t, (function (extra) {
        return expr_list(acc, k, extra);
      }), _error_eof);
    }
    let c = _get(t);
    if (c > 32 || c < 9) {
      if (c === 41) {
        return k(undefined, {
          NAME: "List",
          VAL: List.rev(acc)
        });
      }
      
    } else if (c > 31 || c < 11) {
      continue;
    }
    return expr_starting_with(c, (function (last, e) {
      if (last !== undefined) {
        if (last !== 40) {
          if (last !== 41) {
            return expr_list({
              hd: e,
              tl: acc
            }, k, t);
          } else {
            return k(undefined, {
              NAME: "List",
              VAL: List.rev({
                hd: e,
                tl: acc
              })
            });
          }
        } else {
          return expr_list(/* [] */0, (function (param, l) {
            return expr_list({
              hd: l,
              tl: acc
            }, k, t);
          }), t);
        }
      } else {
        return expr_list({
          hd: e,
          tl: acc
        }, k, t);
      }
    }), t);
  };
}

function _return_atom(last, k, t) {
  let s = Buffer.contents(t.atom);
  t.atom.position = 0;
  return k(last, {
    NAME: "Atom",
    VAL: s
  });
}

function atom(k, t) {
  while (true) {
    if (t.i === t.len) {
      return _refill(t, (function (extra) {
        return atom(k, extra);
      }), (function (extra) {
        return _return_atom(undefined, k, extra);
      }));
    }
    let c = _get(t);
    let exit = 0;
    if (c >= 35) {
      if (c >= 42) {
        if (c === 92) {
          return _error(t, "unexpected '\\' in non-quoted string");
        }
        exit = 1;
      } else {
        exit = c >= 40 ? 2 : 1;
      }
    } else if (c >= 11) {
      if (c >= 32) {
        switch (c) {
          case 32 :
            exit = 2;
            break;
          case 33 :
            exit = 1;
            break;
          case 34 :
            return _error(t, "unexpected '\"' in the middle of an atom");
        }
      } else {
        exit = 1;
      }
    } else {
      exit = c >= 9 ? 2 : 1;
    }
    switch (exit) {
      case 1 :
        Buffer.add_char(t.atom, c);
        continue;
      case 2 :
        return _return_atom(c, k, t);
    }
  };
}

function quoted(k, t) {
  while (true) {
    if (t.i === t.len) {
      return _refill(t, (function (extra) {
        return quoted(k, extra);
      }), _error_eof);
    }
    let c = _get(t);
    if (c === 34) {
      return _return_atom(undefined, k, t);
    }
    if (c === 92) {
      return escaped((function (c) {
        Buffer.add_char(t.atom, c);
        return quoted(k, t);
      }), t);
    }
    Buffer.add_char(t.atom, c);
    continue;
  };
}

function escaped(k, t) {
  if (t.i === t.len) {
    return _refill(t, (function (extra) {
      return escaped(k, extra);
    }), _error_eof);
  }
  let c = _get(t);
  if (c >= 92) {
    if (c < 117) {
      switch (c) {
        case 92 :
          return k(/* '\\' */92);
        case 98 :
          return k(/* '\b' */8);
        case 110 :
          return k(/* '\n' */10);
        case 114 :
          return k(/* '\r' */13);
        case 93 :
        case 94 :
        case 95 :
        case 96 :
        case 97 :
        case 99 :
        case 100 :
        case 101 :
        case 102 :
        case 103 :
        case 104 :
        case 105 :
        case 106 :
        case 107 :
        case 108 :
        case 109 :
        case 111 :
        case 112 :
        case 113 :
        case 115 :
          break;
        case 116 :
          return k(/* '\t' */9);
      }
    }
    
  } else if (c === 34) {
    return k(/* '"' */34);
  }
  if (_is_digit(c)) {
    return read2int(c - /* '0' */48 | 0, (function (n) {
      return k(Char.chr(n));
    }), t);
  } else {
    return _error(t, "unexpected escaped char '" + (c + "'"));
  }
}

function read2int(i, k, t) {
  if (t.i === t.len) {
    return _refill(t, (function (extra) {
      return read2int(i, k, extra);
    }), _error_eof);
  }
  let c = _get(t);
  if (_is_digit(c)) {
    return read1int(Math.imul(10, i) + (c - /* '0' */48 | 0) | 0, k, t);
  } else {
    return _error(t, "unexpected escaped char '" + (c + "' when reading byte"));
  }
}

function read1int(i, k, t) {
  if (t.i === t.len) {
    return _refill(t, (function (extra) {
      return read1int(i, k, extra);
    }), _error_eof);
  }
  let c = _get(t);
  if (_is_digit(c)) {
    return k(Math.imul(10, i) + (c - /* '0' */48 | 0) | 0);
  } else {
    return _error(t, "unexpected escaped char '" + (c + "' when reading byte"));
  }
}

function skip_comment(k, t) {
  while (true) {
    if (t.i === t.len) {
      return _refill(t, (function (extra) {
        return skip_comment(k, extra);
      }), _error_eof);
    }
    let match = _get(t);
    if (match === 10) {
      return k(undefined, undefined);
    }
    continue;
  };
}

function expr_or_end(k, t) {
  while (true) {
    if (t.i === t.len) {
      return _refill(t, (function (extra) {
        return expr_or_end(k, extra);
      }), (function (param) {
        return "End";
      }));
    }
    let c = _get(t);
    if (c >= 11) {
      if (c !== 32) {
        return expr_starting_with(c, k, t);
      }
      continue;
    }
    if (c < 9) {
      return expr_starting_with(c, k, t);
    }
    continue;
  };
}

function next(t) {
  return expr_or_end((function (param, x) {
    return {
      NAME: "Ok",
      VAL: x
    };
  }), t);
}

function parse_string(s) {
  let n = s.length;
  let stop = {
    contents: false
  };
  let refill = function (bytes, i, _len) {
    if (stop.contents) {
      return 0;
    } else {
      stop.contents = true;
      Bytes.blit_string(s, 0, bytes, i, n);
      return n;
    }
  };
  let d = make(n, refill);
  let res = next(d);
  if (typeof res === "object") {
    return res;
  } else {
    return {
      NAME: "Error",
      VAL: "unexpected end of file"
    };
  }
}

exports.to_buf = to_buf;
exports.to_string = to_string;
exports.parse_string = parse_string;
/* No side effect */
