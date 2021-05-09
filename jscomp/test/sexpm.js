'use strict';

var Sys = require("../../lib/js/sys.js");
var Caml = require("../../lib/js/caml.js");
var Char = require("../../lib/js/char.js");
var List = require("../../lib/js/list.js");
var Bytes = require("../../lib/js/bytes.js");
var Curry = require("../../lib/js/curry.js");
var $$Buffer = require("../../lib/js/buffer.js");
var $$String = require("../../lib/js/string.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function _must_escape(s) {
  try {
    for(var i = 0 ,i_finish = s.length; i < i_finish; ++i){
      var c = s.charCodeAt(i);
      var exit = 0;
      if (c >= 42) {
        if (c !== 59) {
          if (c !== 92) {
            exit = 1;
          } else {
            throw {
                  RE_EXN_ID: Pervasives.Exit,
                  Error: new Error()
                };
          }
        } else {
          throw {
                RE_EXN_ID: Pervasives.Exit,
                Error: new Error()
              };
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
                throw {
                      RE_EXN_ID: Pervasives.Exit,
                      Error: new Error()
                    };
            
          }
        } else {
          exit = 1;
        }
      } else {
        if (c >= 9) {
          throw {
                RE_EXN_ID: Pervasives.Exit,
                Error: new Error()
              };
        }
        exit = 1;
      }
      if (exit === 1 && c > 127) {
        throw {
              RE_EXN_ID: Pervasives.Exit,
              Error: new Error()
            };
      }
      
    }
    return false;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === Pervasives.Exit) {
      return true;
    }
    throw exn;
  }
}

function to_buf(b, t) {
  if (t.NAME === "List") {
    var l = t.VAL;
    if (l) {
      if (l.tl) {
        $$Buffer.add_char(b, /* '(' */40);
        List.iteri((function (i, t$p) {
                if (i > 0) {
                  $$Buffer.add_char(b, /* ' ' */32);
                }
                return to_buf(b, t$p);
              }), l);
        return $$Buffer.add_char(b, /* ')' */41);
      } else {
        $$Buffer.add_string(b, "(");
        to_buf(b, l.hd);
        return $$Buffer.add_string(b, ")");
      }
    } else {
      return $$Buffer.add_string(b, "()");
    }
  }
  var s = t.VAL;
  if (_must_escape(s)) {
    return $$Buffer.add_string(b, "\"" + ($$String.escaped(s) + "\""));
  } else {
    return $$Buffer.add_string(b, s);
  }
}

function to_string(t) {
  var b = $$Buffer.create(128);
  to_buf(b, t);
  return $$Buffer.contents(b);
}

function make(bufsizeOpt, refill) {
  var bufsize = bufsizeOpt !== undefined ? bufsizeOpt : 1024;
  var bufsize$1 = Caml.caml_int_min(bufsize > 16 ? bufsize : 16, Sys.max_string_length);
  return {
          buf: Caml_bytes.create(bufsize$1),
          refill: refill,
          atom: $$Buffer.create(32),
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
  var n = Curry._3(t.refill, t.buf, 0, t.buf.length);
  t.i = 0;
  t.len = n;
  if (n === 0) {
    return Curry._1(k_fail, t);
  } else {
    return Curry._1(k_succ, t);
  }
}

function _get(t) {
  if (t.i >= t.len) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "sexpm.ml",
            99,
            4
          ],
          Error: new Error()
        };
  }
  var c = Caml_bytes.get(t.buf, t.i);
  t.i = t.i + 1 | 0;
  if (c === /* '\n' */10) {
    t.col = 1;
    t.line = t.line + 1 | 0;
  } else {
    t.col = t.col + 1 | 0;
  }
  return c;
}

function _error(param) {
  var line = param.line;
  var col = param.col;
  return function (msg) {
    var b = $$Buffer.create(32);
    $$Buffer.add_string(b, "at " + line + ", " + col + ": ");
    $$Buffer.add_string(b, msg);
    var msg$p = $$Buffer.contents(b);
    return {
            NAME: "Error",
            VAL: msg$p
          };
  };
}

function _error_eof(t) {
  return _error(t)("unexpected end of input");
}

function expr(k, t) {
  while(true) {
    if (t.i === t.len) {
      return _refill(t, (function (param) {
                    return expr(k, param);
                  }), _error_eof);
    }
    var c = _get(t);
    if (c >= 11) {
      if (c !== 32) {
        return expr_starting_with(c, k, t);
      }
      continue ;
    }
    if (c < 9) {
      return expr_starting_with(c, k, t);
    }
    continue ;
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
      return _error(t)("unexpected '\\'");
    }
    
  } else if (c >= 11) {
    if (c >= 32) {
      switch (c) {
        case 32 :
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "sexpm.ml",
                    129,
                    27
                  ],
                  Error: new Error()
                };
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
            return _error(t)("unexpected ')'");
        
      }
    }
    
  } else if (c >= 9) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "sexpm.ml",
            129,
            27
          ],
          Error: new Error()
        };
  }
  $$Buffer.add_char(t.atom, c);
  return atom(k, t);
}

function expr_list(acc, k, t) {
  while(true) {
    if (t.i === t.len) {
      return _refill(t, (function (param) {
                    return expr_list(acc, k, param);
                  }), _error_eof);
    }
    var c = _get(t);
    if (c > 32 || c < 9) {
      if (c === 41) {
        return Curry._2(k, undefined, {
                    NAME: "List",
                    VAL: List.rev(acc)
                  });
      }
      
    } else if (c > 31 || c < 11) {
      continue ;
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
                        return Curry._2(k, undefined, {
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
  var s = $$Buffer.contents(t.atom);
  t.atom.position = 0;
  return Curry._2(k, last, {
              NAME: "Atom",
              VAL: s
            });
}

function atom(k, t) {
  while(true) {
    if (t.i === t.len) {
      return _refill(t, (function (param) {
                    return atom(k, param);
                  }), (function (param) {
                    return _return_atom(undefined, k, param);
                  }));
    }
    var c = _get(t);
    var exit = 0;
    if (c >= 35) {
      if (c >= 42) {
        if (c === 92) {
          return _error(t)("unexpected '\\' in non-quoted string");
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
              return _error(t)("unexpected '\"' in the middle of an atom");
          
        }
      } else {
        exit = 1;
      }
    } else {
      exit = c >= 9 ? 2 : 1;
    }
    switch (exit) {
      case 1 :
          $$Buffer.add_char(t.atom, c);
          continue ;
      case 2 :
          return _return_atom(c, k, t);
      
    }
  };
}

function quoted(k, t) {
  while(true) {
    if (t.i === t.len) {
      return _refill(t, (function (param) {
                    return quoted(k, param);
                  }), _error_eof);
    }
    var c = _get(t);
    if (c === 34) {
      return _return_atom(undefined, k, t);
    }
    if (c === 92) {
      return escaped((function (c) {
                    $$Buffer.add_char(t.atom, c);
                    return quoted(k, t);
                  }), t);
    }
    $$Buffer.add_char(t.atom, c);
    continue ;
  };
}

function escaped(k, t) {
  if (t.i === t.len) {
    return _refill(t, (function (param) {
                  return escaped(k, param);
                }), _error_eof);
  }
  var c = _get(t);
  if (c >= 92) {
    if (c < 117) {
      switch (c) {
        case 92 :
            return Curry._1(k, /* '\\' */92);
        case 98 :
            return Curry._1(k, /* '\b' */8);
        case 110 :
            return Curry._1(k, /* '\n' */10);
        case 114 :
            return Curry._1(k, /* '\r' */13);
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
            return Curry._1(k, /* '\t' */9);
        
      }
    }
    
  } else if (c === 34) {
    return Curry._1(k, /* '"' */34);
  }
  if (_is_digit(c)) {
    return read2int(c - /* '0' */48 | 0, (function (n) {
                  return Curry._1(k, Char.chr(n));
                }), t);
  } else {
    return _error(t)("unexpected escaped char '" + c + "'");
  }
}

function read2int(i, k, t) {
  if (t.i === t.len) {
    return _refill(t, (function (param) {
                  return read2int(i, k, param);
                }), _error_eof);
  }
  var c = _get(t);
  if (_is_digit(c)) {
    return read1int(Math.imul(10, i) + (c - /* '0' */48 | 0) | 0, k, t);
  } else {
    return _error(t)("unexpected char '" + c + "' when reading byte");
  }
}

function read1int(i, k, t) {
  if (t.i === t.len) {
    return _refill(t, (function (param) {
                  return read1int(i, k, param);
                }), _error_eof);
  }
  var c = _get(t);
  if (_is_digit(c)) {
    return Curry._1(k, Math.imul(10, i) + (c - /* '0' */48 | 0) | 0);
  } else {
    return _error(t)("unexpected char '" + c + "' when reading byte");
  }
}

function skip_comment(k, t) {
  while(true) {
    if (t.i === t.len) {
      return _refill(t, (function (param) {
                    return skip_comment(k, param);
                  }), _error_eof);
    }
    var match = _get(t);
    if (match === 10) {
      return Curry._2(k, undefined, undefined);
    }
    continue ;
  };
}

function expr_or_end(k, t) {
  while(true) {
    if (t.i === t.len) {
      return _refill(t, (function (param) {
                    return expr_or_end(k, param);
                  }), (function (param) {
                    return "End";
                  }));
    }
    var c = _get(t);
    if (c >= 11) {
      if (c !== 32) {
        return expr_starting_with(c, k, t);
      }
      continue ;
    }
    if (c < 9) {
      return expr_starting_with(c, k, t);
    }
    continue ;
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
  var n = s.length;
  var stop = {
    contents: false
  };
  var refill = function (bytes, i, _len) {
    if (stop.contents) {
      return 0;
    } else {
      stop.contents = true;
      Bytes.blit_string(s, 0, bytes, i, n);
      return n;
    }
  };
  var d = make(n, refill);
  var res = next(d);
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
