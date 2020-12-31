'use strict';

var Sys = require("../../lib/js/sys.js");
var Char = require("../../lib/js/char.js");
var List = require("../../lib/js/list.js");
var Bytes = require("../../lib/js/bytes.js");
var Curry = require("../../lib/js/curry.js");
var $$Buffer = require("../../lib/js/buffer.js");
var Format = require("../../lib/js/format.js");
var Printf = require("../../lib/js/printf.js");
var $$String = require("../../lib/js/string.js");
var Caml_io = require("../../lib/js/caml_io.js");
var Printexc = require("../../lib/js/printexc.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_external_polyfill = require("../../lib/js/caml_external_polyfill.js");

function _with_in(filename, f) {
  var ic = Pervasives.open_in_bin(filename);
  try {
    var x = Curry._1(f, ic);
    Caml_external_polyfill.resolve("caml_ml_close_channel")(ic);
    return x;
  }
  catch (raw_e){
    var e = Caml_js_exceptions.internalToOCamlException(raw_e);
    Caml_external_polyfill.resolve("caml_ml_close_channel")(ic);
    return {
            NAME: "Error",
            VAL: Printexc.to_string(e)
          };
  }
}

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
        List.iteri((function (i, t$prime) {
                if (i > 0) {
                  $$Buffer.add_char(b, /* ' ' */32);
                }
                return to_buf(b, t$prime);
              }), l);
        return $$Buffer.add_char(b, /* ')' */41);
      } else {
        return Curry._2(Printf.bprintf(b, /* Format */{
                        _0: {
                          TAG: /* Char_literal */12,
                          _0: /* '(' */40,
                          _1: {
                            TAG: /* Alpha */15,
                            _0: {
                              TAG: /* Char_literal */12,
                              _0: /* ')' */41,
                              _1: /* End_of_format */0
                            }
                          }
                        },
                        _1: "(%a)"
                      }), to_buf, l.hd);
      }
    } else {
      return $$Buffer.add_string(b, "()");
    }
  }
  var s = t.VAL;
  if (_must_escape(s)) {
    return Curry._1(Printf.bprintf(b, /* Format */{
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* '"' */34,
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* Char_literal */12,
                          _0: /* '"' */34,
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: "\"%s\""
                  }), $$String.escaped(s));
  } else {
    return $$Buffer.add_string(b, s);
  }
}

function to_string(t) {
  var b = $$Buffer.create(128);
  to_buf(b, t);
  return $$Buffer.contents(b);
}

function print(fmt, t) {
  if (t.NAME === "List") {
    var l = t.VAL;
    if (l) {
      if (l.tl) {
        Format.fprintf(fmt, /* Format */{
              _0: {
                TAG: /* Formatting_gen */18,
                _0: {
                  TAG: /* Open_box */1,
                  _0: /* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "<hov1>",
                      _1: /* End_of_format */0
                    },
                    _1: "<hov1>"
                  }
                },
                _1: {
                  TAG: /* Char_literal */12,
                  _0: /* '(' */40,
                  _1: /* End_of_format */0
                }
              },
              _1: "@[<hov1>("
            });
        List.iteri((function (i, t$prime) {
                if (i > 0) {
                  Format.fprintf(fmt, /* Format */{
                        _0: {
                          TAG: /* Formatting_lit */17,
                          _0: {
                            TAG: /* Break */0,
                            _0: "@ ",
                            _1: 1,
                            _2: 0
                          },
                          _1: /* End_of_format */0
                        },
                        _1: "@ "
                      });
                }
                return print(fmt, t$prime);
              }), l);
        return Format.fprintf(fmt, /* Format */{
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* ')' */41,
                      _1: {
                        TAG: /* Formatting_lit */17,
                        _0: /* Close_box */0,
                        _1: /* End_of_format */0
                      }
                    },
                    _1: ")@]"
                  });
      } else {
        return Curry._2(Format.fprintf(fmt, /* Format */{
                        _0: {
                          TAG: /* Formatting_gen */18,
                          _0: {
                            TAG: /* Open_box */1,
                            _0: /* Format */{
                              _0: {
                                TAG: /* String_literal */11,
                                _0: "<hov2>",
                                _1: /* End_of_format */0
                              },
                              _1: "<hov2>"
                            }
                          },
                          _1: {
                            TAG: /* Char_literal */12,
                            _0: /* '(' */40,
                            _1: {
                              TAG: /* Alpha */15,
                              _0: {
                                TAG: /* Char_literal */12,
                                _0: /* ')' */41,
                                _1: {
                                  TAG: /* Formatting_lit */17,
                                  _0: /* Close_box */0,
                                  _1: /* End_of_format */0
                                }
                              }
                            }
                          }
                        },
                        _1: "@[<hov2>(%a)@]"
                      }), print, l.hd);
      }
    } else {
      return Format.pp_print_string(fmt, "()");
    }
  }
  var s = t.VAL;
  if (_must_escape(s)) {
    return Curry._1(Format.fprintf(fmt, /* Format */{
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* '"' */34,
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* Char_literal */12,
                          _0: /* '"' */34,
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: "\"%s\""
                  }), $$String.escaped(s));
  } else {
    return Format.pp_print_string(fmt, s);
  }
}

function print_noindent(fmt, t) {
  if (t.NAME === "List") {
    var l = t.VAL;
    if (l) {
      if (l.tl) {
        Format.pp_print_char(fmt, /* '(' */40);
        List.iteri((function (i, t$prime) {
                if (i > 0) {
                  Format.pp_print_char(fmt, /* ' ' */32);
                }
                return print_noindent(fmt, t$prime);
              }), l);
        return Format.pp_print_char(fmt, /* ')' */41);
      } else {
        return Curry._2(Format.fprintf(fmt, /* Format */{
                        _0: {
                          TAG: /* Char_literal */12,
                          _0: /* '(' */40,
                          _1: {
                            TAG: /* Alpha */15,
                            _0: {
                              TAG: /* Char_literal */12,
                              _0: /* ')' */41,
                              _1: /* End_of_format */0
                            }
                          }
                        },
                        _1: "(%a)"
                      }), print_noindent, l.hd);
      }
    } else {
      return Format.pp_print_string(fmt, "()");
    }
  }
  var s = t.VAL;
  if (_must_escape(s)) {
    return Curry._1(Format.fprintf(fmt, /* Format */{
                    _0: {
                      TAG: /* Char_literal */12,
                      _0: /* '"' */34,
                      _1: {
                        TAG: /* String */2,
                        _0: /* No_padding */0,
                        _1: {
                          TAG: /* Char_literal */12,
                          _0: /* '"' */34,
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: "\"%s\""
                  }), $$String.escaped(s));
  } else {
    return Format.pp_print_string(fmt, s);
  }
}

function to_chan(oc, t) {
  var fmt = Format.formatter_of_out_channel(oc);
  print(fmt, t);
  return Format.pp_print_flush(fmt, undefined);
}

function to_file_seq(filename, seq) {
  var f = function (oc) {
    return Curry._1(seq, (function (t) {
                  to_chan(oc, t);
                  return Caml_io.caml_ml_output_char(oc, /* '
' */10);
                }));
  };
  var oc = Pervasives.open_out(filename);
  try {
    var x = Curry._1(f, oc);
    Caml_io.caml_ml_flush(oc);
    Caml_external_polyfill.resolve("caml_ml_close_channel")(oc);
    return x;
  }
  catch (e){
    Caml_io.caml_ml_flush(oc);
    Caml_external_polyfill.resolve("caml_ml_close_channel")(oc);
    throw e;
  }
}

function to_file(filename, t) {
  return to_file_seq(filename, (function (k) {
                return Curry._1(k, t);
              }));
}

function $$return(x) {
  return x;
}

function $great$great$eq(x, f) {
  return Curry._1(f, x);
}

var ID_MONAD = {
  $$return: $$return,
  $great$great$eq: $great$great$eq
};

function make(bufsizeOpt, refill) {
  var bufsize = bufsizeOpt !== undefined ? bufsizeOpt : 1024;
  var bufsize$1 = Caml_primitive.caml_int_min(bufsize > 16 ? bufsize : 16, Sys.max_string_length);
  return {
          buf: Caml_bytes.caml_create_bytes(bufsize$1),
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
            152,
            4
          ],
          Error: new Error()
        };
  }
  var c = Caml_bytes.get(t.buf, t.i);
  t.i = t.i + 1 | 0;
  if (c === /* '
' */10) {
    t.col = 1;
    t.line = t.line + 1 | 0;
  } else {
    t.col = t.col + 1 | 0;
  }
  return c;
}

function _error(t, msg) {
  var b = $$Buffer.create(32);
  Curry._2(Printf.bprintf(b, /* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "at ",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* String_literal */11,
                  _0: ", ",
                  _1: {
                    TAG: /* Int */4,
                    _0: /* Int_d */0,
                    _1: /* No_padding */0,
                    _2: /* No_precision */0,
                    _3: {
                      TAG: /* String_literal */11,
                      _0: ": ",
                      _1: /* End_of_format */0
                    }
                  }
                }
              }
            },
            _1: "at %d, %d: "
          }), t.line, t.col);
  return Printf.kbprintf((function (b) {
                var msg$prime = $$Buffer.contents(b);
                return {
                        NAME: "Error",
                        VAL: msg$prime
                      };
              }), b, msg);
}

function _error_eof(t) {
  return _error(t, /* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "unexpected end of input",
                _1: /* End_of_format */0
              },
              _1: "unexpected end of input"
            });
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
      return _error(t, /* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "unexpected '\\'",
                    _1: /* End_of_format */0
                  },
                  _1: "unexpected '\\'"
                });
    }
    
  } else if (c >= 11) {
    if (c >= 32) {
      switch (c) {
        case 32 :
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "sexpm.ml",
                    183,
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
            return _error(t, /* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "unexpected ')'",
                          _1: /* End_of_format */0
                        },
                        _1: "unexpected ')'"
                      });
        
      }
    }
    
  } else if (c >= 9) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "sexpm.ml",
            183,
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
          return _error(t, /* Format */{
                      _0: {
                        TAG: /* String_literal */11,
                        _0: "unexpected '\\' in non-quoted string",
                        _1: /* End_of_format */0
                      },
                      _1: "unexpected '\\' in non-quoted string"
                    });
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
              return _error(t, /* Format */{
                          _0: {
                            TAG: /* String_literal */11,
                            _0: "unexpected '\"' in the middle of an atom",
                            _1: /* End_of_format */0
                          },
                          _1: "unexpected '\"' in the middle of an atom"
                        });
          
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
            return Curry._1(k, /* '\' */92);
        case 98 :
            return Curry._1(k, /* '' */8);
        case 110 :
            return Curry._1(k, /* '
' */10);
        case 114 :
            return Curry._1(k, /* '' */13);
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
            return Curry._1(k, /* '	' */9);
        
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
    return Curry._1(_error(t, /* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "unexpected escaped char '",
                      _1: {
                        TAG: /* Char */0,
                        _0: {
                          TAG: /* Char_literal */12,
                          _0: /* ''' */39,
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: "unexpected escaped char '%c'"
                  }), c);
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
    return Curry._1(_error(t, /* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "unexpected char '",
                      _1: {
                        TAG: /* Char */0,
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "' when reading byte",
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: "unexpected char '%c' when reading byte"
                  }), c);
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
    return Curry._1(_error(t, /* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "unexpected char '",
                      _1: {
                        TAG: /* Char */0,
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "' when reading byte",
                          _1: /* End_of_format */0
                        }
                      }
                    },
                    _1: "unexpected char '%c' when reading byte"
                  }), c);
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
  if (typeof res === "string") {
    return {
            NAME: "Error",
            VAL: "unexpected end of file"
          };
  } else {
    return res;
  }
}

function parse_chan(bufsize, ic) {
  var d = make(bufsize, (function (param, param$1, param$2) {
          return Pervasives.input(ic, param, param$1, param$2);
        }));
  var res = next(d);
  if (typeof res === "string") {
    return {
            NAME: "Error",
            VAL: "unexpected end of file"
          };
  } else {
    return res;
  }
}

function parse_chan_gen(bufsize, ic) {
  var d = make(bufsize, (function (param, param$1, param$2) {
          return Pervasives.input(ic, param, param$1, param$2);
        }));
  return function (param) {
    var e = next(d);
    if (typeof e === "string") {
      return ;
    } else {
      return e;
    }
  };
}

function parse_chan_list(bufsize, ic) {
  var d = make(bufsize, (function (param, param$1, param$2) {
          return Pervasives.input(ic, param, param$1, param$2);
        }));
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var e = next(d);
    if (typeof e === "string") {
      return {
              NAME: "Ok",
              VAL: List.rev(acc)
            };
    }
    if (e.NAME === "Error") {
      return e;
    }
    _acc = {
      hd: e.VAL,
      tl: acc
    };
    continue ;
  };
}

function parse_file(filename) {
  return _with_in(filename, (function (ic) {
                return parse_chan(undefined, ic);
              }));
}

function parse_file_list(filename) {
  return _with_in(filename, (function (ic) {
                return parse_chan_list(undefined, ic);
              }));
}

function MakeDecode(funarg) {
  var $great$great$eq = funarg.$great$great$eq;
  var make = function (bufsizeOpt, refill) {
    var bufsize = bufsizeOpt !== undefined ? bufsizeOpt : 1024;
    var bufsize$1 = Caml_primitive.caml_int_min(bufsize > 16 ? bufsize : 16, Sys.max_string_length);
    return {
            buf: Caml_bytes.caml_create_bytes(bufsize$1),
            refill: refill,
            atom: $$Buffer.create(32),
            i: 0,
            len: 0,
            line: 1,
            col: 1
          };
  };
  var _is_digit = function (c) {
    if (/* '0' */48 <= c) {
      return c <= /* '9' */57;
    } else {
      return false;
    }
  };
  var _refill = function (t, k_succ, k_fail) {
    return Curry._2($great$great$eq, Curry._3(t.refill, t.buf, 0, t.buf.length), (function (n) {
                  t.i = 0;
                  t.len = n;
                  if (n === 0) {
                    return Curry._1(k_fail, t);
                  } else {
                    return Curry._1(k_succ, t);
                  }
                }));
  };
  var _get = function (t) {
    if (t.i >= t.len) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "sexpm.ml",
              152,
              4
            ],
            Error: new Error()
          };
    }
    var c = Caml_bytes.get(t.buf, t.i);
    t.i = t.i + 1 | 0;
    if (c === /* '
' */10) {
      t.col = 1;
      t.line = t.line + 1 | 0;
    } else {
      t.col = t.col + 1 | 0;
    }
    return c;
  };
  var _error = function (t, msg) {
    var b = $$Buffer.create(32);
    Curry._2(Printf.bprintf(b, /* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "at ",
                _1: {
                  TAG: /* Int */4,
                  _0: /* Int_d */0,
                  _1: /* No_padding */0,
                  _2: /* No_precision */0,
                  _3: {
                    TAG: /* String_literal */11,
                    _0: ", ",
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_d */0,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* String_literal */11,
                        _0: ": ",
                        _1: /* End_of_format */0
                      }
                    }
                  }
                }
              },
              _1: "at %d, %d: "
            }), t.line, t.col);
    return Printf.kbprintf((function (b) {
                  var msg$prime = $$Buffer.contents(b);
                  return Curry._1(funarg.$$return, {
                              NAME: "Error",
                              VAL: msg$prime
                            });
                }), b, msg);
  };
  var _error_eof = function (t) {
    return _error(t, /* Format */{
                _0: {
                  TAG: /* String_literal */11,
                  _0: "unexpected end of input",
                  _1: /* End_of_format */0
                },
                _1: "unexpected end of input"
              });
  };
  var expr = function (k, t) {
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
  };
  var expr_starting_with = function (c, k, t) {
    if (c >= 42) {
      if (c === 59) {
        return skip_comment((function (param, param$1) {
                      return expr(k, t);
                    }), t);
      }
      if (c === 92) {
        return _error(t, /* Format */{
                    _0: {
                      TAG: /* String_literal */11,
                      _0: "unexpected '\\'",
                      _1: /* End_of_format */0
                    },
                    _1: "unexpected '\\'"
                  });
      }
      
    } else if (c >= 11) {
      if (c >= 32) {
        switch (c) {
          case 32 :
              throw {
                    RE_EXN_ID: "Assert_failure",
                    _1: [
                      "sexpm.ml",
                      183,
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
              return _error(t, /* Format */{
                          _0: {
                            TAG: /* String_literal */11,
                            _0: "unexpected ')'",
                            _1: /* End_of_format */0
                          },
                          _1: "unexpected ')'"
                        });
          
        }
      }
      
    } else if (c >= 9) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "sexpm.ml",
              183,
              27
            ],
            Error: new Error()
          };
    }
    $$Buffer.add_char(t.atom, c);
    return atom(k, t);
  };
  var expr_list = function (acc, k, t) {
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
  };
  var _return_atom = function (last, k, t) {
    var s = $$Buffer.contents(t.atom);
    t.atom.position = 0;
    return Curry._2(k, last, {
                NAME: "Atom",
                VAL: s
              });
  };
  var atom = function (k, t) {
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
            return _error(t, /* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "unexpected '\\' in non-quoted string",
                          _1: /* End_of_format */0
                        },
                        _1: "unexpected '\\' in non-quoted string"
                      });
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
                return _error(t, /* Format */{
                            _0: {
                              TAG: /* String_literal */11,
                              _0: "unexpected '\"' in the middle of an atom",
                              _1: /* End_of_format */0
                            },
                            _1: "unexpected '\"' in the middle of an atom"
                          });
            
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
  };
  var quoted = function (k, t) {
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
  };
  var escaped = function (k, t) {
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
              return Curry._1(k, /* '\' */92);
          case 98 :
              return Curry._1(k, /* '' */8);
          case 110 :
              return Curry._1(k, /* '
' */10);
          case 114 :
              return Curry._1(k, /* '' */13);
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
              return Curry._1(k, /* '	' */9);
          
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
      return Curry._1(_error(t, /* Format */{
                      _0: {
                        TAG: /* String_literal */11,
                        _0: "unexpected escaped char '",
                        _1: {
                          TAG: /* Char */0,
                          _0: {
                            TAG: /* Char_literal */12,
                            _0: /* ''' */39,
                            _1: /* End_of_format */0
                          }
                        }
                      },
                      _1: "unexpected escaped char '%c'"
                    }), c);
    }
  };
  var read2int = function (i, k, t) {
    if (t.i === t.len) {
      return _refill(t, (function (param) {
                    return read2int(i, k, param);
                  }), _error_eof);
    }
    var c = _get(t);
    if (_is_digit(c)) {
      return read1int(Math.imul(10, i) + (c - /* '0' */48 | 0) | 0, k, t);
    } else {
      return Curry._1(_error(t, /* Format */{
                      _0: {
                        TAG: /* String_literal */11,
                        _0: "unexpected char '",
                        _1: {
                          TAG: /* Char */0,
                          _0: {
                            TAG: /* String_literal */11,
                            _0: "' when reading byte",
                            _1: /* End_of_format */0
                          }
                        }
                      },
                      _1: "unexpected char '%c' when reading byte"
                    }), c);
    }
  };
  var read1int = function (i, k, t) {
    if (t.i === t.len) {
      return _refill(t, (function (param) {
                    return read1int(i, k, param);
                  }), _error_eof);
    }
    var c = _get(t);
    if (_is_digit(c)) {
      return Curry._1(k, Math.imul(10, i) + (c - /* '0' */48 | 0) | 0);
    } else {
      return Curry._1(_error(t, /* Format */{
                      _0: {
                        TAG: /* String_literal */11,
                        _0: "unexpected char '",
                        _1: {
                          TAG: /* Char */0,
                          _0: {
                            TAG: /* String_literal */11,
                            _0: "' when reading byte",
                            _1: /* End_of_format */0
                          }
                        }
                      },
                      _1: "unexpected char '%c' when reading byte"
                    }), c);
    }
  };
  var skip_comment = function (k, t) {
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
  };
  var expr_or_end = function (k, t) {
    while(true) {
      if (t.i === t.len) {
        return _refill(t, (function (param) {
                      return expr_or_end(k, param);
                    }), (function (param) {
                      return Curry._1(funarg.$$return, "End");
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
  };
  var next = function (t) {
    return expr_or_end((function (param, x) {
                  return Curry._1(funarg.$$return, {
                              NAME: "Ok",
                              VAL: x
                            });
                }), t);
  };
  return {
          make: make,
          next: next
        };
}

var D = {
  make: make,
  next: next
};

exports.to_buf = to_buf;
exports.to_string = to_string;
exports.to_file = to_file;
exports.to_file_seq = to_file_seq;
exports.to_chan = to_chan;
exports.print = print;
exports.print_noindent = print_noindent;
exports.MakeDecode = MakeDecode;
exports.ID_MONAD = ID_MONAD;
exports.D = D;
exports.parse_string = parse_string;
exports.parse_chan = parse_chan;
exports.parse_chan_gen = parse_chan_gen;
exports.parse_chan_list = parse_chan_list;
exports.parse_file = parse_file;
exports.parse_file_list = parse_file_list;
/* Format Not a pure module */
