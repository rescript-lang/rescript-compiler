'use strict';

var Sys                     = require("../../lib/js/sys.js");
var Char                    = require("../../lib/js/char.js");
var List                    = require("../../lib/js/list.js");
var Block                   = require("../../lib/js/block.js");
var Bytes                   = require("../../lib/js/bytes.js");
var Curry                   = require("../../lib/js/curry.js");
var Buffer                  = require("../../lib/js/buffer.js");
var Format                  = require("../../lib/js/format.js");
var Js_exn                  = require("../../lib/js/js_exn.js");
var Printf                  = require("../../lib/js/printf.js");
var $$String                = require("../../lib/js/string.js");
var Caml_io                 = require("../../lib/js/caml_io.js");
var Printexc                = require("../../lib/js/printexc.js");
var Caml_bytes              = require("../../lib/js/caml_bytes.js");
var Caml_int32              = require("../../lib/js/caml_int32.js");
var Pervasives              = require("../../lib/js/pervasives.js");
var Caml_string             = require("../../lib/js/caml_string.js");
var Caml_primitive          = require("../../lib/js/caml_primitive.js");
var Caml_missing_polyfill   = require("../../lib/js/caml_missing_polyfill.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function _with_in(filename, f) {
  var ic = Pervasives.open_in_bin(filename);
  try {
    var x = Curry._1(f, ic);
    Caml_missing_polyfill.not_implemented("caml_ml_close_channel not implemented by bucklescript yet\n");
    return x;
  }
  catch (raw_e){
    var e = Js_exn.internalToOCamlException(raw_e);
    Caml_missing_polyfill.not_implemented("caml_ml_close_channel not implemented by bucklescript yet\n");
    return /* `Error */[
            106380200,
            Printexc.to_string(e)
          ];
  }
}

function _must_escape(s) {
  try {
    for(var i = 0 ,i_finish = s.length - 1 | 0; i <= i_finish; ++i){
      var c = s.charCodeAt(i);
      var exit = 0;
      if (c >= 42) {
        if (c !== 59) {
          if (c !== 92) {
            exit = 1;
          } else {
            throw Pervasives.Exit;
          }
        } else {
          throw Pervasives.Exit;
        }
      } else if (c >= 11) {
        if (c >= 32) {
          switch (c - 32 | 0) {
            case 1 : 
            case 3 : 
            case 4 : 
            case 5 : 
            case 6 : 
            case 7 : 
                exit = 1;
                break;
            case 0 : 
            case 2 : 
            case 8 : 
            case 9 : 
                throw Pervasives.Exit;
            
          }
        } else {
          exit = 1;
        }
      } else if (c >= 9) {
        throw Pervasives.Exit;
      } else {
        exit = 1;
      }
      if (exit === 1) {
        if (c > 127) {
          throw Pervasives.Exit;
        }
        
      }
      
    }
    return /* false */0;
  }
  catch (exn){
    if (exn === Pervasives.Exit) {
      return /* true */1;
    } else {
      throw exn;
    }
  }
}

function to_buf(b, t) {
  if (t[0] >= 848054398) {
    var l = t[1];
    if (l) {
      if (l[1]) {
        Buffer.add_char(b, /* "(" */40);
        List.iteri((function (i, t$prime) {
                if (i > 0) {
                  Buffer.add_char(b, /* " " */32);
                }
                return to_buf(b, t$prime);
              }), l);
        return Buffer.add_char(b, /* ")" */41);
      } else {
        return Curry._2(Printf.bprintf(b, /* Format */[
                        /* Char_literal */Block.__(12, [
                            /* "(" */40,
                            /* Alpha */Block.__(15, [/* Char_literal */Block.__(12, [
                                    /* ")" */41,
                                    /* End_of_format */0
                                  ])])
                          ]),
                        "(%a)"
                      ]), to_buf, l[0]);
      }
    } else {
      return Buffer.add_string(b, "()");
    }
  } else {
    var s = t[1];
    if (_must_escape(s)) {
      return Curry._1(Printf.bprintf(b, /* Format */[
                      /* Char_literal */Block.__(12, [
                          /* "\"" */34,
                          /* String */Block.__(2, [
                              /* No_padding */0,
                              /* Char_literal */Block.__(12, [
                                  /* "\"" */34,
                                  /* End_of_format */0
                                ])
                            ])
                        ]),
                      "\"%s\""
                    ]), $$String.escaped(s));
    } else {
      return Buffer.add_string(b, s);
    }
  }
}

function to_string(t) {
  var b = Buffer.create(128);
  to_buf(b, t);
  return Buffer.contents(b);
}

function print(fmt, t) {
  if (t[0] >= 848054398) {
    var l = t[1];
    if (l) {
      if (l[1]) {
        Format.fprintf(fmt, /* Format */[
              /* Formatting_gen */Block.__(18, [
                  /* Open_box */Block.__(1, [/* Format */[
                        /* String_literal */Block.__(11, [
                            "<hov1>",
                            /* End_of_format */0
                          ]),
                        "<hov1>"
                      ]]),
                  /* Char_literal */Block.__(12, [
                      /* "(" */40,
                      /* End_of_format */0
                    ])
                ]),
              "@[<hov1>("
            ]);
        List.iteri((function (i, t$prime) {
                if (i > 0) {
                  Format.fprintf(fmt, /* Format */[
                        /* Formatting_lit */Block.__(17, [
                            /* Break */Block.__(0, [
                                "@ ",
                                1,
                                0
                              ]),
                            /* End_of_format */0
                          ]),
                        "@ "
                      ]);
                }
                return print(fmt, t$prime);
              }), l);
        return Format.fprintf(fmt, /* Format */[
                    /* Char_literal */Block.__(12, [
                        /* ")" */41,
                        /* Formatting_lit */Block.__(17, [
                            /* Close_box */0,
                            /* End_of_format */0
                          ])
                      ]),
                    ")@]"
                  ]);
      } else {
        return Curry._2(Format.fprintf(fmt, /* Format */[
                        /* Formatting_gen */Block.__(18, [
                            /* Open_box */Block.__(1, [/* Format */[
                                  /* String_literal */Block.__(11, [
                                      "<hov2>",
                                      /* End_of_format */0
                                    ]),
                                  "<hov2>"
                                ]]),
                            /* Char_literal */Block.__(12, [
                                /* "(" */40,
                                /* Alpha */Block.__(15, [/* Char_literal */Block.__(12, [
                                        /* ")" */41,
                                        /* Formatting_lit */Block.__(17, [
                                            /* Close_box */0,
                                            /* End_of_format */0
                                          ])
                                      ])])
                              ])
                          ]),
                        "@[<hov2>(%a)@]"
                      ]), print, l[0]);
      }
    } else {
      return Format.pp_print_string(fmt, "()");
    }
  } else {
    var s = t[1];
    if (_must_escape(s)) {
      return Curry._1(Format.fprintf(fmt, /* Format */[
                      /* Char_literal */Block.__(12, [
                          /* "\"" */34,
                          /* String */Block.__(2, [
                              /* No_padding */0,
                              /* Char_literal */Block.__(12, [
                                  /* "\"" */34,
                                  /* End_of_format */0
                                ])
                            ])
                        ]),
                      "\"%s\""
                    ]), $$String.escaped(s));
    } else {
      return Format.pp_print_string(fmt, s);
    }
  }
}

function print_noindent(fmt, t) {
  if (t[0] >= 848054398) {
    var l = t[1];
    if (l) {
      if (l[1]) {
        Format.pp_print_char(fmt, /* "(" */40);
        List.iteri((function (i, t$prime) {
                if (i > 0) {
                  Format.pp_print_char(fmt, /* " " */32);
                }
                return print_noindent(fmt, t$prime);
              }), l);
        return Format.pp_print_char(fmt, /* ")" */41);
      } else {
        return Curry._2(Format.fprintf(fmt, /* Format */[
                        /* Char_literal */Block.__(12, [
                            /* "(" */40,
                            /* Alpha */Block.__(15, [/* Char_literal */Block.__(12, [
                                    /* ")" */41,
                                    /* End_of_format */0
                                  ])])
                          ]),
                        "(%a)"
                      ]), print_noindent, l[0]);
      }
    } else {
      return Format.pp_print_string(fmt, "()");
    }
  } else {
    var s = t[1];
    if (_must_escape(s)) {
      return Curry._1(Format.fprintf(fmt, /* Format */[
                      /* Char_literal */Block.__(12, [
                          /* "\"" */34,
                          /* String */Block.__(2, [
                              /* No_padding */0,
                              /* Char_literal */Block.__(12, [
                                  /* "\"" */34,
                                  /* End_of_format */0
                                ])
                            ])
                        ]),
                      "\"%s\""
                    ]), $$String.escaped(s));
    } else {
      return Format.pp_print_string(fmt, s);
    }
  }
}

function to_chan(oc, t) {
  var fmt = Format.formatter_of_out_channel(oc);
  print(fmt, t);
  return Format.pp_print_flush(fmt, /* () */0);
}

function to_file_seq(filename, seq) {
  var filename$1 = filename;
  var f = function (oc) {
    return Curry._1(seq, (function (t) {
                  to_chan(oc, t);
                  return Caml_io.caml_ml_output_char(oc, /* "\n" */10);
                }));
  };
  var oc = Pervasives.open_out(filename$1);
  try {
    var x = Curry._1(f, oc);
    Caml_io.caml_ml_flush(oc);
    Caml_missing_polyfill.not_implemented("caml_ml_close_channel not implemented by bucklescript yet\n");
    return x;
  }
  catch (e){
    Caml_io.caml_ml_flush(oc);
    Caml_missing_polyfill.not_implemented("caml_ml_close_channel not implemented by bucklescript yet\n");
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

var ID_MONAD = /* module */[
  /* return */$$return,
  /* >>= */$great$great$eq
];

function make($staropt$star, refill) {
  var bufsize = $staropt$star ? $staropt$star[0] : 1024;
  var bufsize$1 = Caml_primitive.caml_int_min(bufsize > 16 ? bufsize : 16, Sys.max_string_length);
  return /* record */[
          /* buf */Caml_string.caml_create_string(bufsize$1),
          /* refill */refill,
          /* atom */Buffer.create(32),
          /* i */0,
          /* len */0,
          /* line */1,
          /* col */1
        ];
}

function _is_digit(c) {
  if (/* "0" */48 <= c) {
    return +(c <= /* "9" */57);
  } else {
    return /* false */0;
  }
}

function _refill(t, k_succ, k_fail) {
  var n = Curry._3(t[/* refill */1], t[/* buf */0], 0, t[/* buf */0].length);
  t[/* i */3] = 0;
  t[/* len */4] = n;
  if (n) {
    return Curry._1(k_succ, t);
  } else {
    return Curry._1(k_fail, t);
  }
}

function _get(t) {
  if (t[/* i */3] >= t[/* len */4]) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "sexpm.ml",
            152,
            4
          ]
        ];
  }
  var c = Caml_bytes.get(t[/* buf */0], t[/* i */3]);
  t[/* i */3] = t[/* i */3] + 1 | 0;
  if (c === /* "\n" */10) {
    t[/* col */6] = 1;
    t[/* line */5] = t[/* line */5] + 1 | 0;
  } else {
    t[/* col */6] = t[/* col */6] + 1 | 0;
  }
  return c;
}

function _error(t, msg) {
  var b = Buffer.create(32);
  Curry._2(Printf.bprintf(b, /* Format */[
            /* String_literal */Block.__(11, [
                "at ",
                /* Int */Block.__(4, [
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    /* String_literal */Block.__(11, [
                        ", ",
                        /* Int */Block.__(4, [
                            /* Int_d */0,
                            /* No_padding */0,
                            /* No_precision */0,
                            /* String_literal */Block.__(11, [
                                ": ",
                                /* End_of_format */0
                              ])
                          ])
                      ])
                  ])
              ]),
            "at %d, %d: "
          ]), t[/* line */5], t[/* col */6]);
  return Printf.kbprintf((function (b) {
                var msg$prime = Buffer.contents(b);
                return /* `Error */[
                        106380200,
                        msg$prime
                      ];
              }), b, msg);
}

function _error_eof(t) {
  return _error(t, /* Format */[
              /* String_literal */Block.__(11, [
                  "unexpected end of input",
                  /* End_of_format */0
                ]),
              "unexpected end of input"
            ]);
}

function expr(k, t) {
  while(true) {
    if (t[/* i */3] === t[/* len */4]) {
      return _refill(t, (function (param) {
                    return expr(k, param);
                  }), _error_eof);
    } else {
      var c = _get(t);
      if (c >= 11) {
        if (c !== 32) {
          return expr_starting_with(c, k, t);
        } else {
          continue ;
          
        }
      } else if (c >= 9) {
        continue ;
        
      } else {
        return expr_starting_with(c, k, t);
      }
    }
  };
}

function expr_starting_with(c, k, t) {
  var exit = 0;
  if (c >= 42) {
    if (c !== 59) {
      if (c !== 92) {
        exit = 1;
      } else {
        return _error(t, /* Format */[
                    /* String_literal */Block.__(11, [
                        "unexpected '\\'",
                        /* End_of_format */0
                      ]),
                    "unexpected '\\'"
                  ]);
      }
    } else {
      return skip_comment((function (_, _$1) {
                    return expr(k, t);
                  }), t);
    }
  } else if (c >= 11) {
    if (c >= 32) {
      switch (c - 32 | 0) {
        case 0 : 
            exit = 2;
            break;
        case 2 : 
            return quoted(k, t);
        case 1 : 
        case 3 : 
        case 4 : 
        case 5 : 
        case 6 : 
        case 7 : 
            exit = 1;
            break;
        case 8 : 
            return expr_list(/* [] */0, k, t);
        case 9 : 
            return _error(t, /* Format */[
                        /* String_literal */Block.__(11, [
                            "unexpected ')'",
                            /* End_of_format */0
                          ]),
                        "unexpected ')'"
                      ]);
        
      }
    } else {
      exit = 1;
    }
  } else {
    exit = c >= 9 ? 2 : 1;
  }
  switch (exit) {
    case 1 : 
        Buffer.add_char(t[/* atom */2], c);
        return atom(k, t);
    case 2 : 
        throw [
              Caml_builtin_exceptions.assert_failure,
              [
                "sexpm.ml",
                183,
                27
              ]
            ];
    
  }
}

function expr_list(acc, k, t) {
  while(true) {
    if (t[/* i */3] === t[/* len */4]) {
      return _refill(t, (function (param) {
                    return expr_list(acc, k, param);
                  }), _error_eof);
    } else {
      var c = _get(t);
      var exit = 0;
      var switcher = c - 9 | 0;
      if (switcher > 23 || switcher < 0) {
        if (switcher !== 32) {
          exit = 1;
        } else {
          return Curry._2(k, /* None */0, /* `List */[
                      848054398,
                      List.rev(acc)
                    ]);
        }
      } else if (switcher > 22 || switcher < 2) {
        continue ;
        
      } else {
        exit = 1;
      }
      if (exit === 1) {
        return expr_starting_with(c, (function (last, e) {
                      var exit = 0;
                      if (last) {
                        var match = last[0];
                        if (match !== 40) {
                          if (match !== 41) {
                            exit = 1;
                          } else {
                            return Curry._2(k, /* None */0, /* `List */[
                                        848054398,
                                        List.rev(/* :: */[
                                              e,
                                              acc
                                            ])
                                      ]);
                          }
                        } else {
                          return expr_list(/* [] */0, (function (_, l) {
                                        return expr_list(/* :: */[
                                                    l,
                                                    acc
                                                  ], k, t);
                                      }), t);
                        }
                      } else {
                        exit = 1;
                      }
                      if (exit === 1) {
                        return expr_list(/* :: */[
                                    e,
                                    acc
                                  ], k, t);
                      }
                      
                    }), t);
      }
      
    }
  };
}

function _return_atom(last, k, t) {
  var s = Buffer.contents(t[/* atom */2]);
  t[/* atom */2][/* position */1] = 0;
  return Curry._2(k, last, /* `Atom */[
              726615281,
              s
            ]);
}

function atom(k, t) {
  while(true) {
    if (t[/* i */3] === t[/* len */4]) {
      return _refill(t, (function (param) {
                    return atom(k, param);
                  }), (function (param) {
                    return _return_atom(/* None */0, k, param);
                  }));
    } else {
      var c = _get(t);
      var exit = 0;
      if (c >= 35) {
        if (c >= 42) {
          if (c !== 92) {
            exit = 1;
          } else {
            return _error(t, /* Format */[
                        /* String_literal */Block.__(11, [
                            "unexpected '\\' in non-quoted string",
                            /* End_of_format */0
                          ]),
                        "unexpected '\\' in non-quoted string"
                      ]);
          }
        } else {
          exit = c >= 40 ? 2 : 1;
        }
      } else if (c >= 11) {
        if (c >= 32) {
          switch (c - 32 | 0) {
            case 0 : 
                exit = 2;
                break;
            case 1 : 
                exit = 1;
                break;
            case 2 : 
                return _error(t, /* Format */[
                            /* String_literal */Block.__(11, [
                                "unexpected '\"' in the middle of an atom",
                                /* End_of_format */0
                              ]),
                            "unexpected '\"' in the middle of an atom"
                          ]);
            
          }
        } else {
          exit = 1;
        }
      } else {
        exit = c >= 9 ? 2 : 1;
      }
      switch (exit) {
        case 1 : 
            Buffer.add_char(t[/* atom */2], c);
            continue ;
            case 2 : 
            return _return_atom(/* Some */[c], k, t);
        
      }
    }
  };
}

function quoted(k, t) {
  while(true) {
    if (t[/* i */3] === t[/* len */4]) {
      return _refill(t, (function (param) {
                    return quoted(k, param);
                  }), _error_eof);
    } else {
      var c = _get(t);
      var exit = 0;
      if (c !== 34) {
        if (c !== 92) {
          exit = 1;
        } else {
          return escaped((function (c) {
                        Buffer.add_char(t[/* atom */2], c);
                        return quoted(k, t);
                      }), t);
        }
      } else {
        return _return_atom(/* None */0, k, t);
      }
      if (exit === 1) {
        Buffer.add_char(t[/* atom */2], c);
        continue ;
        
      }
      
    }
  };
}

function escaped(k, t) {
  if (t[/* i */3] === t[/* len */4]) {
    return _refill(t, (function (param) {
                  return escaped(k, param);
                }), _error_eof);
  } else {
    var c = _get(t);
    var exit = 0;
    var exit$1 = 0;
    if (c >= 92) {
      if (c >= 117) {
        exit$1 = 2;
      } else {
        switch (c - 92 | 0) {
          case 0 : 
              return Curry._1(k, /* "\\" */92);
          case 6 : 
              return Curry._1(k, /* "\b" */8);
          case 18 : 
              return Curry._1(k, /* "\n" */10);
          case 22 : 
              return Curry._1(k, /* "\r" */13);
          case 1 : 
          case 2 : 
          case 3 : 
          case 4 : 
          case 5 : 
          case 7 : 
          case 8 : 
          case 9 : 
          case 10 : 
          case 11 : 
          case 12 : 
          case 13 : 
          case 14 : 
          case 15 : 
          case 16 : 
          case 17 : 
          case 19 : 
          case 20 : 
          case 21 : 
          case 23 : 
              exit$1 = 2;
              break;
          case 24 : 
              return Curry._1(k, /* "\t" */9);
          
        }
      }
    } else if (c !== 34) {
      exit$1 = 2;
    } else {
      return Curry._1(k, /* "\"" */34);
    }
    if (exit$1 === 2) {
      if (_is_digit(c)) {
        return read2int(c - /* "0" */48 | 0, (function (n) {
                      return Curry._1(k, Char.chr(n));
                    }), t);
      } else {
        exit = 1;
      }
    }
    if (exit === 1) {
      return Curry._1(_error(t, /* Format */[
                      /* String_literal */Block.__(11, [
                          "unexpected escaped char '",
                          /* Char */Block.__(0, [/* Char_literal */Block.__(12, [
                                  /* "'" */39,
                                  /* End_of_format */0
                                ])])
                        ]),
                      "unexpected escaped char '%c'"
                    ]), c);
    }
    
  }
}

function read2int(i, k, t) {
  if (t[/* i */3] === t[/* len */4]) {
    return _refill(t, (function (param) {
                  return read2int(i, k, param);
                }), _error_eof);
  } else {
    var c = _get(t);
    if (_is_digit(c)) {
      return read1int(Caml_int32.imul(10, i) + (c - /* "0" */48 | 0) | 0, k, t);
    } else {
      return Curry._1(_error(t, /* Format */[
                      /* String_literal */Block.__(11, [
                          "unexpected char '",
                          /* Char */Block.__(0, [/* String_literal */Block.__(11, [
                                  "' when reading byte",
                                  /* End_of_format */0
                                ])])
                        ]),
                      "unexpected char '%c' when reading byte"
                    ]), c);
    }
  }
}

function read1int(i, k, t) {
  if (t[/* i */3] === t[/* len */4]) {
    return _refill(t, (function (param) {
                  return read1int(i, k, param);
                }), _error_eof);
  } else {
    var c = _get(t);
    if (_is_digit(c)) {
      return Curry._1(k, Caml_int32.imul(10, i) + (c - /* "0" */48 | 0) | 0);
    } else {
      return Curry._1(_error(t, /* Format */[
                      /* String_literal */Block.__(11, [
                          "unexpected char '",
                          /* Char */Block.__(0, [/* String_literal */Block.__(11, [
                                  "' when reading byte",
                                  /* End_of_format */0
                                ])])
                        ]),
                      "unexpected char '%c' when reading byte"
                    ]), c);
    }
  }
}

function skip_comment(k, t) {
  while(true) {
    if (t[/* i */3] === t[/* len */4]) {
      return _refill(t, (function (param) {
                    return skip_comment(k, param);
                  }), _error_eof);
    } else {
      var match = _get(t);
      if (match !== 10) {
        continue ;
        
      } else {
        return Curry._2(k, /* None */0, /* () */0);
      }
    }
  };
}

function expr_or_end(k, t) {
  while(true) {
    if (t[/* i */3] === t[/* len */4]) {
      return _refill(t, (function (param) {
                    return expr_or_end(k, param);
                  }), (function () {
                    return /* End */3455931;
                  }));
    } else {
      var c = _get(t);
      if (c >= 11) {
        if (c !== 32) {
          return expr_starting_with(c, k, t);
        } else {
          continue ;
          
        }
      } else if (c >= 9) {
        continue ;
        
      } else {
        return expr_starting_with(c, k, t);
      }
    }
  };
}

function next(t) {
  return expr_or_end((function (_, x) {
                return /* `Ok */[
                        17724,
                        x
                      ];
              }), t);
}

function parse_string(s) {
  var n = s.length;
  var stop = [/* false */0];
  var refill = function (bytes, i, _) {
    if (stop[0]) {
      return 0;
    } else {
      stop[0] = /* true */1;
      Bytes.blit_string(s, 0, bytes, i, n);
      return n;
    }
  };
  var d = make(/* Some */[n], refill);
  var res = next(d);
  if (typeof res === "number") {
    return /* `Error */[
            106380200,
            "unexpected end of file"
          ];
  } else {
    return res;
  }
}

function parse_chan(bufsize, ic) {
  var d = make(bufsize, (function (param, param$1, param$2) {
          return Pervasives.input(ic, param, param$1, param$2);
        }));
  var res = next(d);
  if (typeof res === "number") {
    return /* `Error */[
            106380200,
            "unexpected end of file"
          ];
  } else {
    return res;
  }
}

function parse_chan_gen(bufsize, ic) {
  var d = make(bufsize, (function (param, param$1, param$2) {
          return Pervasives.input(ic, param, param$1, param$2);
        }));
  return (function () {
      var e = next(d);
      if (typeof e === "number") {
        return /* None */0;
      } else {
        return /* Some */[e];
      }
    });
}

function parse_chan_list(bufsize, ic) {
  var d = make(bufsize, (function (param, param$1, param$2) {
          return Pervasives.input(ic, param, param$1, param$2);
        }));
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var e = next(d);
    if (typeof e === "number") {
      return /* `Ok */[
              17724,
              List.rev(acc)
            ];
    } else if (e[0] >= 106380200) {
      return e;
    } else {
      _acc = /* :: */[
        e[1],
        acc
      ];
      continue ;
      
    }
  };
}

function parse_file(filename) {
  return _with_in(filename, (function (ic) {
                return parse_chan(/* None */0, ic);
              }));
}

function parse_file_list(filename) {
  return _with_in(filename, (function (ic) {
                return parse_chan_list(/* None */0, ic);
              }));
}

function MakeDecode(funarg) {
  var $great$great$eq = funarg[/* >>= */1];
  var make = function ($staropt$star, refill) {
    var bufsize = $staropt$star ? $staropt$star[0] : 1024;
    var bufsize$1 = Caml_primitive.caml_int_min(bufsize > 16 ? bufsize : 16, Sys.max_string_length);
    return /* record */[
            /* buf */Caml_string.caml_create_string(bufsize$1),
            /* refill */refill,
            /* atom */Buffer.create(32),
            /* i */0,
            /* len */0,
            /* line */1,
            /* col */1
          ];
  };
  var _is_digit = function (c) {
    if (/* "0" */48 <= c) {
      return +(c <= /* "9" */57);
    } else {
      return /* false */0;
    }
  };
  var _refill = function (t, k_succ, k_fail) {
    return Curry._2($great$great$eq, Curry._3(t[/* refill */1], t[/* buf */0], 0, t[/* buf */0].length), (function (n) {
                  t[/* i */3] = 0;
                  t[/* len */4] = n;
                  if (n) {
                    return Curry._1(k_succ, t);
                  } else {
                    return Curry._1(k_fail, t);
                  }
                }));
  };
  var _get = function (t) {
    if (t[/* i */3] >= t[/* len */4]) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "sexpm.ml",
              152,
              4
            ]
          ];
    }
    var c = Caml_bytes.get(t[/* buf */0], t[/* i */3]);
    t[/* i */3] = t[/* i */3] + 1 | 0;
    if (c === /* "\n" */10) {
      t[/* col */6] = 1;
      t[/* line */5] = t[/* line */5] + 1 | 0;
    } else {
      t[/* col */6] = t[/* col */6] + 1 | 0;
    }
    return c;
  };
  var _error = function (t, msg) {
    var b = Buffer.create(32);
    Curry._2(Printf.bprintf(b, /* Format */[
              /* String_literal */Block.__(11, [
                  "at ",
                  /* Int */Block.__(4, [
                      /* Int_d */0,
                      /* No_padding */0,
                      /* No_precision */0,
                      /* String_literal */Block.__(11, [
                          ", ",
                          /* Int */Block.__(4, [
                              /* Int_d */0,
                              /* No_padding */0,
                              /* No_precision */0,
                              /* String_literal */Block.__(11, [
                                  ": ",
                                  /* End_of_format */0
                                ])
                            ])
                        ])
                    ])
                ]),
              "at %d, %d: "
            ]), t[/* line */5], t[/* col */6]);
    return Printf.kbprintf((function (b) {
                  var msg$prime = Buffer.contents(b);
                  return Curry._1(funarg[/* return */0], /* `Error */[
                              106380200,
                              msg$prime
                            ]);
                }), b, msg);
  };
  var _error_eof = function (t) {
    return _error(t, /* Format */[
                /* String_literal */Block.__(11, [
                    "unexpected end of input",
                    /* End_of_format */0
                  ]),
                "unexpected end of input"
              ]);
  };
  var expr = function (k, t) {
    while(true) {
      if (t[/* i */3] === t[/* len */4]) {
        return _refill(t, (function (param) {
                      return expr(k, param);
                    }), _error_eof);
      } else {
        var c = _get(t);
        if (c >= 11) {
          if (c !== 32) {
            return expr_starting_with(c, k, t);
          } else {
            continue ;
            
          }
        } else if (c >= 9) {
          continue ;
          
        } else {
          return expr_starting_with(c, k, t);
        }
      }
    };
  };
  var expr_starting_with = function (c, k, t) {
    var exit = 0;
    if (c >= 42) {
      if (c !== 59) {
        if (c !== 92) {
          exit = 1;
        } else {
          return _error(t, /* Format */[
                      /* String_literal */Block.__(11, [
                          "unexpected '\\'",
                          /* End_of_format */0
                        ]),
                      "unexpected '\\'"
                    ]);
        }
      } else {
        return skip_comment((function (_, _$1) {
                      return expr(k, t);
                    }), t);
      }
    } else if (c >= 11) {
      if (c >= 32) {
        switch (c - 32 | 0) {
          case 0 : 
              exit = 2;
              break;
          case 2 : 
              return quoted(k, t);
          case 1 : 
          case 3 : 
          case 4 : 
          case 5 : 
          case 6 : 
          case 7 : 
              exit = 1;
              break;
          case 8 : 
              return expr_list(/* [] */0, k, t);
          case 9 : 
              return _error(t, /* Format */[
                          /* String_literal */Block.__(11, [
                              "unexpected ')'",
                              /* End_of_format */0
                            ]),
                          "unexpected ')'"
                        ]);
          
        }
      } else {
        exit = 1;
      }
    } else {
      exit = c >= 9 ? 2 : 1;
    }
    switch (exit) {
      case 1 : 
          Buffer.add_char(t[/* atom */2], c);
          return atom(k, t);
      case 2 : 
          throw [
                Caml_builtin_exceptions.assert_failure,
                [
                  "sexpm.ml",
                  183,
                  27
                ]
              ];
      
    }
  };
  var expr_list = function (acc, k, t) {
    while(true) {
      if (t[/* i */3] === t[/* len */4]) {
        return _refill(t, (function (param) {
                      return expr_list(acc, k, param);
                    }), _error_eof);
      } else {
        var c = _get(t);
        var exit = 0;
        var switcher = c - 9 | 0;
        if (switcher > 23 || switcher < 0) {
          if (switcher !== 32) {
            exit = 1;
          } else {
            return Curry._2(k, /* None */0, /* `List */[
                        848054398,
                        List.rev(acc)
                      ]);
          }
        } else if (switcher > 22 || switcher < 2) {
          continue ;
          
        } else {
          exit = 1;
        }
        if (exit === 1) {
          return expr_starting_with(c, (function (last, e) {
                        var exit = 0;
                        if (last) {
                          var match = last[0];
                          if (match !== 40) {
                            if (match !== 41) {
                              exit = 1;
                            } else {
                              return Curry._2(k, /* None */0, /* `List */[
                                          848054398,
                                          List.rev(/* :: */[
                                                e,
                                                acc
                                              ])
                                        ]);
                            }
                          } else {
                            return expr_list(/* [] */0, (function (_, l) {
                                          return expr_list(/* :: */[
                                                      l,
                                                      acc
                                                    ], k, t);
                                        }), t);
                          }
                        } else {
                          exit = 1;
                        }
                        if (exit === 1) {
                          return expr_list(/* :: */[
                                      e,
                                      acc
                                    ], k, t);
                        }
                        
                      }), t);
        }
        
      }
    };
  };
  var _return_atom = function (last, k, t) {
    var s = Buffer.contents(t[/* atom */2]);
    t[/* atom */2][/* position */1] = 0;
    return Curry._2(k, last, /* `Atom */[
                726615281,
                s
              ]);
  };
  var atom = function (k, t) {
    while(true) {
      if (t[/* i */3] === t[/* len */4]) {
        return _refill(t, (function (param) {
                      return atom(k, param);
                    }), (function (param) {
                      return _return_atom(/* None */0, k, param);
                    }));
      } else {
        var c = _get(t);
        var exit = 0;
        if (c >= 35) {
          if (c >= 42) {
            if (c !== 92) {
              exit = 1;
            } else {
              return _error(t, /* Format */[
                          /* String_literal */Block.__(11, [
                              "unexpected '\\' in non-quoted string",
                              /* End_of_format */0
                            ]),
                          "unexpected '\\' in non-quoted string"
                        ]);
            }
          } else {
            exit = c >= 40 ? 2 : 1;
          }
        } else if (c >= 11) {
          if (c >= 32) {
            switch (c - 32 | 0) {
              case 0 : 
                  exit = 2;
                  break;
              case 1 : 
                  exit = 1;
                  break;
              case 2 : 
                  return _error(t, /* Format */[
                              /* String_literal */Block.__(11, [
                                  "unexpected '\"' in the middle of an atom",
                                  /* End_of_format */0
                                ]),
                              "unexpected '\"' in the middle of an atom"
                            ]);
              
            }
          } else {
            exit = 1;
          }
        } else {
          exit = c >= 9 ? 2 : 1;
        }
        switch (exit) {
          case 1 : 
              Buffer.add_char(t[/* atom */2], c);
              continue ;
              case 2 : 
              return _return_atom(/* Some */[c], k, t);
          
        }
      }
    };
  };
  var quoted = function (k, t) {
    while(true) {
      if (t[/* i */3] === t[/* len */4]) {
        return _refill(t, (function (param) {
                      return quoted(k, param);
                    }), _error_eof);
      } else {
        var c = _get(t);
        var exit = 0;
        if (c !== 34) {
          if (c !== 92) {
            exit = 1;
          } else {
            return escaped((function (c) {
                          Buffer.add_char(t[/* atom */2], c);
                          return quoted(k, t);
                        }), t);
          }
        } else {
          return _return_atom(/* None */0, k, t);
        }
        if (exit === 1) {
          Buffer.add_char(t[/* atom */2], c);
          continue ;
          
        }
        
      }
    };
  };
  var escaped = function (k, t) {
    if (t[/* i */3] === t[/* len */4]) {
      return _refill(t, (function (param) {
                    return escaped(k, param);
                  }), _error_eof);
    } else {
      var c = _get(t);
      var exit = 0;
      var exit$1 = 0;
      if (c >= 92) {
        if (c >= 117) {
          exit$1 = 2;
        } else {
          switch (c - 92 | 0) {
            case 0 : 
                return Curry._1(k, /* "\\" */92);
            case 6 : 
                return Curry._1(k, /* "\b" */8);
            case 18 : 
                return Curry._1(k, /* "\n" */10);
            case 22 : 
                return Curry._1(k, /* "\r" */13);
            case 1 : 
            case 2 : 
            case 3 : 
            case 4 : 
            case 5 : 
            case 7 : 
            case 8 : 
            case 9 : 
            case 10 : 
            case 11 : 
            case 12 : 
            case 13 : 
            case 14 : 
            case 15 : 
            case 16 : 
            case 17 : 
            case 19 : 
            case 20 : 
            case 21 : 
            case 23 : 
                exit$1 = 2;
                break;
            case 24 : 
                return Curry._1(k, /* "\t" */9);
            
          }
        }
      } else if (c !== 34) {
        exit$1 = 2;
      } else {
        return Curry._1(k, /* "\"" */34);
      }
      if (exit$1 === 2) {
        if (_is_digit(c)) {
          return read2int(c - /* "0" */48 | 0, (function (n) {
                        return Curry._1(k, Char.chr(n));
                      }), t);
        } else {
          exit = 1;
        }
      }
      if (exit === 1) {
        return Curry._1(_error(t, /* Format */[
                        /* String_literal */Block.__(11, [
                            "unexpected escaped char '",
                            /* Char */Block.__(0, [/* Char_literal */Block.__(12, [
                                    /* "'" */39,
                                    /* End_of_format */0
                                  ])])
                          ]),
                        "unexpected escaped char '%c'"
                      ]), c);
      }
      
    }
  };
  var read2int = function (i, k, t) {
    if (t[/* i */3] === t[/* len */4]) {
      return _refill(t, (function (param) {
                    return read2int(i, k, param);
                  }), _error_eof);
    } else {
      var c = _get(t);
      if (_is_digit(c)) {
        return read1int(Caml_int32.imul(10, i) + (c - /* "0" */48 | 0) | 0, k, t);
      } else {
        return Curry._1(_error(t, /* Format */[
                        /* String_literal */Block.__(11, [
                            "unexpected char '",
                            /* Char */Block.__(0, [/* String_literal */Block.__(11, [
                                    "' when reading byte",
                                    /* End_of_format */0
                                  ])])
                          ]),
                        "unexpected char '%c' when reading byte"
                      ]), c);
      }
    }
  };
  var read1int = function (i, k, t) {
    if (t[/* i */3] === t[/* len */4]) {
      return _refill(t, (function (param) {
                    return read1int(i, k, param);
                  }), _error_eof);
    } else {
      var c = _get(t);
      if (_is_digit(c)) {
        return Curry._1(k, Caml_int32.imul(10, i) + (c - /* "0" */48 | 0) | 0);
      } else {
        return Curry._1(_error(t, /* Format */[
                        /* String_literal */Block.__(11, [
                            "unexpected char '",
                            /* Char */Block.__(0, [/* String_literal */Block.__(11, [
                                    "' when reading byte",
                                    /* End_of_format */0
                                  ])])
                          ]),
                        "unexpected char '%c' when reading byte"
                      ]), c);
      }
    }
  };
  var skip_comment = function (k, t) {
    while(true) {
      if (t[/* i */3] === t[/* len */4]) {
        return _refill(t, (function (param) {
                      return skip_comment(k, param);
                    }), _error_eof);
      } else {
        var match = _get(t);
        if (match !== 10) {
          continue ;
          
        } else {
          return Curry._2(k, /* None */0, /* () */0);
        }
      }
    };
  };
  var expr_or_end = function (k, t) {
    while(true) {
      if (t[/* i */3] === t[/* len */4]) {
        return _refill(t, (function (param) {
                      return expr_or_end(k, param);
                    }), (function () {
                      return Curry._1(funarg[/* return */0], /* End */3455931);
                    }));
      } else {
        var c = _get(t);
        if (c >= 11) {
          if (c !== 32) {
            return expr_starting_with(c, k, t);
          } else {
            continue ;
            
          }
        } else if (c >= 9) {
          continue ;
          
        } else {
          return expr_starting_with(c, k, t);
        }
      }
    };
  };
  var next = function (t) {
    return expr_or_end((function (_, x) {
                  return Curry._1(funarg[/* return */0], /* `Ok */[
                              17724,
                              x
                            ]);
                }), t);
  };
  return [
          make,
          next
        ];
}

var D = [
  make,
  next
];

exports.to_buf          = to_buf;
exports.to_string       = to_string;
exports.to_file         = to_file;
exports.to_file_seq     = to_file_seq;
exports.to_chan         = to_chan;
exports.print           = print;
exports.print_noindent  = print_noindent;
exports.MakeDecode      = MakeDecode;
exports.ID_MONAD        = ID_MONAD;
exports.D               = D;
exports.parse_string    = parse_string;
exports.parse_chan      = parse_chan;
exports.parse_chan_gen  = parse_chan_gen;
exports.parse_chan_list = parse_chan_list;
exports.parse_file      = parse_file;
exports.parse_file_list = parse_file_list;
/* Format Not a pure module */
