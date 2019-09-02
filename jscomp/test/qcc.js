'use strict';

var Sys = require("../../lib/js/sys.js");
var Char = require("../../lib/js/char.js");
var List = require("../../lib/js/list.js");
var Bytes = require("../../lib/js/bytes.js");
var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");
var $$String = require("../../lib/js/string.js");
var Caml_io = require("../../lib/js/caml_io.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_external_polyfill = require("../../lib/js/caml_external_polyfill.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var dbg = /* record */[/* contents */true];

var inch = /* record */[/* contents */Pervasives.stdin];

function bufferize(f) {
  var buf = /* record */[/* contents */undefined];
  return /* tuple */[
          (function (param) {
              var match = buf[0];
              if (match !== undefined) {
                buf[0] = undefined;
                return Caml_option.valFromOption(match);
              } else {
                return Curry._1(f, /* () */0);
              }
            }),
          (function (x) {
              if (buf[0] !== undefined) {
                throw [
                      Caml_builtin_exceptions.assert_failure,
                      /* tuple */[
                        "qcc.ml",
                        17,
                        4
                      ]
                    ];
              }
              buf[0] = Caml_option.some(x);
              return /* () */0;
            })
        ];
}

var match = bufferize((function (param) {
        return Caml_external_polyfill.resolve("caml_ml_input_char")(inch[0]);
      }));

var ungetch = match[1];

var getch = match[0];

function peekch(param) {
  var ch = Curry._1(getch, /* () */0);
  Curry._1(ungetch, ch);
  return ch;
}

var symtab = Caml_array.caml_make_vect(100, "");

var syms = /* record */[/* contents */0];

function find(s, _n) {
  while(true) {
    var n = _n;
    if (n >= syms[0]) {
      syms[0] = syms[0] + 1 | 0;
      return n;
    } else if (Caml_array.caml_array_get(symtab, n) === s) {
      return n;
    } else {
      _n = n + 1 | 0;
      continue ;
    }
  };
}

function addsym(s) {
  var sid = find(s, 0);
  Caml_array.caml_array_set(symtab, sid, s);
  return sid;
}

function symstr(n) {
  if (n >= syms[0]) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "qcc.ml",
            40,
            4
          ]
        ];
  }
  return Caml_array.caml_array_get(symtab, n);
}

function symitr(f) {
  for(var i = 0 ,i_finish = syms[0] - 1 | 0; i <= i_finish; ++i){
    Curry._2(f, i, Caml_array.caml_array_get(symtab, i));
  }
  return /* () */0;
}

var glo = Bytes.make(4096, /* "\000" */0);

var gpos = /* record */[/* contents */0];

var s = Caml_bytes.caml_create_bytes(100);

function getq(param) {
  var c = Curry._1(getch, /* () */0);
  if (c !== 92 || peekch(/* () */0) !== /* "n" */110) {
    return c;
  } else {
    Curry._1(getch, /* () */0);
    return /* "\n" */10;
  }
}

function isid(param) {
  var switcher = param - 91 | 0;
  if (switcher > 5 || switcher < 0) {
    return (switcher + 26 >>> 0) <= 57;
  } else {
    return switcher === 4;
  }
}

function skip(_param) {
  while(true) {
    var ch = Curry._1(getch, /* () */0);
    if (ch >= 14) {
      if (ch !== 32) {
        if (ch !== 47 || peekch(/* () */0) !== /* "*" */42) {
          return ch;
        } else {
          var _param$1 = (Curry._1(getch, /* () */0), /* () */0);
          while(true) {
            var match = Curry._1(getch, /* () */0);
            if (match !== 42) {
              _param$1 = /* () */0;
              continue ;
            } else if (peekch(/* () */0) === /* "/" */47) {
              return skip((Curry._1(getch, /* () */0), /* () */0));
            } else {
              _param$1 = /* () */0;
              continue ;
            }
          };
        }
      } else {
        _param = /* () */0;
        continue ;
      }
    } else if (ch >= 11) {
      if (ch >= 13) {
        _param = /* () */0;
        continue ;
      } else {
        return ch;
      }
    } else if (ch >= 9) {
      _param = /* () */0;
      continue ;
    } else {
      return ch;
    }
  };
}

function next(param) {
  var match;
  try {
    match = skip(/* () */0);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.end_of_file) {
      match = undefined;
    } else {
      throw exn;
    }
  }
  if (match !== undefined) {
    var c = match;
    if (c !== 34) {
      if (c >= 48) {
        if (c < 58) {
          var _n = c - 48 | 0;
          while(true) {
            var n = _n;
            var match$1 = peekch(/* () */0);
            if (match$1 > 57 || match$1 < 48) {
              return /* constructor */{
                      tag: "ILit",
                      Arg0: n
                    };
            } else {
              _n = (Caml_int32.imul(10, n) + Curry._1(getch, /* () */0) | 0) - 48 | 0;
              continue ;
            }
          };
        }
        
      } else if (c === 39) {
        var ch = getq(/* () */0);
        var qt = Curry._1(getch, /* () */0);
        if (qt !== /* "'" */39) {
          throw [
                Caml_builtin_exceptions.failure,
                "syntax error"
              ];
        }
        return /* constructor */{
                tag: "ILit",
                Arg0: ch
              };
      }
      
    } else {
      var b = gpos[0];
      var _e = gpos[0];
      while(true) {
        var e = _e;
        var match$2 = peekch(/* () */0);
        if (match$2 !== 34) {
          glo[e] = getq(/* () */0);
          _e = e + 1 | 0;
          continue ;
        } else {
          Curry._1(getch, /* () */0);
          gpos[0] = e + 8 & -8;
          return /* constructor */{
                  tag: "SLit",
                  Arg0: (b + 232 | 0) + 4194304 | 0,
                  Arg1: Bytes.to_string(Bytes.sub(glo, b, e - b | 0))
                };
        }
      };
    }
    if (isid(c)) {
      var _n$1 = 0;
      var _ch = c;
      while(true) {
        var ch$1 = _ch;
        var n$1 = _n$1;
        s[n$1] = ch$1;
        if (isid(peekch(/* () */0))) {
          _ch = Curry._1(getch, /* () */0);
          _n$1 = n$1 + 1 | 0;
          continue ;
        } else {
          return /* constructor */{
                  tag: "Sym",
                  Arg0: addsym(Bytes.to_string(Bytes.sub(s, 0, n$1 + 1 | 0)))
                };
        }
      };
    } else {
      var ch$2 = c;
      var _param = /* constructor */{
        tag: "::",
        Arg0: "++",
        Arg1: /* constructor */{
          tag: "::",
          Arg0: "--",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "&&",
            Arg1: /* constructor */{
              tag: "::",
              Arg0: "||",
              Arg1: /* constructor */{
                tag: "::",
                Arg0: "==",
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: "<=",
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: ">=",
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: "!=",
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: ">>",
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: "<<",
                          Arg1: "[]"
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      };
      while(true) {
        var param$1 = _param;
        if (param$1 !== "[]") {
          var lop = param$1.Arg0;
          if (Caml_string.get(lop, 0) === ch$2 && Caml_string.get(lop, 1) === peekch(/* () */0)) {
            Curry._1(getch, /* () */0);
            return /* constructor */{
                    tag: "Op",
                    Arg0: lop
                  };
          } else {
            _param = param$1.Arg1;
            continue ;
          }
        } else {
          return /* constructor */{
                  tag: "Op",
                  Arg0: Caml_bytes.bytes_to_string(Bytes.make(1, ch$2))
                };
        }
      };
    }
  } else {
    return /* constructor */{
            tag: "Op",
            Arg0: "EOF!"
          };
  }
}

var match$1 = bufferize(next);

var unnext = match$1[1];

var next$1 = match$1[0];

function nextis(t) {
  var nt = Curry._1(next$1, /* () */0);
  Curry._1(unnext, nt);
  return Caml_obj.caml_equal(t, nt);
}

var obuf = Bytes.make(1048576, /* "\000" */0);

var opos = /* record */[/* contents */0];

function out(x) {
  if (x !== 0) {
    out(x / 256 | 0);
    obuf[opos[0]] = Char.chr(x & 255);
    opos[0] = opos[0] + 1 | 0;
    return /* () */0;
  } else {
    return 0;
  }
}

function le(n, x) {
  for(var i = 0 ,i_finish = (n / 8 | 0) - 1 | 0; i <= i_finish; ++i){
    var $$byte = (x >>> (i << 3)) & 255;
    obuf[opos[0]] = Char.chr($$byte);
    opos[0] = opos[0] + 1 | 0;
  }
  return /* () */0;
}

function get32(l) {
  return ((Caml_bytes.get(obuf, l) + (Caml_bytes.get(obuf, l + 1 | 0) << 8) | 0) + (Caml_bytes.get(obuf, l + 2 | 0) << 16) | 0) + (Caml_bytes.get(obuf, l + 3 | 0) << 24) | 0;
}

function patch(rel, loc, n) {
  if (n >= 0) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "qcc.ml",
            157,
            2
          ]
        ];
  }
  if (loc !== 0) {
    var i = opos[0];
    var loc$prime = get32(loc);
    var x = rel ? n - (loc + 4 | 0) | 0 : n;
    if (dbg[0]) {
      Curry._3(Printf.eprintf(/* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "String_literal",
                  Arg0: "patching at ",
                  Arg1: /* constructor */{
                    tag: "Int",
                    Arg0: "Int_d",
                    Arg1: "No_padding",
                    Arg2: "No_precision",
                    Arg3: /* constructor */{
                      tag: "String_literal",
                      Arg0: " to ",
                      Arg1: /* constructor */{
                        tag: "Int",
                        Arg0: "Int_d",
                        Arg1: "No_padding",
                        Arg2: "No_precision",
                        Arg3: /* constructor */{
                          tag: "String_literal",
                          Arg0: " (n=",
                          Arg1: /* constructor */{
                            tag: "Int",
                            Arg0: "Int_d",
                            Arg1: "No_padding",
                            Arg2: "No_precision",
                            Arg3: /* constructor */{
                              tag: "String_literal",
                              Arg0: ")\n",
                              Arg1: "End_of_format"
                            }
                          }
                        }
                      }
                    }
                  }
                },
                Arg1: "patching at %d to %d (n=%d)\n"
              }), loc, x, n);
    }
    opos[0] = loc;
    le(32, x);
    patch(rel, loc$prime, n);
    opos[0] = i;
    return /* () */0;
  } else {
    return 0;
  }
}

function load(r, n) {
  out(184 + r | 0);
  return le(32, n);
}

function cmp(n) {
  load(0, 0);
  return out(1020608 + (n << 8) | 0);
}

function test(n, l) {
  out(4752832);
  out(3972 + n | 0);
  var loc = opos[0];
  le(32, l);
  return loc;
}

var align = /* record */[/* contents */0];

function push(r) {
  align[0] = align[0] + 1 | 0;
  if (r < 8) {
    return out(80 + r | 0);
  } else {
    return out((16720 + r | 0) - 8 | 0);
  }
}

function pop(r) {
  align[0] = align[0] - 1 | 0;
  if (r < 8) {
    return out(88 + r | 0);
  } else {
    return out((16728 + r | 0) - 8 | 0);
  }
}

var lval = /* record */[/* contents : tuple */[
    /* constructor */{
      tag: "Mov",
      Arg0: 0
    },
    "Int"
  ]];

function patchlval(param) {
  var match = lval[0][0];
  if (/* XXX */match.tag === "Mov") {
    obuf[opos[0] - match.Arg0 | 0] = /* "\141" */141;
    return /* () */0;
  } else {
    opos[0] = opos[0] - match.Arg0 | 0;
    return /* () */0;
  }
}

function read(param) {
  if (param !== "Int") {
    out(4722614);
    le(8, 0);
    lval[0] = /* tuple */[
      /* constructor */{
        tag: "Del",
        Arg0: 4
      },
      "Chr"
    ];
    return /* () */0;
  } else {
    out(18571);
    le(8, 0);
    lval[0] = /* tuple */[
      /* constructor */{
        tag: "Del",
        Arg0: 3
      },
      "Int"
    ];
    return /* () */0;
  }
}

var globs = Caml_array.caml_make_vect(100, /* record */[
      /* loc */0,
      /* va */-1
    ]);

var lvls = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "*",
    0
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "/",
      0
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "%",
        0
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "+",
          1
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "-",
            1
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "<<",
              2
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                ">>",
                2
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "<",
                  3
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "<=",
                    3
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      ">",
                      3
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        ">=",
                        3
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          "==",
                          4
                        ],
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            "!=",
                            4
                          ],
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: /* tuple */[
                              "&",
                              5
                            ],
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: /* tuple */[
                                "^",
                                6
                              ],
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* tuple */[
                                  "|",
                                  7
                                ],
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: /* tuple */[
                                    "&&",
                                    8
                                  ],
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: /* tuple */[
                                      "||",
                                      9
                                    ],
                                    Arg1: "[]"
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};

var inss = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "*",
    /* constructor */{
      tag: "Bin",
      Arg0: /* constructor */{
        tag: "::",
        Arg0: 1208987585,
        Arg1: "[]"
      }
    }
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "/",
      /* constructor */{
        tag: "Bin",
        Arg0: /* constructor */{
          tag: "::",
          Arg0: 18577,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 18585,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 4782073,
              Arg1: "[]"
            }
          }
        }
      }
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "%",
        /* constructor */{
          tag: "Bin",
          Arg0: /* constructor */{
            tag: "::",
            Arg0: 18577,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 18585,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 4782073,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 18578,
                  Arg1: "[]"
                }
              }
            }
          }
        }
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "+",
          /* constructor */{
            tag: "Bin",
            Arg0: /* constructor */{
              tag: "::",
              Arg0: 4719048,
              Arg1: "[]"
            }
          }
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "-",
            /* constructor */{
              tag: "Bin",
              Arg0: /* constructor */{
                tag: "::",
                Arg0: 18577,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 4729288,
                  Arg1: "[]"
                }
              }
            }
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "<<",
              /* constructor */{
                tag: "Bin",
                Arg0: /* constructor */{
                  tag: "::",
                  Arg0: 18577,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 4772832,
                    Arg1: "[]"
                  }
                }
              }
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                ">>",
                /* constructor */{
                  tag: "Bin",
                  Arg0: /* constructor */{
                    tag: "::",
                    Arg0: 18577,
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: 4772856,
                      Arg1: "[]"
                    }
                  }
                }
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "<",
                  /* constructor */{
                    tag: "Cmp",
                    Arg0: 10
                  }
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "<=",
                    /* constructor */{
                      tag: "Cmp",
                      Arg0: 12
                    }
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      ">",
                      /* constructor */{
                        tag: "Cmp",
                        Arg0: 13
                      }
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        ">=",
                        /* constructor */{
                          tag: "Cmp",
                          Arg0: 11
                        }
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          "==",
                          /* constructor */{
                            tag: "Cmp",
                            Arg0: 2
                          }
                        ],
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            "!=",
                            /* constructor */{
                              tag: "Cmp",
                              Arg0: 3
                            }
                          ],
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: /* tuple */[
                              "&",
                              /* constructor */{
                                tag: "Bin",
                                Arg0: /* constructor */{
                                  tag: "::",
                                  Arg0: 4727240,
                                  Arg1: "[]"
                                }
                              }
                            ],
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: /* tuple */[
                                "^",
                                /* constructor */{
                                  tag: "Bin",
                                  Arg0: /* constructor */{
                                    tag: "::",
                                    Arg0: 4731336,
                                    Arg1: "[]"
                                  }
                                }
                              ],
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* tuple */[
                                  "|",
                                  /* constructor */{
                                    tag: "Bin",
                                    Arg0: /* constructor */{
                                      tag: "::",
                                      Arg0: 4721096,
                                      Arg1: "[]"
                                    }
                                  }
                                ],
                                Arg1: "[]"
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};

var tokint = /* constructor */{
  tag: "Sym",
  Arg0: addsym("int")
};

var tokchar = /* constructor */{
  tag: "Sym",
  Arg0: addsym("char")
};

var tokret = /* constructor */{
  tag: "Sym",
  Arg0: addsym("return")
};

var tokif = /* constructor */{
  tag: "Sym",
  Arg0: addsym("if")
};

var tokelse = /* constructor */{
  tag: "Sym",
  Arg0: addsym("else")
};

var tokwhile = /* constructor */{
  tag: "Sym",
  Arg0: addsym("while")
};

var tokfor = /* constructor */{
  tag: "Sym",
  Arg0: addsym("for")
};

var tokbreak = /* constructor */{
  tag: "Sym",
  Arg0: addsym("break")
};

function binary(stk, lvl) {
  if (lvl === -1) {
    return unary(stk);
  } else {
    var lvlof = function (o) {
      if (List.mem_assoc(o, lvls)) {
        return List.assoc(o, lvls);
      } else {
        return -1;
      }
    };
    var foldtst = function (_loc) {
      while(true) {
        var loc = _loc;
        var t = Curry._1(next$1, /* () */0);
        if (/* XXX */t.tag === "Op" && lvlof(t.Arg0) === lvl) {
          var loc$prime = test(lvl - 8 | 0, loc);
          binary(stk, lvl - 1 | 0);
          _loc = loc$prime;
          continue ;
        } else {
          Curry._1(unnext, t);
          return loc;
        }
      };
    };
    binary(stk, lvl - 1 | 0);
    if (lvl < 8) {
      var _param = /* () */0;
      while(true) {
        var t = Curry._1(next$1, /* () */0);
        if (/* XXX */t.tag === "Op") {
          var o = t.Arg0;
          if (lvlof(o) === lvl) {
            push(0);
            binary(stk, lvl - 1 | 0);
            pop(1);
            var match = List.assoc(o, inss);
            if (/* XXX */match.tag === "Bin") {
              List.iter(out, match.Arg0);
            } else {
              out(4733377);
              cmp(match.Arg0);
            }
            _param = /* () */0;
            continue ;
          } else {
            return Curry._1(unnext, t);
          }
        } else {
          return Curry._1(unnext, t);
        }
      };
    } else {
      var loc = foldtst(0);
      return patch(true, loc, opos[0]);
    }
  }
}

function unary(stk) {
  var match = Curry._1(next$1, /* () */0);
  switch (/* XXX */match.tag) {
    case "Op" :
        var o = match.Arg0;
        switch (o) {
          case "&" :
              unary(stk);
              return patchlval(/* () */0);
          case "(" :
              expr(stk);
              Curry._1(next$1, /* () */0);
              return postfix(stk);
          case "*" :
              Curry._1(next$1, /* () */0);
              var t = Curry._1(next$1, /* () */0);
              var match$1;
              if (Caml_obj.caml_equal(t, tokint)) {
                match$1 = Caml_obj.caml_equal(Curry._1(next$1, /* () */0), /* constructor */{
                      tag: "Op",
                      Arg0: "*"
                    }) ? /* tuple */[
                    "Int",
                    1
                  ] : /* tuple */[
                    "Int",
                    5
                  ];
              } else if (Caml_obj.caml_equal(t, tokchar)) {
                match$1 = /* tuple */[
                  "Chr",
                  2
                ];
              } else {
                throw [
                      Caml_builtin_exceptions.failure,
                      "[cast] expected"
                    ];
              }
              for(var k = 1 ,k_finish = match$1[1]; k <= k_finish; ++k){
                Curry._1(next$1, /* () */0);
              }
              unary(stk);
              return read(match$1[0]);
          default:
            var unops = /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "+",
                0
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "-",
                  4782040
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "~",
                    4782032
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "!",
                      4752832
                    ],
                    Arg1: "[]"
                  }
                }
              }
            };
            unary(stk);
            if (!List.mem_assoc(o, unops)) {
              var s = Curry._1(Printf.sprintf(/* constructor */{
                        tag: "Format",
                        Arg0: /* constructor */{
                          tag: "String_literal",
                          Arg0: "unknown operator ",
                          Arg1: /* constructor */{
                            tag: "String",
                            Arg0: "No_padding",
                            Arg1: "End_of_format"
                          }
                        },
                        Arg1: "unknown operator %s"
                      }), o);
              throw [
                    Caml_builtin_exceptions.failure,
                    s
                  ];
            }
            out(List.assoc(o, unops));
            if (o === "!") {
              return cmp(2);
            } else {
              return 0;
            }
        }
    case "ILit" :
        return load(0, match.Arg0);
    case "SLit" :
        out(18616);
        return le(64, match.Arg0);
    case "Sym" :
        var i = match.Arg0;
        if (List.mem_assoc(i, stk)) {
          var l = List.assoc(i, stk);
          if (l <= -256) {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "qcc.ml",
                    295,
                    6
                  ]
                ];
          }
          out(4754245);
          out(l & 255);
          lval[0] = /* tuple */[
            /* constructor */{
              tag: "Mov",
              Arg0: 3
            },
            "Int"
          ];
        } else {
          out(18616);
          var g = Caml_array.caml_array_get(globs, i);
          var loc = opos[0];
          le(64, g[/* loc */0]);
          Caml_array.caml_array_set(globs, i, /* record */[
                /* loc */loc,
                /* va */g[/* va */1]
              ]);
          read("Int");
        }
        return postfix(stk);
    
  }
}

function postfix(stk) {
  var t = Curry._1(next$1, /* () */0);
  if (/* XXX */t.tag === "Op") {
    var op = t.Arg0;
    switch (op) {
      case "(" :
          var emitargs = function (_l, _rl) {
            while(true) {
              var rl = _rl;
              var l = _l;
              if (nextis(/* constructor */{
                      tag: "Op",
                      Arg0: ")"
                    })) {
                Curry._1(next$1, /* () */0);
                return List.iter(pop, l);
              } else {
                expr(stk);
                push(0);
                if (nextis(/* constructor */{
                        tag: "Op",
                        Arg0: ","
                      })) {
                  Curry._1(next$1, /* () */0);
                }
                _rl = List.tl(rl);
                _l = /* constructor */{
                  tag: "::",
                  Arg0: List.hd(rl),
                  Arg1: l
                };
                continue ;
              }
            };
          };
          patchlval(/* () */0);
          push(0);
          emitargs("[]", /* constructor */{
                tag: "::",
                Arg0: 7,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 6,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 2,
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: 1,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: 8,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: 9,
                          Arg1: "[]"
                        }
                      }
                    }
                  }
                }
              });
          pop(0);
          if (align[0] % 2 !== 0) {
            out(1216605192);
          }
          out(65488);
          if (align[0] % 2 !== 0) {
            return out(1216594952);
          } else {
            return 0;
          }
      case "++" :
      case "--" :
          break;
      default:
        return Curry._1(unnext, t);
    }
    patchlval(/* () */0);
    out(4753857);
    read(lval[0][1]);
    return out(List.assoc(/* tuple */[
                    op,
                    lval[0][1]
                  ], /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      /* tuple */[
                        "++",
                        "Int"
                      ],
                      4783873
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        /* tuple */[
                          "--",
                          "Int"
                        ],
                        4783881
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          /* tuple */[
                            "++",
                            "Chr"
                          ],
                          65025
                        ],
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            /* tuple */[
                              "--",
                              "Chr"
                            ],
                            65033
                          ],
                          Arg1: "[]"
                        }
                      }
                    }
                  }));
  } else {
    return Curry._1(unnext, t);
  }
}

function expr(stk) {
  binary(stk, 10);
  var _param = /* () */0;
  while(true) {
    var t = Curry._1(next$1, /* () */0);
    if (/* XXX */t.tag === "Op" && t.Arg0 === "=") {
      patchlval(/* () */0);
      var ty = lval[0][1];
      push(0);
      expr(stk);
      pop(1);
      if (ty === "Int") {
        out(4753665);
      } else {
        out(34817);
      }
      _param = /* () */0;
      continue ;
    } else {
      return Curry._1(unnext, t);
    }
  };
}

function decl(g, _n, _stk) {
  while(true) {
    var stk = _stk;
    var n = _n;
    var t = Curry._1(next$1, /* () */0);
    if (Caml_obj.caml_equal(t, tokint)) {
      var top = stk !== "[]" ? stk.Arg0[1] : 0;
      var vars = (function(top){
      return function vars(_n, _stk) {
        while(true) {
          var stk = _stk;
          var n = _n;
          while(nextis(/* constructor */{
                  tag: "Op",
                  Arg0: "*"
                })) {
            Curry._1(next$1, /* () */0);
          };
          if (nextis(/* constructor */{
                  tag: "Op",
                  Arg0: ";"
                })) {
            return /* tuple */[
                    n,
                    stk
                  ];
          } else {
            var match = Curry._1(next$1, /* () */0);
            if (/* XXX */match.tag === "Sym") {
              var s = match.Arg0;
              var n$prime = n + 1 | 0;
              var stk$prime;
              if (g) {
                var glo = Caml_array.caml_array_get(globs, s);
                if (glo[/* va */1] >= 0) {
                  throw [
                        Caml_builtin_exceptions.failure,
                        "symbol defined twice"
                      ];
                }
                var va = (gpos[0] + 232 | 0) + 4194304 | 0;
                Caml_array.caml_array_set(globs, s, /* record */[
                      /* loc */glo[/* loc */0],
                      /* va */va
                    ]);
                gpos[0] = gpos[0] + 8 | 0;
                stk$prime = stk;
              } else {
                stk$prime = /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    s,
                    top - (n$prime << 3) | 0
                  ],
                  Arg1: stk
                };
              }
              if (nextis(/* constructor */{
                      tag: "Op",
                      Arg0: ","
                    })) {
                Curry._1(next$1, /* () */0);
                _stk = stk$prime;
                _n = n$prime;
                continue ;
              } else {
                return /* tuple */[
                        n$prime,
                        stk$prime
                      ];
              }
            } else {
              throw [
                    Caml_builtin_exceptions.failure,
                    "[var] expected in [decl]"
                  ];
            }
          }
        };
      }
      }(top));
      var match = vars(0, stk);
      Curry._1(next$1, /* () */0);
      if (dbg[0]) {
        Curry._1(Printf.eprintf(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String_literal",
                    Arg0: "end of decl (",
                    Arg1: /* constructor */{
                      tag: "Int",
                      Arg0: "Int_d",
                      Arg1: "No_padding",
                      Arg2: "No_precision",
                      Arg3: /* constructor */{
                        tag: "String_literal",
                        Arg0: " vars)\n",
                        Arg1: "End_of_format"
                      }
                    }
                  },
                  Arg1: "end of decl (%d vars)\n"
                }), n);
      }
      _stk = match[1];
      _n = n + match[0] | 0;
      continue ;
    } else {
      Curry._1(unnext, t);
      if (!g && n !== 0) {
        if ((n << 3) >= 256) {
          throw [
                Caml_builtin_exceptions.assert_failure,
                /* tuple */[
                  "qcc.ml",
                  436,
                  6
                ]
              ];
        }
        out(4752364);
        out((n << 3));
        align[0] = align[0] + n | 0;
      }
      if (dbg[0] && !g) {
        console.error("end of blk decls");
      }
      return /* tuple */[
              n,
              stk
            ];
    }
  };
}

var retl = /* record */[/* contents */0];

function stmt(brk, stk) {
  var pexpr = function (stk) {
    Curry._1(next$1, /* () */0);
    expr(stk);
    Curry._1(next$1, /* () */0);
    return /* () */0;
  };
  var t = Curry._1(next$1, /* () */0);
  if (Caml_obj.caml_equal(t, tokif)) {
    pexpr(stk);
    var loc = test(0, 0);
    stmt(brk, stk);
    var loc$1;
    if (nextis(tokelse)) {
      Curry._1(next$1, /* () */0);
      out(233);
      var l = opos[0];
      le(32, 0);
      patch(true, loc, opos[0]);
      stmt(brk, stk);
      loc$1 = l;
    } else {
      loc$1 = loc;
    }
    return patch(true, loc$1, opos[0]);
  } else if (Caml_obj.caml_equal(t, tokwhile) || Caml_obj.caml_equal(t, tokfor)) {
    var bl = /* record */[/* contents */0];
    var ba = align[0];
    var match;
    if (Caml_obj.caml_equal(t, tokwhile)) {
      var loc$2 = opos[0];
      pexpr(stk);
      bl[0] = test(0, 0);
      match = /* tuple */[
        0,
        loc$2
      ];
    } else {
      Curry._1(next$1, /* () */0);
      if (!nextis(/* constructor */{
              tag: "Op",
              Arg0: ";"
            })) {
        expr(stk);
      }
      Curry._1(next$1, /* () */0);
      var top = opos[0];
      if (nextis(/* constructor */{
              tag: "Op",
              Arg0: ";"
            })) {
        bl[0] = 0;
      } else {
        expr(stk);
        bl[0] = test(0, 0);
      }
      Curry._1(next$1, /* () */0);
      out(233);
      var bdy = opos[0];
      le(32, 0);
      var itr = opos[0];
      expr(stk);
      Curry._1(next$1, /* () */0);
      out(233);
      le(32, (top - opos[0] | 0) - 4 | 0);
      match = /* tuple */[
        bdy,
        itr
      ];
    }
    patch(true, match[0], opos[0]);
    stmt(/* tuple */[
          bl,
          ba
        ], stk);
    out(233);
    le(32, (match[1] - opos[0] | 0) - 4 | 0);
    return patch(true, bl[0], opos[0]);
  } else if (Caml_obj.caml_equal(t, tokret)) {
    if (!nextis(/* constructor */{
            tag: "Op",
            Arg0: ";"
          })) {
      expr(stk);
    }
    Curry._1(next$1, /* () */0);
    out(233);
    var loc$3 = opos[0];
    le(32, retl[0]);
    retl[0] = loc$3;
    return /* () */0;
  } else if (Caml_obj.caml_equal(t, tokbreak)) {
    Curry._1(next$1, /* () */0);
    var brkl = brk[0];
    var n = align[0] - brk[1] | 0;
    if (n < 0) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            /* tuple */[
              "qcc.ml",
              515,
              4
            ]
          ];
    }
    if (n !== 0) {
      out(4752324);
      out((n << 3));
    }
    out(233);
    var loc$4 = opos[0];
    le(32, brkl[0]);
    brkl[0] = loc$4;
    return /* () */0;
  } else if (/* XXX */t.tag === "Op") {
    switch (t.Arg0) {
      case ";" :
          return /* () */0;
      case "{" :
          return block(brk, stk);
      default:
        
    }
  }
  Curry._1(unnext, t);
  expr(stk);
  Curry._1(next$1, /* () */0);
  return /* () */0;
}

function block(brk, stk) {
  var match = decl(false, 0, stk);
  var stk$prime = match[1];
  var n = match[0];
  while(!nextis(/* constructor */{
          tag: "Op",
          Arg0: "}"
        })) {
    stmt(brk, stk$prime);
  };
  Curry._1(next$1, /* () */0);
  if (n !== 0) {
    out(4752324);
    out((n << 3));
    align[0] = align[0] - n | 0;
    return /* () */0;
  } else {
    return 0;
  }
}

function top(_param) {
  while(true) {
    if (nextis(/* constructor */{
            tag: "Op",
            Arg0: "EOF!"
          })) {
      return 0;
    } else if (nextis(tokint)) {
      decl(true, 0, "[]");
      _param = /* () */0;
      continue ;
    } else {
      var match = Curry._1(next$1, /* () */0);
      if (/* XXX */match.tag === "Sym") {
        var f = match.Arg0;
        var g = Caml_array.caml_array_get(globs, f);
        if (g[/* va */1] >= 0) {
          throw [
                Caml_builtin_exceptions.failure,
                "symbol defined twice"
              ];
        }
        Caml_array.caml_array_set(globs, f, /* record */[
              /* loc */g[/* loc */0],
              /* va */opos[0]
            ]);
        var emitargs = function (_regs, _n, _stk) {
          while(true) {
            var stk = _stk;
            var n = _n;
            var regs = _regs;
            var match = Curry._1(next$1, /* () */0);
            switch (/* XXX */match.tag) {
              case "Op" :
                  if (match.Arg0 === ")") {
                    return stk;
                  } else {
                    throw [
                          Caml_builtin_exceptions.failure,
                          "[var] or ) expected"
                        ];
                  }
              case "ILit" :
              case "SLit" :
                  throw [
                        Caml_builtin_exceptions.failure,
                        "[var] or ) expected"
                      ];
              case "Sym" :
                  var r = List.hd(regs);
                  push(r);
                  if (nextis(/* constructor */{
                          tag: "Op",
                          Arg0: ","
                        })) {
                    Curry._1(next$1, /* () */0);
                  }
                  var stk$prime = /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      match.Arg0,
                      ((-n | 0) << 3)
                    ],
                    Arg1: stk
                  };
                  _stk = stk$prime;
                  _n = n + 1 | 0;
                  _regs = List.tl(regs);
                  continue ;
              
            }
          };
        };
        Curry._1(next$1, /* () */0);
        align[0] = 0;
        out(85);
        out(4753893);
        var stk = emitargs(/* constructor */{
              tag: "::",
              Arg0: 7,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 6,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 2,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 1,
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: 8,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: 9,
                        Arg1: "[]"
                      }
                    }
                  }
                }
              }
            }, 1, "[]");
        while(Caml_obj.caml_notequal(Curry._1(next$1, /* () */0), /* constructor */{
                tag: "Op",
                Arg0: "{"
              })) {
          
        };
        retl[0] = 0;
        block(/* tuple */[
              /* record */[/* contents */0],
              0
            ], stk);
        patch(true, retl[0], opos[0]);
        out(51651);
        if (dbg[0]) {
          Curry._1(Printf.eprintf(/* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "String_literal",
                      Arg0: "done with function ",
                      Arg1: /* constructor */{
                        tag: "String",
                        Arg0: "No_padding",
                        Arg1: /* constructor */{
                          tag: "Char_literal",
                          Arg0: /* "\n" */10,
                          Arg1: "End_of_format"
                        }
                      }
                    },
                    Arg1: "done with function %s\n"
                  }), symstr(f));
        }
        _param = /* () */0;
        continue ;
      } else {
        throw [
              Caml_builtin_exceptions.failure,
              "[decl] or [fun] expected"
            ];
      }
    }
  };
}

var elfhdr = Bytes.of_string($$String.concat("", /* constructor */{
          tag: "::",
          Arg0: "\x7fELF\x02\x01\x01\0",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "\0\0\0\0\0\0\0\0",
            Arg1: /* constructor */{
              tag: "::",
              Arg0: "\x02\0",
              Arg1: /* constructor */{
                tag: "::",
                Arg0: ">\0",
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: "\x01\0\0\0",
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: "\0\0\0\0\0\0\0\0",
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: "@\0\0\0\0\0\0\0",
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: "\0\0\0\0\0\0\0\0",
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: "\0\0\0\0",
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: "@\0",
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: "8\0",
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: "\x03\0",
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: "@\0",
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: "\0\0",
                                    Arg1: /* constructor */{
                                      tag: "::",
                                      Arg0: "\0\0",
                                      Arg1: "[]"
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }));

function elfphdr(ty, off, sz, align) {
  le(32, ty);
  le(32, 7);
  le(64, off);
  le(64, off + 4194304 | 0);
  le(64, off + 4194304 | 0);
  le(64, sz);
  le(64, sz);
  return le(64, align);
}

function elfgen(outf) {
  var entry = opos[0];
  var main = addsym("main");
  var gmain = Caml_array.caml_array_get(globs, main);
  out(1217084452);
  out(-1921768440);
  out(18616);
  le(64, gmain[/* loc */0]);
  Caml_array.caml_array_set(globs, main, /* record */[
        /* loc */opos[0] - 8 | 0,
        /* va */gmain[/* va */1]
      ]);
  out(65488);
  out(35271);
  load(0, 60);
  out(3845);
  var off = 232 + gpos[0] | 0;
  var itr = function (f) {
    return symitr((function (i, s) {
                  var g = Caml_array.caml_array_get(globs, i);
                  if (g[/* va */1] < 0 && g[/* loc */0] !== 0) {
                    return Curry._3(f, s, s.length, g[/* loc */0]);
                  } else {
                    return 0;
                  }
                }));
  };
  var va = function (x) {
    return (x + off | 0) + 4194304 | 0;
  };
  var patchloc = function (i, param) {
    var g = Caml_array.caml_array_get(globs, i);
    if (g[/* va */1] >= 0 && g[/* va */1] < 4194304) {
      return patch(false, g[/* loc */0], va(g[/* va */1]));
    } else if (g[/* va */1] >= 0) {
      return patch(false, g[/* loc */0], g[/* va */1]);
    } else {
      return 0;
    }
  };
  symitr(patchloc);
  var strtab = opos[0];
  opos[0] = opos[0] + 1 | 0;
  $$String.blit("/lib64/ld-linux-x86-64.so.2\0libc.so.6", 0, obuf, opos[0], 37);
  opos[0] = (opos[0] + 37 | 0) + 1 | 0;
  itr((function (s, sl, param) {
          $$String.blit(s, 0, obuf, opos[0], sl);
          opos[0] = (opos[0] + sl | 0) + 1 | 0;
          return /* () */0;
        }));
  opos[0] = opos[0] + 7 & -8;
  var symtab = opos[0];
  var n = /* record */[/* contents */39];
  opos[0] = opos[0] + 24 | 0;
  itr((function (param, sl, param$1) {
          le(32, n[0]);
          le(32, 16);
          le(64, 0);
          le(64, 0);
          n[0] = (n[0] + sl | 0) + 1 | 0;
          return /* () */0;
        }));
  var rel = opos[0];
  var n$1 = /* record */[/* contents */1];
  itr((function (param, param$1, l) {
          var genrel = function (_l) {
            while(true) {
              var l = _l;
              if (l !== 0) {
                le(64, va(l));
                le(64, 1 + (n$1[0] << 32) | 0);
                le(64, 0);
                _l = get32(l);
                continue ;
              } else {
                return 0;
              }
            };
          };
          genrel(l);
          n$1[0] = n$1[0] + 1 | 0;
          return /* () */0;
        }));
  var hash = opos[0];
  var n$2 = ((rel - symtab | 0) / 24 | 0) - 1 | 0;
  le(32, 1);
  le(32, n$2 + 1 | 0);
  le(32, n$2 > 0 ? 1 : 0);
  for(var i = 1; i <= n$2; ++i){
    le(32, i);
  }
  le(32, 0);
  var dyn = opos[0];
  List.iter((function (param) {
          return le(64, param);
        }), /* constructor */{
        tag: "::",
        Arg0: 1,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 29,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 4,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: va(hash),
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 5,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: va(strtab),
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 6,
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: va(symtab),
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: 7,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: va(rel),
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: 8,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: hash - rel | 0,
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: 9,
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: 24,
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: 10,
                                    Arg1: /* constructor */{
                                      tag: "::",
                                      Arg0: symtab - strtab | 0,
                                      Arg1: /* constructor */{
                                        tag: "::",
                                        Arg0: 11,
                                        Arg1: /* constructor */{
                                          tag: "::",
                                          Arg0: 24,
                                          Arg1: /* constructor */{
                                            tag: "::",
                                            Arg0: 0,
                                            Arg1: "[]"
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      });
  var tend = opos[0];
  Bytes.blit(obuf, 0, obuf, off, tend);
  Bytes.blit(glo, 0, obuf, 232, gpos[0]);
  Bytes.blit(elfhdr, 0, obuf, 0, 64);
  opos[0] = 64;
  elfphdr(3, (strtab + 1 | 0) + off | 0, 28, 1);
  elfphdr(1, 0, tend + off | 0, 2097152);
  elfphdr(2, dyn + off | 0, tend - dyn | 0, 8);
  if (opos[0] !== 232) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "qcc.ml",
            698,
            2
          ]
        ];
  }
  patch(false, 24, va(entry));
  return Pervasives.output_bytes(outf, Bytes.sub(obuf, 0, tend + off | 0));
}

function main(param) {
  var ppsym = function (param) {
    switch (/* XXX */param.tag) {
      case "Op" :
          return Curry._1(Printf.printf(/* constructor */{
                          tag: "Format",
                          Arg0: /* constructor */{
                            tag: "String_literal",
                            Arg0: "Operator '",
                            Arg1: /* constructor */{
                              tag: "String",
                              Arg0: "No_padding",
                              Arg1: /* constructor */{
                                tag: "String_literal",
                                Arg0: "'\n",
                                Arg1: "End_of_format"
                              }
                            }
                          },
                          Arg1: "Operator '%s'\n"
                        }), param.Arg0);
      case "ILit" :
          return Curry._1(Printf.printf(/* constructor */{
                          tag: "Format",
                          Arg0: /* constructor */{
                            tag: "String_literal",
                            Arg0: "Int literal ",
                            Arg1: /* constructor */{
                              tag: "Int",
                              Arg0: "Int_d",
                              Arg1: "No_padding",
                              Arg2: "No_precision",
                              Arg3: /* constructor */{
                                tag: "Char_literal",
                                Arg0: /* "\n" */10,
                                Arg1: "End_of_format"
                              }
                            }
                          },
                          Arg1: "Int literal %d\n"
                        }), param.Arg0);
      case "SLit" :
          return Curry._1(Printf.printf(/* constructor */{
                          tag: "Format",
                          Arg0: /* constructor */{
                            tag: "String_literal",
                            Arg0: "Str literal ",
                            Arg1: /* constructor */{
                              tag: "Caml_string",
                              Arg0: "No_padding",
                              Arg1: /* constructor */{
                                tag: "Char_literal",
                                Arg0: /* "\n" */10,
                                Arg1: "End_of_format"
                              }
                            }
                          },
                          Arg1: "Str literal %S\n"
                        }), param.Arg1);
      case "Sym" :
          var i = param.Arg0;
          return Curry._2(Printf.printf(/* constructor */{
                          tag: "Format",
                          Arg0: /* constructor */{
                            tag: "String_literal",
                            Arg0: "Symbol '",
                            Arg1: /* constructor */{
                              tag: "String",
                              Arg0: "No_padding",
                              Arg1: /* constructor */{
                                tag: "String_literal",
                                Arg0: "' (",
                                Arg1: /* constructor */{
                                  tag: "Int",
                                  Arg0: "Int_d",
                                  Arg1: "No_padding",
                                  Arg2: "No_precision",
                                  Arg3: /* constructor */{
                                    tag: "String_literal",
                                    Arg0: ")\n",
                                    Arg1: "End_of_format"
                                  }
                                }
                              }
                            }
                          },
                          Arg1: "Symbol '%s' (%d)\n"
                        }), symstr(i), i);
      
    }
  };
  var f = Sys.argv.length < 2 ? "-blk" : Caml_array.caml_array_get(Sys.argv, 1);
  switch (f) {
    case "-blk" :
        var partial_arg_000 = /* record */[/* contents */0];
        var partial_arg = /* tuple */[
          partial_arg_000,
          0
        ];
        var c = function (param) {
          return block(partial_arg, param);
        };
        var stk = "[]";
        opos[0] = 0;
        Curry._1(c, stk);
        return Pervasives.print_bytes(Bytes.sub(obuf, 0, opos[0]));
    case "-lex" :
        var _param = /* () */0;
        while(true) {
          var tok = Curry._1(next$1, /* () */0);
          if (/* XXX */tok.tag === "Op") {
            if (tok.Arg0 === "EOF!") {
              return Printf.printf(/* constructor */{
                          tag: "Format",
                          Arg0: /* constructor */{
                            tag: "String_literal",
                            Arg0: "End of input stream\n",
                            Arg1: "End_of_format"
                          },
                          Arg1: "End of input stream\n"
                        });
            } else {
              ppsym(tok);
              _param = /* () */0;
              continue ;
            }
          } else {
            ppsym(tok);
            _param = /* () */0;
            continue ;
          }
        };
    default:
      var oc = Pervasives.open_out("a.out");
      inch[0] = Pervasives.open_in_bin(f);
      top(/* () */0);
      elfgen(oc);
      Caml_io.caml_ml_flush(oc);
      return Caml_external_polyfill.resolve("caml_ml_close_channel")(oc);
  }
}

main(/* () */0);

var base = 4194304;

var textoff = 232;

exports.dbg = dbg;
exports.inch = inch;
exports.bufferize = bufferize;
exports.getch = getch;
exports.ungetch = ungetch;
exports.peekch = peekch;
exports.addsym = addsym;
exports.symstr = symstr;
exports.symitr = symitr;
exports.glo = glo;
exports.gpos = gpos;
exports.base = base;
exports.textoff = textoff;
exports.next = next$1;
exports.unnext = unnext;
exports.nextis = nextis;
exports.obuf = obuf;
exports.opos = opos;
exports.out = out;
exports.le = le;
exports.get32 = get32;
exports.patch = patch;
exports.load = load;
exports.cmp = cmp;
exports.test = test;
exports.align = align;
exports.push = push;
exports.pop = pop;
exports.lval = lval;
exports.patchlval = patchlval;
exports.read = read;
exports.globs = globs;
exports.lvls = lvls;
exports.inss = inss;
exports.tokint = tokint;
exports.tokchar = tokchar;
exports.tokret = tokret;
exports.tokif = tokif;
exports.tokelse = tokelse;
exports.tokwhile = tokwhile;
exports.tokfor = tokfor;
exports.tokbreak = tokbreak;
exports.binary = binary;
exports.unary = unary;
exports.postfix = postfix;
exports.expr = expr;
exports.decl = decl;
exports.retl = retl;
exports.stmt = stmt;
exports.block = block;
exports.top = top;
exports.elfhdr = elfhdr;
exports.elfphdr = elfphdr;
exports.elfgen = elfgen;
exports.main = main;
/* match Not a pure module */
