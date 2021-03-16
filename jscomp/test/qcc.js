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
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_external_polyfill = require("../../lib/js/caml_external_polyfill.js");

var dbg = {
  contents: true
};

var inch = {
  contents: Pervasives.stdin
};

function bufferize(f) {
  var buf = {
    contents: undefined
  };
  return [
          (function (param) {
              var x = buf.contents;
              if (x !== undefined) {
                buf.contents = undefined;
                return Caml_option.valFromOption(x);
              } else {
                return Curry._1(f, undefined);
              }
            }),
          (function (x) {
              if (buf.contents !== undefined) {
                throw {
                      RE_EXN_ID: "Assert_failure",
                      _1: [
                        "qcc.ml",
                        17,
                        4
                      ],
                      Error: new Error()
                    };
              }
              buf.contents = Caml_option.some(x);
              
            })
        ];
}

var match = bufferize(function (param) {
      return Caml_external_polyfill.resolve("caml_ml_input_char")(inch.contents);
    });

var ungetch = match[1];

var getch = match[0];

function peekch(param) {
  var ch = Curry._1(getch, undefined);
  Curry._1(ungetch, ch);
  return ch;
}

var symtab = Caml_array.make(100, "");

var syms = {
  contents: 0
};

function find(s, _n) {
  while(true) {
    var n = _n;
    if (n >= syms.contents) {
      syms.contents = syms.contents + 1 | 0;
      return n;
    }
    if (Caml_array.get(symtab, n) === s) {
      return n;
    }
    _n = n + 1 | 0;
    continue ;
  };
}

function addsym(s) {
  var sid = find(s, 0);
  Caml_array.set(symtab, sid, s);
  return sid;
}

function symstr(n) {
  if (n >= syms.contents) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "qcc.ml",
            40,
            4
          ],
          Error: new Error()
        };
  }
  return Caml_array.get(symtab, n);
}

function symitr(f) {
  for(var i = 0 ,i_finish = syms.contents; i < i_finish; ++i){
    Curry._2(f, i, Caml_array.get(symtab, i));
  }
  
}

var glo = Bytes.make(4096, /* '\000' */0);

var gpos = {
  contents: 0
};

var s = Caml_bytes.caml_create_bytes(100);

function getq(param) {
  var c = Curry._1(getch, undefined);
  if (c !== 92 || peekch(undefined) !== /* 'n' */110) {
    return c;
  } else {
    Curry._1(getch, undefined);
    return /* '\n' */10;
  }
}

function isid(param) {
  if (param > 96 || param < 91) {
    return !(param > 122 || param < 65);
  } else {
    return param === 95;
  }
}

function skip(_param) {
  while(true) {
    var ch = Curry._1(getch, undefined);
    if (ch >= 14) {
      if (ch !== 32) {
        if (ch !== 47 || peekch(undefined) !== /* '*' */42) {
          return ch;
        } else {
          var _param$1 = (Curry._1(getch, undefined), undefined);
          while(true) {
            var match = Curry._1(getch, undefined);
            if (match !== 42) {
              _param$1 = undefined;
              continue ;
            }
            if (peekch(undefined) === /* '/' */47) {
              return skip((Curry._1(getch, undefined), undefined));
            }
            _param$1 = undefined;
            continue ;
          };
        }
      }
      _param = undefined;
      continue ;
    }
    if (ch >= 11) {
      if (ch < 13) {
        return ch;
      }
      _param = undefined;
      continue ;
    }
    if (ch < 9) {
      return ch;
    }
    _param = undefined;
    continue ;
  };
}

function next(param) {
  var c;
  try {
    c = skip(undefined);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "End_of_file") {
      c = undefined;
    } else {
      throw exn;
    }
  }
  if (c === undefined) {
    return {
            TAG: /* Op */0,
            _0: "EOF!"
          };
  }
  if (c === 34) {
    var b = gpos.contents;
    var _e = gpos.contents;
    while(true) {
      var e = _e;
      var match = peekch(undefined);
      if (match !== 34) {
        Caml_bytes.set(glo, e, getq(undefined));
        _e = e + 1 | 0;
        continue ;
      }
      Curry._1(getch, undefined);
      gpos.contents = e + 8 & -8;
      return {
              TAG: /* SLit */2,
              _0: (b + 232 | 0) + 4194304 | 0,
              _1: Bytes.to_string(Bytes.sub(glo, b, e - b | 0))
            };
    };
  }
  if (c >= 48) {
    if (c < 58) {
      var _n = c - 48 | 0;
      while(true) {
        var n = _n;
        var match$1 = peekch(undefined);
        if (match$1 > 57 || match$1 < 48) {
          return {
                  TAG: /* ILit */1,
                  _0: n
                };
        }
        _n = (Math.imul(10, n) + Curry._1(getch, undefined) | 0) - 48 | 0;
        continue ;
      };
    }
    
  } else if (c === 39) {
    var ch = getq(undefined);
    var qt = Curry._1(getch, undefined);
    if (qt !== /* '\'' */39) {
      throw {
            RE_EXN_ID: "Failure",
            _1: "syntax error",
            Error: new Error()
          };
    }
    return {
            TAG: /* ILit */1,
            _0: ch
          };
  }
  if (isid(c)) {
    var _n$1 = 0;
    var _ch = c;
    while(true) {
      var ch$1 = _ch;
      var n$1 = _n$1;
      Caml_bytes.set(s, n$1, ch$1);
      if (!isid(peekch(undefined))) {
        return {
                TAG: /* Sym */3,
                _0: addsym(Bytes.to_string(Bytes.sub(s, 0, n$1 + 1 | 0)))
              };
      }
      _ch = Curry._1(getch, undefined);
      _n$1 = n$1 + 1 | 0;
      continue ;
    };
  } else {
    var _param = {
      hd: "++",
      tl: {
        hd: "--",
        tl: {
          hd: "&&",
          tl: {
            hd: "||",
            tl: {
              hd: "==",
              tl: {
                hd: "<=",
                tl: {
                  hd: ">=",
                  tl: {
                    hd: "!=",
                    tl: {
                      hd: ">>",
                      tl: {
                        hd: "<<",
                        tl: /* [] */0
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
      if (!param$1) {
        return {
                TAG: /* Op */0,
                _0: Caml_string.make(1, c)
              };
      }
      var lop = param$1.hd;
      if (Caml_string.get(lop, 0) === c && Caml_string.get(lop, 1) === peekch(undefined)) {
        Curry._1(getch, undefined);
        return {
                TAG: /* Op */0,
                _0: lop
              };
      }
      _param = param$1.tl;
      continue ;
    };
  }
}

var match$1 = bufferize(next);

var unnext = match$1[1];

var next$1 = match$1[0];

function nextis(t) {
  var nt = Curry._1(next$1, undefined);
  Curry._1(unnext, nt);
  return Caml_obj.caml_equal(t, nt);
}

var obuf = Bytes.make(1048576, /* '\000' */0);

var opos = {
  contents: 0
};

function out(x) {
  if (x !== 0) {
    out(x / 256 | 0);
    Caml_bytes.set(obuf, opos.contents, Char.chr(x & 255));
    opos.contents = opos.contents + 1 | 0;
    return ;
  }
  
}

function le(n, x) {
  for(var i = 0 ,i_finish = n / 8 | 0; i < i_finish; ++i){
    var $$byte = (x >>> (i << 3)) & 255;
    Caml_bytes.set(obuf, opos.contents, Char.chr($$byte));
    opos.contents = opos.contents + 1 | 0;
  }
  
}

function get32(l) {
  return ((Caml_bytes.get(obuf, l) + (Caml_bytes.get(obuf, l + 1 | 0) << 8) | 0) + (Caml_bytes.get(obuf, l + 2 | 0) << 16) | 0) + (Caml_bytes.get(obuf, l + 3 | 0) << 24) | 0;
}

function patch(rel, loc, n) {
  if (n >= 0) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "qcc.ml",
            157,
            2
          ],
          Error: new Error()
        };
  }
  if (loc === 0) {
    return ;
  }
  var i = opos.contents;
  var loc$p = get32(loc);
  var x = rel ? n - (loc + 4 | 0) | 0 : n;
  if (dbg.contents) {
    Curry._3(Printf.eprintf(/* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "patching at ",
                _1: {
                  TAG: /* Int */4,
                  _0: /* Int_d */0,
                  _1: /* No_padding */0,
                  _2: /* No_precision */0,
                  _3: {
                    TAG: /* String_literal */11,
                    _0: " to ",
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_d */0,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* String_literal */11,
                        _0: " (n=",
                        _1: {
                          TAG: /* Int */4,
                          _0: /* Int_d */0,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: {
                            TAG: /* String_literal */11,
                            _0: ")\n",
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    }
                  }
                }
              },
              _1: "patching at %d to %d (n=%d)\n"
            }), loc, x, n);
  }
  opos.contents = loc;
  le(32, x);
  patch(rel, loc$p, n);
  opos.contents = i;
  
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
  var loc = opos.contents;
  le(32, l);
  return loc;
}

var align = {
  contents: 0
};

function push(r) {
  align.contents = align.contents + 1 | 0;
  if (r < 8) {
    return out(80 + r | 0);
  } else {
    return out((16720 + r | 0) - 8 | 0);
  }
}

function pop(r) {
  align.contents = align.contents - 1 | 0;
  if (r < 8) {
    return out(88 + r | 0);
  } else {
    return out((16728 + r | 0) - 8 | 0);
  }
}

var lval = {
  contents: [
    {
      TAG: /* Mov */0,
      _0: 0
    },
    /* Int */0
  ]
};

function patchlval(param) {
  var n = lval.contents[0];
  if (n.TAG === /* Mov */0) {
    return Caml_bytes.set(obuf, opos.contents - n._0 | 0, /* '\141' */141);
  } else {
    opos.contents = opos.contents - n._0 | 0;
    return ;
  }
}

function read(param) {
  if (param) {
    out(4722614);
    le(8, 0);
    lval.contents = [
      {
        TAG: /* Del */1,
        _0: 4
      },
      /* Chr */1
    ];
  } else {
    out(18571);
    le(8, 0);
    lval.contents = [
      {
        TAG: /* Del */1,
        _0: 3
      },
      /* Int */0
    ];
  }
  
}

var globs = Caml_array.make(100, {
      loc: 0,
      va: -1
    });

var lvls = {
  hd: [
    "*",
    0
  ],
  tl: {
    hd: [
      "/",
      0
    ],
    tl: {
      hd: [
        "%",
        0
      ],
      tl: {
        hd: [
          "+",
          1
        ],
        tl: {
          hd: [
            "-",
            1
          ],
          tl: {
            hd: [
              "<<",
              2
            ],
            tl: {
              hd: [
                ">>",
                2
              ],
              tl: {
                hd: [
                  "<",
                  3
                ],
                tl: {
                  hd: [
                    "<=",
                    3
                  ],
                  tl: {
                    hd: [
                      ">",
                      3
                    ],
                    tl: {
                      hd: [
                        ">=",
                        3
                      ],
                      tl: {
                        hd: [
                          "==",
                          4
                        ],
                        tl: {
                          hd: [
                            "!=",
                            4
                          ],
                          tl: {
                            hd: [
                              "&",
                              5
                            ],
                            tl: {
                              hd: [
                                "^",
                                6
                              ],
                              tl: {
                                hd: [
                                  "|",
                                  7
                                ],
                                tl: {
                                  hd: [
                                    "&&",
                                    8
                                  ],
                                  tl: {
                                    hd: [
                                      "||",
                                      9
                                    ],
                                    tl: /* [] */0
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

var inss = {
  hd: [
    "*",
    {
      TAG: /* Bin */0,
      _0: {
        hd: 1208987585,
        tl: /* [] */0
      }
    }
  ],
  tl: {
    hd: [
      "/",
      {
        TAG: /* Bin */0,
        _0: {
          hd: 18577,
          tl: {
            hd: 18585,
            tl: {
              hd: 4782073,
              tl: /* [] */0
            }
          }
        }
      }
    ],
    tl: {
      hd: [
        "%",
        {
          TAG: /* Bin */0,
          _0: {
            hd: 18577,
            tl: {
              hd: 18585,
              tl: {
                hd: 4782073,
                tl: {
                  hd: 18578,
                  tl: /* [] */0
                }
              }
            }
          }
        }
      ],
      tl: {
        hd: [
          "+",
          {
            TAG: /* Bin */0,
            _0: {
              hd: 4719048,
              tl: /* [] */0
            }
          }
        ],
        tl: {
          hd: [
            "-",
            {
              TAG: /* Bin */0,
              _0: {
                hd: 18577,
                tl: {
                  hd: 4729288,
                  tl: /* [] */0
                }
              }
            }
          ],
          tl: {
            hd: [
              "<<",
              {
                TAG: /* Bin */0,
                _0: {
                  hd: 18577,
                  tl: {
                    hd: 4772832,
                    tl: /* [] */0
                  }
                }
              }
            ],
            tl: {
              hd: [
                ">>",
                {
                  TAG: /* Bin */0,
                  _0: {
                    hd: 18577,
                    tl: {
                      hd: 4772856,
                      tl: /* [] */0
                    }
                  }
                }
              ],
              tl: {
                hd: [
                  "<",
                  {
                    TAG: /* Cmp */1,
                    _0: 10
                  }
                ],
                tl: {
                  hd: [
                    "<=",
                    {
                      TAG: /* Cmp */1,
                      _0: 12
                    }
                  ],
                  tl: {
                    hd: [
                      ">",
                      {
                        TAG: /* Cmp */1,
                        _0: 13
                      }
                    ],
                    tl: {
                      hd: [
                        ">=",
                        {
                          TAG: /* Cmp */1,
                          _0: 11
                        }
                      ],
                      tl: {
                        hd: [
                          "==",
                          {
                            TAG: /* Cmp */1,
                            _0: 2
                          }
                        ],
                        tl: {
                          hd: [
                            "!=",
                            {
                              TAG: /* Cmp */1,
                              _0: 3
                            }
                          ],
                          tl: {
                            hd: [
                              "&",
                              {
                                TAG: /* Bin */0,
                                _0: {
                                  hd: 4727240,
                                  tl: /* [] */0
                                }
                              }
                            ],
                            tl: {
                              hd: [
                                "^",
                                {
                                  TAG: /* Bin */0,
                                  _0: {
                                    hd: 4731336,
                                    tl: /* [] */0
                                  }
                                }
                              ],
                              tl: {
                                hd: [
                                  "|",
                                  {
                                    TAG: /* Bin */0,
                                    _0: {
                                      hd: 4721096,
                                      tl: /* [] */0
                                    }
                                  }
                                ],
                                tl: /* [] */0
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

var tokint = {
  TAG: /* Sym */3,
  _0: addsym("int")
};

var tokchar = {
  TAG: /* Sym */3,
  _0: addsym("char")
};

var tokret = {
  TAG: /* Sym */3,
  _0: addsym("return")
};

var tokif = {
  TAG: /* Sym */3,
  _0: addsym("if")
};

var tokelse = {
  TAG: /* Sym */3,
  _0: addsym("else")
};

var tokwhile = {
  TAG: /* Sym */3,
  _0: addsym("while")
};

var tokfor = {
  TAG: /* Sym */3,
  _0: addsym("for")
};

var tokbreak = {
  TAG: /* Sym */3,
  _0: addsym("break")
};

function binary(stk, lvl) {
  if (lvl === -1) {
    return unary(stk);
  }
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
      var o = Curry._1(next$1, undefined);
      if (o.TAG === /* Op */0) {
        if (lvlof(o._0) === lvl) {
          var loc$p = test(lvl - 8 | 0, loc);
          binary(stk, lvl - 1 | 0);
          _loc = loc$p;
          continue ;
        }
        Curry._1(unnext, o);
        return loc;
      }
      Curry._1(unnext, o);
      return loc;
    };
  };
  binary(stk, lvl - 1 | 0);
  if (lvl < 8) {
    var _param;
    while(true) {
      var o = Curry._1(next$1, undefined);
      if (o.TAG !== /* Op */0) {
        return Curry._1(unnext, o);
      }
      var o$1 = o._0;
      if (lvlof(o$1) !== lvl) {
        return Curry._1(unnext, o);
      }
      push(0);
      binary(stk, lvl - 1 | 0);
      pop(1);
      var ops = List.assoc(o$1, inss);
      if (ops.TAG === /* Bin */0) {
        List.iter(out, ops._0);
      } else {
        out(4733377);
        cmp(ops._0);
      }
      _param = undefined;
      continue ;
    };
  }
  var loc = foldtst(0);
  return patch(true, loc, opos.contents);
}

function unary(stk) {
  var i = Curry._1(next$1, undefined);
  switch (i.TAG | 0) {
    case /* Op */0 :
        var o = i._0;
        switch (o) {
          case "&" :
              unary(stk);
              return patchlval(undefined);
          case "(" :
              expr(stk);
              Curry._1(next$1, undefined);
              return postfix(stk);
          case "*" :
              Curry._1(next$1, undefined);
              var t = Curry._1(next$1, undefined);
              var match;
              if (Caml_obj.caml_equal(t, tokint)) {
                match = Caml_obj.caml_equal(Curry._1(next$1, undefined), {
                      TAG: /* Op */0,
                      _0: "*"
                    }) ? [
                    /* Int */0,
                    1
                  ] : [
                    /* Int */0,
                    5
                  ];
              } else if (Caml_obj.caml_equal(t, tokchar)) {
                match = [
                  /* Chr */1,
                  2
                ];
              } else {
                throw {
                      RE_EXN_ID: "Failure",
                      _1: "[cast] expected",
                      Error: new Error()
                    };
              }
              for(var k = 1 ,k_finish = match[1]; k <= k_finish; ++k){
                Curry._1(next$1, undefined);
              }
              unary(stk);
              return read(match[0]);
          default:
            var unops = {
              hd: [
                "+",
                0
              ],
              tl: {
                hd: [
                  "-",
                  4782040
                ],
                tl: {
                  hd: [
                    "~",
                    4782032
                  ],
                  tl: {
                    hd: [
                      "!",
                      4752832
                    ],
                    tl: /* [] */0
                  }
                }
              }
            };
            unary(stk);
            if (!List.mem_assoc(o, unops)) {
              var s = Curry._1(Printf.sprintf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "unknown operator ",
                          _1: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: /* End_of_format */0
                          }
                        },
                        _1: "unknown operator %s"
                      }), o);
              throw {
                    RE_EXN_ID: "Failure",
                    _1: s,
                    Error: new Error()
                  };
            }
            out(List.assoc(o, unops));
            if (o === "!") {
              return cmp(2);
            } else {
              return ;
            }
        }
    case /* ILit */1 :
        return load(0, i._0);
    case /* SLit */2 :
        out(18616);
        return le(64, i._0);
    case /* Sym */3 :
        var i$1 = i._0;
        if (List.mem_assoc(i$1, stk)) {
          var l = List.assoc(i$1, stk);
          if (l <= -256) {
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "qcc.ml",
                    295,
                    6
                  ],
                  Error: new Error()
                };
          }
          out(4754245);
          out(l & 255);
          lval.contents = [
            {
              TAG: /* Mov */0,
              _0: 3
            },
            /* Int */0
          ];
        } else {
          out(18616);
          var g = Caml_array.get(globs, i$1);
          var loc = opos.contents;
          le(64, g.loc);
          Caml_array.set(globs, i$1, {
                loc: loc,
                va: g.va
              });
          read(/* Int */0);
        }
        return postfix(stk);
    
  }
}

function postfix(stk) {
  var op = Curry._1(next$1, undefined);
  if (op.TAG !== /* Op */0) {
    return Curry._1(unnext, op);
  }
  var op$1 = op._0;
  switch (op$1) {
    case "(" :
        var emitargs = function (_l, _rl) {
          while(true) {
            var rl = _rl;
            var l = _l;
            if (nextis({
                    TAG: /* Op */0,
                    _0: ")"
                  })) {
              Curry._1(next$1, undefined);
              return List.iter(pop, l);
            }
            expr(stk);
            push(0);
            if (nextis({
                    TAG: /* Op */0,
                    _0: ","
                  })) {
              Curry._1(next$1, undefined);
            }
            _rl = List.tl(rl);
            _l = {
              hd: List.hd(rl),
              tl: l
            };
            continue ;
          };
        };
        patchlval(undefined);
        push(0);
        emitargs(/* [] */0, {
              hd: 7,
              tl: {
                hd: 6,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 1,
                    tl: {
                      hd: 8,
                      tl: {
                        hd: 9,
                        tl: /* [] */0
                      }
                    }
                  }
                }
              }
            });
        pop(0);
        if (align.contents % 2 !== 0) {
          out(1216605192);
        }
        out(65488);
        if (align.contents % 2 !== 0) {
          return out(1216594952);
        } else {
          return ;
        }
    case "++" :
    case "--" :
        break;
    default:
      return Curry._1(unnext, op);
  }
  patchlval(undefined);
  out(4753857);
  read(lval.contents[1]);
  return out(List.assoc([
                  op$1,
                  lval.contents[1]
                ], {
                  hd: [
                    [
                      "++",
                      /* Int */0
                    ],
                    4783873
                  ],
                  tl: {
                    hd: [
                      [
                        "--",
                        /* Int */0
                      ],
                      4783881
                    ],
                    tl: {
                      hd: [
                        [
                          "++",
                          /* Chr */1
                        ],
                        65025
                      ],
                      tl: {
                        hd: [
                          [
                            "--",
                            /* Chr */1
                          ],
                          65033
                        ],
                        tl: /* [] */0
                      }
                    }
                  }
                }));
}

function expr(stk) {
  binary(stk, 10);
  var _param;
  while(true) {
    var t = Curry._1(next$1, undefined);
    if (t.TAG !== /* Op */0) {
      return Curry._1(unnext, t);
    }
    if (t._0 !== "=") {
      return Curry._1(unnext, t);
    }
    patchlval(undefined);
    var ty = lval.contents[1];
    push(0);
    expr(stk);
    pop(1);
    if (ty === /* Int */0) {
      out(4753665);
    } else {
      out(34817);
    }
    _param = undefined;
    continue ;
  };
}

function decl(g, _n, _stk) {
  while(true) {
    var stk = _stk;
    var n = _n;
    var t = Curry._1(next$1, undefined);
    if (Caml_obj.caml_equal(t, tokint)) {
      var top = stk ? stk.hd[1] : 0;
      var vars = (function(top){
      return function vars(_n, _stk) {
        while(true) {
          var stk = _stk;
          var n = _n;
          while(nextis({
                  TAG: /* Op */0,
                  _0: "*"
                })) {
            Curry._1(next$1, undefined);
          };
          if (nextis({
                  TAG: /* Op */0,
                  _0: ";"
                })) {
            return [
                    n,
                    stk
                  ];
          }
          var s = Curry._1(next$1, undefined);
          if (s.TAG === /* Sym */3) {
            var s$1 = s._0;
            var n$p = n + 1 | 0;
            var stk$p;
            if (g) {
              var glo = Caml_array.get(globs, s$1);
              if (glo.va >= 0) {
                throw {
                      RE_EXN_ID: "Failure",
                      _1: "symbol defined twice",
                      Error: new Error()
                    };
              }
              var va = (gpos.contents + 232 | 0) + 4194304 | 0;
              Caml_array.set(globs, s$1, {
                    loc: glo.loc,
                    va: va
                  });
              gpos.contents = gpos.contents + 8 | 0;
              stk$p = stk;
            } else {
              stk$p = {
                hd: [
                  s$1,
                  top - (n$p << 3) | 0
                ],
                tl: stk
              };
            }
            if (!nextis({
                    TAG: /* Op */0,
                    _0: ","
                  })) {
              return [
                      n$p,
                      stk$p
                    ];
            }
            Curry._1(next$1, undefined);
            _stk = stk$p;
            _n = n$p;
            continue ;
          }
          throw {
                RE_EXN_ID: "Failure",
                _1: "[var] expected in [decl]",
                Error: new Error()
              };
        };
      }
      }(top));
      var match = vars(0, stk);
      Curry._1(next$1, undefined);
      if (dbg.contents) {
        Curry._1(Printf.eprintf(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "end of decl (",
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_d */0,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* String_literal */11,
                        _0: " vars)\n",
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "end of decl (%d vars)\n"
                }), n);
      }
      _stk = match[1];
      _n = n + match[0] | 0;
      continue ;
    }
    Curry._1(unnext, t);
    if (!g && n !== 0) {
      if ((n << 3) >= 256) {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "qcc.ml",
                436,
                6
              ],
              Error: new Error()
            };
      }
      out(4752364);
      out((n << 3));
      align.contents = align.contents + n | 0;
    }
    if (dbg.contents && !g) {
      console.error("end of blk decls");
    }
    return [
            n,
            stk
          ];
  };
}

var retl = {
  contents: 0
};

function stmt(brk, stk) {
  var pexpr = function (stk) {
    Curry._1(next$1, undefined);
    expr(stk);
    Curry._1(next$1, undefined);
    
  };
  var t = Curry._1(next$1, undefined);
  if (Caml_obj.caml_equal(t, tokif)) {
    pexpr(stk);
    var loc = test(0, 0);
    stmt(brk, stk);
    var loc$1;
    if (nextis(tokelse)) {
      Curry._1(next$1, undefined);
      out(233);
      var l = opos.contents;
      le(32, 0);
      patch(true, loc, opos.contents);
      stmt(brk, stk);
      loc$1 = l;
    } else {
      loc$1 = loc;
    }
    return patch(true, loc$1, opos.contents);
  }
  if (Caml_obj.caml_equal(t, tokwhile) || Caml_obj.caml_equal(t, tokfor)) {
    var bl = {
      contents: 0
    };
    var ba = align.contents;
    var match;
    if (Caml_obj.caml_equal(t, tokwhile)) {
      var loc$2 = opos.contents;
      pexpr(stk);
      bl.contents = test(0, 0);
      match = [
        0,
        loc$2
      ];
    } else {
      Curry._1(next$1, undefined);
      if (!nextis({
              TAG: /* Op */0,
              _0: ";"
            })) {
        expr(stk);
      }
      Curry._1(next$1, undefined);
      var top = opos.contents;
      if (nextis({
              TAG: /* Op */0,
              _0: ";"
            })) {
        bl.contents = 0;
      } else {
        expr(stk);
        bl.contents = test(0, 0);
      }
      Curry._1(next$1, undefined);
      out(233);
      var bdy = opos.contents;
      le(32, 0);
      var itr = opos.contents;
      expr(stk);
      Curry._1(next$1, undefined);
      out(233);
      le(32, (top - opos.contents | 0) - 4 | 0);
      match = [
        bdy,
        itr
      ];
    }
    patch(true, match[0], opos.contents);
    stmt([
          bl,
          ba
        ], stk);
    out(233);
    le(32, (match[1] - opos.contents | 0) - 4 | 0);
    return patch(true, bl.contents, opos.contents);
  }
  if (Caml_obj.caml_equal(t, tokret)) {
    if (!nextis({
            TAG: /* Op */0,
            _0: ";"
          })) {
      expr(stk);
    }
    Curry._1(next$1, undefined);
    out(233);
    var loc$3 = opos.contents;
    le(32, retl.contents);
    retl.contents = loc$3;
    return ;
  }
  if (Caml_obj.caml_equal(t, tokbreak)) {
    Curry._1(next$1, undefined);
    var brkl = brk[0];
    var n = align.contents - brk[1] | 0;
    if (n < 0) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "qcc.ml",
              515,
              4
            ],
            Error: new Error()
          };
    }
    if (n !== 0) {
      out(4752324);
      out((n << 3));
    }
    out(233);
    var loc$4 = opos.contents;
    le(32, brkl.contents);
    brkl.contents = loc$4;
    return ;
  }
  if (t.TAG === /* Op */0) {
    switch (t._0) {
      case ";" :
          return ;
      case "{" :
          return block(brk, stk);
      default:
        
    }
  }
  Curry._1(unnext, t);
  expr(stk);
  Curry._1(next$1, undefined);
  
}

function block(brk, stk) {
  var match = decl(false, 0, stk);
  var stk$p = match[1];
  var n = match[0];
  while(!nextis({
          TAG: /* Op */0,
          _0: "}"
        })) {
    stmt(brk, stk$p);
  };
  Curry._1(next$1, undefined);
  if (n !== 0) {
    out(4752324);
    out((n << 3));
    align.contents = align.contents - n | 0;
    return ;
  }
  
}

function top(_param) {
  while(true) {
    if (nextis({
            TAG: /* Op */0,
            _0: "EOF!"
          })) {
      return ;
    }
    if (nextis(tokint)) {
      decl(true, 0, /* [] */0);
      _param = undefined;
      continue ;
    }
    var f = Curry._1(next$1, undefined);
    if (f.TAG === /* Sym */3) {
      var f$1 = f._0;
      var g = Caml_array.get(globs, f$1);
      if (g.va >= 0) {
        throw {
              RE_EXN_ID: "Failure",
              _1: "symbol defined twice",
              Error: new Error()
            };
      }
      Caml_array.set(globs, f$1, {
            loc: g.loc,
            va: opos.contents
          });
      var emitargs = function (_regs, _n, _stk) {
        while(true) {
          var stk = _stk;
          var n = _n;
          var regs = _regs;
          var i = Curry._1(next$1, undefined);
          switch (i.TAG | 0) {
            case /* Op */0 :
                if (i._0 === ")") {
                  return stk;
                }
                throw {
                      RE_EXN_ID: "Failure",
                      _1: "[var] or ) expected",
                      Error: new Error()
                    };
            case /* ILit */1 :
            case /* SLit */2 :
                throw {
                      RE_EXN_ID: "Failure",
                      _1: "[var] or ) expected",
                      Error: new Error()
                    };
            case /* Sym */3 :
                var r = List.hd(regs);
                push(r);
                if (nextis({
                        TAG: /* Op */0,
                        _0: ","
                      })) {
                  Curry._1(next$1, undefined);
                }
                var stk$p_0 = [
                  i._0,
                  ((-n | 0) << 3)
                ];
                var stk$p = {
                  hd: stk$p_0,
                  tl: stk
                };
                _stk = stk$p;
                _n = n + 1 | 0;
                _regs = List.tl(regs);
                continue ;
            
          }
        };
      };
      Curry._1(next$1, undefined);
      align.contents = 0;
      out(85);
      out(4753893);
      var stk = emitargs({
            hd: 7,
            tl: {
              hd: 6,
              tl: {
                hd: 2,
                tl: {
                  hd: 1,
                  tl: {
                    hd: 8,
                    tl: {
                      hd: 9,
                      tl: /* [] */0
                    }
                  }
                }
              }
            }
          }, 1, /* [] */0);
      while(Caml_obj.caml_notequal(Curry._1(next$1, undefined), {
              TAG: /* Op */0,
              _0: "{"
            })) {
        
      };
      retl.contents = 0;
      block([
            {
              contents: 0
            },
            0
          ], stk);
      patch(true, retl.contents, opos.contents);
      out(51651);
      if (dbg.contents) {
        Curry._1(Printf.eprintf(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "done with function ",
                    _1: {
                      TAG: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Char_literal */12,
                        _0: /* '\n' */10,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "done with function %s\n"
                }), symstr(f$1));
      }
      _param = undefined;
      continue ;
    }
    throw {
          RE_EXN_ID: "Failure",
          _1: "[decl] or [fun] expected",
          Error: new Error()
        };
  };
}

var elfhdr = Bytes.of_string($$String.concat("", {
          hd: "\x7fELF\x02\x01\x01\0",
          tl: {
            hd: "\0\0\0\0\0\0\0\0",
            tl: {
              hd: "\x02\0",
              tl: {
                hd: ">\0",
                tl: {
                  hd: "\x01\0\0\0",
                  tl: {
                    hd: "\0\0\0\0\0\0\0\0",
                    tl: {
                      hd: "@\0\0\0\0\0\0\0",
                      tl: {
                        hd: "\0\0\0\0\0\0\0\0",
                        tl: {
                          hd: "\0\0\0\0",
                          tl: {
                            hd: "@\0",
                            tl: {
                              hd: "8\0",
                              tl: {
                                hd: "\x03\0",
                                tl: {
                                  hd: "@\0",
                                  tl: {
                                    hd: "\0\0",
                                    tl: {
                                      hd: "\0\0",
                                      tl: /* [] */0
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
  var entry = opos.contents;
  var main = addsym("main");
  var gmain = Caml_array.get(globs, main);
  out(1217084452);
  out(-1921768440);
  out(18616);
  le(64, gmain.loc);
  Caml_array.set(globs, main, {
        loc: opos.contents - 8 | 0,
        va: gmain.va
      });
  out(65488);
  out(35271);
  load(0, 60);
  out(3845);
  var off = 232 + gpos.contents | 0;
  var itr = function (f) {
    return symitr(function (i, s) {
                var g = Caml_array.get(globs, i);
                if (g.va < 0 && g.loc !== 0) {
                  return Curry._3(f, s, s.length, g.loc);
                }
                
              });
  };
  var va = function (x) {
    return (x + off | 0) + 4194304 | 0;
  };
  var patchloc = function (i, param) {
    var g = Caml_array.get(globs, i);
    if (g.va >= 0 && g.va < 4194304) {
      return patch(false, g.loc, va(g.va));
    } else if (g.va >= 0) {
      return patch(false, g.loc, g.va);
    } else {
      return ;
    }
  };
  symitr(patchloc);
  var strtab = opos.contents;
  opos.contents = opos.contents + 1 | 0;
  $$String.blit("/lib64/ld-linux-x86-64.so.2\0libc.so.6", 0, obuf, opos.contents, 37);
  opos.contents = (opos.contents + 37 | 0) + 1 | 0;
  itr(function (s, sl, param) {
        $$String.blit(s, 0, obuf, opos.contents, sl);
        opos.contents = (opos.contents + sl | 0) + 1 | 0;
        
      });
  opos.contents = opos.contents + 7 & -8;
  var symtab = opos.contents;
  var n = {
    contents: 39
  };
  opos.contents = opos.contents + 24 | 0;
  itr(function (param, sl, param$1) {
        le(32, n.contents);
        le(32, 16);
        le(64, 0);
        le(64, 0);
        n.contents = (n.contents + sl | 0) + 1 | 0;
        
      });
  var rel = opos.contents;
  var n$1 = {
    contents: 1
  };
  itr(function (param, param$1, l) {
        var genrel = function (_l) {
          while(true) {
            var l = _l;
            if (l === 0) {
              return ;
            }
            le(64, va(l));
            le(64, 1 + (n$1.contents << 32) | 0);
            le(64, 0);
            _l = get32(l);
            continue ;
          };
        };
        genrel(l);
        n$1.contents = n$1.contents + 1 | 0;
        
      });
  var hash = opos.contents;
  var n$2 = ((rel - symtab | 0) / 24 | 0) - 1 | 0;
  le(32, 1);
  le(32, n$2 + 1 | 0);
  le(32, n$2 > 0 ? 1 : 0);
  for(var i = 1; i <= n$2; ++i){
    le(32, i);
  }
  le(32, 0);
  var dyn = opos.contents;
  List.iter((function (param) {
          return le(64, param);
        }), {
        hd: 1,
        tl: {
          hd: 29,
          tl: {
            hd: 4,
            tl: {
              hd: va(hash),
              tl: {
                hd: 5,
                tl: {
                  hd: va(strtab),
                  tl: {
                    hd: 6,
                    tl: {
                      hd: va(symtab),
                      tl: {
                        hd: 7,
                        tl: {
                          hd: va(rel),
                          tl: {
                            hd: 8,
                            tl: {
                              hd: hash - rel | 0,
                              tl: {
                                hd: 9,
                                tl: {
                                  hd: 24,
                                  tl: {
                                    hd: 10,
                                    tl: {
                                      hd: symtab - strtab | 0,
                                      tl: {
                                        hd: 11,
                                        tl: {
                                          hd: 24,
                                          tl: {
                                            hd: 0,
                                            tl: /* [] */0
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
  var tend = opos.contents;
  Bytes.blit(obuf, 0, obuf, off, tend);
  Bytes.blit(glo, 0, obuf, 232, gpos.contents);
  Bytes.blit(elfhdr, 0, obuf, 0, 64);
  opos.contents = 64;
  elfphdr(3, (strtab + 1 | 0) + off | 0, 28, 1);
  elfphdr(1, 0, tend + off | 0, 2097152);
  elfphdr(2, dyn + off | 0, tend - dyn | 0, 8);
  if (opos.contents !== 232) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "qcc.ml",
            698,
            2
          ],
          Error: new Error()
        };
  }
  patch(false, 24, va(entry));
  return Pervasives.output_bytes(outf, Bytes.sub(obuf, 0, tend + off | 0));
}

function main(param) {
  var ppsym = function (s) {
    switch (s.TAG | 0) {
      case /* Op */0 :
          return Curry._1(Printf.printf(/* Format */{
                          _0: {
                            TAG: /* String_literal */11,
                            _0: "Operator '",
                            _1: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* String_literal */11,
                                _0: "'\n",
                                _1: /* End_of_format */0
                              }
                            }
                          },
                          _1: "Operator '%s'\n"
                        }), s._0);
      case /* ILit */1 :
          return Curry._1(Printf.printf(/* Format */{
                          _0: {
                            TAG: /* String_literal */11,
                            _0: "Int literal ",
                            _1: {
                              TAG: /* Int */4,
                              _0: /* Int_d */0,
                              _1: /* No_padding */0,
                              _2: /* No_precision */0,
                              _3: {
                                TAG: /* Char_literal */12,
                                _0: /* '\n' */10,
                                _1: /* End_of_format */0
                              }
                            }
                          },
                          _1: "Int literal %d\n"
                        }), s._0);
      case /* SLit */2 :
          return Curry._1(Printf.printf(/* Format */{
                          _0: {
                            TAG: /* String_literal */11,
                            _0: "Str literal ",
                            _1: {
                              TAG: /* Caml_string */3,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* Char_literal */12,
                                _0: /* '\n' */10,
                                _1: /* End_of_format */0
                              }
                            }
                          },
                          _1: "Str literal %S\n"
                        }), s._1);
      case /* Sym */3 :
          var i = s._0;
          return Curry._2(Printf.printf(/* Format */{
                          _0: {
                            TAG: /* String_literal */11,
                            _0: "Symbol '",
                            _1: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* String_literal */11,
                                _0: "' (",
                                _1: {
                                  TAG: /* Int */4,
                                  _0: /* Int_d */0,
                                  _1: /* No_padding */0,
                                  _2: /* No_precision */0,
                                  _3: {
                                    TAG: /* String_literal */11,
                                    _0: ")\n",
                                    _1: /* End_of_format */0
                                  }
                                }
                              }
                            }
                          },
                          _1: "Symbol '%s' (%d)\n"
                        }), symstr(i), i);
      
    }
  };
  var f = Sys.argv.length < 2 ? "-blk" : Caml_array.get(Sys.argv, 1);
  switch (f) {
    case "-blk" :
        var partial_arg_0 = {
          contents: 0
        };
        var partial_arg = [
          partial_arg_0,
          0
        ];
        var c = function (param) {
          return block(partial_arg, param);
        };
        var stk = /* [] */0;
        opos.contents = 0;
        Curry._1(c, stk);
        return Pervasives.print_bytes(Bytes.sub(obuf, 0, opos.contents));
    case "-lex" :
        var _param;
        while(true) {
          var tok = Curry._1(next$1, undefined);
          if (tok.TAG === /* Op */0) {
            if (tok._0 === "EOF!") {
              return Printf.printf(/* Format */{
                          _0: {
                            TAG: /* String_literal */11,
                            _0: "End of input stream\n",
                            _1: /* End_of_format */0
                          },
                          _1: "End of input stream\n"
                        });
            }
            ppsym(tok);
            _param = undefined;
            continue ;
          }
          ppsym(tok);
          _param = undefined;
          continue ;
        };
    default:
      var oc = Pervasives.open_out("a.out");
      inch.contents = Pervasives.open_in_bin(f);
      top(undefined);
      elfgen(oc);
      Caml_io.caml_ml_flush(oc);
      return Caml_external_polyfill.resolve("caml_ml_close_channel")(oc);
  }
}

main(undefined);

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
