// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Bytes           = require("../stdlib/bytes");
var Pervasives      = require("../stdlib/pervasives");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Char            = require("../stdlib/char");
var Sys             = require("../stdlib/sys");
var Printf          = require("../stdlib/printf");
var Caml_primitive  = require("../runtime/caml_primitive");
var Caml_array      = require("../runtime/caml_array");
var $$String        = require("../stdlib/string");
var List            = require("../stdlib/list");

var dbg = [
  0,
  /* true */1
];

var inch = [
  0,
  Pervasives.stdin
];

function bufferize(f) {
  var buf = [
    0,
    /* None */0
  ];
  return [
          /* tuple */0,
          function () {
            var match = buf[1];
            if (match) {
              buf[1] = /* None */0;
              return match[1];
            }
            else {
              return f(/* () */0);
            }
          },
          function (x) {
            if (buf[1] !== /* None */0) {
              throw [
                    0,
                    Caml_exceptions.Assert_failure,
                    [
                      0,
                      "qcc.ml",
                      17,
                      4
                    ]
                  ];
            }
            buf[1] = [
              /* Some */0,
              x
            ];
            return /* () */0;
          }
        ];
}

var match = bufferize(function () {
      return Pervasives.input_char(inch[1]);
    });

var ungetch = match[2];

var getch = match[1];

function peekch() {
  var ch = getch(/* () */0);
  ungetch(ch);
  return ch;
}

var symtab = Caml_array.caml_make_vect(100, "");

var syms = [
  0,
  0
];

function find(s, _n) {
  while(true) {
    var n = _n;
    if (n >= syms[1]) {
      ++ syms[1];
      return n;
    }
    else {
      if (symtab[n] === s) {
        return n;
      }
      else {
        _n = n + 1;
      }
    }
  };
}

function match_001(s) {
  var sid = find(s, 0);
  symtab[sid] = s;
  return sid;
}

function match_002(n) {
  if (!(n < syms[1])) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "qcc.ml",
            40,
            4
          ]
        ];
  }
  return symtab[n];
}

function match_003(f) {
  for(var i = 0 ,i_finish = syms[1] - 1; i<= i_finish; ++i){
    f(i, symtab[i]);
  }
  return /* () */0;
}

var symitr = match_003;

var symstr = match_002;

var addsym = match_001;

var glo = Bytes.make(4096, /* "\000" */0);

var gpos = [
  0,
  0
];

var base = 4194304;

var textoff = 232;

var s = new Array(100);

function getq() {
  var c = getch(/* () */0);
  if (c !== 92) {
    return c;
  }
  else {
    if (peekch(/* () */0) === /* "n" */110) {
      getch(/* () */0);
      return /* "\n" */10;
    }
    else {
      return c;
    }
  }
}

function isid(param) {
  var switcher = -91 + param;
  if (5 < (switcher >>> 0)) {
    if (57 < (26 + switcher >>> 0)) {
      return /* false */0;
    }
    else {
      return /* true */1;
    }
  }
  else {
    if (switcher !== 4) {
      return /* false */0;
    }
    else {
      return /* true */1;
    }
  }
}

function skip(_param) {
  while(true) {
    var ch = getch(/* () */0);
    var exit = 0;
    if (ch >= 14) {
      if (ch !== 32) {
        if (ch !== 47) {
          return ch;
        }
        else {
          if (peekch(/* () */0) === /* "*" */42) {
            var _param$1 = getch(/* () */0);
            while(true) {
              var match = getch(/* () */0);
              if (match !== 42) {
                _param$1 = /* () */0;
              }
              else {
                if (peekch(/* () */0) === /* "/" */47) {
                  return skip(getch(/* () */0));
                }
                else {
                  _param$1 = /* () */0;
                }
              }
            };
          }
          else {
            return ch;
          }
        }
      }
      else {
        exit = 1;
      }
    }
    else {
      if (ch >= 11) {
        if (ch >= 13) {
          exit = 1;
        }
        else {
          return ch;
        }
      }
      else {
        if (ch >= 9) {
          exit = 1;
        }
        else {
          return ch;
        }
      }
    }
    if (exit === 1) {
      _param = /* () */0;
    }
    
  };
}

function next() {
  var match;
  try {
    match = [
      /* Some */0,
      skip(/* () */0)
    ];
  }
  catch (exn){
    if (exn === Caml_exceptions.End_of_file) {
      match = /* None */0;
    }
    else {
      throw exn;
    }
  }
  if (match) {
    var c = match[1];
    var exit = 0;
    if (c !== 34) {
      if (c >= 48) {
        if (c >= 58) {
          exit = 1;
        }
        else {
          var _n = c - 48;
          while(true) {
            var n = _n;
            var match$1 = peekch(/* () */0);
            if (9 < (-48 + match$1 >>> 0)) {
              return [
                      /* ILit */1,
                      n
                    ];
            }
            else {
              _n = 10 * n + getch(/* () */0) - 48;
            }
          };
        }
      }
      else {
        if (c !== 39) {
          exit = 1;
        }
        else {
          var ch = getq(/* () */0);
          var qt = getch(/* () */0);
          if (qt !== /* "'" */39) {
            return Pervasives.failwith("syntax error");
          }
          else {
            return [
                    /* ILit */1,
                    ch
                  ];
          }
        }
      }
    }
    else {
      var b = gpos[1];
      var _e = gpos[1];
      while(true) {
        var e = _e;
        var match$2 = peekch(/* () */0);
        if (match$2 !== 34) {
          glo[e] = getq(/* () */0);
          _e = e + 1;
        }
        else {
          getch(/* () */0);
          gpos[1] = e + 8 & -8;
          return [
                  /* SLit */2,
                  b + textoff + base,
                  Bytes.to_string(Bytes.sub(glo, b, e - b))
                ];
        }
      };
    }
    if (exit === 1) {
      if (isid(c)) {
        var _n$1 = 0;
        var _ch = c;
        while(true) {
          var ch$1 = _ch;
          var n$1 = _n$1;
          s[n$1] = ch$1;
          if (isid(peekch(/* () */0))) {
            _ch = getch(/* () */0);
            _n$1 = n$1 + 1;
          }
          else {
            return [
                    /* Sym */3,
                    addsym(Bytes.to_string(Bytes.sub(s, 0, n$1 + 1)))
                  ];
          }
        };
      }
      else {
        var ch$2 = c;
        var _param = [
          /* :: */0,
          "++",
          [
            /* :: */0,
            "--",
            [
              /* :: */0,
              "&&",
              [
                /* :: */0,
                "||",
                [
                  /* :: */0,
                  "==",
                  [
                    /* :: */0,
                    "<=",
                    [
                      /* :: */0,
                      ">=",
                      [
                        /* :: */0,
                        "!=",
                        [
                          /* :: */0,
                          ">>",
                          [
                            /* :: */0,
                            "<<",
                            /* [] */0
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ];
        while(true) {
          var param = _param;
          if (param) {
            var lop = param[1];
            if (lop.charCodeAt(0) === ch$2 && lop.charCodeAt(1) === peekch(/* () */0)) {
              getch(/* () */0);
              return [
                      /* Op */0,
                      lop
                    ];
            }
            else {
              _param = param[2];
            }
          }
          else {
            return [
                    /* Op */0,
                    $$String.make(1, ch$2)
                  ];
          }
        };
      }
    }
    
  }
  else {
    return [
            /* Op */0,
            "EOF!"
          ];
  }
}

var match$1 = bufferize(next);

var unnext = match$1[2];

var next$1 = match$1[1];

function nextis(t) {
  var nt = next$1(/* () */0);
  unnext(nt);
  return Caml_primitive.caml_equal(t, nt);
}

var obuf = Bytes.make(1048576, /* "\000" */0);

var opos = [
  0,
  0
];

function out(x) {
  if (x !== 0) {
    out(x / 256 | 0);
    obuf[opos[1]] = Char.chr(x & 255);
    return ++ opos[1];
  }
  else {
    return 0;
  }
}

function le(n, x) {
  for(var i = 0 ,i_finish = (n / 8 | 0) - 1; i<= i_finish; ++i){
    var $$byte = (x >>> i * 8) & 255;
    obuf[opos[1]] = Char.chr($$byte);
    ++ opos[1];
  }
  return /* () */0;
}

function get32(l) {
  return obuf[l] + obuf[l + 1] * 256 + obuf[l + 2] * 65536 + obuf[l + 3] * 16777216;
}

function patch(rel, loc, n) {
  if (!(n < 4294967296)) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "qcc.ml",
            157,
            2
          ]
        ];
  }
  if (loc !== 0) {
    var i = opos[1];
    var loc$prime = get32(loc);
    var x = rel ? n - (loc + 4) : n;
    if (dbg[1]) {
      Printf.eprintf([
              /* Format */0,
              [
                /* String_literal */11,
                "patching at ",
                [
                  /* Int */4,
                  /* Int_d */0,
                  /* No_padding */0,
                  /* No_precision */0,
                  [
                    /* String_literal */11,
                    " to ",
                    [
                      /* Int */4,
                      /* Int_d */0,
                      /* No_padding */0,
                      /* No_precision */0,
                      [
                        /* String_literal */11,
                        " (n=",
                        [
                          /* Int */4,
                          /* Int_d */0,
                          /* No_padding */0,
                          /* No_precision */0,
                          [
                            /* String_literal */11,
                            ")\n",
                            /* End_of_format */0
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ],
              "patching at %d to %d (n=%d)\n"
            ])(loc, x, n);
    }
    opos[1] = loc;
    le(32, x);
    patch(rel, loc$prime, n);
    opos[1] = i;
    return /* () */0;
  }
  else {
    return 0;
  }
}

function load(r, n) {
  out(184 + r);
  return le(32, n);
}

function cmp(n) {
  load(0, 0);
  return out(1020608 + (n << 8));
}

function test(n, l) {
  out(4752832);
  out(3972 + n);
  var loc = opos[1];
  le(32, l);
  return loc;
}

var align = [
  0,
  0
];

function push(r) {
  ++ align[1];
  if (r < 8) {
    return out(80 + r);
  }
  else {
    return out(16720 + r - 8);
  }
}

function pop(r) {
  -- align[1];
  if (r < 8) {
    return out(88 + r);
  }
  else {
    return out(16728 + r - 8);
  }
}

var lval = [
  0,
  [
    /* tuple */0,
    [
      /* Mov */0,
      0
    ],
    /* Int */0
  ]
];

function patchlval() {
  var match = lval[1][1];
  if (match[0]) {
    opos[1] -= match[1];
    return /* () */0;
  }
  else {
    obuf[opos[1] - match[1]] = /* "\141" */141;
    return /* () */0;
  }
}

function read(param) {
  if (param !== 0) {
    out(4722614);
    le(8, 0);
    lval[1] = [
      /* tuple */0,
      [
        /* Del */1,
        4
      ],
      /* Chr */1
    ];
    return /* () */0;
  }
  else {
    out(18571);
    le(8, 0);
    lval[1] = [
      /* tuple */0,
      [
        /* Del */1,
        3
      ],
      /* Int */0
    ];
    return /* () */0;
  }
}

var globs = Caml_array.caml_make_vect(100, [
      /* record */0,
      0,
      -1
    ]);

var lvls = [
  /* :: */0,
  [
    /* tuple */0,
    "*",
    0
  ],
  [
    /* :: */0,
    [
      /* tuple */0,
      "/",
      0
    ],
    [
      /* :: */0,
      [
        /* tuple */0,
        "%",
        0
      ],
      [
        /* :: */0,
        [
          /* tuple */0,
          "+",
          1
        ],
        [
          /* :: */0,
          [
            /* tuple */0,
            "-",
            1
          ],
          [
            /* :: */0,
            [
              /* tuple */0,
              "<<",
              2
            ],
            [
              /* :: */0,
              [
                /* tuple */0,
                ">>",
                2
              ],
              [
                /* :: */0,
                [
                  /* tuple */0,
                  "<",
                  3
                ],
                [
                  /* :: */0,
                  [
                    /* tuple */0,
                    "<=",
                    3
                  ],
                  [
                    /* :: */0,
                    [
                      /* tuple */0,
                      ">",
                      3
                    ],
                    [
                      /* :: */0,
                      [
                        /* tuple */0,
                        ">=",
                        3
                      ],
                      [
                        /* :: */0,
                        [
                          /* tuple */0,
                          "==",
                          4
                        ],
                        [
                          /* :: */0,
                          [
                            /* tuple */0,
                            "!=",
                            4
                          ],
                          [
                            /* :: */0,
                            [
                              /* tuple */0,
                              "&",
                              5
                            ],
                            [
                              /* :: */0,
                              [
                                /* tuple */0,
                                "^",
                                6
                              ],
                              [
                                /* :: */0,
                                [
                                  /* tuple */0,
                                  "|",
                                  7
                                ],
                                [
                                  /* :: */0,
                                  [
                                    /* tuple */0,
                                    "&&",
                                    8
                                  ],
                                  [
                                    /* :: */0,
                                    [
                                      /* tuple */0,
                                      "||",
                                      9
                                    ],
                                    /* [] */0
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
];

var inss = [
  /* :: */0,
  [
    /* tuple */0,
    "*",
    [
      /* Bin */0,
      [
        /* :: */0,
        1208987585,
        /* [] */0
      ]
    ]
  ],
  [
    /* :: */0,
    [
      /* tuple */0,
      "/",
      [
        /* Bin */0,
        [
          /* :: */0,
          18577,
          [
            /* :: */0,
            18585,
            [
              /* :: */0,
              4782073,
              /* [] */0
            ]
          ]
        ]
      ]
    ],
    [
      /* :: */0,
      [
        /* tuple */0,
        "%",
        [
          /* Bin */0,
          [
            /* :: */0,
            18577,
            [
              /* :: */0,
              18585,
              [
                /* :: */0,
                4782073,
                [
                  /* :: */0,
                  18578,
                  /* [] */0
                ]
              ]
            ]
          ]
        ]
      ],
      [
        /* :: */0,
        [
          /* tuple */0,
          "+",
          [
            /* Bin */0,
            [
              /* :: */0,
              4719048,
              /* [] */0
            ]
          ]
        ],
        [
          /* :: */0,
          [
            /* tuple */0,
            "-",
            [
              /* Bin */0,
              [
                /* :: */0,
                18577,
                [
                  /* :: */0,
                  4729288,
                  /* [] */0
                ]
              ]
            ]
          ],
          [
            /* :: */0,
            [
              /* tuple */0,
              "<<",
              [
                /* Bin */0,
                [
                  /* :: */0,
                  18577,
                  [
                    /* :: */0,
                    4772832,
                    /* [] */0
                  ]
                ]
              ]
            ],
            [
              /* :: */0,
              [
                /* tuple */0,
                ">>",
                [
                  /* Bin */0,
                  [
                    /* :: */0,
                    18577,
                    [
                      /* :: */0,
                      4772856,
                      /* [] */0
                    ]
                  ]
                ]
              ],
              [
                /* :: */0,
                [
                  /* tuple */0,
                  "<",
                  [
                    /* Cmp */1,
                    10
                  ]
                ],
                [
                  /* :: */0,
                  [
                    /* tuple */0,
                    "<=",
                    [
                      /* Cmp */1,
                      12
                    ]
                  ],
                  [
                    /* :: */0,
                    [
                      /* tuple */0,
                      ">",
                      [
                        /* Cmp */1,
                        13
                      ]
                    ],
                    [
                      /* :: */0,
                      [
                        /* tuple */0,
                        ">=",
                        [
                          /* Cmp */1,
                          11
                        ]
                      ],
                      [
                        /* :: */0,
                        [
                          /* tuple */0,
                          "==",
                          [
                            /* Cmp */1,
                            2
                          ]
                        ],
                        [
                          /* :: */0,
                          [
                            /* tuple */0,
                            "!=",
                            [
                              /* Cmp */1,
                              3
                            ]
                          ],
                          [
                            /* :: */0,
                            [
                              /* tuple */0,
                              "&",
                              [
                                /* Bin */0,
                                [
                                  /* :: */0,
                                  4727240,
                                  /* [] */0
                                ]
                              ]
                            ],
                            [
                              /* :: */0,
                              [
                                /* tuple */0,
                                "^",
                                [
                                  /* Bin */0,
                                  [
                                    /* :: */0,
                                    4731336,
                                    /* [] */0
                                  ]
                                ]
                              ],
                              [
                                /* :: */0,
                                [
                                  /* tuple */0,
                                  "|",
                                  [
                                    /* Bin */0,
                                    [
                                      /* :: */0,
                                      4721096,
                                      /* [] */0
                                    ]
                                  ]
                                ],
                                /* [] */0
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
];

var tokint_001 = addsym("int");

var tokint = [
  /* Sym */3,
  tokint_001
];

var tokchar_001 = addsym("char");

var tokchar = [
  /* Sym */3,
  tokchar_001
];

var tokret_001 = addsym("return");

var tokret = [
  /* Sym */3,
  tokret_001
];

var tokif_001 = addsym("if");

var tokif = [
  /* Sym */3,
  tokif_001
];

var tokelse_001 = addsym("else");

var tokelse = [
  /* Sym */3,
  tokelse_001
];

var tokwhile_001 = addsym("while");

var tokwhile = [
  /* Sym */3,
  tokwhile_001
];

var tokfor_001 = addsym("for");

var tokfor = [
  /* Sym */3,
  tokfor_001
];

var tokbreak_001 = addsym("break");

var tokbreak = [
  /* Sym */3,
  tokbreak_001
];

function binary(stk, lvl) {
  if (lvl === -1) {
    return unary(stk);
  }
  else {
    var lvlof = function (o) {
      if (!List.mem_assoc(o, lvls)) {
        return -1;
      }
      else {
        return List.assoc(o, lvls);
      }
    };
    var foldtst = function (_loc) {
      while(true) {
        var loc = _loc;
        var t = next$1(/* () */0);
        if (t[0]) {
          unnext(t);
          return loc;
        }
        else {
          if (lvlof(t[1]) === lvl) {
            var loc$prime = test(lvl - 8, loc);
            binary(stk, lvl - 1);
            _loc = loc$prime;
          }
          else {
            unnext(t);
            return loc;
          }
        }
      };
    };
    binary(stk, lvl - 1);
    if (lvl < 8) {
      var _param = /* () */0;
      while(true) {
        var t = next$1(/* () */0);
        if (t[0]) {
          return unnext(t);
        }
        else {
          var o = t[1];
          if (lvlof(o) === lvl) {
            push(0);
            binary(stk, lvl - 1);
            pop(1);
            var match = List.assoc(o, inss);
            if (match[0]) {
              out(4733377);
              cmp(match[1]);
            }
            else {
              List.iter(out, match[1]);
            }
            _param = /* () */0;
          }
          else {
            return unnext(t);
          }
        }
      };
    }
    else {
      var loc = foldtst(0);
      return patch(/* true */1, loc, opos[1]);
    }
  }
}

function unary(stk) {
  var match = next$1(/* () */0);
  switch (match[0]) {
    case 0 : 
        var o = match[1];
        switch (o) {
          case "&" : 
              unary(stk);
              return patchlval(/* () */0);
          case "(" : 
              expr(stk);
              next$1(/* () */0);
              return postfix(stk);
          case "*" : 
              next$1(/* () */0);
              var t = next$1(/* () */0);
              var match$1 = Caml_primitive.caml_equal(t, tokint) ? (
                  Caml_primitive.caml_equal(next$1(/* () */0), [
                        /* Op */0,
                        "*"
                      ]) ? [
                      /* tuple */0,
                      /* Int */0,
                      1
                    ] : [
                      /* tuple */0,
                      /* Int */0,
                      5
                    ]
                ) : (
                  Caml_primitive.caml_equal(t, tokchar) ? [
                      /* tuple */0,
                      /* Chr */1,
                      2
                    ] : Pervasives.failwith("[cast] expected")
                );
              for(var k = 1 ,k_finish = match$1[2]; k<= k_finish; ++k){
                next$1(/* () */0);
              }
              unary(stk);
              return read(match$1[1]);
          default:
            var unops = [
              /* :: */0,
              [
                /* tuple */0,
                "+",
                0
              ],
              [
                /* :: */0,
                [
                  /* tuple */0,
                  "-",
                  4782040
                ],
                [
                  /* :: */0,
                  [
                    /* tuple */0,
                    "~",
                    4782032
                  ],
                  [
                    /* :: */0,
                    [
                      /* tuple */0,
                      "!",
                      4752832
                    ],
                    /* [] */0
                  ]
                ]
              ]
            ];
            unary(stk);
            if (!List.mem_assoc(o, unops)) {
              Pervasives.failwith(Printf.sprintf([
                          /* Format */0,
                          [
                            /* String_literal */11,
                            "unknown operator ",
                            [
                              /* String */2,
                              /* No_padding */0,
                              /* End_of_format */0
                            ]
                          ],
                          "unknown operator %s"
                        ])(o));
            }
            out(List.assoc(o, unops));
            if (o === "!") {
              return cmp(2);
            }
            else {
              return 0;
            }
        }
        break;
    case 1 : 
        return load(0, match[1]);
    case 2 : 
        out(18616);
        return le(64, match[1]);
    case 3 : 
        var i = match[1];
        if (List.mem_assoc(i, stk)) {
          var l = List.assoc(i, stk);
          if (!(l > -256)) {
            throw [
                  0,
                  Caml_exceptions.Assert_failure,
                  [
                    0,
                    "qcc.ml",
                    295,
                    6
                  ]
                ];
          }
          out(4754245);
          out(l & 255);
          lval[1] = [
            /* tuple */0,
            [
              /* Mov */0,
              3
            ],
            /* Int */0
          ];
        }
        else {
          out(18616);
          var g = globs[i];
          var loc = opos[1];
          le(64, g[1]);
          globs[i] = [
            /* record */0,
            loc,
            g[2]
          ];
          read(/* Int */0);
        }
        return postfix(stk);
    
  }
}

function postfix(stk) {
  var t = next$1(/* () */0);
  if (t[0]) {
    return unnext(t);
  }
  else {
    var op = t[1];
    var exit = 0;
    switch (op) {
      case "(" : 
          var emitargs = function (_l, _rl) {
            while(true) {
              var rl = _rl;
              var l = _l;
              if (nextis([
                      /* Op */0,
                      ")"
                    ])) {
                next$1(/* () */0);
                return List.iter(pop, l);
              }
              else {
                expr(stk);
                push(0);
                if (nextis([
                        /* Op */0,
                        ","
                      ])) {
                  next$1(/* () */0);
                }
                _rl = List.tl(rl);
                _l = [
                  /* :: */0,
                  List.hd(rl),
                  l
                ];
              }
            };
          };
          patchlval(/* () */0);
          push(0);
          emitargs(/* [] */0, [
                /* :: */0,
                7,
                [
                  /* :: */0,
                  6,
                  [
                    /* :: */0,
                    2,
                    [
                      /* :: */0,
                      1,
                      [
                        /* :: */0,
                        8,
                        [
                          /* :: */0,
                          9,
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ]
              ]);
          pop(0);
          if (align[1] % 2 !== 0) {
            out(1216605192);
          }
          out(65488);
          if (align[1] % 2 !== 0) {
            return out(1216594952);
          }
          else {
            return 0;
          }
      case "++" : 
      case "--" : 
          exit = 1;
          break;
      default:
        return unnext(t);
    }
    if (exit === 1) {
      patchlval(/* () */0);
      out(4753857);
      read(lval[1][2]);
      return out(List.assoc([
                      /* tuple */0,
                      op,
                      lval[1][2]
                    ], [
                      /* :: */0,
                      [
                        /* tuple */0,
                        [
                          /* tuple */0,
                          "++",
                          /* Int */0
                        ],
                        4783873
                      ],
                      [
                        /* :: */0,
                        [
                          /* tuple */0,
                          [
                            /* tuple */0,
                            "--",
                            /* Int */0
                          ],
                          4783881
                        ],
                        [
                          /* :: */0,
                          [
                            /* tuple */0,
                            [
                              /* tuple */0,
                              "++",
                              /* Chr */1
                            ],
                            65025
                          ],
                          [
                            /* :: */0,
                            [
                              /* tuple */0,
                              [
                                /* tuple */0,
                                "--",
                                /* Chr */1
                              ],
                              65033
                            ],
                            /* [] */0
                          ]
                        ]
                      ]
                    ]));
    }
    
  }
}

function expr(stk) {
  binary(stk, 10);
  var _param = /* () */0;
  while(true) {
    var t = next$1(/* () */0);
    if (t[0]) {
      return unnext(t);
    }
    else {
      if (t[1] === "=") {
        patchlval(/* () */0);
        var ty = lval[1][2];
        push(0);
        expr(stk);
        pop(1);
        if (ty) {
          out(34817);
        }
        else {
          out(4753665);
        }
        _param = /* () */0;
      }
      else {
        return unnext(t);
      }
    }
  };
}

function decl(g, _n, _stk) {
  while(true) {
    var stk = _stk;
    var n = _n;
    var t = next$1(/* () */0);
    if (Caml_primitive.caml_equal(t, tokint)) {
      var top = stk ? stk[1][2] : 0;
      var vars = (function(top){
      return function (_n, _stk) {
        while(true) {
          var stk = _stk;
          var n = _n;
          while(nextis([
                  /* Op */0,
                  "*"
                ])) {
            next$1(/* () */0);
          };
          if (nextis([
                  /* Op */0,
                  ";"
                ])) {
            return [
                    /* tuple */0,
                    n,
                    stk
                  ];
          }
          else {
            var match = next$1(/* () */0);
            if (match[0] === 3) {
              var s = match[1];
              var n$prime = n + 1;
              var stk$prime;
              if (g) {
                var glo = globs[s];
                if (glo[2] >= 0) {
                  Pervasives.failwith("symbol defined twice");
                }
                var va = gpos[1] + textoff + base;
                globs[s] = [
                  /* record */0,
                  glo[1],
                  va
                ];
                gpos[1] += 8;
                stk$prime = stk;
              }
              else {
                stk$prime = [
                  /* :: */0,
                  [
                    /* tuple */0,
                    s,
                    top - 8 * n$prime
                  ],
                  stk
                ];
              }
              if (!nextis([
                      /* Op */0,
                      ","
                    ])) {
                return [
                        /* tuple */0,
                        n$prime,
                        stk$prime
                      ];
              }
              else {
                next$1(/* () */0);
                _stk = stk$prime;
                _n = n$prime;
              }
            }
            else {
              return Pervasives.failwith("[var] expected in [decl]");
            }
          }
        };
      }
      }(top));
      var match = vars(0, stk);
      next$1(/* () */0);
      if (dbg[1]) {
        Printf.eprintf([
                /* Format */0,
                [
                  /* String_literal */11,
                  "end of decl (",
                  [
                    /* Int */4,
                    /* Int_d */0,
                    /* No_padding */0,
                    /* No_precision */0,
                    [
                      /* String_literal */11,
                      " vars)\n",
                      /* End_of_format */0
                    ]
                  ]
                ],
                "end of decl (%d vars)\n"
              ])(n);
      }
      _stk = match[2];
      _n = n + match[1];
    }
    else {
      unnext(t);
      if (!g && n !== 0) {
        if (!(n * 8 < 256)) {
          throw [
                0,
                Caml_exceptions.Assert_failure,
                [
                  0,
                  "qcc.ml",
                  436,
                  6
                ]
              ];
        }
        out(4752364);
        out(n * 8);
        align[1] += n;
      }
      if (dbg[1] && !g) {
        console.error("end of blk decls");
      }
      return [
              /* tuple */0,
              n,
              stk
            ];
    }
  };
}

var retl = [
  0,
  0
];

function stmt(brk, stk) {
  var pexpr = function (stk) {
    next$1(/* () */0);
    expr(stk);
    return next$1(/* () */0);
  };
  var t = next$1(/* () */0);
  var exit = 0;
  if (Caml_primitive.caml_equal(t, tokif)) {
    pexpr(stk);
    var loc = test(0, 0);
    stmt(brk, stk);
    var loc$1;
    if (!nextis(tokelse)) {
      loc$1 = loc;
    }
    else {
      next$1(/* () */0);
      out(233);
      var l = opos[1];
      le(32, 0);
      patch(/* true */1, loc, opos[1]);
      stmt(brk, stk);
      loc$1 = l;
    }
    return patch(/* true */1, loc$1, opos[1]);
  }
  else {
    if (Caml_primitive.caml_equal(t, tokwhile) || Caml_primitive.caml_equal(t, tokfor)) {
      var match_001 = [
        0,
        0
      ];
      var match_002 = align[1];
      var bl = match_001;
      var match;
      if (Caml_primitive.caml_equal(t, tokwhile)) {
        var loc$2 = opos[1];
        pexpr(stk);
        bl[1] = test(0, 0);
        match = [
          /* tuple */0,
          0,
          loc$2
        ];
      }
      else {
        next$1(/* () */0);
        if (!nextis([
                /* Op */0,
                ";"
              ])) {
          expr(stk);
        }
        next$1(/* () */0);
        var top = opos[1];
        if (!nextis([
                /* Op */0,
                ";"
              ])) {
          expr(stk);
          bl[1] = test(0, 0);
        }
        else {
          bl[1] = 0;
        }
        next$1(/* () */0);
        out(233);
        var bdy = opos[1];
        le(32, 0);
        var itr = opos[1];
        expr(stk);
        next$1(/* () */0);
        out(233);
        le(32, top - opos[1] - 4);
        match = [
          /* tuple */0,
          bdy,
          itr
        ];
      }
      patch(/* true */1, match[1], opos[1]);
      stmt([
            /* tuple */0,
            bl,
            match_002
          ], stk);
      out(233);
      le(32, match[2] - opos[1] - 4);
      return patch(/* true */1, bl[1], opos[1]);
    }
    else {
      if (Caml_primitive.caml_equal(t, tokret)) {
        if (!nextis([
                /* Op */0,
                ";"
              ])) {
          expr(stk);
        }
        next$1(/* () */0);
        out(233);
        var loc$3 = opos[1];
        le(32, retl[1]);
        retl[1] = loc$3;
        return /* () */0;
      }
      else {
        if (Caml_primitive.caml_equal(t, tokbreak)) {
          next$1(/* () */0);
          var brkl = brk[1];
          var n = align[1] - brk[2];
          if (!(n >= 0)) {
            throw [
                  0,
                  Caml_exceptions.Assert_failure,
                  [
                    0,
                    "qcc.ml",
                    515,
                    4
                  ]
                ];
          }
          if (n !== 0) {
            out(4752324);
            out(n * 8);
          }
          out(233);
          var loc$4 = opos[1];
          le(32, brkl[1]);
          brkl[1] = loc$4;
          return /* () */0;
        }
        else {
          if (t[0]) {
            exit = 1;
          }
          else {
            switch (t[1]) {
              case ";" : 
                  return /* () */0;
              case "{" : 
                  return block(brk, stk);
              default:
                exit = 1;
            }
          }
        }
      }
    }
  }
  if (exit === 1) {
    unnext(t);
    expr(stk);
    return next$1(/* () */0);
  }
  
}

function block(brk, stk) {
  var match = decl(/* false */0, 0, stk);
  var stk$prime = match[2];
  var n = match[1];
  while(!nextis([
          /* Op */0,
          "}"
        ])) {
    stmt(brk, stk$prime);
  };
  next$1(/* () */0);
  if (n !== 0) {
    out(4752324);
    out(n * 8);
    align[1] -= n;
    return /* () */0;
  }
  else {
    return 0;
  }
}

function top(_param) {
  while(true) {
    if (!nextis([
            /* Op */0,
            "EOF!"
          ])) {
      if (nextis(tokint)) {
        decl(/* true */1, 0, /* [] */0);
        _param = /* () */0;
      }
      else {
        var match = next$1(/* () */0);
        if (match[0] === 3) {
          var f = match[1];
          var g = globs[f];
          if (g[2] >= 0) {
            Pervasives.failwith("symbol defined twice");
          }
          globs[f] = [
            /* record */0,
            g[1],
            opos[1]
          ];
          var emitargs = function (_regs, _n, _stk) {
            while(true) {
              var stk = _stk;
              var n = _n;
              var regs = _regs;
              var match = next$1(/* () */0);
              var exit = 0;
              switch (match[0]) {
                case 0 : 
                    if (match[1] === ")") {
                      return stk;
                    }
                    else {
                      exit = 1;
                    }
                    break;
                case 1 : 
                case 2 : 
                    exit = 1;
                    break;
                case 3 : 
                    var r = List.hd(regs);
                    push(r);
                    if (nextis([
                            /* Op */0,
                            ","
                          ])) {
                      next$1(/* () */0);
                    }
                    var stk$prime_001 = [
                      /* tuple */0,
                      match[1],
                      -n * 8
                    ];
                    var stk$prime = [
                      /* :: */0,
                      stk$prime_001,
                      stk
                    ];
                    _stk = stk$prime;
                    _n = n + 1;
                    _regs = List.tl(regs);
                    break;
                
              }
              if (exit === 1) {
                return Pervasives.failwith("[var] or ) expected");
              }
              
            };
          };
          next$1(/* () */0);
          align[1] = 0;
          out(85);
          out(4753893);
          var stk = emitargs([
                /* :: */0,
                7,
                [
                  /* :: */0,
                  6,
                  [
                    /* :: */0,
                    2,
                    [
                      /* :: */0,
                      1,
                      [
                        /* :: */0,
                        8,
                        [
                          /* :: */0,
                          9,
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ]
              ], 1, /* [] */0);
          while(Caml_primitive.caml_notequal(next$1(/* () */0), [
                  /* Op */0,
                  "{"
                ])) {
            
          };
          retl[1] = 0;
          block([
                /* tuple */0,
                [
                  0,
                  0
                ],
                0
              ], stk);
          patch(/* true */1, retl[1], opos[1]);
          out(51651);
          if (dbg[1]) {
            Printf.eprintf([
                    /* Format */0,
                    [
                      /* String_literal */11,
                      "done with function ",
                      [
                        /* String */2,
                        /* No_padding */0,
                        [
                          /* Char_literal */12,
                          /* "\n" */10,
                          /* End_of_format */0
                        ]
                      ]
                    ],
                    "done with function %s\n"
                  ])(symstr(f));
          }
          _param = /* () */0;
        }
        else {
          return Pervasives.failwith("[decl] or [fun] expected");
        }
      }
    }
    else {
      return 0;
    }
  };
}

var elfhdr = Bytes.of_string($$String.concat("", [
          /* :: */0,
          "\x7fELF\x02\x01\x01\0",
          [
            /* :: */0,
            "\0\0\0\0\0\0\0\0",
            [
              /* :: */0,
              "\x02\0",
              [
                /* :: */0,
                ">\0",
                [
                  /* :: */0,
                  "\x01\0\0\0",
                  [
                    /* :: */0,
                    "\0\0\0\0\0\0\0\0",
                    [
                      /* :: */0,
                      "@\0\0\0\0\0\0\0",
                      [
                        /* :: */0,
                        "\0\0\0\0\0\0\0\0",
                        [
                          /* :: */0,
                          "\0\0\0\0",
                          [
                            /* :: */0,
                            "@\0",
                            [
                              /* :: */0,
                              "8\0",
                              [
                                /* :: */0,
                                "\x03\0",
                                [
                                  /* :: */0,
                                  "@\0",
                                  [
                                    /* :: */0,
                                    "\0\0",
                                    [
                                      /* :: */0,
                                      "\0\0",
                                      /* [] */0
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]));

function elfphdr(ty, off, sz, align) {
  le(32, ty);
  le(32, 7);
  le(64, off);
  le(64, off + base);
  le(64, off + base);
  le(64, sz);
  le(64, sz);
  return le(64, align);
}

function elfgen(outf) {
  var entry = opos[1];
  var main = addsym("main");
  var gmain = globs[main];
  out(1217084452);
  out(311610844168);
  out(18616);
  le(64, gmain[1]);
  globs[main] = [
    /* record */0,
    opos[1] - 8,
    gmain[2]
  ];
  out(65488);
  out(35271);
  load(0, 60);
  out(3845);
  var off = textoff + gpos[1];
  var itr = function (f) {
    return symitr(function (i, s) {
                var g = globs[i];
                if (g[2] < 0 && g[1] !== 0) {
                  return f(s, s.length, g[1]);
                }
                else {
                  return 0;
                }
              });
  };
  var va = function (x) {
    return x + off + base;
  };
  var patchloc = function (i, _) {
    var g = globs[i];
    if (g[2] >= 0 && g[2] < base) {
      return patch(/* false */0, g[1], va(g[2]));
    }
    else {
      if (g[2] >= 0) {
        return patch(/* false */0, g[1], g[2]);
      }
      else {
        return 0;
      }
    }
  };
  symitr(patchloc);
  var strtab = opos[1];
  ++ opos[1];
  var dllen = 37;
  $$String.blit("/lib64/ld-linux-x86-64.so.2\0libc.so.6", 0, obuf, opos[1], dllen);
  opos[1] = opos[1] + dllen + 1;
  itr(function (s, sl, _) {
        $$String.blit(s, 0, obuf, opos[1], sl);
        opos[1] = opos[1] + sl + 1;
        return /* () */0;
      });
  opos[1] = opos[1] + 7 & -8;
  var symtab = opos[1];
  var n = [
    0,
    dllen + 2
  ];
  opos[1] += 24;
  itr(function (_, sl, _$1) {
        le(32, n[1]);
        le(32, 16);
        le(64, 0);
        le(64, 0);
        n[1] = n[1] + sl + 1;
        return /* () */0;
      });
  var rel = opos[1];
  var n$1 = [
    0,
    1
  ];
  itr(function (_, _$1, l) {
        var genrel = function (_l) {
          while(true) {
            var l = _l;
            if (l !== 0) {
              le(64, va(l));
              le(64, 1 + (n$1[1] << 32));
              le(64, 0);
              _l = get32(l);
            }
            else {
              return 0;
            }
          };
        };
        genrel(l);
        return ++ n$1[1];
      });
  var hash = opos[1];
  var n$2 = ((rel - symtab) / 24 | 0) - 1;
  le(32, 1);
  le(32, n$2 + 1);
  le(32, n$2 > 0 ? 1 : 0);
  for(var i = 1; i<= n$2; ++i){
    le(32, i);
  }
  le(32, 0);
  var dyn = opos[1];
  List.iter(function (param) {
        return le(64, param);
      }, [
        /* :: */0,
        1,
        [
          /* :: */0,
          29,
          [
            /* :: */0,
            4,
            [
              /* :: */0,
              va(hash),
              [
                /* :: */0,
                5,
                [
                  /* :: */0,
                  va(strtab),
                  [
                    /* :: */0,
                    6,
                    [
                      /* :: */0,
                      va(symtab),
                      [
                        /* :: */0,
                        7,
                        [
                          /* :: */0,
                          va(rel),
                          [
                            /* :: */0,
                            8,
                            [
                              /* :: */0,
                              hash - rel,
                              [
                                /* :: */0,
                                9,
                                [
                                  /* :: */0,
                                  24,
                                  [
                                    /* :: */0,
                                    10,
                                    [
                                      /* :: */0,
                                      symtab - strtab,
                                      [
                                        /* :: */0,
                                        11,
                                        [
                                          /* :: */0,
                                          24,
                                          [
                                            /* :: */0,
                                            0,
                                            /* [] */0
                                          ]
                                        ]
                                      ]
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]);
  var tend = opos[1];
  Bytes.blit(obuf, 0, obuf, off, tend);
  Bytes.blit(glo, 0, obuf, textoff, gpos[1]);
  Bytes.blit(elfhdr, 0, obuf, 0, 64);
  opos[1] = 64;
  elfphdr(3, strtab + 1 + off, 28, 1);
  elfphdr(1, 0, tend + off, 2097152);
  elfphdr(2, dyn + off, tend - dyn, 8);
  if (opos[1] !== textoff) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "qcc.ml",
            698,
            2
          ]
        ];
  }
  patch(/* false */0, 24, va(entry));
  return Pervasives.output_bytes(outf, Bytes.sub(obuf, 0, tend + off));
}

function main() {
  var ppsym = function (param) {
    switch (param[0]) {
      case 0 : 
          return Printf.printf([
                        /* Format */0,
                        [
                          /* String_literal */11,
                          "Operator '",
                          [
                            /* String */2,
                            /* No_padding */0,
                            [
                              /* String_literal */11,
                              "'\n",
                              /* End_of_format */0
                            ]
                          ]
                        ],
                        "Operator '%s'\n"
                      ])(param[1]);
      case 1 : 
          return Printf.printf([
                        /* Format */0,
                        [
                          /* String_literal */11,
                          "Int literal ",
                          [
                            /* Int */4,
                            /* Int_d */0,
                            /* No_padding */0,
                            /* No_precision */0,
                            [
                              /* Char_literal */12,
                              /* "\n" */10,
                              /* End_of_format */0
                            ]
                          ]
                        ],
                        "Int literal %d\n"
                      ])(param[1]);
      case 2 : 
          return Printf.printf([
                        /* Format */0,
                        [
                          /* String_literal */11,
                          "Str literal ",
                          [
                            /* Caml_string */3,
                            /* No_padding */0,
                            [
                              /* Char_literal */12,
                              /* "\n" */10,
                              /* End_of_format */0
                            ]
                          ]
                        ],
                        "Str literal %S\n"
                      ])(param[2]);
      case 3 : 
          var i = param[1];
          return Printf.printf([
                        /* Format */0,
                        [
                          /* String_literal */11,
                          "Symbol '",
                          [
                            /* String */2,
                            /* No_padding */0,
                            [
                              /* String_literal */11,
                              "' (",
                              [
                                /* Int */4,
                                /* Int_d */0,
                                /* No_padding */0,
                                /* No_precision */0,
                                [
                                  /* String_literal */11,
                                  ")\n",
                                  /* End_of_format */0
                                ]
                              ]
                            ]
                          ]
                        ],
                        "Symbol '%s' (%d)\n"
                      ])(symstr(i), i);
      
    }
  };
  var f = Sys.argv.length < 2 ? "-blk" : Sys.argv[1];
  switch (f) {
    case "-blk" : 
        var c = function (param) {
          return block([
                      /* tuple */0,
                      [
                        0,
                        0
                      ],
                      0
                    ], param);
        };
        var stk = /* [] */0;
        opos[1] = 0;
        c(stk);
        return Pervasives.print_bytes(Bytes.sub(obuf, 0, opos[1]));
    case "-lex" : 
        var _param = /* () */0;
        while(true) {
          var tok = next$1(/* () */0);
          if (tok[0]) {
            ppsym(tok);
            _param = /* () */0;
          }
          else {
            if (tok[1] === "EOF!") {
              return Printf.printf([
                          /* Format */0,
                          [
                            /* String_literal */11,
                            "End of input stream\n",
                            /* End_of_format */0
                          ],
                          "End of input stream\n"
                        ]);
            }
            else {
              ppsym(tok);
              _param = /* () */0;
            }
          }
        };
    default:
      var oc = Pervasives.open_out("a.out");
      inch[1] = Pervasives.open_in(f);
      top(/* () */0);
      elfgen(oc);
      return Pervasives.close_out(oc);
  }
}

main(/* () */0);

exports.dbg       = dbg;
exports.inch      = inch;
exports.bufferize = bufferize;
exports.getch     = getch;
exports.ungetch   = ungetch;
exports.peekch    = peekch;
exports.addsym    = addsym;
exports.symstr    = symstr;
exports.symitr    = symitr;
exports.glo       = glo;
exports.gpos      = gpos;
exports.base      = base;
exports.textoff   = textoff;
exports.next      = next$1;
exports.unnext    = unnext;
exports.nextis    = nextis;
exports.obuf      = obuf;
exports.opos      = opos;
exports.out       = out;
exports.le        = le;
exports.get32     = get32;
exports.patch     = patch;
exports.load      = load;
exports.cmp       = cmp;
exports.test      = test;
exports.align     = align;
exports.push      = push;
exports.pop       = pop;
exports.lval      = lval;
exports.patchlval = patchlval;
exports.read      = read;
exports.globs     = globs;
exports.lvls      = lvls;
exports.inss      = inss;
exports.tokint    = tokint;
exports.tokchar   = tokchar;
exports.tokret    = tokret;
exports.tokif     = tokif;
exports.tokelse   = tokelse;
exports.tokwhile  = tokwhile;
exports.tokfor    = tokfor;
exports.tokbreak  = tokbreak;
exports.binary    = binary;
exports.unary     = unary;
exports.postfix   = postfix;
exports.expr      = expr;
exports.decl      = decl;
exports.retl      = retl;
exports.stmt      = stmt;
exports.block     = block;
exports.top       = top;
exports.elfhdr    = elfhdr;
exports.elfphdr   = elfphdr;
exports.elfgen    = elfgen;
exports.main      = main;
/* match Not a pure module */
