'use strict';

var Sys = require("../../lib/js/sys.js");
var Char = require("../../lib/js/char.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
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
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_missing_polyfill = require("../../lib/js/caml_missing_polyfill.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var dbg = [/* true */1];

var inch = [Pervasives.stdin];

function bufferize(f) {
  var buf = [/* None */0];
  return /* tuple */[
          (function () {
              var match = buf[0];
              if (match) {
                buf[0] = /* None */0;
                return match[0];
              } else {
                return Curry._1(f, /* () */0);
              }
            }),
          (function (x) {
              if (buf[0] !== /* None */0) {
                throw [
                      Caml_builtin_exceptions.assert_failure,
                      [
                        "qcc.ml",
                        17,
                        4
                      ]
                    ];
              }
              buf[0] = /* Some */[x];
              return /* () */0;
            })
        ];
}

var match = bufferize((function () {
        return Caml_io.caml_ml_input_char(inch[0]);
      }));

var ungetch = match[1];

var getch = match[0];

function peekch() {
  var ch = Curry._1(getch, /* () */0);
  Curry._1(ungetch, ch);
  return ch;
}

var symtab = Caml_array.caml_make_vect(100, "");

var syms = [0];

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

function match_000(s) {
  var sid = find(s, 0);
  Caml_array.caml_array_set(symtab, sid, s);
  return sid;
}

function match_001(n) {
  if (n >= syms[0]) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "qcc.ml",
            40,
            4
          ]
        ];
  }
  return Caml_array.caml_array_get(symtab, n);
}

function match_002(f) {
  for(var i = 0 ,i_finish = syms[0] - 1 | 0; i <= i_finish; ++i){
    Curry._2(f, i, Caml_array.caml_array_get(symtab, i));
  }
  return /* () */0;
}

var symitr = match_002;

var symstr = match_001;

var addsym = match_000;

var glo = Bytes.make(4096, /* "\000" */0);

var gpos = [0];

var s = Caml_string.caml_create_string(100);

function getq() {
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
    if ((switcher + 26 >>> 0) > 57) {
      return /* false */0;
    } else {
      return /* true */1;
    }
  } else if (switcher !== 4) {
    return /* false */0;
  } else {
    return /* true */1;
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

function next() {
  var match;
  try {
    match = /* Some */[skip(/* () */0)];
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.end_of_file) {
      match = /* None */0;
    } else {
      throw exn;
    }
  }
  if (match) {
    var c = match[0];
    var exit = 0;
    if (c !== 34) {
      if (c >= 48) {
        if (c >= 58) {
          exit = 1;
        } else {
          var _n = c - 48 | 0;
          while(true) {
            var n = _n;
            var match$1 = peekch(/* () */0);
            if (match$1 > 57 || match$1 < 48) {
              return /* ILit */Block.__(1, [n]);
            } else {
              _n = (Caml_int32.imul(10, n) + Curry._1(getch, /* () */0) | 0) - 48 | 0;
              continue ;
            }
          };
        }
      } else if (c !== 39) {
        exit = 1;
      } else {
        var ch = getq(/* () */0);
        var qt = Curry._1(getch, /* () */0);
        if (qt !== /* "'" */39) {
          throw [
                Caml_builtin_exceptions.failure,
                "syntax error"
              ];
        } else {
          return /* ILit */Block.__(1, [ch]);
        }
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
          return /* SLit */Block.__(2, [
                    (b + 232 | 0) + 4194304 | 0,
                    Bytes.to_string(Bytes.sub(glo, b, e - b | 0))
                  ]);
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
            _ch = Curry._1(getch, /* () */0);
            _n$1 = n$1 + 1 | 0;
            continue ;
          } else {
            return /* Sym */Block.__(3, [Curry._1(addsym, Bytes.to_string(Bytes.sub(s, 0, n$1 + 1 | 0)))]);
          }
        };
      } else {
        var ch$2 = c;
        var _param = /* :: */[
          "++",
          /* :: */[
            "--",
            /* :: */[
              "&&",
              /* :: */[
                "||",
                /* :: */[
                  "==",
                  /* :: */[
                    "<=",
                    /* :: */[
                      ">=",
                      /* :: */[
                        "!=",
                        /* :: */[
                          ">>",
                          /* :: */[
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
            var lop = param[0];
            if (Caml_string.get(lop, 0) === ch$2 && Caml_string.get(lop, 1) === peekch(/* () */0)) {
              Curry._1(getch, /* () */0);
              return /* Op */Block.__(0, [lop]);
            } else {
              _param = param[1];
              continue ;
            }
          } else {
            return /* Op */Block.__(0, [Caml_string.bytes_to_string(Bytes.make(1, ch$2))]);
          }
        };
      }
    }
    
  } else {
    return /* Op */Block.__(0, ["EOF!"]);
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

var opos = [0];

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
          [
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
      Curry._3(Printf.eprintf(/* Format */[
                /* String_literal */Block.__(11, [
                    "patching at ",
                    /* Int */Block.__(4, [
                        /* Int_d */0,
                        /* No_padding */0,
                        /* No_precision */0,
                        /* String_literal */Block.__(11, [
                            " to ",
                            /* Int */Block.__(4, [
                                /* Int_d */0,
                                /* No_padding */0,
                                /* No_precision */0,
                                /* String_literal */Block.__(11, [
                                    " (n=",
                                    /* Int */Block.__(4, [
                                        /* Int_d */0,
                                        /* No_padding */0,
                                        /* No_precision */0,
                                        /* String_literal */Block.__(11, [
                                            ")\n",
                                            /* End_of_format */0
                                          ])
                                      ])
                                  ])
                              ])
                          ])
                      ])
                  ]),
                "patching at %d to %d (n=%d)\n"
              ]), loc, x, n);
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

var align = [0];

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

var lval = [/* tuple */[
    /* Mov */Block.__(0, [0]),
    /* Int */0
  ]];

function patchlval() {
  var match = lval[0][0];
  if (match.tag) {
    opos[0] = opos[0] - match[0] | 0;
    return /* () */0;
  } else {
    obuf[opos[0] - match[0] | 0] = /* "\141" */141;
    return /* () */0;
  }
}

function read(param) {
  if (param !== 0) {
    out(4722614);
    le(8, 0);
    lval[0] = /* tuple */[
      /* Del */Block.__(1, [4]),
      /* Chr */1
    ];
    return /* () */0;
  } else {
    out(18571);
    le(8, 0);
    lval[0] = /* tuple */[
      /* Del */Block.__(1, [3]),
      /* Int */0
    ];
    return /* () */0;
  }
}

var globs = Caml_array.caml_make_vect(100, /* record */[
      /* loc */0,
      /* va */-1
    ]);

var lvls = /* :: */[
  /* tuple */[
    "*",
    0
  ],
  /* :: */[
    /* tuple */[
      "/",
      0
    ],
    /* :: */[
      /* tuple */[
        "%",
        0
      ],
      /* :: */[
        /* tuple */[
          "+",
          1
        ],
        /* :: */[
          /* tuple */[
            "-",
            1
          ],
          /* :: */[
            /* tuple */[
              "<<",
              2
            ],
            /* :: */[
              /* tuple */[
                ">>",
                2
              ],
              /* :: */[
                /* tuple */[
                  "<",
                  3
                ],
                /* :: */[
                  /* tuple */[
                    "<=",
                    3
                  ],
                  /* :: */[
                    /* tuple */[
                      ">",
                      3
                    ],
                    /* :: */[
                      /* tuple */[
                        ">=",
                        3
                      ],
                      /* :: */[
                        /* tuple */[
                          "==",
                          4
                        ],
                        /* :: */[
                          /* tuple */[
                            "!=",
                            4
                          ],
                          /* :: */[
                            /* tuple */[
                              "&",
                              5
                            ],
                            /* :: */[
                              /* tuple */[
                                "^",
                                6
                              ],
                              /* :: */[
                                /* tuple */[
                                  "|",
                                  7
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "&&",
                                    8
                                  ],
                                  /* :: */[
                                    /* tuple */[
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

var inss = /* :: */[
  /* tuple */[
    "*",
    /* Bin */Block.__(0, [/* :: */[
          1208987585,
          /* [] */0
        ]])
  ],
  /* :: */[
    /* tuple */[
      "/",
      /* Bin */Block.__(0, [/* :: */[
            18577,
            /* :: */[
              18585,
              /* :: */[
                4782073,
                /* [] */0
              ]
            ]
          ]])
    ],
    /* :: */[
      /* tuple */[
        "%",
        /* Bin */Block.__(0, [/* :: */[
              18577,
              /* :: */[
                18585,
                /* :: */[
                  4782073,
                  /* :: */[
                    18578,
                    /* [] */0
                  ]
                ]
              ]
            ]])
      ],
      /* :: */[
        /* tuple */[
          "+",
          /* Bin */Block.__(0, [/* :: */[
                4719048,
                /* [] */0
              ]])
        ],
        /* :: */[
          /* tuple */[
            "-",
            /* Bin */Block.__(0, [/* :: */[
                  18577,
                  /* :: */[
                    4729288,
                    /* [] */0
                  ]
                ]])
          ],
          /* :: */[
            /* tuple */[
              "<<",
              /* Bin */Block.__(0, [/* :: */[
                    18577,
                    /* :: */[
                      4772832,
                      /* [] */0
                    ]
                  ]])
            ],
            /* :: */[
              /* tuple */[
                ">>",
                /* Bin */Block.__(0, [/* :: */[
                      18577,
                      /* :: */[
                        4772856,
                        /* [] */0
                      ]
                    ]])
              ],
              /* :: */[
                /* tuple */[
                  "<",
                  /* Cmp */Block.__(1, [10])
                ],
                /* :: */[
                  /* tuple */[
                    "<=",
                    /* Cmp */Block.__(1, [12])
                  ],
                  /* :: */[
                    /* tuple */[
                      ">",
                      /* Cmp */Block.__(1, [13])
                    ],
                    /* :: */[
                      /* tuple */[
                        ">=",
                        /* Cmp */Block.__(1, [11])
                      ],
                      /* :: */[
                        /* tuple */[
                          "==",
                          /* Cmp */Block.__(1, [2])
                        ],
                        /* :: */[
                          /* tuple */[
                            "!=",
                            /* Cmp */Block.__(1, [3])
                          ],
                          /* :: */[
                            /* tuple */[
                              "&",
                              /* Bin */Block.__(0, [/* :: */[
                                    4727240,
                                    /* [] */0
                                  ]])
                            ],
                            /* :: */[
                              /* tuple */[
                                "^",
                                /* Bin */Block.__(0, [/* :: */[
                                      4731336,
                                      /* [] */0
                                    ]])
                              ],
                              /* :: */[
                                /* tuple */[
                                  "|",
                                  /* Bin */Block.__(0, [/* :: */[
                                        4721096,
                                        /* [] */0
                                      ]])
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

var tokint = /* Sym */Block.__(3, [Curry._1(addsym, "int")]);

var tokchar = /* Sym */Block.__(3, [Curry._1(addsym, "char")]);

var tokret = /* Sym */Block.__(3, [Curry._1(addsym, "return")]);

var tokif = /* Sym */Block.__(3, [Curry._1(addsym, "if")]);

var tokelse = /* Sym */Block.__(3, [Curry._1(addsym, "else")]);

var tokwhile = /* Sym */Block.__(3, [Curry._1(addsym, "while")]);

var tokfor = /* Sym */Block.__(3, [Curry._1(addsym, "for")]);

var tokbreak = /* Sym */Block.__(3, [Curry._1(addsym, "break")]);

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
        if (t.tag) {
          Curry._1(unnext, t);
          return loc;
        } else if (lvlof(t[0]) === lvl) {
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
        if (t.tag) {
          return Curry._1(unnext, t);
        } else {
          var o = t[0];
          if (lvlof(o) === lvl) {
            push(0);
            binary(stk, lvl - 1 | 0);
            pop(1);
            var match = List.assoc(o, inss);
            if (match.tag) {
              out(4733377);
              cmp(match[0]);
            } else {
              List.iter(out, match[0]);
            }
            _param = /* () */0;
            continue ;
          } else {
            return Curry._1(unnext, t);
          }
        }
      };
    } else {
      var loc = foldtst(0);
      return patch(/* true */1, loc, opos[0]);
    }
  }
}

function unary(stk) {
  var match = Curry._1(next$1, /* () */0);
  switch (match.tag | 0) {
    case 0 : 
        var o = match[0];
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
                match$1 = Caml_obj.caml_equal(Curry._1(next$1, /* () */0), /* Op */Block.__(0, ["*"])) ? /* tuple */[
                    /* Int */0,
                    1
                  ] : /* tuple */[
                    /* Int */0,
                    5
                  ];
              } else if (Caml_obj.caml_equal(t, tokchar)) {
                match$1 = /* tuple */[
                  /* Chr */1,
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
            var unops = /* :: */[
              /* tuple */[
                "+",
                0
              ],
              /* :: */[
                /* tuple */[
                  "-",
                  4782040
                ],
                /* :: */[
                  /* tuple */[
                    "~",
                    4782032
                  ],
                  /* :: */[
                    /* tuple */[
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
              var s = Curry._1(Printf.sprintf(/* Format */[
                        /* String_literal */Block.__(11, [
                            "unknown operator ",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* End_of_format */0
                              ])
                          ]),
                        "unknown operator %s"
                      ]), o);
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
    case 1 : 
        return load(0, match[0]);
    case 2 : 
        out(18616);
        return le(64, match[0]);
    case 3 : 
        var i = match[0];
        if (List.mem_assoc(i, stk)) {
          var l = List.assoc(i, stk);
          if (l <= -256) {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  [
                    "qcc.ml",
                    295,
                    6
                  ]
                ];
          }
          out(4754245);
          out(l & 255);
          lval[0] = /* tuple */[
            /* Mov */Block.__(0, [3]),
            /* Int */0
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
          read(/* Int */0);
        }
        return postfix(stk);
    
  }
}

function postfix(stk) {
  var t = Curry._1(next$1, /* () */0);
  if (t.tag) {
    return Curry._1(unnext, t);
  } else {
    var op = t[0];
    var exit = 0;
    switch (op) {
      case "(" : 
          var emitargs = function (_l, _rl) {
            while(true) {
              var rl = _rl;
              var l = _l;
              if (nextis(/* Op */Block.__(0, [")"]))) {
                Curry._1(next$1, /* () */0);
                return List.iter(pop, l);
              } else {
                expr(stk);
                push(0);
                if (nextis(/* Op */Block.__(0, [","]))) {
                  Curry._1(next$1, /* () */0);
                }
                _rl = List.tl(rl);
                _l = /* :: */[
                  List.hd(rl),
                  l
                ];
                continue ;
              }
            };
          };
          patchlval(/* () */0);
          push(0);
          emitargs(/* [] */0, /* :: */[
                7,
                /* :: */[
                  6,
                  /* :: */[
                    2,
                    /* :: */[
                      1,
                      /* :: */[
                        8,
                        /* :: */[
                          9,
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ]
              ]);
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
          exit = 1;
          break;
      default:
        return Curry._1(unnext, t);
    }
    if (exit === 1) {
      patchlval(/* () */0);
      out(4753857);
      read(lval[0][1]);
      return out(List.assoc(/* tuple */[
                      op,
                      lval[0][1]
                    ], /* :: */[
                      /* tuple */[
                        /* tuple */[
                          "++",
                          /* Int */0
                        ],
                        4783873
                      ],
                      /* :: */[
                        /* tuple */[
                          /* tuple */[
                            "--",
                            /* Int */0
                          ],
                          4783881
                        ],
                        /* :: */[
                          /* tuple */[
                            /* tuple */[
                              "++",
                              /* Chr */1
                            ],
                            65025
                          ],
                          /* :: */[
                            /* tuple */[
                              /* tuple */[
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
    var t = Curry._1(next$1, /* () */0);
    if (t.tag || t[0] !== "=") {
      return Curry._1(unnext, t);
    } else {
      patchlval(/* () */0);
      var ty = lval[0][1];
      push(0);
      expr(stk);
      pop(1);
      if (ty === /* Int */0) {
        out(4753665);
      } else {
        out(34817);
      }
      _param = /* () */0;
      continue ;
    }
  };
}

function decl(g, _n, _stk) {
  while(true) {
    var stk = _stk;
    var n = _n;
    var t = Curry._1(next$1, /* () */0);
    if (Caml_obj.caml_equal(t, tokint)) {
      var top = stk ? stk[0][1] : 0;
      var vars = (function(top){
      return function vars(_n, _stk) {
        while(true) {
          var stk = _stk;
          var n = _n;
          while(nextis(/* Op */Block.__(0, ["*"]))) {
            Curry._1(next$1, /* () */0);
          };
          if (nextis(/* Op */Block.__(0, [";"]))) {
            return /* tuple */[
                    n,
                    stk
                  ];
          } else {
            var match = Curry._1(next$1, /* () */0);
            if (match.tag === 3) {
              var s = match[0];
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
                stk$prime = /* :: */[
                  /* tuple */[
                    s,
                    top - (n$prime << 3) | 0
                  ],
                  stk
                ];
              }
              if (nextis(/* Op */Block.__(0, [","]))) {
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
        Curry._1(Printf.eprintf(/* Format */[
                  /* String_literal */Block.__(11, [
                      "end of decl (",
                      /* Int */Block.__(4, [
                          /* Int_d */0,
                          /* No_padding */0,
                          /* No_precision */0,
                          /* String_literal */Block.__(11, [
                              " vars)\n",
                              /* End_of_format */0
                            ])
                        ])
                    ]),
                  "end of decl (%d vars)\n"
                ]), n);
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
                [
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

var retl = [0];

function stmt(brk, stk) {
  var pexpr = function (stk) {
    Curry._1(next$1, /* () */0);
    expr(stk);
    Curry._1(next$1, /* () */0);
    return /* () */0;
  };
  var t = Curry._1(next$1, /* () */0);
  var exit = 0;
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
      patch(/* true */1, loc, opos[0]);
      stmt(brk, stk);
      loc$1 = l;
    } else {
      loc$1 = loc;
    }
    return patch(/* true */1, loc$1, opos[0]);
  } else if (Caml_obj.caml_equal(t, tokwhile) || Caml_obj.caml_equal(t, tokfor)) {
    var bl = [0];
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
      if (!nextis(/* Op */Block.__(0, [";"]))) {
        expr(stk);
      }
      Curry._1(next$1, /* () */0);
      var top = opos[0];
      if (nextis(/* Op */Block.__(0, [";"]))) {
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
    patch(/* true */1, match[0], opos[0]);
    stmt(/* tuple */[
          bl,
          ba
        ], stk);
    out(233);
    le(32, (match[1] - opos[0] | 0) - 4 | 0);
    return patch(/* true */1, bl[0], opos[0]);
  } else if (Caml_obj.caml_equal(t, tokret)) {
    if (!nextis(/* Op */Block.__(0, [";"]))) {
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
            [
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
  } else if (t.tag) {
    exit = 1;
  } else {
    switch (t[0]) {
      case ";" : 
          return /* () */0;
      case "{" : 
          return block(brk, stk);
      default:
        exit = 1;
    }
  }
  if (exit === 1) {
    Curry._1(unnext, t);
    expr(stk);
    Curry._1(next$1, /* () */0);
    return /* () */0;
  }
  
}

function block(brk, stk) {
  var match = decl(/* false */0, 0, stk);
  var stk$prime = match[1];
  var n = match[0];
  while(!nextis(/* Op */Block.__(0, ["}"]))) {
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
    if (nextis(/* Op */Block.__(0, ["EOF!"]))) {
      return 0;
    } else if (nextis(tokint)) {
      decl(/* true */1, 0, /* [] */0);
      _param = /* () */0;
      continue ;
    } else {
      var match = Curry._1(next$1, /* () */0);
      if (match.tag === 3) {
        var f = match[0];
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
            switch (match.tag | 0) {
              case 0 : 
                  if (match[0] === ")") {
                    return stk;
                  } else {
                    throw [
                          Caml_builtin_exceptions.failure,
                          "[var] or ) expected"
                        ];
                  }
              case 1 : 
              case 2 : 
                  throw [
                        Caml_builtin_exceptions.failure,
                        "[var] or ) expected"
                      ];
              case 3 : 
                  var r = List.hd(regs);
                  push(r);
                  if (nextis(/* Op */Block.__(0, [","]))) {
                    Curry._1(next$1, /* () */0);
                  }
                  var stk$prime_000 = /* tuple */[
                    match[0],
                    ((-n | 0) << 3)
                  ];
                  var stk$prime = /* :: */[
                    stk$prime_000,
                    stk
                  ];
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
        var stk = emitargs(/* :: */[
              7,
              /* :: */[
                6,
                /* :: */[
                  2,
                  /* :: */[
                    1,
                    /* :: */[
                      8,
                      /* :: */[
                        9,
                        /* [] */0
                      ]
                    ]
                  ]
                ]
              ]
            ], 1, /* [] */0);
        while(Caml_obj.caml_notequal(Curry._1(next$1, /* () */0), /* Op */Block.__(0, ["{"]))) {
          
        };
        retl[0] = 0;
        block(/* tuple */[
              [0],
              0
            ], stk);
        patch(/* true */1, retl[0], opos[0]);
        out(51651);
        if (dbg[0]) {
          Curry._1(Printf.eprintf(/* Format */[
                    /* String_literal */Block.__(11, [
                        "done with function ",
                        /* String */Block.__(2, [
                            /* No_padding */0,
                            /* Char_literal */Block.__(12, [
                                /* "\n" */10,
                                /* End_of_format */0
                              ])
                          ])
                      ]),
                    "done with function %s\n"
                  ]), Curry._1(symstr, f));
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

var elfhdr = Bytes.of_string($$String.concat("", /* :: */[
          "\x7fELF\x02\x01\x01\0",
          /* :: */[
            "\0\0\0\0\0\0\0\0",
            /* :: */[
              "\x02\0",
              /* :: */[
                ">\0",
                /* :: */[
                  "\x01\0\0\0",
                  /* :: */[
                    "\0\0\0\0\0\0\0\0",
                    /* :: */[
                      "@\0\0\0\0\0\0\0",
                      /* :: */[
                        "\0\0\0\0\0\0\0\0",
                        /* :: */[
                          "\0\0\0\0",
                          /* :: */[
                            "@\0",
                            /* :: */[
                              "8\0",
                              /* :: */[
                                "\x03\0",
                                /* :: */[
                                  "@\0",
                                  /* :: */[
                                    "\0\0",
                                    /* :: */[
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
  le(64, off + 4194304 | 0);
  le(64, off + 4194304 | 0);
  le(64, sz);
  le(64, sz);
  return le(64, align);
}

function elfgen(outf) {
  var entry = opos[0];
  var main = Curry._1(addsym, "main");
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
    return Curry._1(symitr, (function (i, s) {
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
  var patchloc = function (i, _) {
    var g = Caml_array.caml_array_get(globs, i);
    if (g[/* va */1] >= 0 && g[/* va */1] < 4194304) {
      return patch(/* false */0, g[/* loc */0], va(g[/* va */1]));
    } else if (g[/* va */1] >= 0) {
      return patch(/* false */0, g[/* loc */0], g[/* va */1]);
    } else {
      return 0;
    }
  };
  Curry._1(symitr, patchloc);
  var strtab = opos[0];
  opos[0] = opos[0] + 1 | 0;
  $$String.blit("/lib64/ld-linux-x86-64.so.2\0libc.so.6", 0, obuf, opos[0], 37);
  opos[0] = (opos[0] + 37 | 0) + 1 | 0;
  itr((function (s, sl, _) {
          $$String.blit(s, 0, obuf, opos[0], sl);
          opos[0] = (opos[0] + sl | 0) + 1 | 0;
          return /* () */0;
        }));
  opos[0] = opos[0] + 7 & -8;
  var symtab = opos[0];
  var n = [39];
  opos[0] = opos[0] + 24 | 0;
  itr((function (_, sl, _$1) {
          le(32, n[0]);
          le(32, 16);
          le(64, 0);
          le(64, 0);
          n[0] = (n[0] + sl | 0) + 1 | 0;
          return /* () */0;
        }));
  var rel = opos[0];
  var n$1 = [1];
  itr((function (_, _$1, l) {
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
        }), /* :: */[
        1,
        /* :: */[
          29,
          /* :: */[
            4,
            /* :: */[
              va(hash),
              /* :: */[
                5,
                /* :: */[
                  va(strtab),
                  /* :: */[
                    6,
                    /* :: */[
                      va(symtab),
                      /* :: */[
                        7,
                        /* :: */[
                          va(rel),
                          /* :: */[
                            8,
                            /* :: */[
                              hash - rel | 0,
                              /* :: */[
                                9,
                                /* :: */[
                                  24,
                                  /* :: */[
                                    10,
                                    /* :: */[
                                      symtab - strtab | 0,
                                      /* :: */[
                                        11,
                                        /* :: */[
                                          24,
                                          /* :: */[
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
          [
            "qcc.ml",
            698,
            2
          ]
        ];
  }
  patch(/* false */0, 24, va(entry));
  return Pervasives.output_bytes(outf, Bytes.sub(obuf, 0, tend + off | 0));
}

function main() {
  var ppsym = function (param) {
    switch (param.tag | 0) {
      case 0 : 
          return Curry._1(Printf.printf(/* Format */[
                          /* String_literal */Block.__(11, [
                              "Operator '",
                              /* String */Block.__(2, [
                                  /* No_padding */0,
                                  /* String_literal */Block.__(11, [
                                      "'\n",
                                      /* End_of_format */0
                                    ])
                                ])
                            ]),
                          "Operator '%s'\n"
                        ]), param[0]);
      case 1 : 
          return Curry._1(Printf.printf(/* Format */[
                          /* String_literal */Block.__(11, [
                              "Int literal ",
                              /* Int */Block.__(4, [
                                  /* Int_d */0,
                                  /* No_padding */0,
                                  /* No_precision */0,
                                  /* Char_literal */Block.__(12, [
                                      /* "\n" */10,
                                      /* End_of_format */0
                                    ])
                                ])
                            ]),
                          "Int literal %d\n"
                        ]), param[0]);
      case 2 : 
          return Curry._1(Printf.printf(/* Format */[
                          /* String_literal */Block.__(11, [
                              "Str literal ",
                              /* Caml_string */Block.__(3, [
                                  /* No_padding */0,
                                  /* Char_literal */Block.__(12, [
                                      /* "\n" */10,
                                      /* End_of_format */0
                                    ])
                                ])
                            ]),
                          "Str literal %S\n"
                        ]), param[1]);
      case 3 : 
          var i = param[0];
          return Curry._2(Printf.printf(/* Format */[
                          /* String_literal */Block.__(11, [
                              "Symbol '",
                              /* String */Block.__(2, [
                                  /* No_padding */0,
                                  /* String_literal */Block.__(11, [
                                      "' (",
                                      /* Int */Block.__(4, [
                                          /* Int_d */0,
                                          /* No_padding */0,
                                          /* No_precision */0,
                                          /* String_literal */Block.__(11, [
                                              ")\n",
                                              /* End_of_format */0
                                            ])
                                        ])
                                    ])
                                ])
                            ]),
                          "Symbol '%s' (%d)\n"
                        ]), Curry._1(symstr, i), i);
      
    }
  };
  var f = Sys.argv.length < 2 ? "-blk" : Caml_array.caml_array_get(Sys.argv, 1);
  switch (f) {
    case "-blk" : 
        var partial_arg_000 = [0];
        var partial_arg = /* tuple */[
          partial_arg_000,
          0
        ];
        var c = function (param) {
          return block(partial_arg, param);
        };
        var stk = /* [] */0;
        opos[0] = 0;
        Curry._1(c, stk);
        return Pervasives.print_bytes(Bytes.sub(obuf, 0, opos[0]));
    case "-lex" : 
        var _param = /* () */0;
        while(true) {
          var tok = Curry._1(next$1, /* () */0);
          if (tok.tag) {
            ppsym(tok);
            _param = /* () */0;
            continue ;
          } else if (tok[0] === "EOF!") {
            return Printf.printf(/* Format */[
                        /* String_literal */Block.__(11, [
                            "End of input stream\n",
                            /* End_of_format */0
                          ]),
                        "End of input stream\n"
                      ]);
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
      return Caml_missing_polyfill.not_implemented("caml_ml_close_channel not implemented by bucklescript yet\n");
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
