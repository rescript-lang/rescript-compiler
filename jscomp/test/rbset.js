'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function blackify(s) {
  if (s) {
    if (s[0] !== 0) {
      return /* tuple */[
              /* Node */[
                /* Black */0,
                s[1],
                s[2],
                s[3]
              ],
              /* false */0
            ];
    } else {
      return /* tuple */[
              s,
              /* true */1
            ];
    }
  } else {
    return /* tuple */[
            s,
            /* true */1
          ];
  }
}

function is_empty(param) {
  if (param) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var y = param[2];
      if (x === y) {
        return /* true */1;
      } else if (x < y) {
        _param = param[1];
        continue ;
        
      } else {
        _param = param[3];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function balance_left(l, x, r) {
  var exit = 0;
  var a;
  var x$1;
  var b;
  var y;
  var c;
  var z;
  var d;
  if (l) {
    if (l[0] !== 0) {
      var a$1 = l[1];
      var exit$1 = 0;
      if (a$1) {
        if (a$1[0] !== 0) {
          a = a$1[1];
          x$1 = a$1[2];
          b = a$1[3];
          y = l[2];
          c = l[3];
          z = x;
          d = r;
          exit = 2;
        } else {
          exit$1 = 3;
        }
      } else {
        exit$1 = 3;
      }
      if (exit$1 === 3) {
        var match = l[3];
        if (match) {
          if (match[0] !== 0) {
            a = a$1;
            x$1 = l[2];
            b = match[1];
            y = match[2];
            c = match[3];
            z = x;
            d = r;
            exit = 2;
          } else {
            exit = 1;
          }
        } else {
          exit = 1;
        }
      }
      
    } else {
      exit = 1;
    }
  } else {
    exit = 1;
  }
  switch (exit) {
    case 1 : 
        return /* Node */[
                /* Black */0,
                l,
                x,
                r
              ];
    case 2 : 
        return /* Node */[
                /* Red */1,
                /* Node */[
                  /* Black */0,
                  a,
                  x$1,
                  b
                ],
                y,
                /* Node */[
                  /* Black */0,
                  c,
                  z,
                  d
                ]
              ];
    
  }
}

function balance_right(l, x, r) {
  var exit = 0;
  var a;
  var x$1;
  var b;
  var y;
  var c;
  var z;
  var d;
  if (r) {
    if (r[0] !== 0) {
      var b$1 = r[1];
      var exit$1 = 0;
      if (b$1) {
        if (b$1[0] !== 0) {
          a = l;
          x$1 = x;
          b = b$1[1];
          y = b$1[2];
          c = b$1[3];
          z = r[2];
          d = r[3];
          exit = 2;
        } else {
          exit$1 = 3;
        }
      } else {
        exit$1 = 3;
      }
      if (exit$1 === 3) {
        var match = r[3];
        if (match) {
          if (match[0] !== 0) {
            a = l;
            x$1 = x;
            b = b$1;
            y = r[2];
            c = match[1];
            z = match[2];
            d = match[3];
            exit = 2;
          } else {
            exit = 1;
          }
        } else {
          exit = 1;
        }
      }
      
    } else {
      exit = 1;
    }
  } else {
    exit = 1;
  }
  switch (exit) {
    case 1 : 
        return /* Node */[
                /* Black */0,
                l,
                x,
                r
              ];
    case 2 : 
        return /* Node */[
                /* Red */1,
                /* Node */[
                  /* Black */0,
                  a,
                  x$1,
                  b
                ],
                y,
                /* Node */[
                  /* Black */0,
                  c,
                  z,
                  d
                ]
              ];
    
  }
}

function singleton(x) {
  return /* Node */[
          /* Black */0,
          /* Empty */0,
          x,
          /* Empty */0
        ];
}

function unbalanced_left(param) {
  var exit = 0;
  if (param) {
    if (param[0] !== 0) {
      var match = param[1];
      if (match) {
        if (match[0] !== 0) {
          exit = 1;
        } else {
          return /* tuple */[
                  balance_left(/* Node */[
                        /* Red */1,
                        match[1],
                        match[2],
                        match[3]
                      ], param[2], param[3]),
                  /* false */0
                ];
        }
      } else {
        exit = 1;
      }
    } else {
      var match$1 = param[1];
      if (match$1) {
        if (match$1[0] !== 0) {
          var match$2 = match$1[3];
          if (match$2) {
            if (match$2[0] !== 0) {
              exit = 1;
            } else {
              return /* tuple */[
                      /* Node */[
                        /* Black */0,
                        match$1[1],
                        match$1[2],
                        balance_left(/* Node */[
                              /* Red */1,
                              match$2[1],
                              match$2[2],
                              match$2[3]
                            ], param[2], param[3])
                      ],
                      /* false */0
                    ];
            }
          } else {
            exit = 1;
          }
        } else {
          return /* tuple */[
                  balance_left(/* Node */[
                        /* Red */1,
                        match$1[1],
                        match$1[2],
                        match$1[3]
                      ], param[2], param[3]),
                  /* true */1
                ];
        }
      } else {
        exit = 1;
      }
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "rbset.ml",
            57,
            9
          ]
        ];
  }
  
}

function unbalanced_right(param) {
  var exit = 0;
  if (param) {
    if (param[0] !== 0) {
      var match = param[3];
      if (match) {
        if (match[0] !== 0) {
          exit = 1;
        } else {
          return /* tuple */[
                  balance_right(param[1], param[2], /* Node */[
                        /* Red */1,
                        match[1],
                        match[2],
                        match[3]
                      ]),
                  /* false */0
                ];
        }
      } else {
        exit = 1;
      }
    } else {
      var match$1 = param[3];
      if (match$1) {
        var x = param[2];
        var a = param[1];
        if (match$1[0] !== 0) {
          var match$2 = match$1[1];
          if (match$2) {
            if (match$2[0] !== 0) {
              exit = 1;
            } else {
              return /* tuple */[
                      /* Node */[
                        /* Black */0,
                        balance_right(a, x, /* Node */[
                              /* Red */1,
                              match$2[1],
                              match$2[2],
                              match$2[3]
                            ]),
                        match$1[2],
                        match$1[3]
                      ],
                      /* false */0
                    ];
            }
          } else {
            exit = 1;
          }
        } else {
          return /* tuple */[
                  balance_right(a, x, /* Node */[
                        /* Red */1,
                        match$1[1],
                        match$1[2],
                        match$1[3]
                      ]),
                  /* true */1
                ];
        }
      } else {
        exit = 1;
      }
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "rbset.ml",
            63,
            9
          ]
        ];
  }
  
}

function lbalance(x1, x2, x3) {
  if (x1) {
    if (x1[0] !== 0) {
      var r = x1[3];
      var l = x1[1];
      var exit = 0;
      if (l) {
        if (l[0] !== 0) {
          return /* Node */[
                  /* Red */1,
                  /* Node */[
                    /* Black */0,
                    l[1],
                    l[2],
                    l[3]
                  ],
                  x1[2],
                  /* Node */[
                    /* Black */0,
                    r,
                    x2,
                    x3
                  ]
                ];
        } else {
          exit = 1;
        }
      } else {
        exit = 1;
      }
      if (exit === 1) {
        if (r) {
          if (r[0] !== 0) {
            var y = r[2];
            return /* Node */[
                    /* Red */1,
                    /* Node */[
                      /* Black */0,
                      l,
                      y,
                      r[1]
                    ],
                    y,
                    /* Node */[
                      /* Black */0,
                      r[3],
                      x2,
                      x3
                    ]
                  ];
          } else {
            return /* Node */[
                    /* Black */0,
                    x1,
                    x2,
                    x3
                  ];
          }
        } else {
          return /* Node */[
                  /* Black */0,
                  x1,
                  x2,
                  x3
                ];
        }
      }
      
    } else {
      return /* Node */[
              /* Black */0,
              x1,
              x2,
              x3
            ];
    }
  } else {
    return /* Node */[
            /* Black */0,
            x1,
            x2,
            x3
          ];
  }
}

function rbalance(x1, x2, x3) {
  var exit = 0;
  if (x3) {
    if (x3[0] !== 0) {
      var b = x3[1];
      var exit$1 = 0;
      if (b) {
        if (b[0] !== 0) {
          return /* Node */[
                  /* Red */1,
                  /* Node */[
                    /* Black */0,
                    x1,
                    x2,
                    b[1]
                  ],
                  b[2],
                  /* Node */[
                    /* Black */0,
                    b[3],
                    x3[2],
                    x3[3]
                  ]
                ];
        } else {
          exit$1 = 2;
        }
      } else {
        exit$1 = 2;
      }
      if (exit$1 === 2) {
        var match = x3[3];
        if (match) {
          if (match[0] !== 0) {
            return /* Node */[
                    /* Red */1,
                    /* Node */[
                      /* Black */0,
                      x1,
                      x2,
                      b
                    ],
                    x3[2],
                    /* Node */[
                      /* Black */0,
                      match[1],
                      match[2],
                      match[3]
                    ]
                  ];
          } else {
            exit = 1;
          }
        } else {
          exit = 1;
        }
      }
      
    } else {
      exit = 1;
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    return /* Node */[
            /* Black */0,
            x1,
            x2,
            x3
          ];
  }
  
}

function ins(x, s) {
  if (s) {
    if (s[0] !== 0) {
      var y = s[2];
      if (x === y) {
        return s;
      } else {
        var b = s[3];
        var a = s[1];
        if (x < y) {
          return /* Node */[
                  /* Red */1,
                  ins(x, a),
                  y,
                  b
                ];
        } else {
          return /* Node */[
                  /* Red */1,
                  a,
                  y,
                  ins(x, b)
                ];
        }
      }
    } else {
      var y$1 = s[2];
      if (x === y$1) {
        return s;
      } else {
        var b$1 = s[3];
        var a$1 = s[1];
        if (x < y$1) {
          return lbalance(ins(x, a$1), y$1, b$1);
        } else {
          return rbalance(a$1, y$1, ins(x, b$1));
        }
      }
    }
  } else {
    return /* Node */[
            /* Red */1,
            /* Empty */0,
            x,
            /* Empty */0
          ];
  }
}

function add(x, s) {
  var s$1 = ins(x, s);
  if (s$1 && s$1[0] !== 0) {
    return /* Node */[
            /* Black */0,
            s$1[1],
            s$1[2],
            s$1[3]
          ];
  } else {
    return s$1;
  }
}

function remove_min(param) {
  if (param) {
    var c = param[0];
    var exit = 0;
    if (c !== 0) {
      if (param[1]) {
        exit = 1;
      } else {
        return /* tuple */[
                param[3],
                param[2],
                /* false */0
              ];
      }
    } else if (param[1]) {
      exit = 1;
    } else {
      var match = param[3];
      var x = param[2];
      if (match) {
        if (match[0] !== 0) {
          return /* tuple */[
                  /* Node */[
                    /* Black */0,
                    match[1],
                    match[2],
                    match[3]
                  ],
                  x,
                  /* false */0
                ];
        } else {
          throw [
                Caml_builtin_exceptions.assert_failure,
                [
                  "rbset.ml",
                  115,
                  4
                ]
              ];
        }
      } else {
        return /* tuple */[
                /* Empty */0,
                x,
                /* true */1
              ];
      }
    }
    if (exit === 1) {
      var match$1 = remove_min(param[1]);
      var y = match$1[1];
      var s_001 = match$1[0];
      var s_002 = param[2];
      var s_003 = param[3];
      var s = /* Node */[
        c,
        s_001,
        s_002,
        s_003
      ];
      if (match$1[2]) {
        var match$2 = unbalanced_right(s);
        return /* tuple */[
                match$2[0],
                y,
                match$2[1]
              ];
      } else {
        return /* tuple */[
                s,
                y,
                /* false */0
              ];
      }
    }
    
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "rbset.ml",
            115,
            4
          ]
        ];
  }
}

function remove_aux(x, n) {
  if (n) {
    var r = n[3];
    var y = n[2];
    var l = n[1];
    var c = n[0];
    if (x === y) {
      if (r) {
        var match = remove_min(r);
        var n_002 = match[1];
        var n_003 = match[0];
        var n$1 = /* Node */[
          c,
          l,
          n_002,
          n_003
        ];
        if (match[2]) {
          return unbalanced_left(n$1);
        } else {
          return /* tuple */[
                  n$1,
                  /* false */0
                ];
        }
      } else if (c === /* Red */1) {
        return /* tuple */[
                l,
                /* false */0
              ];
      } else {
        return blackify(l);
      }
    } else if (x < y) {
      var match$1 = remove_aux(x, l);
      var n_001 = match$1[0];
      var n$2 = /* Node */[
        c,
        n_001,
        y,
        r
      ];
      if (match$1[1]) {
        return unbalanced_right(n$2);
      } else {
        return /* tuple */[
                n$2,
                /* false */0
              ];
      }
    } else {
      var match$2 = remove_aux(x, r);
      var n_003$1 = match$2[0];
      var n$3 = /* Node */[
        c,
        l,
        y,
        n_003$1
      ];
      if (match$2[1]) {
        return unbalanced_left(n$3);
      } else {
        return /* tuple */[
                n$3,
                /* false */0
              ];
      }
    }
  } else {
    return /* tuple */[
            /* Empty */0,
            /* false */0
          ];
  }
}

function remove(x, s) {
  return remove_aux(x, s)[0];
}

function cardinal(param) {
  if (param) {
    return (1 + cardinal(param[1]) | 0) + cardinal(param[3]) | 0;
  } else {
    return 0;
  }
}

var empty = /* Empty */0;

exports.blackify = blackify;
exports.empty = empty;
exports.is_empty = is_empty;
exports.mem = mem;
exports.balance_left = balance_left;
exports.balance_right = balance_right;
exports.singleton = singleton;
exports.unbalanced_left = unbalanced_left;
exports.unbalanced_right = unbalanced_right;
exports.lbalance = lbalance;
exports.rbalance = rbalance;
exports.ins = ins;
exports.add = add;
exports.remove_min = remove_min;
exports.remove_aux = remove_aux;
exports.remove = remove;
exports.cardinal = cardinal;
/* No side effect */
