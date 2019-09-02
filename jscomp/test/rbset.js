'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function blackify(s) {
  if (s !== "Empty" && s.Arg0 !== "Black") {
    return /* tuple */[
            /* constructor */{
              tag: "Node",
              Arg0: "Black",
              Arg1: s.Arg1,
              Arg2: s.Arg2,
              Arg3: s.Arg3
            },
            false
          ];
  } else {
    return /* tuple */[
            s,
            true
          ];
  }
}

function is_empty(param) {
  return param === "Empty";
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var y = param.Arg2;
      if (x === y) {
        return true;
      } else if (x < y) {
        _param = param.Arg1;
        continue ;
      } else {
        _param = param.Arg3;
        continue ;
      }
    } else {
      return false;
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
  if (l !== "Empty" && l.Arg0 !== "Black") {
    var a$1 = l.Arg1;
    if (a$1 !== "Empty" && a$1.Arg0 !== "Black") {
      a = a$1.Arg1;
      x$1 = a$1.Arg2;
      b = a$1.Arg3;
      y = l.Arg2;
      c = l.Arg3;
      z = x;
      d = r;
      exit = 2;
    }
    var match = l.Arg3;
    if (match !== "Empty" && match.Arg0 !== "Black") {
      a = a$1;
      x$1 = l.Arg2;
      b = match.Arg1;
      y = match.Arg2;
      c = match.Arg3;
      z = x;
      d = r;
      exit = 2;
    } else {
      exit = 1;
    }
  } else {
    exit = 1;
  }
  switch (exit) {
    case 1 :
        return /* constructor */{
                tag: "Node",
                Arg0: "Black",
                Arg1: l,
                Arg2: x,
                Arg3: r
              };
    case 2 :
        return /* constructor */{
                tag: "Node",
                Arg0: "Red",
                Arg1: /* constructor */{
                  tag: "Node",
                  Arg0: "Black",
                  Arg1: a,
                  Arg2: x$1,
                  Arg3: b
                },
                Arg2: y,
                Arg3: /* constructor */{
                  tag: "Node",
                  Arg0: "Black",
                  Arg1: c,
                  Arg2: z,
                  Arg3: d
                }
              };
    
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
  if (r !== "Empty" && r.Arg0 !== "Black") {
    var b$1 = r.Arg1;
    if (b$1 !== "Empty" && b$1.Arg0 !== "Black") {
      a = l;
      x$1 = x;
      b = b$1.Arg1;
      y = b$1.Arg2;
      c = b$1.Arg3;
      z = r.Arg2;
      d = r.Arg3;
      exit = 2;
    }
    var match = r.Arg3;
    if (match !== "Empty" && match.Arg0 !== "Black") {
      a = l;
      x$1 = x;
      b = b$1;
      y = r.Arg2;
      c = match.Arg1;
      z = match.Arg2;
      d = match.Arg3;
      exit = 2;
    } else {
      exit = 1;
    }
  } else {
    exit = 1;
  }
  switch (exit) {
    case 1 :
        return /* constructor */{
                tag: "Node",
                Arg0: "Black",
                Arg1: l,
                Arg2: x,
                Arg3: r
              };
    case 2 :
        return /* constructor */{
                tag: "Node",
                Arg0: "Red",
                Arg1: /* constructor */{
                  tag: "Node",
                  Arg0: "Black",
                  Arg1: a,
                  Arg2: x$1,
                  Arg3: b
                },
                Arg2: y,
                Arg3: /* constructor */{
                  tag: "Node",
                  Arg0: "Black",
                  Arg1: c,
                  Arg2: z,
                  Arg3: d
                }
              };
    
  }
}

function singleton(x) {
  return /* constructor */{
          tag: "Node",
          Arg0: "Black",
          Arg1: "Empty",
          Arg2: x,
          Arg3: "Empty"
        };
}

function unbalanced_left(param) {
  if (param !== "Empty") {
    if (param.Arg0 !== "Black") {
      var match = param.Arg1;
      if (match !== "Empty" && match.Arg0 === "Black") {
        return /* tuple */[
                balance_left(/* constructor */{
                      tag: "Node",
                      Arg0: "Red",
                      Arg1: match.Arg1,
                      Arg2: match.Arg2,
                      Arg3: match.Arg3
                    }, param.Arg2, param.Arg3),
                false
              ];
      }
      
    } else {
      var match$1 = param.Arg1;
      if (match$1 !== "Empty") {
        if (match$1.Arg0 !== "Black") {
          var match$2 = match$1.Arg3;
          if (match$2 !== "Empty" && match$2.Arg0 === "Black") {
            return /* tuple */[
                    /* constructor */{
                      tag: "Node",
                      Arg0: "Black",
                      Arg1: match$1.Arg1,
                      Arg2: match$1.Arg2,
                      Arg3: balance_left(/* constructor */{
                            tag: "Node",
                            Arg0: "Red",
                            Arg1: match$2.Arg1,
                            Arg2: match$2.Arg2,
                            Arg3: match$2.Arg3
                          }, param.Arg2, param.Arg3)
                    },
                    false
                  ];
          }
          
        } else {
          return /* tuple */[
                  balance_left(/* constructor */{
                        tag: "Node",
                        Arg0: "Red",
                        Arg1: match$1.Arg1,
                        Arg2: match$1.Arg2,
                        Arg3: match$1.Arg3
                      }, param.Arg2, param.Arg3),
                  true
                ];
        }
      }
      
    }
  }
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "rbset.ml",
          57,
          9
        ]
      ];
}

function unbalanced_right(param) {
  if (param !== "Empty") {
    if (param.Arg0 !== "Black") {
      var match = param.Arg3;
      if (match !== "Empty" && match.Arg0 === "Black") {
        return /* tuple */[
                balance_right(param.Arg1, param.Arg2, /* constructor */{
                      tag: "Node",
                      Arg0: "Red",
                      Arg1: match.Arg1,
                      Arg2: match.Arg2,
                      Arg3: match.Arg3
                    }),
                false
              ];
      }
      
    } else {
      var match$1 = param.Arg3;
      if (match$1 !== "Empty") {
        var x = param.Arg2;
        var a = param.Arg1;
        if (match$1.Arg0 !== "Black") {
          var match$2 = match$1.Arg1;
          if (match$2 !== "Empty" && match$2.Arg0 === "Black") {
            return /* tuple */[
                    /* constructor */{
                      tag: "Node",
                      Arg0: "Black",
                      Arg1: balance_right(a, x, /* constructor */{
                            tag: "Node",
                            Arg0: "Red",
                            Arg1: match$2.Arg1,
                            Arg2: match$2.Arg2,
                            Arg3: match$2.Arg3
                          }),
                      Arg2: match$1.Arg2,
                      Arg3: match$1.Arg3
                    },
                    false
                  ];
          }
          
        } else {
          return /* tuple */[
                  balance_right(a, x, /* constructor */{
                        tag: "Node",
                        Arg0: "Red",
                        Arg1: match$1.Arg1,
                        Arg2: match$1.Arg2,
                        Arg3: match$1.Arg3
                      }),
                  true
                ];
        }
      }
      
    }
  }
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "rbset.ml",
          63,
          9
        ]
      ];
}

function lbalance(x1, x2, x3) {
  if (x1 !== "Empty") {
    if (x1.Arg0 !== "Black") {
      var r = x1.Arg3;
      var l = x1.Arg1;
      if (l !== "Empty" && l.Arg0 !== "Black") {
        return /* constructor */{
                tag: "Node",
                Arg0: "Red",
                Arg1: /* constructor */{
                  tag: "Node",
                  Arg0: "Black",
                  Arg1: l.Arg1,
                  Arg2: l.Arg2,
                  Arg3: l.Arg3
                },
                Arg2: x1.Arg2,
                Arg3: /* constructor */{
                  tag: "Node",
                  Arg0: "Black",
                  Arg1: r,
                  Arg2: x2,
                  Arg3: x3
                }
              };
      }
      if (r !== "Empty") {
        if (r.Arg0 !== "Black") {
          var y = r.Arg2;
          return /* constructor */{
                  tag: "Node",
                  Arg0: "Red",
                  Arg1: /* constructor */{
                    tag: "Node",
                    Arg0: "Black",
                    Arg1: l,
                    Arg2: y,
                    Arg3: r.Arg1
                  },
                  Arg2: y,
                  Arg3: /* constructor */{
                    tag: "Node",
                    Arg0: "Black",
                    Arg1: r.Arg3,
                    Arg2: x2,
                    Arg3: x3
                  }
                };
        } else {
          return /* constructor */{
                  tag: "Node",
                  Arg0: "Black",
                  Arg1: x1,
                  Arg2: x2,
                  Arg3: x3
                };
        }
      } else {
        return /* constructor */{
                tag: "Node",
                Arg0: "Black",
                Arg1: x1,
                Arg2: x2,
                Arg3: x3
              };
      }
    } else {
      return /* constructor */{
              tag: "Node",
              Arg0: "Black",
              Arg1: x1,
              Arg2: x2,
              Arg3: x3
            };
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: "Black",
            Arg1: x1,
            Arg2: x2,
            Arg3: x3
          };
  }
}

function rbalance(x1, x2, x3) {
  if (x3 !== "Empty" && x3.Arg0 !== "Black") {
    var b = x3.Arg1;
    var exit = 0;
    if (b !== "Empty" && b.Arg0 !== "Black") {
      return /* constructor */{
              tag: "Node",
              Arg0: "Red",
              Arg1: /* constructor */{
                tag: "Node",
                Arg0: "Black",
                Arg1: x1,
                Arg2: x2,
                Arg3: b.Arg1
              },
              Arg2: b.Arg2,
              Arg3: /* constructor */{
                tag: "Node",
                Arg0: "Black",
                Arg1: b.Arg3,
                Arg2: x3.Arg2,
                Arg3: x3.Arg3
              }
            };
    } else {
      exit = 2;
    }
    if (exit === 2) {
      var match = x3.Arg3;
      if (match !== "Empty" && match.Arg0 !== "Black") {
        return /* constructor */{
                tag: "Node",
                Arg0: "Red",
                Arg1: /* constructor */{
                  tag: "Node",
                  Arg0: "Black",
                  Arg1: x1,
                  Arg2: x2,
                  Arg3: b
                },
                Arg2: x3.Arg2,
                Arg3: /* constructor */{
                  tag: "Node",
                  Arg0: "Black",
                  Arg1: match.Arg1,
                  Arg2: match.Arg2,
                  Arg3: match.Arg3
                }
              };
      }
      
    }
    
  }
  return /* constructor */{
          tag: "Node",
          Arg0: "Black",
          Arg1: x1,
          Arg2: x2,
          Arg3: x3
        };
}

function ins(x, s) {
  if (s !== "Empty") {
    if (s.Arg0 !== "Black") {
      var y = s.Arg2;
      if (x === y) {
        return s;
      } else {
        var b = s.Arg3;
        var a = s.Arg1;
        if (x < y) {
          return /* constructor */{
                  tag: "Node",
                  Arg0: "Red",
                  Arg1: ins(x, a),
                  Arg2: y,
                  Arg3: b
                };
        } else {
          return /* constructor */{
                  tag: "Node",
                  Arg0: "Red",
                  Arg1: a,
                  Arg2: y,
                  Arg3: ins(x, b)
                };
        }
      }
    } else {
      var y$1 = s.Arg2;
      if (x === y$1) {
        return s;
      } else {
        var b$1 = s.Arg3;
        var a$1 = s.Arg1;
        if (x < y$1) {
          return lbalance(ins(x, a$1), y$1, b$1);
        } else {
          return rbalance(a$1, y$1, ins(x, b$1));
        }
      }
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: "Red",
            Arg1: "Empty",
            Arg2: x,
            Arg3: "Empty"
          };
  }
}

function add(x, s) {
  var s$1 = ins(x, s);
  if (s$1 !== "Empty" && s$1.Arg0 !== "Black") {
    return /* constructor */{
            tag: "Node",
            Arg0: "Black",
            Arg1: s$1.Arg1,
            Arg2: s$1.Arg2,
            Arg3: s$1.Arg3
          };
  } else {
    return s$1;
  }
}

function remove_min(param) {
  if (param !== "Empty") {
    var c = param.Arg0;
    if (c !== "Black") {
      if (param.Arg1 === "Empty") {
        return /* tuple */[
                param.Arg3,
                param.Arg2,
                false
              ];
      }
      
    } else if (param.Arg1 === "Empty") {
      var match = param.Arg3;
      var x = param.Arg2;
      if (match !== "Empty") {
        if (match.Arg0 !== "Black") {
          return /* tuple */[
                  /* constructor */{
                    tag: "Node",
                    Arg0: "Black",
                    Arg1: match.Arg1,
                    Arg2: match.Arg2,
                    Arg3: match.Arg3
                  },
                  x,
                  false
                ];
        } else {
          throw [
                Caml_builtin_exceptions.assert_failure,
                /* tuple */[
                  "rbset.ml",
                  115,
                  4
                ]
              ];
        }
      } else {
        return /* tuple */[
                "Empty",
                x,
                true
              ];
      }
    }
    var match$1 = remove_min(param.Arg1);
    var y = match$1[1];
    var s = /* constructor */{
      tag: "Node",
      Arg0: c,
      Arg1: match$1[0],
      Arg2: param.Arg2,
      Arg3: param.Arg3
    };
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
              false
            ];
    }
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "rbset.ml",
            115,
            4
          ]
        ];
  }
}

function remove_aux(x, n) {
  if (n !== "Empty") {
    var r = n.Arg3;
    var y = n.Arg2;
    var l = n.Arg1;
    var c = n.Arg0;
    if (x === y) {
      if (r !== "Empty") {
        var match = remove_min(r);
        var n$1 = /* constructor */{
          tag: "Node",
          Arg0: c,
          Arg1: l,
          Arg2: match[1],
          Arg3: match[0]
        };
        if (match[2]) {
          return unbalanced_left(n$1);
        } else {
          return /* tuple */[
                  n$1,
                  false
                ];
        }
      } else if (c === "Red") {
        return /* tuple */[
                l,
                false
              ];
      } else {
        return blackify(l);
      }
    } else if (x < y) {
      var match$1 = remove_aux(x, l);
      var n$2 = /* constructor */{
        tag: "Node",
        Arg0: c,
        Arg1: match$1[0],
        Arg2: y,
        Arg3: r
      };
      if (match$1[1]) {
        return unbalanced_right(n$2);
      } else {
        return /* tuple */[
                n$2,
                false
              ];
      }
    } else {
      var match$2 = remove_aux(x, r);
      var n$3 = /* constructor */{
        tag: "Node",
        Arg0: c,
        Arg1: l,
        Arg2: y,
        Arg3: match$2[0]
      };
      if (match$2[1]) {
        return unbalanced_left(n$3);
      } else {
        return /* tuple */[
                n$3,
                false
              ];
      }
    }
  } else {
    return /* tuple */[
            "Empty",
            false
          ];
  }
}

function remove(x, s) {
  return remove_aux(x, s)[0];
}

function cardinal(param) {
  if (param !== "Empty") {
    return (1 + cardinal(param.Arg1) | 0) + cardinal(param.Arg3) | 0;
  } else {
    return 0;
  }
}

var empty = "Empty";

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
