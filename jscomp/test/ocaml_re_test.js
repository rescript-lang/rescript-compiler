'use strict';

var Mt = require("./mt.js");
var Char = require("../../lib/js/char.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Bytes = require("../../lib/js/bytes.js");
var Curry = require("../../lib/js/curry.js");
var Format = require("../../lib/js/format.js");
var $$String = require("../../lib/js/string.js");
var Hashtbl = require("../../lib/js/hashtbl.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = /* record */[/* contents */"[]"];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: x,
                  Arg1: y
                };
        })
    ],
    Arg1: suites[0]
  };
  return /* () */0;
}

function union(_l, _l$prime) {
  while(true) {
    var l$prime = _l$prime;
    var l = _l;
    if (l$prime !== "[]") {
      if (l !== "[]") {
        var r$prime = l$prime.Arg1;
        var match = l$prime.Arg0;
        var c2$prime = match[1];
        var c1$prime = match[0];
        var r = l.Arg1;
        var match$1 = l.Arg0;
        var c2 = match$1[1];
        var c1 = match$1[0];
        if ((c2 + 1 | 0) < c1$prime) {
          return /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    c1,
                    c2
                  ],
                  Arg1: union(r, l$prime)
                };
        } else if ((c2$prime + 1 | 0) < c1) {
          return /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    c1$prime,
                    c2$prime
                  ],
                  Arg1: union(l, r$prime)
                };
        } else if (c2 < c2$prime) {
          _l$prime = /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              c1 < c1$prime ? c1 : c1$prime,
              c2$prime
            ],
            Arg1: r$prime
          };
          _l = r;
          continue ;
        } else {
          _l$prime = r$prime;
          _l = /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              c1 < c1$prime ? c1 : c1$prime,
              c2
            ],
            Arg1: r
          };
          continue ;
        }
      } else {
        return l$prime;
      }
    } else {
      return l;
    }
  };
}

function inter(_l, _l$prime) {
  while(true) {
    var l$prime = _l$prime;
    var l = _l;
    if (l$prime !== "[]" && l !== "[]") {
      var r$prime = l$prime.Arg1;
      var match = l$prime.Arg0;
      var c2$prime = match[1];
      var c1$prime = match[0];
      var r = l.Arg1;
      var match$1 = l.Arg0;
      var c2 = match$1[1];
      var c1 = match$1[0];
      if (Caml_obj.caml_lessthan(c2, c1$prime)) {
        _l = r;
        continue ;
      } else if (Caml_obj.caml_lessthan(c2$prime, c1)) {
        _l$prime = r$prime;
        continue ;
      } else if (Caml_obj.caml_lessthan(c2, c2$prime)) {
        return /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  Caml_obj.caml_max(c1, c1$prime),
                  c2
                ],
                Arg1: inter(r, l$prime)
              };
      } else {
        return /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  Caml_obj.caml_max(c1, c1$prime),
                  c2$prime
                ],
                Arg1: inter(l, r$prime)
              };
      }
    } else {
      return "[]";
    }
  };
}

function diff(_l, _l$prime) {
  while(true) {
    var l$prime = _l$prime;
    var l = _l;
    if (l$prime !== "[]") {
      if (l !== "[]") {
        var r$prime = l$prime.Arg1;
        var match = l$prime.Arg0;
        var c2$prime = match[1];
        var c1$prime = match[0];
        var r = l.Arg1;
        var match$1 = l.Arg0;
        var c2 = match$1[1];
        var c1 = match$1[0];
        if (c2 < c1$prime) {
          return /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    c1,
                    c2
                  ],
                  Arg1: diff(r, l$prime)
                };
        } else if (c2$prime < c1) {
          _l$prime = r$prime;
          continue ;
        } else {
          var r$prime$prime = c2$prime < c2 ? /* constructor */({
                tag: "::",
                Arg0: /* tuple */[
                  c2$prime + 1 | 0,
                  c2
                ],
                Arg1: r
              }) : r;
          if (c1 < c1$prime) {
            return /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      c1,
                      c1$prime - 1 | 0
                    ],
                    Arg1: diff(r$prime$prime, r$prime)
                  };
          } else {
            _l$prime = r$prime;
            _l = r$prime$prime;
            continue ;
          }
        }
      } else {
        return "[]";
      }
    } else {
      return l;
    }
  };
}

function single(c) {
  return /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            c,
            c
          ],
          Arg1: "[]"
        };
}

function seq(c, c$prime) {
  if (Caml_obj.caml_lessequal(c, c$prime)) {
    return /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              c,
              c$prime
            ],
            Arg1: "[]"
          };
  } else {
    return /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              c$prime,
              c
            ],
            Arg1: "[]"
          };
  }
}

function offset(o, l) {
  if (l !== "[]") {
    var match = l.Arg0;
    return /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              match[0] + o | 0,
              match[1] + o | 0
            ],
            Arg1: offset(o, l.Arg1)
          };
  } else {
    return "[]";
  }
}

function mem(c, _s) {
  while(true) {
    var s = _s;
    if (s !== "[]") {
      var match = s.Arg0;
      if (c <= match[1]) {
        return c >= match[0];
      } else {
        _s = s.Arg1;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function hash_rec(param) {
  if (param !== "[]") {
    var match = param.Arg0;
    return (match[0] + Caml_int32.imul(13, match[1]) | 0) + Caml_int32.imul(257, hash_rec(param.Arg1)) | 0;
  } else {
    return 0;
  }
}

function one_char(param) {
  if (param !== "[]" && param.Arg1 === "[]") {
    var match = param.Arg0;
    var i = match[0];
    if (Caml_obj.caml_equal(i, match[1])) {
      return Caml_option.some(i);
    } else {
      return ;
    }
  }
  
}

function compare(param, param$1) {
  var c = Caml_obj.caml_compare(param[0], param$1[0]);
  if (c !== 0) {
    return c;
  } else {
    return Caml_obj.caml_compare(param[1], param$1[1]);
  }
}

function height(param) {
  if (param !== "Empty") {
    return param.Arg4;
  } else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* constructor */{
          tag: "Node",
          Arg0: l,
          Arg1: x,
          Arg2: d,
          Arg3: r,
          Arg4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal(l, x, d, r) {
  var hl = l !== "Empty" ? l.Arg4 : 0;
  var hr = r !== "Empty" ? r.Arg4 : 0;
  if (hl > (hr + 2 | 0)) {
    if (l !== "Empty") {
      var lr = l.Arg3;
      var ld = l.Arg2;
      var lv = l.Arg1;
      var ll = l.Arg0;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      } else if (lr !== "Empty") {
        return create(create(ll, lv, ld, lr.Arg0), lr.Arg1, lr.Arg2, create(lr.Arg3, x, d, r));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r !== "Empty") {
      var rr = r.Arg3;
      var rd = r.Arg2;
      var rv = r.Arg1;
      var rl = r.Arg0;
      if (height(rr) >= height(rl)) {
        return create(create(l, x, d, rl), rv, rd, rr);
      } else if (rl !== "Empty") {
        return create(create(l, x, d, rl.Arg0), rl.Arg1, rl.Arg2, create(rl.Arg3, rv, rd, rr));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: l,
            Arg1: x,
            Arg2: d,
            Arg3: r,
            Arg4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
}

function add(x, data, param) {
  if (param !== "Empty") {
    var r = param.Arg3;
    var d = param.Arg2;
    var v = param.Arg1;
    var l = param.Arg0;
    var c = compare(x, v);
    if (c === 0) {
      return /* constructor */{
              tag: "Node",
              Arg0: l,
              Arg1: x,
              Arg2: data,
              Arg3: r,
              Arg4: param.Arg4
            };
    } else if (c < 0) {
      return bal(add(x, data, l), v, d, r);
    } else {
      return bal(l, v, d, add(x, data, r));
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: "Empty",
            Arg1: x,
            Arg2: data,
            Arg3: "Empty",
            Arg4: 1
          };
  }
}

var cany = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    0,
    255
  ],
  Arg1: "[]"
};

function intersect(x, y) {
  return (x & y) !== 0;
}

function $plus$plus(x, y) {
  return x | y;
}

function from_char(param) {
  if (param >= 170) {
    if (param >= 187) {
      var switcher = param - 192 | 0;
      if (switcher > 54 || switcher < 0) {
        if (switcher >= 56) {
          return 2;
        } else {
          return 4;
        }
      } else if (switcher !== 23) {
        return 2;
      } else {
        return 4;
      }
    } else {
      var switcher$1 = param - 171 | 0;
      if (!(switcher$1 > 14 || switcher$1 < 0) && switcher$1 !== 10) {
        return 4;
      } else {
        return 2;
      }
    }
  } else if (param >= 65) {
    var switcher$2 = param - 91 | 0;
    if (switcher$2 > 5 || switcher$2 < 0) {
      if (switcher$2 >= 32) {
        return 4;
      } else {
        return 2;
      }
    } else if (switcher$2 !== 4) {
      return 4;
    } else {
      return 2;
    }
  } else if (param >= 48) {
    if (param >= 58) {
      return 4;
    } else {
      return 2;
    }
  } else if (param !== 10) {
    return 4;
  } else {
    return 12;
  }
}

function height$1(param) {
  if (param !== "Empty") {
    return param.Arg3;
  } else {
    return 0;
  }
}

function create$1(l, v, r) {
  var hl = l !== "Empty" ? l.Arg3 : 0;
  var hr = r !== "Empty" ? r.Arg3 : 0;
  return /* constructor */{
          tag: "Node",
          Arg0: l,
          Arg1: v,
          Arg2: r,
          Arg3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal$1(l, v, r) {
  var hl = l !== "Empty" ? l.Arg3 : 0;
  var hr = r !== "Empty" ? r.Arg3 : 0;
  if (hl > (hr + 2 | 0)) {
    if (l !== "Empty") {
      var lr = l.Arg2;
      var lv = l.Arg1;
      var ll = l.Arg0;
      if (height$1(ll) >= height$1(lr)) {
        return create$1(ll, lv, create$1(lr, v, r));
      } else if (lr !== "Empty") {
        return create$1(create$1(ll, lv, lr.Arg0), lr.Arg1, create$1(lr.Arg2, v, r));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Set.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Set.bal"
          ];
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r !== "Empty") {
      var rr = r.Arg2;
      var rv = r.Arg1;
      var rl = r.Arg0;
      if (height$1(rr) >= height$1(rl)) {
        return create$1(create$1(l, v, rl), rv, rr);
      } else if (rl !== "Empty") {
        return create$1(create$1(l, v, rl.Arg0), rl.Arg1, create$1(rl.Arg2, rv, rr));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Set.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Set.bal"
          ];
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: l,
            Arg1: v,
            Arg2: r,
            Arg3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
}

function add$1(x, t) {
  if (t !== "Empty") {
    var r = t.Arg2;
    var v = t.Arg1;
    var l = t.Arg0;
    var c = Caml_primitive.caml_int_compare(x, v);
    if (c === 0) {
      return t;
    } else if (c < 0) {
      return bal$1(add$1(x, l), v, r);
    } else {
      return bal$1(l, v, add$1(x, r));
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: "Empty",
            Arg1: x,
            Arg2: "Empty",
            Arg3: 1
          };
  }
}

function hash_combine(h, accu) {
  return Caml_int32.imul(accu, 65599) + h | 0;
}

var empty = /* record */[
  /* marks */"[]",
  /* pmarks */"Empty"
];

function hash(m, accu) {
  var _l = m[/* marks */0];
  var _accu = hash_combine(Hashtbl.hash(m[/* pmarks */1]), accu);
  while(true) {
    var accu$1 = _accu;
    var l = _l;
    if (l !== "[]") {
      var match = l.Arg0;
      _accu = hash_combine(match[0], hash_combine(match[1], accu$1));
      _l = l.Arg1;
      continue ;
    } else {
      return accu$1;
    }
  };
}

function marks_set_idx(idx, marks) {
  if (marks !== "[]") {
    var match = marks.Arg0;
    if (match[1] !== -1) {
      return marks;
    } else {
      return /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                match[0],
                idx
              ],
              Arg1: marks_set_idx(idx, marks.Arg1)
            };
    }
  } else {
    return marks;
  }
}

function marks_set_idx$1(marks, idx) {
  return /* record */[
          /* marks */marks_set_idx(idx, marks[/* marks */0]),
          /* pmarks */marks[/* pmarks */1]
        ];
}

function first(f, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      var res = Curry._1(f, param.Arg0);
      if (res !== undefined) {
        return res;
      } else {
        _param = param.Arg1;
        continue ;
      }
    } else {
      return ;
    }
  };
}

var eps_expr = /* record */[
  /* id */0,
  /* def */"Eps"
];

function mk_expr(ids, def) {
  ids[0] = ids[0] + 1 | 0;
  return /* record */[
          /* id */ids[0],
          /* def */def
        ];
}

function cst(ids, s) {
  if (s === "[]") {
    return mk_expr(ids, /* constructor */{
                tag: "Alt",
                Arg0: "[]"
              });
  } else {
    return mk_expr(ids, /* constructor */{
                tag: "Cst",
                Arg0: s
              });
  }
}

function alt(ids, l) {
  if (l !== "[]") {
    if (l.Arg1 !== "[]") {
      return mk_expr(ids, /* constructor */{
                  tag: "Alt",
                  Arg0: l
                });
    } else {
      return l.Arg0;
    }
  } else {
    return mk_expr(ids, /* constructor */{
                tag: "Alt",
                Arg0: "[]"
              });
  }
}

function seq$1(ids, kind, x, y) {
  var match = x[/* def */1];
  var match$1 = y[/* def */1];
  var exit = 0;
  if (typeof match === "string") {
    return y;
  } else if (/* XXX */match.tag === "Alt" && match.Arg0 === "[]") {
    return x;
  } else {
    exit = 2;
  }
  if (exit === 2) {
    if (typeof match$1 === "string") {
      if (kind === /* First */332064784) {
        return x;
      }
      
    } else if (/* XXX */match$1.tag === "Alt" && match$1.Arg0 === "[]") {
      return y;
    }
    
  }
  return mk_expr(ids, /* constructor */{
              tag: "Seq",
              Arg0: kind,
              Arg1: x,
              Arg2: y
            });
}

function is_eps(expr) {
  var match = expr[/* def */1];
  if (typeof match === "string") {
    return true;
  } else {
    return false;
  }
}

function rep(ids, kind, sem, x) {
  return mk_expr(ids, /* constructor */{
              tag: "Rep",
              Arg0: kind,
              Arg1: sem,
              Arg2: x
            });
}

function erase(ids, m, m$prime) {
  return mk_expr(ids, /* constructor */{
              tag: "Erase",
              Arg0: m,
              Arg1: m$prime
            });
}

function rename(ids, x) {
  var match = x[/* def */1];
  if (typeof match === "string") {
    return mk_expr(ids, x[/* def */1]);
  } else {
    switch (/* XXX */match.tag) {
      case "Alt" :
          return mk_expr(ids, /* constructor */{
                      tag: "Alt",
                      Arg0: List.map((function (param) {
                              return rename(ids, param);
                            }), match.Arg0)
                    });
      case "Seq" :
          return mk_expr(ids, /* constructor */{
                      tag: "Seq",
                      Arg0: match.Arg0,
                      Arg1: rename(ids, match.Arg1),
                      Arg2: rename(ids, match.Arg2)
                    });
      case "Rep" :
          return mk_expr(ids, /* constructor */{
                      tag: "Rep",
                      Arg0: match.Arg0,
                      Arg1: match.Arg1,
                      Arg2: rename(ids, match.Arg2)
                    });
      default:
        return mk_expr(ids, x[/* def */1]);
    }
  }
}

function equal(_l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1 !== "[]") {
      var match = l1.Arg0;
      switch (/* XXX */match.tag) {
        case "TSeq" :
            if (l2 !== "[]") {
              var match$1 = l2.Arg0;
              switch (/* XXX */match$1.tag) {
                case "TSeq" :
                    if (match.Arg1[/* id */0] === match$1.Arg1[/* id */0] && equal(match.Arg0, match$1.Arg0)) {
                      _l2 = l2.Arg1;
                      _l1 = l1.Arg1;
                      continue ;
                    } else {
                      return false;
                    }
                case "TExp" :
                case "TMatch" :
                    return false;
                
              }
            } else {
              return false;
            }
        case "TExp" :
            if (l2 !== "[]") {
              var match$2 = l2.Arg0;
              switch (/* XXX */match$2.tag) {
                case "TExp" :
                    if (match.Arg1[/* id */0] === match$2.Arg1[/* id */0] && Caml_obj.caml_equal(match.Arg0, match$2.Arg0)) {
                      _l2 = l2.Arg1;
                      _l1 = l1.Arg1;
                      continue ;
                    } else {
                      return false;
                    }
                case "TSeq" :
                case "TMatch" :
                    return false;
                
              }
            } else {
              return false;
            }
        case "TMatch" :
            if (l2 !== "[]") {
              var match$3 = l2.Arg0;
              switch (/* XXX */match$3.tag) {
                case "TSeq" :
                case "TExp" :
                    return false;
                case "TMatch" :
                    if (Caml_obj.caml_equal(match.Arg0, match$3.Arg0)) {
                      _l2 = l2.Arg1;
                      _l1 = l1.Arg1;
                      continue ;
                    } else {
                      return false;
                    }
                
              }
            } else {
              return false;
            }
        
      }
    } else {
      return l2 === "[]";
    }
  };
}

function hash$1(_l, _accu) {
  while(true) {
    var accu = _accu;
    var l = _l;
    if (l !== "[]") {
      var match = l.Arg0;
      switch (/* XXX */match.tag) {
        case "TSeq" :
            _accu = hash_combine(388635598, hash_combine(match.Arg1[/* id */0], hash$1(match.Arg0, accu)));
            _l = l.Arg1;
            continue ;
        case "TExp" :
            _accu = hash_combine(726404471, hash_combine(match.Arg1[/* id */0], hash(match.Arg0, accu)));
            _l = l.Arg1;
            continue ;
        case "TMatch" :
            _accu = hash_combine(471882453, hash(match.Arg0, accu));
            _l = l.Arg1;
            continue ;
        
      }
    } else {
      return accu;
    }
  };
}

function tseq(kind, x, y, rem) {
  if (x !== "[]") {
    var match = x.Arg0;
    switch (/* XXX */match.tag) {
      case "TExp" :
          var tmp = match.Arg1[/* def */1];
          if (typeof tmp === "string" && x.Arg1 === "[]") {
            return /* constructor */{
                    tag: "::",
                    Arg0: /* constructor */{
                      tag: "TExp",
                      Arg0: match.Arg0,
                      Arg1: y
                    },
                    Arg1: rem
                  };
          }
          break;
      case "TSeq" :
      case "TMatch" :
          break;
      
    }
  } else {
    return rem;
  }
  return /* constructor */{
          tag: "::",
          Arg0: /* constructor */{
            tag: "TSeq",
            Arg0: x,
            Arg1: y,
            Arg2: kind
          },
          Arg1: rem
        };
}

var dummy = /* record */[
  /* idx */-1,
  /* category */-1,
  /* desc */"[]",
  /* status */undefined,
  /* hash */-1
];

function hash$2(idx, cat, desc) {
  return hash$1(desc, hash_combine(idx, hash_combine(cat, 0))) & 1073741823;
}

function mk(idx, cat, desc) {
  return /* record */[
          /* idx */idx,
          /* category */cat,
          /* desc */desc,
          /* status */undefined,
          /* hash */hash$2(idx, cat, desc)
        ];
}

function create$2(cat, e) {
  return mk(0, cat, /* constructor */{
              tag: "::",
              Arg0: /* constructor */{
                tag: "TExp",
                Arg0: empty,
                Arg1: e
              },
              Arg1: "[]"
            });
}

function equal$1(x, y) {
  if (x[/* hash */4] === y[/* hash */4] && x[/* idx */0] === y[/* idx */0] && x[/* category */1] === y[/* category */1]) {
    return equal(x[/* desc */2], y[/* desc */2]);
  } else {
    return false;
  }
}

function hash$3(t) {
  return t[/* hash */4];
}

var Table = Hashtbl.Make({
      equal: equal$1,
      hash: hash$3
    });

function reset_table(a) {
  return $$Array.fill(a, 0, a.length, false);
}

function mark_used_indices(tbl) {
  return (function (param) {
      return List.iter((function (param) {
                    switch (/* XXX */param.tag) {
                      case "TSeq" :
                          return mark_used_indices(tbl)(param.Arg0);
                      case "TExp" :
                      case "TMatch" :
                          break;
                      
                    }
                    return List.iter((function (param) {
                                  var i = param[1];
                                  if (i >= 0) {
                                    return Caml_array.caml_array_set(tbl, i, true);
                                  } else {
                                    return 0;
                                  }
                                }), param.Arg0[/* marks */0]);
                  }), param);
    });
}

function find_free(tbl, _idx, len) {
  while(true) {
    var idx = _idx;
    if (idx === len || !Caml_array.caml_array_get(tbl, idx)) {
      return idx;
    } else {
      _idx = idx + 1 | 0;
      continue ;
    }
  };
}

function free_index(tbl_ref, l) {
  var tbl = tbl_ref[0];
  reset_table(tbl);
  mark_used_indices(tbl)(l);
  var len = tbl.length;
  var idx = find_free(tbl, 0, len);
  if (idx === len) {
    tbl_ref[0] = Caml_array.caml_make_vect((len << 1), false);
  }
  return idx;
}

var remove_matches = List.filter((function (param) {
        switch (/* XXX */param.tag) {
          case "TSeq" :
          case "TExp" :
              return true;
          case "TMatch" :
              return false;
          
        }
      }));

function split_at_match_rec(_l$prime, _param) {
  while(true) {
    var param = _param;
    var l$prime = _l$prime;
    if (param !== "[]") {
      var x = param.Arg0;
      switch (/* XXX */x.tag) {
        case "TSeq" :
        case "TExp" :
            _param = param.Arg1;
            _l$prime = /* constructor */{
              tag: "::",
              Arg0: x,
              Arg1: l$prime
            };
            continue ;
        case "TMatch" :
            return /* tuple */[
                    List.rev(l$prime),
                    Curry._1(remove_matches, param.Arg1)
                  ];
        
      }
    } else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            /* tuple */[
              "re_automata.ml",
              429,
              21
            ]
          ];
    }
  };
}

function remove_duplicates(prev, _l, y) {
  while(true) {
    var l = _l;
    if (l !== "[]") {
      var x = l.Arg0;
      switch (/* XXX */x.tag) {
        case "TSeq" :
            var x$1 = x.Arg1;
            var match = remove_duplicates(prev, x.Arg0, x$1);
            var match$1 = remove_duplicates(match[1], l.Arg1, y);
            return /* tuple */[
                    tseq(x.Arg2, match[0], x$1, match$1[0]),
                    match$1[1]
                  ];
        case "TExp" :
            var x$2 = x.Arg1;
            var tmp = x$2[/* def */1];
            if (typeof tmp === "string") {
              var r = l.Arg1;
              if (List.memq(y[/* id */0], prev)) {
                _l = r;
                continue ;
              } else {
                var match$2 = remove_duplicates(/* constructor */{
                      tag: "::",
                      Arg0: y[/* id */0],
                      Arg1: prev
                    }, r, y);
                return /* tuple */[
                        /* constructor */{
                          tag: "::",
                          Arg0: x,
                          Arg1: match$2[0]
                        },
                        match$2[1]
                      ];
              }
            } else {
              var r$1 = l.Arg1;
              if (List.memq(x$2[/* id */0], prev)) {
                _l = r$1;
                continue ;
              } else {
                var match$3 = remove_duplicates(/* constructor */{
                      tag: "::",
                      Arg0: x$2[/* id */0],
                      Arg1: prev
                    }, r$1, y);
                return /* tuple */[
                        /* constructor */{
                          tag: "::",
                          Arg0: x,
                          Arg1: match$3[0]
                        },
                        match$3[1]
                      ];
              }
            }
        case "TMatch" :
            return /* tuple */[
                    /* constructor */{
                      tag: "::",
                      Arg0: x,
                      Arg1: "[]"
                    },
                    prev
                  ];
        
      }
    } else {
      return /* tuple */[
              "[]",
              prev
            ];
    }
  };
}

function set_idx(idx, param) {
  if (param !== "[]") {
    var match = param.Arg0;
    switch (/* XXX */match.tag) {
      case "TSeq" :
          return /* constructor */{
                  tag: "::",
                  Arg0: /* constructor */{
                    tag: "TSeq",
                    Arg0: set_idx(idx, match.Arg0),
                    Arg1: match.Arg1,
                    Arg2: match.Arg2
                  },
                  Arg1: set_idx(idx, param.Arg1)
                };
      case "TExp" :
          return /* constructor */{
                  tag: "::",
                  Arg0: /* constructor */{
                    tag: "TExp",
                    Arg0: marks_set_idx$1(match.Arg0, idx),
                    Arg1: match.Arg1
                  },
                  Arg1: set_idx(idx, param.Arg1)
                };
      case "TMatch" :
          return /* constructor */{
                  tag: "::",
                  Arg0: /* constructor */{
                    tag: "TMatch",
                    Arg0: marks_set_idx$1(match.Arg0, idx)
                  },
                  Arg1: set_idx(idx, param.Arg1)
                };
      
    }
  } else {
    return "[]";
  }
}

function filter_marks(b, e, marks) {
  return /* record */[
          /* marks */List.filter((function (param) {
                    var i = param[0];
                    if (i < b) {
                      return true;
                    } else {
                      return i > e;
                    }
                  }))(marks[/* marks */0]),
          /* pmarks */marks[/* pmarks */1]
        ];
}

function delta_1(marks, c, next_cat, prev_cat, x, rem) {
  var match = x[/* def */1];
  if (typeof match === "string") {
    return /* constructor */{
            tag: "::",
            Arg0: /* constructor */{
              tag: "TMatch",
              Arg0: marks
            },
            Arg1: rem
          };
  } else {
    switch (/* XXX */match.tag) {
      case "Cst" :
          if (mem(c, match.Arg0)) {
            return /* constructor */{
                    tag: "::",
                    Arg0: /* constructor */{
                      tag: "TExp",
                      Arg0: marks,
                      Arg1: eps_expr
                    },
                    Arg1: rem
                  };
          } else {
            return rem;
          }
      case "Alt" :
          return delta_2(marks, c, next_cat, prev_cat, match.Arg0, rem);
      case "Seq" :
          var y$prime = delta_1(marks, c, next_cat, prev_cat, match.Arg1, "[]");
          return delta_seq(c, next_cat, prev_cat, match.Arg0, y$prime, match.Arg2, rem);
      case "Rep" :
          var kind = match.Arg1;
          var y$prime$1 = delta_1(marks, c, next_cat, prev_cat, match.Arg2, "[]");
          var match$1 = first((function (param) {
                  switch (/* XXX */param.tag) {
                    case "TSeq" :
                    case "TExp" :
                        return ;
                    case "TMatch" :
                        return param.Arg0;
                    
                  }
                }), y$prime$1);
          var match$2 = match$1 !== undefined ? /* tuple */[
              Curry._1(remove_matches, y$prime$1),
              match$1
            ] : /* tuple */[
              y$prime$1,
              marks
            ];
          var y$prime$prime = match$2[0];
          if (match.Arg0 >= 620821490) {
            return /* constructor */{
                    tag: "::",
                    Arg0: /* constructor */{
                      tag: "TMatch",
                      Arg0: marks
                    },
                    Arg1: tseq(kind, y$prime$prime, x, rem)
                  };
          } else {
            return tseq(kind, y$prime$prime, x, /* constructor */{
                        tag: "::",
                        Arg0: /* constructor */{
                          tag: "TMatch",
                          Arg0: match$2[1]
                        },
                        Arg1: rem
                      });
          }
      case "Mark" :
          var i = match.Arg0;
          var marks_000 = /* marks : constructor */{
            tag: "::",
            Arg0: /* tuple */[
              i,
              -1
            ],
            Arg1: List.remove_assq(i, marks[/* marks */0])
          };
          var marks_001 = /* pmarks */marks[/* pmarks */1];
          var marks$1 = /* record */[
            marks_000,
            marks_001
          ];
          return /* constructor */{
                  tag: "::",
                  Arg0: /* constructor */{
                    tag: "TMatch",
                    Arg0: marks$1
                  },
                  Arg1: rem
                };
      case "Erase" :
          return /* constructor */{
                  tag: "::",
                  Arg0: /* constructor */{
                    tag: "TMatch",
                    Arg0: filter_marks(match.Arg0, match.Arg1, marks)
                  },
                  Arg1: rem
                };
      case "Before" :
          if (intersect(next_cat, match.Arg0)) {
            return /* constructor */{
                    tag: "::",
                    Arg0: /* constructor */{
                      tag: "TMatch",
                      Arg0: marks
                    },
                    Arg1: rem
                  };
          } else {
            return rem;
          }
      case "After" :
          if (intersect(prev_cat, match.Arg0)) {
            return /* constructor */{
                    tag: "::",
                    Arg0: /* constructor */{
                      tag: "TMatch",
                      Arg0: marks
                    },
                    Arg1: rem
                  };
          } else {
            return rem;
          }
      case "Pmark" :
          var marks_000$1 = /* marks */marks[/* marks */0];
          var marks_001$1 = /* pmarks */add$1(match.Arg0, marks[/* pmarks */1]);
          var marks$2 = /* record */[
            marks_000$1,
            marks_001$1
          ];
          return /* constructor */{
                  tag: "::",
                  Arg0: /* constructor */{
                    tag: "TMatch",
                    Arg0: marks$2
                  },
                  Arg1: rem
                };
      
    }
  }
}

function delta_2(marks, c, next_cat, prev_cat, l, rem) {
  if (l !== "[]") {
    return delta_1(marks, c, next_cat, prev_cat, l.Arg0, delta_2(marks, c, next_cat, prev_cat, l.Arg1, rem));
  } else {
    return rem;
  }
}

function delta_seq(c, next_cat, prev_cat, kind, y, z, rem) {
  var match = first((function (param) {
          switch (/* XXX */param.tag) {
            case "TSeq" :
            case "TExp" :
                return ;
            case "TMatch" :
                return param.Arg0;
            
          }
        }), y);
  if (match !== undefined) {
    var marks = match;
    if (kind !== -730718166) {
      if (kind >= 332064784) {
        var match$1 = split_at_match_rec("[]", y);
        return tseq(kind, match$1[0], z, delta_1(marks, c, next_cat, prev_cat, z, tseq(kind, match$1[1], z, rem)));
      } else {
        return delta_1(marks, c, next_cat, prev_cat, z, tseq(kind, Curry._1(remove_matches, y), z, rem));
      }
    } else {
      return tseq(kind, Curry._1(remove_matches, y), z, delta_1(marks, c, next_cat, prev_cat, z, rem));
    }
  } else {
    return tseq(kind, y, z, rem);
  }
}

function delta_4(c, next_cat, prev_cat, l, rem) {
  if (l !== "[]") {
    var c$1 = c;
    var next_cat$1 = next_cat;
    var prev_cat$1 = prev_cat;
    var x = l.Arg0;
    var rem$1 = delta_4(c, next_cat, prev_cat, l.Arg1, rem);
    switch (/* XXX */x.tag) {
      case "TSeq" :
          var y$prime = delta_4(c$1, next_cat$1, prev_cat$1, x.Arg0, "[]");
          return delta_seq(c$1, next_cat$1, prev_cat$1, x.Arg2, y$prime, x.Arg1, rem$1);
      case "TExp" :
          return delta_1(x.Arg0, c$1, next_cat$1, prev_cat$1, x.Arg1, rem$1);
      case "TMatch" :
          return /* constructor */{
                  tag: "::",
                  Arg0: x,
                  Arg1: rem$1
                };
      
    }
  } else {
    return rem;
  }
}

function delta(tbl_ref, next_cat, $$char, st) {
  var prev_cat = st[/* category */1];
  var match = remove_duplicates("[]", delta_4($$char, next_cat, prev_cat, st[/* desc */2], "[]"), eps_expr);
  var expr$prime = match[0];
  var idx = free_index(tbl_ref, expr$prime);
  var expr$prime$prime = set_idx(idx, expr$prime);
  return mk(idx, next_cat, expr$prime$prime);
}

function flatten_match(m) {
  var ma = List.fold_left((function (ma, param) {
          return Caml_primitive.caml_int_max(ma, param[0]);
        }), -1, m);
  var res = Caml_array.caml_make_vect(ma + 1 | 0, -1);
  List.iter((function (param) {
          return Caml_array.caml_array_set(res, param[0], param[1]);
        }), m);
  return res;
}

function status(s) {
  var match = s[/* status */3];
  if (match !== undefined) {
    return match;
  } else {
    var match$1 = s[/* desc */2];
    var st;
    if (match$1 !== "[]") {
      var match$2 = match$1.Arg0;
      switch (/* XXX */match$2.tag) {
        case "TSeq" :
        case "TExp" :
            st = "Running";
            break;
        case "TMatch" :
            var m = match$2.Arg0;
            st = /* constructor */{
              tag: "Match",
              Arg0: flatten_match(m[/* marks */0]),
              Arg1: m[/* pmarks */1]
            };
            break;
        
      }
    } else {
      st = "Failed";
    }
    s[/* status */3] = st;
    return st;
  }
}

var Re_automata_Category = {
  "++": $plus$plus,
  from_char: from_char,
  inexistant: 1,
  letter: 2,
  not_letter: 4,
  newline: 8,
  lastnewline: 16,
  search_boundary: 32
};

var Re_automata_State = {
  dummy: dummy,
  create: create$2,
  Table: Table
};

function iter(_n, f, _v) {
  while(true) {
    var v = _v;
    var n = _n;
    if (n === 0) {
      return v;
    } else {
      _v = Curry._1(f, v);
      _n = n - 1 | 0;
      continue ;
    }
  };
}

function category(re, c) {
  if (c === -1) {
    return Re_automata_Category.inexistant;
  } else if (c === re[/* lnl */5]) {
    return Curry._2(Re_automata_Category["++"], Curry._2(Re_automata_Category["++"], Re_automata_Category.lastnewline, Re_automata_Category.newline), Re_automata_Category.not_letter);
  } else {
    return Curry._1(Re_automata_Category.from_char, Caml_bytes.get(re[/* col_repr */3], c));
  }
}

var dummy_next = /* array */[];

var unknown_state = /* record */[
  /* idx */-2,
  /* real_idx */0,
  /* next */dummy_next,
  /* final */"[]",
  /* desc */Re_automata_State.dummy
];

function mk_state(ncol, desc) {
  var match = status(desc);
  var break_state;
  break_state = typeof match === "string" && match !== "Failed" ? false : true;
  return /* record */[
          /* idx */break_state ? -3 : desc[/* idx */0],
          /* real_idx */desc[/* idx */0],
          /* next */break_state ? dummy_next : Caml_array.caml_make_vect(ncol, unknown_state),
          /* final */"[]",
          /* desc */desc
        ];
}

function find_state(re, desc) {
  try {
    return Curry._2(Re_automata_State.Table.find, re[/* states */7], desc);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      var st = mk_state(re[/* ncol */4], desc);
      Curry._3(Re_automata_State.Table.add, re[/* states */7], desc, st);
      return st;
    } else {
      throw exn;
    }
  }
}

function delta$1(info, cat, c, st) {
  var desc = delta(info[/* re */0][/* tbl */6], cat, c, st[/* desc */4]);
  var len = info[/* positions */2].length;
  if (desc[/* idx */0] === len && len > 0) {
    var pos = info[/* positions */2];
    info[/* positions */2] = Caml_array.caml_make_vect((len << 1), 0);
    $$Array.blit(pos, 0, info[/* positions */2], 0, len);
  }
  return desc;
}

function validate(info, s, pos, st) {
  var c = Caml_bytes.get(info[/* i_cols */1], Caml_string.get(s, pos));
  var cat = category(info[/* re */0], c);
  var desc$prime = delta$1(info, cat, c, st);
  var st$prime = find_state(info[/* re */0], desc$prime);
  return Caml_array.caml_array_set(st[/* next */2], c, st$prime);
}

function loop(info, s, pos, st) {
  if (pos < info[/* last */4]) {
    var st$prime = Caml_array.caml_array_get(st[/* next */2], Caml_bytes.get(info[/* i_cols */1], Caml_string.get(s, pos)));
    var info$1 = info;
    var s$1 = s;
    var _pos = pos;
    var _st = st;
    var _st$prime = st$prime;
    while(true) {
      var st$prime$1 = _st$prime;
      var st$1 = _st;
      var pos$1 = _pos;
      if (st$prime$1[/* idx */0] >= 0) {
        var pos$2 = pos$1 + 1 | 0;
        if (pos$2 < info$1[/* last */4]) {
          var st$prime$prime = Caml_array.caml_array_get(st$prime$1[/* next */2], Caml_bytes.get(info$1[/* i_cols */1], Caml_string.get(s$1, pos$2)));
          Caml_array.caml_array_set(info$1[/* positions */2], st$prime$1[/* idx */0], pos$2);
          _st$prime = st$prime$prime;
          _st = st$prime$1;
          _pos = pos$2;
          continue ;
        } else {
          Caml_array.caml_array_set(info$1[/* positions */2], st$prime$1[/* idx */0], pos$2);
          return st$prime$1;
        }
      } else if (st$prime$1[/* idx */0] === -3) {
        Caml_array.caml_array_set(info$1[/* positions */2], st$prime$1[/* real_idx */1], pos$1 + 1 | 0);
        return st$prime$1;
      } else {
        validate(info$1, s$1, pos$1, st$1);
        return loop(info$1, s$1, pos$1, st$1);
      }
    };
  } else {
    return st;
  }
}

function $$final(info, st, cat) {
  try {
    return List.assq(cat, st[/* final */3]);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      var st$prime = delta$1(info, cat, -1, st);
      var res_000 = st$prime[/* idx */0];
      var res_001 = status(st$prime);
      var res = /* tuple */[
        res_000,
        res_001
      ];
      st[/* final */3] = /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          cat,
          res
        ],
        Arg1: st[/* final */3]
      };
      return res;
    } else {
      throw exn;
    }
  }
}

function find_initial_state(re, cat) {
  try {
    return List.assq(cat, re[/* initial_states */1]);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      var st = find_state(re, Curry._2(Re_automata_State.create, cat, re[/* initial */0]));
      re[/* initial_states */1] = /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          cat,
          st
        ],
        Arg1: re[/* initial_states */1]
      };
      return st;
    } else {
      throw exn;
    }
  }
}

function get_color(re, s, pos) {
  if (pos < 0) {
    return -1;
  } else {
    var slen = s.length;
    if (pos >= slen) {
      return -1;
    } else if (pos === (slen - 1 | 0) && re[/* lnl */5] !== -1 && Caml_string.get(s, pos) === /* "\n" */10) {
      return re[/* lnl */5];
    } else {
      return Caml_bytes.get(re[/* cols */2], Caml_string.get(s, pos));
    }
  }
}

function scan_str(info, s, initial_state, groups) {
  var pos = info[/* pos */3];
  var last = info[/* last */4];
  if (last === s.length && info[/* re */0][/* lnl */5] !== -1 && last > pos && Caml_string.get(s, last - 1 | 0) === /* "\n" */10) {
    var info$1 = /* record */[
      /* re */info[/* re */0],
      /* i_cols */info[/* i_cols */1],
      /* positions */info[/* positions */2],
      /* pos */info[/* pos */3],
      /* last */last - 1 | 0
    ];
    var st = scan_str(info$1, s, initial_state, groups);
    if (st[/* idx */0] === -3) {
      return st;
    } else {
      var info$2 = info$1;
      var pos$1 = last - 1 | 0;
      var st$1 = st;
      var groups$1 = groups;
      while(true) {
        var st$prime = Caml_array.caml_array_get(st$1[/* next */2], info$2[/* re */0][/* lnl */5]);
        if (st$prime[/* idx */0] >= 0) {
          if (groups$1) {
            Caml_array.caml_array_set(info$2[/* positions */2], st$prime[/* idx */0], pos$1 + 1 | 0);
          }
          return st$prime;
        } else if (st$prime[/* idx */0] === -3) {
          if (groups$1) {
            Caml_array.caml_array_set(info$2[/* positions */2], st$prime[/* real_idx */1], pos$1 + 1 | 0);
          }
          return st$prime;
        } else {
          var c = info$2[/* re */0][/* lnl */5];
          var real_c = Caml_bytes.get(info$2[/* i_cols */1], /* "\n" */10);
          var cat = category(info$2[/* re */0], c);
          var desc$prime = delta$1(info$2, cat, real_c, st$1);
          var st$prime$1 = find_state(info$2[/* re */0], desc$prime);
          Caml_array.caml_array_set(st$1[/* next */2], c, st$prime$1);
          continue ;
        }
      };
    }
  } else if (groups) {
    return loop(info, s, pos, initial_state);
  } else {
    var info$3 = info;
    var s$1 = s;
    var _pos = pos;
    var last$1 = last;
    var _st = initial_state;
    while(true) {
      var st$2 = _st;
      var pos$2 = _pos;
      if (pos$2 < last$1) {
        var st$prime$2 = Caml_array.caml_array_get(st$2[/* next */2], Caml_bytes.get(info$3[/* i_cols */1], Caml_string.get(s$1, pos$2)));
        if (st$prime$2[/* idx */0] >= 0) {
          _st = st$prime$2;
          _pos = pos$2 + 1 | 0;
          continue ;
        } else if (st$prime$2[/* idx */0] === -3) {
          return st$prime$2;
        } else {
          validate(info$3, s$1, pos$2, st$2);
          continue ;
        }
      } else {
        return st$2;
      }
    };
  }
}

function cadd(c, s) {
  return union(single(c), s);
}

function trans_set(cache, cm, s) {
  var match = one_char(s);
  if (match !== undefined) {
    return single(Caml_bytes.get(cm, match));
  } else {
    var v_000 = hash_rec(s);
    var v = /* tuple */[
      v_000,
      s
    ];
    try {
      var x = v;
      var _param = cache[0];
      while(true) {
        var param = _param;
        if (param !== "Empty") {
          var c = compare(x, param.Arg1);
          if (c === 0) {
            return param.Arg2;
          } else {
            _param = c < 0 ? param.Arg0 : param.Arg3;
            continue ;
          }
        } else {
          throw Caml_builtin_exceptions.not_found;
        }
      };
    }
    catch (exn){
      if (exn === Caml_builtin_exceptions.not_found) {
        var l = List.fold_right((function (param, l) {
                return union(seq(Caml_bytes.get(cm, param[0]), Caml_bytes.get(cm, param[1])), l);
              }), s, "[]");
        cache[0] = add(v, l, cache[0]);
        return l;
      } else {
        throw exn;
      }
    }
  }
}

function is_charset(_param) {
  while(true) {
    var param = _param;
    if (typeof param === "string") {
      return false;
    } else {
      switch (/* XXX */param.tag) {
        case "Set" :
            return true;
        case "Sem" :
        case "Sem_greedy" :
            _param = param.Arg1;
            continue ;
        case "No_group" :
        case "Case" :
        case "No_case" :
            _param = param.Arg0;
            continue ;
        case "Alternative" :
        case "Intersection" :
        case "Complement" :
            return List.for_all(is_charset, param.Arg0);
        case "Difference" :
            if (is_charset(param.Arg0)) {
              _param = param.Arg1;
              continue ;
            } else {
              return false;
            }
        default:
          return false;
      }
    }
  };
}

function split(s, cm) {
  var _t = s;
  var f = function (i, j) {
    cm[i] = /* "\001" */1;
    cm[j + 1 | 0] = /* "\001" */1;
    return /* () */0;
  };
  while(true) {
    var t = _t;
    if (t !== "[]") {
      var match = t.Arg0;
      Curry._2(f, match[0], match[1]);
      _t = t.Arg1;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

var cupper = union(seq(/* "A" */65, /* "Z" */90), union(seq(/* "\192" */192, /* "\214" */214), seq(/* "\216" */216, /* "\222" */222)));

var clower = offset(32, cupper);

var calpha = List.fold_right(cadd, /* constructor */{
      tag: "::",
      Arg0: /* "\170" */170,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* "\181" */181,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* "\186" */186,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* "\223" */223,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* "\255" */255,
              Arg1: "[]"
            }
          }
        }
      }
    }, union(clower, cupper));

var cdigit = seq(/* "0" */48, /* "9" */57);

var calnum = union(calpha, cdigit);

var cword = union(/* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        /* "_" */95,
        /* "_" */95
      ],
      Arg1: "[]"
    }, calnum);

function colorize(c, regexp) {
  var lnl = /* record */[/* contents */false];
  var colorize$1 = function (_regexp) {
    while(true) {
      var regexp = _regexp;
      if (typeof regexp === "string") {
        switch (regexp) {
          case "Beg_of_line" :
          case "End_of_line" :
              return split(/* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            /* "\n" */10,
                            /* "\n" */10
                          ],
                          Arg1: "[]"
                        }, c);
          case "Beg_of_word" :
          case "End_of_word" :
          case "Not_bound" :
              return split(cword, c);
          case "Last_end_of_line" :
              lnl[0] = true;
              return /* () */0;
          case "Beg_of_str" :
          case "End_of_str" :
          case "Start" :
          case "Stop" :
              return /* () */0;
          
        }
      } else {
        switch (/* XXX */regexp.tag) {
          case "Set" :
              return split(regexp.Arg0, c);
          case "Sequence" :
          case "Alternative" :
              return List.iter(colorize$1, regexp.Arg0);
          case "Repeat" :
          case "Group" :
          case "No_group" :
          case "Nest" :
              _regexp = regexp.Arg0;
              continue ;
          case "Sem" :
          case "Sem_greedy" :
          case "Pmark" :
              _regexp = regexp.Arg1;
              continue ;
          default:
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "re.ml",
                    502,
                    35
                  ]
                ];
        }
      }
    };
  };
  colorize$1(regexp);
  return lnl[0];
}

function flatten_cmap(cm) {
  var c = Caml_bytes.caml_create_bytes(256);
  var col_repr = Caml_bytes.caml_create_bytes(256);
  var v = 0;
  c[0] = /* "\000" */0;
  col_repr[0] = /* "\000" */0;
  for(var i = 1; i <= 255; ++i){
    if (Caml_bytes.get(cm, i) !== /* "\000" */0) {
      v = v + 1 | 0;
    }
    c[i] = Char.chr(v);
    col_repr[v] = Char.chr(i);
  }
  return /* tuple */[
          c,
          Bytes.sub(col_repr, 0, v + 1 | 0),
          v + 1 | 0
        ];
}

function equal$2(_x1, _x2) {
  while(true) {
    var x2 = _x2;
    var x1 = _x1;
    if (typeof x1 === "string") {
      switch (x1) {
        case "Beg_of_line" :
            if (typeof x2 === "string" && x2 === "Beg_of_line") {
              return true;
            } else {
              return false;
            }
        case "End_of_line" :
            if (typeof x2 === "string" && x2 === "End_of_line") {
              return true;
            } else {
              return false;
            }
        case "Beg_of_word" :
            if (typeof x2 === "string" && x2 === "Beg_of_word") {
              return true;
            } else {
              return false;
            }
        case "End_of_word" :
            if (typeof x2 === "string" && x2 === "End_of_word") {
              return true;
            } else {
              return false;
            }
        case "Not_bound" :
            if (typeof x2 === "string" && x2 === "Not_bound") {
              return true;
            } else {
              return false;
            }
        case "Beg_of_str" :
            if (typeof x2 === "string" && x2 === "Beg_of_str") {
              return true;
            } else {
              return false;
            }
        case "End_of_str" :
            if (typeof x2 === "string" && x2 === "End_of_str") {
              return true;
            } else {
              return false;
            }
        case "Last_end_of_line" :
            if (typeof x2 === "string" && x2 === "Last_end_of_line") {
              return true;
            } else {
              return false;
            }
        case "Start" :
            if (typeof x2 === "string" && x2 === "Start") {
              return true;
            } else {
              return false;
            }
        case "Stop" :
            if (typeof x2 === "string" && x2 === "Stop") {
              return true;
            } else {
              return false;
            }
        
      }
    } else {
      switch (/* XXX */x1.tag) {
        case "Set" :
            if (typeof x2 === "string" || /* XXX */x2.tag !== "Set") {
              return false;
            } else {
              return Caml_obj.caml_equal(x1.Arg0, x2.Arg0);
            }
        case "Sequence" :
            if (typeof x2 === "string" || /* XXX */x2.tag !== "Sequence") {
              return false;
            } else {
              return eq_list(x1.Arg0, x2.Arg0);
            }
        case "Alternative" :
            if (typeof x2 === "string" || /* XXX */x2.tag !== "Alternative") {
              return false;
            } else {
              return eq_list(x1.Arg0, x2.Arg0);
            }
        case "Repeat" :
            if (typeof x2 === "string" || !(/* XXX */x2.tag === "Repeat" && x1.Arg1 === x2.Arg1 && Caml_obj.caml_equal(x1.Arg2, x2.Arg2))) {
              return false;
            } else {
              _x2 = x2.Arg0;
              _x1 = x1.Arg0;
              continue ;
            }
        case "Sem" :
            if (typeof x2 === "string" || !(/* XXX */x2.tag === "Sem" && x1.Arg0 === x2.Arg0)) {
              return false;
            } else {
              _x2 = x2.Arg1;
              _x1 = x1.Arg1;
              continue ;
            }
        case "Sem_greedy" :
            if (typeof x2 === "string" || !(/* XXX */x2.tag === "Sem_greedy" && x1.Arg0 === x2.Arg0)) {
              return false;
            } else {
              _x2 = x2.Arg1;
              _x1 = x1.Arg1;
              continue ;
            }
        case "Group" :
            return false;
        case "No_group" :
            if (typeof x2 === "string" || /* XXX */x2.tag !== "No_group") {
              return false;
            } else {
              _x2 = x2.Arg0;
              _x1 = x1.Arg0;
              continue ;
            }
        case "Nest" :
            if (typeof x2 === "string" || /* XXX */x2.tag !== "Nest") {
              return false;
            } else {
              _x2 = x2.Arg0;
              _x1 = x1.Arg0;
              continue ;
            }
        case "Case" :
            if (typeof x2 === "string" || /* XXX */x2.tag !== "Case") {
              return false;
            } else {
              _x2 = x2.Arg0;
              _x1 = x1.Arg0;
              continue ;
            }
        case "No_case" :
            if (typeof x2 === "string" || /* XXX */x2.tag !== "No_case") {
              return false;
            } else {
              _x2 = x2.Arg0;
              _x1 = x1.Arg0;
              continue ;
            }
        case "Intersection" :
            if (typeof x2 === "string" || /* XXX */x2.tag !== "Intersection") {
              return false;
            } else {
              return eq_list(x1.Arg0, x2.Arg0);
            }
        case "Complement" :
            if (typeof x2 === "string" || /* XXX */x2.tag !== "Complement") {
              return false;
            } else {
              return eq_list(x1.Arg0, x2.Arg0);
            }
        case "Difference" :
            if (typeof x2 === "string" || !(/* XXX */x2.tag === "Difference" && equal$2(x1.Arg0, x2.Arg0))) {
              return false;
            } else {
              _x2 = x2.Arg1;
              _x1 = x1.Arg1;
              continue ;
            }
        case "Pmark" :
            if (typeof x2 === "string" || !(/* XXX */x2.tag === "Pmark" && x1.Arg0 === x2.Arg0)) {
              return false;
            } else {
              _x2 = x2.Arg1;
              _x1 = x1.Arg1;
              continue ;
            }
        
      }
    }
  };
}

function eq_list(_l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1 !== "[]") {
      if (l2 !== "[]" && equal$2(l1.Arg0, l2.Arg0)) {
        _l2 = l2.Arg1;
        _l1 = l1.Arg1;
        continue ;
      } else {
        return false;
      }
    } else {
      return l2 === "[]";
    }
  };
}

function sequence(l) {
  if (l !== "[]") {
    if (l.Arg1 !== "[]") {
      return /* constructor */{
              tag: "Sequence",
              Arg0: l
            };
    } else {
      return l.Arg0;
    }
  } else {
    return /* constructor */{
            tag: "Sequence",
            Arg0: l
          };
  }
}

function merge_sequences(_param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      var x = param.Arg0;
      if (typeof x !== "string") {
        switch (/* XXX */x.tag) {
          case "Sequence" :
              var match = x.Arg0;
              if (match !== "[]") {
                var y = match.Arg1;
                var x$1 = match.Arg0;
                var r$prime = merge_sequences(param.Arg1);
                var exit = 0;
                if (r$prime !== "[]") {
                  var match$1 = r$prime.Arg0;
                  if (typeof match$1 === "string" || /* XXX */match$1.tag !== "Sequence") {
                    exit = 2;
                  } else {
                    var match$2 = match$1.Arg0;
                    if (match$2 !== "[]" && equal$2(x$1, match$2.Arg0)) {
                      return /* constructor */{
                              tag: "::",
                              Arg0: /* constructor */{
                                tag: "Sequence",
                                Arg0: /* constructor */{
                                  tag: "::",
                                  Arg0: x$1,
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: /* constructor */{
                                      tag: "Alternative",
                                      Arg0: /* constructor */{
                                        tag: "::",
                                        Arg0: sequence(y),
                                        Arg1: /* constructor */{
                                          tag: "::",
                                          Arg0: sequence(match$2.Arg1),
                                          Arg1: "[]"
                                        }
                                      }
                                    },
                                    Arg1: "[]"
                                  }
                                }
                              },
                              Arg1: r$prime.Arg1
                            };
                    } else {
                      exit = 2;
                    }
                  }
                } else {
                  exit = 2;
                }
                if (exit === 2) {
                  return /* constructor */{
                          tag: "::",
                          Arg0: /* constructor */{
                            tag: "Sequence",
                            Arg0: /* constructor */{
                              tag: "::",
                              Arg0: x$1,
                              Arg1: y
                            }
                          },
                          Arg1: r$prime
                        };
                }
                
              }
              break;
          case "Alternative" :
              _param = Pervasives.$at(x.Arg0, param.Arg1);
              continue ;
          default:
            
        }
      }
      return /* constructor */{
              tag: "::",
              Arg0: x,
              Arg1: merge_sequences(param.Arg1)
            };
    } else {
      return "[]";
    }
  };
}

function enforce_kind(ids, kind, kind$prime, cr) {
  if (kind !== 332064784 || kind$prime === 332064784) {
    return cr;
  } else {
    return seq$1(ids, kind$prime, cr, mk_expr(ids, "Eps"));
  }
}

function translate(ids, kind, _ign_group, ign_case, _greedy, pos, cache, c, _param) {
  while(true) {
    var param = _param;
    var greedy = _greedy;
    var ign_group = _ign_group;
    if (typeof param === "string") {
      switch (param) {
        case "Beg_of_line" :
            var c$1 = Curry._2(Re_automata_Category["++"], Re_automata_Category.inexistant, Re_automata_Category.newline);
            return /* tuple */[
                    mk_expr(ids, /* constructor */{
                          tag: "After",
                          Arg0: c$1
                        }),
                    kind
                  ];
        case "End_of_line" :
            var c$2 = Curry._2(Re_automata_Category["++"], Re_automata_Category.inexistant, Re_automata_Category.newline);
            return /* tuple */[
                    mk_expr(ids, /* constructor */{
                          tag: "Before",
                          Arg0: c$2
                        }),
                    kind
                  ];
        case "Beg_of_word" :
            var c$3 = Curry._2(Re_automata_Category["++"], Re_automata_Category.inexistant, Re_automata_Category.not_letter);
            var c$4 = Curry._2(Re_automata_Category["++"], Re_automata_Category.inexistant, Re_automata_Category.letter);
            return /* tuple */[
                    seq$1(ids, /* First */332064784, mk_expr(ids, /* constructor */{
                              tag: "After",
                              Arg0: c$3
                            }), mk_expr(ids, /* constructor */{
                              tag: "Before",
                              Arg0: c$4
                            })),
                    kind
                  ];
        case "End_of_word" :
            var c$5 = Curry._2(Re_automata_Category["++"], Re_automata_Category.inexistant, Re_automata_Category.letter);
            var c$6 = Curry._2(Re_automata_Category["++"], Re_automata_Category.inexistant, Re_automata_Category.not_letter);
            return /* tuple */[
                    seq$1(ids, /* First */332064784, mk_expr(ids, /* constructor */{
                              tag: "After",
                              Arg0: c$5
                            }), mk_expr(ids, /* constructor */{
                              tag: "Before",
                              Arg0: c$6
                            })),
                    kind
                  ];
        case "Not_bound" :
            return /* tuple */[
                    alt(ids, /* constructor */{
                          tag: "::",
                          Arg0: seq$1(ids, /* First */332064784, mk_expr(ids, /* constructor */{
                                    tag: "After",
                                    Arg0: Re_automata_Category.letter
                                  }), mk_expr(ids, /* constructor */{
                                    tag: "Before",
                                    Arg0: Re_automata_Category.letter
                                  })),
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: seq$1(ids, /* First */332064784, mk_expr(ids, /* constructor */{
                                      tag: "After",
                                      Arg0: Re_automata_Category.letter
                                    }), mk_expr(ids, /* constructor */{
                                      tag: "Before",
                                      Arg0: Re_automata_Category.letter
                                    })),
                            Arg1: "[]"
                          }
                        }),
                    kind
                  ];
        case "Beg_of_str" :
            return /* tuple */[
                    mk_expr(ids, /* constructor */{
                          tag: "After",
                          Arg0: Re_automata_Category.inexistant
                        }),
                    kind
                  ];
        case "End_of_str" :
            return /* tuple */[
                    mk_expr(ids, /* constructor */{
                          tag: "Before",
                          Arg0: Re_automata_Category.inexistant
                        }),
                    kind
                  ];
        case "Last_end_of_line" :
            var c$7 = Curry._2(Re_automata_Category["++"], Re_automata_Category.inexistant, Re_automata_Category.lastnewline);
            return /* tuple */[
                    mk_expr(ids, /* constructor */{
                          tag: "Before",
                          Arg0: c$7
                        }),
                    kind
                  ];
        case "Start" :
            return /* tuple */[
                    mk_expr(ids, /* constructor */{
                          tag: "After",
                          Arg0: Re_automata_Category.search_boundary
                        }),
                    kind
                  ];
        case "Stop" :
            return /* tuple */[
                    mk_expr(ids, /* constructor */{
                          tag: "Before",
                          Arg0: Re_automata_Category.search_boundary
                        }),
                    kind
                  ];
        
      }
    } else {
      switch (/* XXX */param.tag) {
        case "Set" :
            return /* tuple */[
                    cst(ids, trans_set(cache, c, param.Arg0)),
                    kind
                  ];
        case "Sequence" :
            return /* tuple */[
                    trans_seq(ids, kind, ign_group, ign_case, greedy, pos, cache, c, param.Arg0),
                    kind
                  ];
        case "Alternative" :
            var merged_sequences = merge_sequences(param.Arg0);
            if (merged_sequences !== "[]" && merged_sequences.Arg1 === "[]") {
              var match = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, merged_sequences.Arg0);
              return /* tuple */[
                      enforce_kind(ids, kind, match[1], match[0]),
                      kind
                    ];
            }
            return /* tuple */[
                    alt(ids, List.map((function(ign_group,greedy){
                            return function (r$prime) {
                              var match = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r$prime);
                              return enforce_kind(ids, kind, match[1], match[0]);
                            }
                            }(ign_group,greedy)), merged_sequences)),
                    kind
                  ];
        case "Repeat" :
            var j = param.Arg2;
            var i = param.Arg1;
            var match$1 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, param.Arg0);
            var kind$prime = match$1[1];
            var cr = match$1[0];
            var rem;
            if (j !== undefined) {
              var f = greedy >= 620821490 ? (function(cr,kind$prime){
                return function (rem) {
                  return alt(ids, /* constructor */{
                              tag: "::",
                              Arg0: mk_expr(ids, "Eps"),
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: seq$1(ids, kind$prime, rename(ids, cr), rem),
                                Arg1: "[]"
                              }
                            });
                }
                }(cr,kind$prime)) : (function(cr,kind$prime){
                return function (rem) {
                  return alt(ids, /* constructor */{
                              tag: "::",
                              Arg0: seq$1(ids, kind$prime, rename(ids, cr), rem),
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: mk_expr(ids, "Eps"),
                                Arg1: "[]"
                              }
                            });
                }
                }(cr,kind$prime));
              rem = iter(j - i | 0, f, mk_expr(ids, "Eps"));
            } else {
              rem = rep(ids, greedy, kind$prime, cr);
            }
            return /* tuple */[
                    iter(i, (function(cr,kind$prime){
                        return function (rem) {
                          return seq$1(ids, kind$prime, rename(ids, cr), rem);
                        }
                        }(cr,kind$prime)), rem),
                    kind
                  ];
        case "Sem" :
            var kind$prime$1 = param.Arg0;
            var match$2 = translate(ids, kind$prime$1, ign_group, ign_case, greedy, pos, cache, c, param.Arg1);
            return /* tuple */[
                    enforce_kind(ids, kind$prime$1, match$2[1], match$2[0]),
                    kind$prime$1
                  ];
        case "Sem_greedy" :
            _param = param.Arg1;
            _greedy = param.Arg0;
            continue ;
        case "Group" :
            var r$prime = param.Arg0;
            if (ign_group) {
              _param = r$prime;
              continue ;
            } else {
              var p = pos[0];
              pos[0] = pos[0] + 2 | 0;
              var match$3 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r$prime);
              return /* tuple */[
                      seq$1(ids, /* First */332064784, mk_expr(ids, /* constructor */{
                                tag: "Mark",
                                Arg0: p
                              }), seq$1(ids, /* First */332064784, match$3[0], mk_expr(ids, /* constructor */{
                                    tag: "Mark",
                                    Arg0: p + 1 | 0
                                  }))),
                      match$3[1]
                    ];
            }
        case "No_group" :
            _param = param.Arg0;
            _ign_group = true;
            continue ;
        case "Nest" :
            var b = pos[0];
            var match$4 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, param.Arg0);
            var kind$prime$2 = match$4[1];
            var cr$1 = match$4[0];
            var e = pos[0] - 1 | 0;
            if (e < b) {
              return /* tuple */[
                      cr$1,
                      kind$prime$2
                    ];
            } else {
              return /* tuple */[
                      seq$1(ids, /* First */332064784, erase(ids, b, e), cr$1),
                      kind$prime$2
                    ];
            }
        case "Pmark" :
            var match$5 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, param.Arg1);
            return /* tuple */[
                    seq$1(ids, /* First */332064784, mk_expr(ids, /* constructor */{
                              tag: "Pmark",
                              Arg0: param.Arg0
                            }), match$5[0]),
                    match$5[1]
                  ];
        default:
          throw [
                Caml_builtin_exceptions.assert_failure,
                /* tuple */[
                  "re.ml",
                  714,
                  4
                ]
              ];
      }
    }
  };
}

function trans_seq(ids, kind, ign_group, ign_case, greedy, pos, cache, c, param) {
  if (param !== "[]") {
    var rem = param.Arg1;
    var r = param.Arg0;
    if (rem !== "[]") {
      var match = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r);
      var cr$prime = match[0];
      var cr$prime$prime = trans_seq(ids, kind, ign_group, ign_case, greedy, pos, cache, c, rem);
      if (is_eps(cr$prime$prime)) {
        return cr$prime;
      } else if (is_eps(cr$prime)) {
        return cr$prime$prime;
      } else {
        return seq$1(ids, match[1], cr$prime, cr$prime$prime);
      }
    } else {
      var match$1 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r);
      return enforce_kind(ids, kind, match$1[1], match$1[0]);
    }
  } else {
    return mk_expr(ids, "Eps");
  }
}

function case_insens(s) {
  return union(s, union(offset(32, inter(s, cupper)), offset(-32, inter(s, clower))));
}

function as_set(param) {
  if (typeof param === "string") {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "re.ml",
            747,
            13
          ]
        ];
  } else if (/* XXX */param.tag === "Set") {
    return param.Arg0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "re.ml",
            747,
            13
          ]
        ];
  }
}

function handle_case(_ign_case, _r) {
  while(true) {
    var r = _r;
    var ign_case = _ign_case;
    if (typeof r === "string") {
      return r;
    } else {
      switch (/* XXX */r.tag) {
        case "Set" :
            var s = r.Arg0;
            return /* constructor */{
                    tag: "Set",
                    Arg0: ign_case ? case_insens(s) : s
                  };
        case "Sequence" :
            return /* constructor */{
                    tag: "Sequence",
                    Arg0: List.map((function(ign_case){
                        return function (param) {
                          return handle_case(ign_case, param);
                        }
                        }(ign_case)), r.Arg0)
                  };
        case "Alternative" :
            var l$prime = List.map((function(ign_case){
                return function (param) {
                  return handle_case(ign_case, param);
                }
                }(ign_case)), r.Arg0);
            if (is_charset(/* constructor */{
                    tag: "Alternative",
                    Arg0: l$prime
                  })) {
              return /* constructor */{
                      tag: "Set",
                      Arg0: List.fold_left((function (s, r) {
                              return union(s, as_set(r));
                            }), "[]", l$prime)
                    };
            } else {
              return /* constructor */{
                      tag: "Alternative",
                      Arg0: l$prime
                    };
            }
        case "Repeat" :
            return /* constructor */{
                    tag: "Repeat",
                    Arg0: handle_case(ign_case, r.Arg0),
                    Arg1: r.Arg1,
                    Arg2: r.Arg2
                  };
        case "Sem" :
            var r$prime = handle_case(ign_case, r.Arg1);
            if (is_charset(r$prime)) {
              return r$prime;
            } else {
              return /* constructor */{
                      tag: "Sem",
                      Arg0: r.Arg0,
                      Arg1: r$prime
                    };
            }
        case "Sem_greedy" :
            var r$prime$1 = handle_case(ign_case, r.Arg1);
            if (is_charset(r$prime$1)) {
              return r$prime$1;
            } else {
              return /* constructor */{
                      tag: "Sem_greedy",
                      Arg0: r.Arg0,
                      Arg1: r$prime$1
                    };
            }
        case "Group" :
            return /* constructor */{
                    tag: "Group",
                    Arg0: handle_case(ign_case, r.Arg0)
                  };
        case "No_group" :
            var r$prime$2 = handle_case(ign_case, r.Arg0);
            if (is_charset(r$prime$2)) {
              return r$prime$2;
            } else {
              return /* constructor */{
                      tag: "No_group",
                      Arg0: r$prime$2
                    };
            }
        case "Nest" :
            var r$prime$3 = handle_case(ign_case, r.Arg0);
            if (is_charset(r$prime$3)) {
              return r$prime$3;
            } else {
              return /* constructor */{
                      tag: "Nest",
                      Arg0: r$prime$3
                    };
            }
        case "Case" :
            _r = r.Arg0;
            _ign_case = false;
            continue ;
        case "No_case" :
            _r = r.Arg0;
            _ign_case = true;
            continue ;
        case "Intersection" :
            var l$prime$1 = List.map((function(ign_case){
                return function (r) {
                  return handle_case(ign_case, r);
                }
                }(ign_case)), r.Arg0);
            return /* constructor */{
                    tag: "Set",
                    Arg0: List.fold_left((function (s, r) {
                            return inter(s, as_set(r));
                          }), cany, l$prime$1)
                  };
        case "Complement" :
            var l$prime$2 = List.map((function(ign_case){
                return function (r) {
                  return handle_case(ign_case, r);
                }
                }(ign_case)), r.Arg0);
            return /* constructor */{
                    tag: "Set",
                    Arg0: diff(cany, List.fold_left((function (s, r) {
                                return union(s, as_set(r));
                              }), "[]", l$prime$2))
                  };
        case "Difference" :
            return /* constructor */{
                    tag: "Set",
                    Arg0: inter(as_set(handle_case(ign_case, r.Arg0)), diff(cany, as_set(handle_case(ign_case, r.Arg1))))
                  };
        case "Pmark" :
            return /* constructor */{
                    tag: "Pmark",
                    Arg0: r.Arg0,
                    Arg1: handle_case(ign_case, r.Arg1)
                  };
        
      }
    }
  };
}

function anchored(_param) {
  while(true) {
    var param = _param;
    if (typeof param === "string") {
      switch (param) {
        case "Beg_of_str" :
        case "Start" :
            return true;
        default:
          return false;
      }
    } else {
      switch (/* XXX */param.tag) {
        case "Sequence" :
            return List.exists(anchored, param.Arg0);
        case "Alternative" :
            return List.for_all(anchored, param.Arg0);
        case "Repeat" :
            if (param.Arg1 > 0) {
              _param = param.Arg0;
              continue ;
            } else {
              return false;
            }
        case "Group" :
        case "No_group" :
        case "Nest" :
        case "Case" :
        case "No_case" :
            _param = param.Arg0;
            continue ;
        case "Sem" :
        case "Sem_greedy" :
        case "Pmark" :
            _param = param.Arg1;
            continue ;
        default:
          return false;
      }
    }
  };
}

function alt$1(l) {
  if (l !== "[]") {
    if (l.Arg1 !== "[]") {
      return /* constructor */{
              tag: "Alternative",
              Arg0: l
            };
    } else {
      return l.Arg0;
    }
  } else {
    return /* constructor */{
            tag: "Alternative",
            Arg0: l
          };
  }
}

function seq$2(l) {
  if (l !== "[]") {
    if (l.Arg1 !== "[]") {
      return /* constructor */{
              tag: "Sequence",
              Arg0: l
            };
    } else {
      return l.Arg0;
    }
  } else {
    return /* constructor */{
            tag: "Sequence",
            Arg0: l
          };
  }
}

alt$1("[]");

var epsilon = seq$2("[]");

function repn(r, i, j) {
  if (i < 0) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Re.repn"
        ];
  }
  if (j !== undefined && j < i) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Re.repn"
        ];
  }
  return /* constructor */{
          tag: "Repeat",
          Arg0: r,
          Arg1: i,
          Arg2: j
        };
}

function set(str) {
  var s = "[]";
  for(var i = 0 ,i_finish = str.length - 1 | 0; i <= i_finish; ++i){
    s = union(single(Caml_string.get(str, i)), s);
  }
  return /* constructor */{
          tag: "Set",
          Arg0: s
        };
}

function compl(l) {
  var r = /* constructor */{
    tag: "Complement",
    Arg0: l
  };
  if (is_charset(r)) {
    return r;
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Re.compl"
        ];
  }
}

var any = /* constructor */{
  tag: "Set",
  Arg0: cany
};

var notnl = /* constructor */{
  tag: "Set",
  Arg0: diff(cany, /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          /* "\n" */10,
          /* "\n" */10
        ],
        Arg1: "[]"
      })
};

var lower = alt$1(/* constructor */{
      tag: "::",
      Arg0: /* constructor */{
        tag: "Set",
        Arg0: seq(/* "a" */97, /* "z" */122)
      },
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* constructor */{
          tag: "Set",
          Arg0: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              /* "\181" */181,
              /* "\181" */181
            ],
            Arg1: "[]"
          }
        },
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* constructor */{
            tag: "Set",
            Arg0: seq(/* "\223" */223, /* "\246" */246)
          },
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* constructor */{
              tag: "Set",
              Arg0: seq(/* "\248" */248, /* "\255" */255)
            },
            Arg1: "[]"
          }
        }
      }
    });

var upper = alt$1(/* constructor */{
      tag: "::",
      Arg0: /* constructor */{
        tag: "Set",
        Arg0: seq(/* "A" */65, /* "Z" */90)
      },
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* constructor */{
          tag: "Set",
          Arg0: seq(/* "\192" */192, /* "\214" */214)
        },
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* constructor */{
            tag: "Set",
            Arg0: seq(/* "\216" */216, /* "\222" */222)
          },
          Arg1: "[]"
        }
      }
    });

var alpha = alt$1(/* constructor */{
      tag: "::",
      Arg0: lower,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: upper,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* constructor */{
            tag: "Set",
            Arg0: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                /* "\170" */170,
                /* "\170" */170
              ],
              Arg1: "[]"
            }
          },
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* constructor */{
              tag: "Set",
              Arg0: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  /* "\186" */186,
                  /* "\186" */186
                ],
                Arg1: "[]"
              }
            },
            Arg1: "[]"
          }
        }
      }
    });

var digit = /* constructor */{
  tag: "Set",
  Arg0: seq(/* "0" */48, /* "9" */57)
};

var alnum = alt$1(/* constructor */{
      tag: "::",
      Arg0: alpha,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: digit,
        Arg1: "[]"
      }
    });

var wordc = alt$1(/* constructor */{
      tag: "::",
      Arg0: alnum,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* constructor */{
          tag: "Set",
          Arg0: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              /* "_" */95,
              /* "_" */95
            ],
            Arg1: "[]"
          }
        },
        Arg1: "[]"
      }
    });

var ascii = /* constructor */{
  tag: "Set",
  Arg0: seq(/* "\000" */0, /* "\127" */127)
};

var blank = set("\t ");

var cntrl = alt$1(/* constructor */{
      tag: "::",
      Arg0: /* constructor */{
        tag: "Set",
        Arg0: seq(/* "\000" */0, /* "\031" */31)
      },
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* constructor */{
          tag: "Set",
          Arg0: seq(/* "\127" */127, /* "\159" */159)
        },
        Arg1: "[]"
      }
    });

var graph = alt$1(/* constructor */{
      tag: "::",
      Arg0: /* constructor */{
        tag: "Set",
        Arg0: seq(/* "!" */33, /* "~" */126)
      },
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* constructor */{
          tag: "Set",
          Arg0: seq(/* "\160" */160, /* "\255" */255)
        },
        Arg1: "[]"
      }
    });

var print = alt$1(/* constructor */{
      tag: "::",
      Arg0: /* constructor */{
        tag: "Set",
        Arg0: seq(/* " " */32, /* "~" */126)
      },
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* constructor */{
          tag: "Set",
          Arg0: seq(/* "\160" */160, /* "\255" */255)
        },
        Arg1: "[]"
      }
    });

var punct = alt$1(/* constructor */{
      tag: "::",
      Arg0: /* constructor */{
        tag: "Set",
        Arg0: seq(/* "!" */33, /* "/" */47)
      },
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* constructor */{
          tag: "Set",
          Arg0: seq(/* ":" */58, /* "@" */64)
        },
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* constructor */{
            tag: "Set",
            Arg0: seq(/* "[" */91, /* "`" */96)
          },
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* constructor */{
              tag: "Set",
              Arg0: seq(/* "{" */123, /* "~" */126)
            },
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* constructor */{
                tag: "Set",
                Arg0: seq(/* "\160" */160, /* "\169" */169)
              },
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* constructor */{
                  tag: "Set",
                  Arg0: seq(/* "\171" */171, /* "\180" */180)
                },
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* constructor */{
                    tag: "Set",
                    Arg0: seq(/* "\182" */182, /* "\185" */185)
                  },
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* constructor */{
                      tag: "Set",
                      Arg0: seq(/* "\187" */187, /* "\191" */191)
                    },
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* constructor */{
                        tag: "Set",
                        Arg0: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            /* "\215" */215,
                            /* "\215" */215
                          ],
                          Arg1: "[]"
                        }
                      },
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* constructor */{
                          tag: "Set",
                          Arg0: /* constructor */{
                            tag: "::",
                            Arg0: /* tuple */[
                              /* "\247" */247,
                              /* "\247" */247
                            ],
                            Arg1: "[]"
                          }
                        },
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
    });

var space = alt$1(/* constructor */{
      tag: "::",
      Arg0: /* constructor */{
        tag: "Set",
        Arg0: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            /* " " */32,
            /* " " */32
          ],
          Arg1: "[]"
        }
      },
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* constructor */{
          tag: "Set",
          Arg0: seq(/* "\t" */9, /* "\r" */13)
        },
        Arg1: "[]"
      }
    });

var xdigit = alt$1(/* constructor */{
      tag: "::",
      Arg0: digit,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* constructor */{
          tag: "Set",
          Arg0: seq(/* "a" */97, /* "f" */102)
        },
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* constructor */{
            tag: "Set",
            Arg0: seq(/* "A" */65, /* "F" */70)
          },
          Arg1: "[]"
        }
      }
    });

function compile(r) {
  var regexp = anchored(r) ? /* constructor */({
        tag: "Group",
        Arg0: r
      }) : seq$2(/* constructor */{
          tag: "::",
          Arg0: /* constructor */{
            tag: "Sem",
            Arg0: /* Shortest */-1034406550,
            Arg1: repn(any, 0, undefined)
          },
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* constructor */{
              tag: "Group",
              Arg0: r
            },
            Arg1: "[]"
          }
        });
  var regexp$1 = handle_case(false, regexp);
  var c = Bytes.make(257, /* "\000" */0);
  var need_lnl = colorize(c, regexp$1);
  var match = flatten_cmap(c);
  var ncol = match[2];
  var col = match[0];
  var lnl = need_lnl ? ncol : -1;
  var ncol$1 = need_lnl ? ncol + 1 | 0 : ncol;
  var ids = /* record */[/* contents */0];
  var pos = /* record */[/* contents */0];
  var match$1 = translate(ids, /* First */332064784, false, false, /* Greedy */-904640576, pos, /* record */[/* contents */"Empty"], col, regexp$1);
  var r$1 = enforce_kind(ids, /* First */332064784, match$1[1], match$1[0]);
  var init = r$1;
  var cols = col;
  var col_repr = match[1];
  var ncol$2 = ncol$1;
  var lnl$1 = lnl;
  var group_count = pos[0] / 2 | 0;
  return /* record */[
          /* initial */init,
          /* initial_states */"[]",
          /* cols */cols,
          /* col_repr */col_repr,
          /* ncol */ncol$2,
          /* lnl */lnl$1,
          /* tbl : record */[/* contents : array */[false]],
          /* states */Curry._1(Re_automata_State.Table.create, 97),
          /* group_count */group_count
        ];
}

function exec_internal(name, $staropt$star, $staropt$star$1, groups, re, s) {
  var pos = $staropt$star !== undefined ? $staropt$star : 0;
  var len = $staropt$star$1 !== undefined ? $staropt$star$1 : -1;
  if (pos < 0 || len < -1 || (pos + len | 0) > s.length) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          name
        ];
  }
  var groups$1 = groups;
  var partial = false;
  var re$1 = re;
  var s$1 = s;
  var pos$1 = pos;
  var len$1 = len;
  var slen = s$1.length;
  var last = len$1 === -1 ? slen : pos$1 + len$1 | 0;
  var tmp;
  if (groups$1) {
    var n = re$1[/* tbl */6][0].length + 1 | 0;
    tmp = n <= 10 ? /* array */[
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      ] : Caml_array.caml_make_vect(n, 0);
  } else {
    tmp = /* array */[];
  }
  var info = /* record */[
    /* re */re$1,
    /* i_cols */re$1[/* cols */2],
    /* positions */tmp,
    /* pos */pos$1,
    /* last */last
  ];
  var initial_cat = pos$1 === 0 ? Curry._2(Re_automata_Category["++"], Re_automata_Category.search_boundary, Re_automata_Category.inexistant) : Curry._2(Re_automata_Category["++"], Re_automata_Category.search_boundary, category(re$1, get_color(re$1, s$1, pos$1 - 1 | 0)));
  var initial_state = find_initial_state(re$1, initial_cat);
  var st = scan_str(info, s$1, initial_state, groups$1);
  var res;
  if (st[/* idx */0] === -3 || partial) {
    res = status(st[/* desc */4]);
  } else {
    var final_cat = last === slen ? Curry._2(Re_automata_Category["++"], Re_automata_Category.search_boundary, Re_automata_Category.inexistant) : Curry._2(Re_automata_Category["++"], Re_automata_Category.search_boundary, category(re$1, get_color(re$1, s$1, last)));
    var match = $$final(info, st, final_cat);
    if (groups$1) {
      Caml_array.caml_array_set(info[/* positions */2], match[0], last + 1 | 0);
    }
    res = match[1];
  }
  if (typeof res === "string") {
    if (res === "Failed") {
      return "Failed";
    } else {
      return "Running";
    }
  } else {
    return /* constructor */{
            tag: "Match",
            Arg0: /* record */[
              /* s */s$1,
              /* marks */res.Arg0,
              /* pmarks */res.Arg1,
              /* gpos */info[/* positions */2],
              /* gcount */re$1[/* group_count */8]
            ]
          };
  }
}

function offset$1(t, i) {
  if (((i << 1) + 1 | 0) >= t[/* marks */1].length) {
    throw Caml_builtin_exceptions.not_found;
  }
  var m1 = Caml_array.caml_array_get(t[/* marks */1], (i << 1));
  if (m1 === -1) {
    throw Caml_builtin_exceptions.not_found;
  }
  var p1 = Caml_array.caml_array_get(t[/* gpos */3], m1) - 1 | 0;
  var p2 = Caml_array.caml_array_get(t[/* gpos */3], Caml_array.caml_array_get(t[/* marks */1], (i << 1) + 1 | 0)) - 1 | 0;
  return /* tuple */[
          p1,
          p2
        ];
}

function get(t, i) {
  var match = offset$1(t, i);
  var p1 = match[0];
  return $$String.sub(t[/* s */0], p1, match[1] - p1 | 0);
}

var Parse_error = Caml_exceptions.create("Parse_error");

var Not_supported = Caml_exceptions.create("Not_supported");

function posix_class_of_string(class_) {
  switch (class_) {
    case "alnum" :
        return alnum;
    case "ascii" :
        return ascii;
    case "blank" :
        return blank;
    case "cntrl" :
        return cntrl;
    case "digit" :
        return digit;
    case "graph" :
        return graph;
    case "lower" :
        return lower;
    case "print" :
        return print;
    case "punct" :
        return punct;
    case "space" :
        return space;
    case "upper" :
        return upper;
    case "word" :
        return wordc;
    case "xdigit" :
        return xdigit;
    default:
      var s = "Invalid pcre class: " + class_;
      throw [
            Caml_builtin_exceptions.invalid_argument,
            s
          ];
  }
}

function parse(multiline, dollar_endonly, dotall, ungreedy, s) {
  var i = /* record */[/* contents */0];
  var l = s.length;
  var test = function (c) {
    if (i[0] !== l) {
      return Caml_string.get(s, i[0]) === c;
    } else {
      return false;
    }
  };
  var accept = function (c) {
    var r = test(c);
    if (r) {
      i[0] = i[0] + 1 | 0;
    }
    return r;
  };
  var accept_s = function (s$prime) {
    var len = s$prime.length;
    try {
      for(var j = 0 ,j_finish = len - 1 | 0; j <= j_finish; ++j){
        try {
          if (Caml_string.get(s$prime, j) !== Caml_string.get(s, i[0] + j | 0)) {
            throw Pervasives.Exit;
          }
          
        }
        catch (exn){
          throw Pervasives.Exit;
        }
      }
      i[0] = i[0] + len | 0;
      return true;
    }
    catch (exn$1){
      if (exn$1 === Pervasives.Exit) {
        return false;
      } else {
        throw exn$1;
      }
    }
  };
  var get = function (param) {
    var r = Caml_string.get(s, i[0]);
    i[0] = i[0] + 1 | 0;
    return r;
  };
  var greedy_mod = function (r) {
    var gr = accept(/* "?" */63);
    var gr$1 = ungreedy ? !gr : gr;
    if (gr$1) {
      return /* constructor */{
              tag: "Sem_greedy",
              Arg0: /* Non_greedy */620821490,
              Arg1: r
            };
    } else {
      return /* constructor */{
              tag: "Sem_greedy",
              Arg0: /* Greedy */-904640576,
              Arg1: r
            };
    }
  };
  var branch$prime = function (_left) {
    while(true) {
      var left = _left;
      if (i[0] === l || test(/* "|" */124) || test(/* ")" */41)) {
        return seq$2(List.rev(left));
      } else {
        _left = /* constructor */{
          tag: "::",
          Arg0: piece(/* () */0),
          Arg1: left
        };
        continue ;
      }
    };
  };
  var regexp$prime = function (_left) {
    while(true) {
      var left = _left;
      if (accept(/* "|" */124)) {
        _left = alt$1(/* constructor */{
              tag: "::",
              Arg0: left,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: branch$prime("[]"),
                Arg1: "[]"
              }
            });
        continue ;
      } else {
        return left;
      }
    };
  };
  var bracket = function (_s) {
    while(true) {
      var s = _s;
      if (s !== "[]" && accept(/* "]" */93)) {
        return s;
      } else {
        var match = $$char(/* () */0);
        if (match[0] >= 748194550) {
          var c = match[1];
          if (accept(/* "-" */45)) {
            if (accept(/* "]" */93)) {
              return /* constructor */{
                      tag: "::",
                      Arg0: /* constructor */{
                        tag: "Set",
                        Arg0: single(c)
                      },
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* constructor */{
                          tag: "Set",
                          Arg0: /* constructor */{
                            tag: "::",
                            Arg0: /* tuple */[
                              /* "-" */45,
                              /* "-" */45
                            ],
                            Arg1: "[]"
                          }
                        },
                        Arg1: s
                      }
                    };
            } else {
              var match$1 = $$char(/* () */0);
              if (match$1[0] >= 748194550) {
                _s = /* constructor */{
                  tag: "::",
                  Arg0: /* constructor */{
                    tag: "Set",
                    Arg0: seq(c, match$1[1])
                  },
                  Arg1: s
                };
                continue ;
              } else {
                return /* constructor */{
                        tag: "::",
                        Arg0: /* constructor */{
                          tag: "Set",
                          Arg0: single(c)
                        },
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* constructor */{
                            tag: "Set",
                            Arg0: /* constructor */{
                              tag: "::",
                              Arg0: /* tuple */[
                                /* "-" */45,
                                /* "-" */45
                              ],
                              Arg1: "[]"
                            }
                          },
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: match$1[1],
                            Arg1: s
                          }
                        }
                      };
              }
            }
          } else {
            _s = /* constructor */{
              tag: "::",
              Arg0: /* constructor */{
                tag: "Set",
                Arg0: single(c)
              },
              Arg1: s
            };
            continue ;
          }
        } else {
          _s = /* constructor */{
            tag: "::",
            Arg0: match[1],
            Arg1: s
          };
          continue ;
        }
      }
    };
  };
  var piece = function (param) {
    var r = atom(/* () */0);
    if (accept(/* "*" */42)) {
      return greedy_mod(repn(r, 0, undefined));
    } else if (accept(/* "+" */43)) {
      return greedy_mod(repn(r, 1, undefined));
    } else if (accept(/* "?" */63)) {
      return greedy_mod(repn(r, 0, 1));
    } else if (accept(/* "{" */123)) {
      var match = integer(/* () */0);
      if (match !== undefined) {
        var i$1 = match;
        var j = accept(/* "," */44) ? integer(/* () */0) : i$1;
        if (!accept(/* "}" */125)) {
          throw Parse_error;
        }
        if (j !== undefined && j < i$1) {
          throw Parse_error;
        }
        return greedy_mod(repn(r, i$1, j));
      } else {
        i[0] = i[0] - 1 | 0;
        return r;
      }
    } else {
      return r;
    }
  };
  var $$char = function (param) {
    if (i[0] === l) {
      throw Parse_error;
    }
    var c = get(/* () */0);
    if (c === /* "[" */91) {
      if (accept(/* "=" */61)) {
        throw Not_supported;
      }
      if (accept(/* ":" */58)) {
        var compl$1 = accept(/* "^" */94);
        var cls;
        try {
          cls = List.find(accept_s, /* constructor */{
                tag: "::",
                Arg0: "alnum",
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: "ascii",
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: "blank",
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: "cntrl",
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: "digit",
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: "lower",
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: "print",
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: "space",
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: "upper",
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: "word",
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: "punct",
                                    Arg1: /* constructor */{
                                      tag: "::",
                                      Arg0: "graph",
                                      Arg1: /* constructor */{
                                        tag: "::",
                                        Arg0: "xdigit",
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
              });
        }
        catch (exn){
          if (exn === Caml_builtin_exceptions.not_found) {
            throw Parse_error;
          }
          throw exn;
        }
        if (!accept_s(":]")) {
          throw Parse_error;
        }
        var posix_class = posix_class_of_string(cls);
        var re = compl$1 ? compl(/* constructor */{
                tag: "::",
                Arg0: posix_class,
                Arg1: "[]"
              }) : posix_class;
        return /* `Set */[
                4150146,
                re
              ];
      } else if (accept(/* "." */46)) {
        if (i[0] === l) {
          throw Parse_error;
        }
        var c$1 = get(/* () */0);
        if (!accept(/* "." */46)) {
          throw Not_supported;
        }
        if (!accept(/* "]" */93)) {
          throw Parse_error;
        }
        return /* `Char */[
                748194550,
                c$1
              ];
      } else {
        return /* `Char */[
                748194550,
                c
              ];
      }
    } else if (c === /* "\\" */92) {
      var c$2 = get(/* () */0);
      if (c$2 >= 58) {
        if (c$2 >= 123) {
          return /* `Char */[
                  748194550,
                  c$2
                ];
        } else {
          switch (c$2 - 58 | 0) {
            case 10 :
                return /* `Set */[
                        4150146,
                        compl(/* constructor */{
                              tag: "::",
                              Arg0: digit,
                              Arg1: "[]"
                            })
                      ];
            case 25 :
                return /* `Set */[
                        4150146,
                        compl(/* constructor */{
                              tag: "::",
                              Arg0: space,
                              Arg1: "[]"
                            })
                      ];
            case 29 :
                return /* `Set */[
                        4150146,
                        compl(/* constructor */{
                              tag: "::",
                              Arg0: alnum,
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* constructor */{
                                  tag: "Set",
                                  Arg0: /* constructor */{
                                    tag: "::",
                                    Arg0: /* tuple */[
                                      /* "_" */95,
                                      /* "_" */95
                                    ],
                                    Arg1: "[]"
                                  }
                                },
                                Arg1: "[]"
                              }
                            })
                      ];
            case 0 :
            case 1 :
            case 2 :
            case 3 :
            case 4 :
            case 5 :
            case 6 :
            case 33 :
            case 34 :
            case 35 :
            case 36 :
            case 37 :
            case 38 :
                return /* `Char */[
                        748194550,
                        c$2
                      ];
            case 40 :
                return /* `Char */[
                        748194550,
                        /* "\b" */8
                      ];
            case 42 :
                return /* `Set */[
                        4150146,
                        digit
                      ];
            case 52 :
                return /* `Char */[
                        748194550,
                        /* "\n" */10
                      ];
            case 56 :
                return /* `Char */[
                        748194550,
                        /* "\r" */13
                      ];
            case 57 :
                return /* `Set */[
                        4150146,
                        space
                      ];
            case 58 :
                return /* `Char */[
                        748194550,
                        /* "\t" */9
                      ];
            case 61 :
                return /* `Set */[
                        4150146,
                        alt$1(/* constructor */{
                              tag: "::",
                              Arg0: alnum,
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* constructor */{
                                  tag: "Set",
                                  Arg0: /* constructor */{
                                    tag: "::",
                                    Arg0: /* tuple */[
                                      /* "_" */95,
                                      /* "_" */95
                                    ],
                                    Arg1: "[]"
                                  }
                                },
                                Arg1: "[]"
                              }
                            })
                      ];
            case 7 :
            case 8 :
            case 9 :
            case 11 :
            case 12 :
            case 13 :
            case 14 :
            case 15 :
            case 16 :
            case 17 :
            case 18 :
            case 19 :
            case 20 :
            case 21 :
            case 22 :
            case 23 :
            case 24 :
            case 26 :
            case 27 :
            case 28 :
            case 30 :
            case 31 :
            case 32 :
            case 39 :
            case 41 :
            case 43 :
            case 44 :
            case 45 :
            case 46 :
            case 47 :
            case 48 :
            case 49 :
            case 50 :
            case 51 :
            case 53 :
            case 54 :
            case 55 :
            case 59 :
            case 60 :
            case 62 :
            case 63 :
            case 64 :
                throw Parse_error;
            
          }
        }
      } else {
        if (c$2 >= 48) {
          throw Not_supported;
        }
        return /* `Char */[
                748194550,
                c$2
              ];
      }
    } else {
      return /* `Char */[
              748194550,
              c
            ];
    }
  };
  var integer = function (param) {
    if (i[0] === l) {
      return ;
    } else {
      var d = get(/* () */0);
      if (d > 57 || d < 48) {
        i[0] = i[0] - 1 | 0;
        return ;
      } else {
        var _i = d - /* "0" */48 | 0;
        while(true) {
          var i$1 = _i;
          if (i[0] === l) {
            return i$1;
          } else {
            var d$1 = get(/* () */0);
            if (d$1 > 57 || d$1 < 48) {
              i[0] = i[0] - 1 | 0;
              return i$1;
            } else {
              var i$prime = Caml_int32.imul(10, i$1) + (d$1 - /* "0" */48 | 0) | 0;
              if (i$prime < i$1) {
                throw Parse_error;
              }
              _i = i$prime;
              continue ;
            }
          }
        };
      }
    }
  };
  var atom = function (param) {
    if (accept(/* "." */46)) {
      if (dotall) {
        return any;
      } else {
        return notnl;
      }
    } else if (accept(/* "(" */40)) {
      if (accept(/* "?" */63)) {
        if (accept(/* ":" */58)) {
          var r = regexp$prime(branch$prime("[]"));
          if (!accept(/* ")" */41)) {
            throw Parse_error;
          }
          return r;
        } else if (accept(/* "#" */35)) {
          var _param = /* () */0;
          while(true) {
            if (accept(/* ")" */41)) {
              return epsilon;
            } else {
              i[0] = i[0] + 1 | 0;
              _param = /* () */0;
              continue ;
            }
          };
        } else {
          throw Parse_error;
        }
      } else {
        var r$1 = regexp$prime(branch$prime("[]"));
        if (!accept(/* ")" */41)) {
          throw Parse_error;
        }
        return /* constructor */{
                tag: "Group",
                Arg0: r$1
              };
      }
    } else if (accept(/* "^" */94)) {
      if (multiline) {
        return "Beg_of_line";
      } else {
        return "Beg_of_str";
      }
    } else if (accept(/* "$" */36)) {
      if (multiline) {
        return "End_of_line";
      } else if (dollar_endonly) {
        return "Last_end_of_line";
      } else {
        return "End_of_str";
      }
    } else if (accept(/* "[" */91)) {
      if (accept(/* "^" */94)) {
        return compl(bracket("[]"));
      } else {
        return alt$1(bracket("[]"));
      }
    } else if (accept(/* "\\" */92)) {
      if (i[0] === l) {
        throw Parse_error;
      }
      var c = get(/* () */0);
      switch (c) {
        case 48 :
        case 49 :
        case 50 :
        case 51 :
        case 52 :
        case 53 :
        case 54 :
        case 55 :
        case 56 :
        case 57 :
            throw Not_supported;
        case 65 :
            return "Beg_of_str";
        case 66 :
            return "Not_bound";
        case 68 :
            return compl(/* constructor */{
                        tag: "::",
                        Arg0: digit,
                        Arg1: "[]"
                      });
        case 71 :
            return "Start";
        case 83 :
            return compl(/* constructor */{
                        tag: "::",
                        Arg0: space,
                        Arg1: "[]"
                      });
        case 87 :
            return compl(/* constructor */{
                        tag: "::",
                        Arg0: alnum,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* constructor */{
                            tag: "Set",
                            Arg0: /* constructor */{
                              tag: "::",
                              Arg0: /* tuple */[
                                /* "_" */95,
                                /* "_" */95
                              ],
                              Arg1: "[]"
                            }
                          },
                          Arg1: "[]"
                        }
                      });
        case 90 :
            return "Last_end_of_line";
        case 58 :
        case 59 :
        case 60 :
        case 61 :
        case 62 :
        case 63 :
        case 64 :
        case 91 :
        case 92 :
        case 93 :
        case 94 :
        case 95 :
        case 96 :
            return /* constructor */{
                    tag: "Set",
                    Arg0: single(c)
                  };
        case 98 :
            return alt$1(/* constructor */{
                        tag: "::",
                        Arg0: "Beg_of_word",
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: "End_of_word",
                          Arg1: "[]"
                        }
                      });
        case 100 :
            return digit;
        case 115 :
            return space;
        case 119 :
            return alt$1(/* constructor */{
                        tag: "::",
                        Arg0: alnum,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* constructor */{
                            tag: "Set",
                            Arg0: /* constructor */{
                              tag: "::",
                              Arg0: /* tuple */[
                                /* "_" */95,
                                /* "_" */95
                              ],
                              Arg1: "[]"
                            }
                          },
                          Arg1: "[]"
                        }
                      });
        case 67 :
        case 69 :
        case 70 :
        case 72 :
        case 73 :
        case 74 :
        case 75 :
        case 76 :
        case 77 :
        case 78 :
        case 79 :
        case 80 :
        case 81 :
        case 82 :
        case 84 :
        case 85 :
        case 86 :
        case 88 :
        case 89 :
        case 97 :
        case 99 :
        case 101 :
        case 102 :
        case 103 :
        case 104 :
        case 105 :
        case 106 :
        case 107 :
        case 108 :
        case 109 :
        case 110 :
        case 111 :
        case 112 :
        case 113 :
        case 114 :
        case 116 :
        case 117 :
        case 118 :
        case 120 :
        case 121 :
            throw Parse_error;
        case 122 :
            return "End_of_str";
        default:
          return /* constructor */{
                  tag: "Set",
                  Arg0: single(c)
                };
      }
    } else {
      if (i[0] === l) {
        throw Parse_error;
      }
      var c$1 = get(/* () */0);
      if (c$1 >= 64) {
        if (c$1 !== 92) {
          if (c$1 !== 123) {
            return /* constructor */{
                    tag: "Set",
                    Arg0: single(c$1)
                  };
          } else {
            throw Parse_error;
          }
        } else {
          throw Parse_error;
        }
      } else if (c$1 >= 44) {
        if (c$1 >= 63) {
          throw Parse_error;
        }
        return /* constructor */{
                tag: "Set",
                Arg0: single(c$1)
              };
      } else {
        if (c$1 >= 42) {
          throw Parse_error;
        }
        return /* constructor */{
                tag: "Set",
                Arg0: single(c$1)
              };
      }
    }
  };
  var res = regexp$prime(branch$prime("[]"));
  if (i[0] !== l) {
    throw Parse_error;
  }
  return res;
}

function re($staropt$star, pat) {
  var flags = $staropt$star !== undefined ? $staropt$star : "[]";
  var opts = List.map((function (param) {
          if (param !== 601676297) {
            if (param >= 613575188) {
              return /* Anchored */616470068;
            } else {
              return /* Multiline */1071952589;
            }
          } else {
            return /* Caseless */604571177;
          }
        }), flags);
  var $staropt$star$1 = opts;
  var s = pat;
  var opts$1 = $staropt$star$1 !== undefined ? $staropt$star$1 : "[]";
  var r = parse(List.memq(/* Multiline */1071952589, opts$1), List.memq(/* Dollar_endonly */-712595228, opts$1), List.memq(/* Dotall */-424303016, opts$1), List.memq(/* Ungreedy */-243745063, opts$1), s);
  var r$1 = List.memq(/* Anchored */616470068, opts$1) ? seq$2(/* constructor */{
          tag: "::",
          Arg0: "Start",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: r,
            Arg1: "[]"
          }
        }) : r;
  if (List.memq(/* Caseless */604571177, opts$1)) {
    return /* constructor */{
            tag: "No_case",
            Arg0: r$1
          };
  } else {
    return r$1;
  }
}

function exec(rex, pos, s) {
  var pos$1 = pos;
  var len = undefined;
  var re = rex;
  var s$1 = s;
  var match = exec_internal("Re.exec", pos$1, len, true, re, s$1);
  if (typeof match === "string") {
    if (match === "Failed") {
      throw Caml_builtin_exceptions.not_found;
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  } else {
    return match.Arg0;
  }
}

var s = Caml_bytes.bytes_to_string(Bytes.make(1048575, /* "a" */97)) + "b";

eq("File \"xx.ml\", line 7, characters 3-10", get(exec(compile(re(undefined, "aa?b")), undefined, s), 0), "aab");

Mt.from_pair_suites("Ocaml_re_test", suites[0]);

/* Table Not a pure module */
