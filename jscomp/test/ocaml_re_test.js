'use strict';

var Mt = require("./mt.js");
var Char = require("../../lib/js/char.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
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

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

function union(_l, _l$prime) {
  while(true) {
    var l$prime = _l$prime;
    var l = _l;
    if (l$prime) {
      if (l) {
        var r$prime = l$prime[1];
        var match = l$prime[0];
        var c2$prime = match[1];
        var c1$prime = match[0];
        var r = l[1];
        var match$1 = l[0];
        var c2 = match$1[1];
        var c1 = match$1[0];
        if ((c2 + 1 | 0) < c1$prime) {
          return /* :: */[
                  /* tuple */[
                    c1,
                    c2
                  ],
                  union(r, l$prime)
                ];
        } else if ((c2$prime + 1 | 0) < c1) {
          return /* :: */[
                  /* tuple */[
                    c1$prime,
                    c2$prime
                  ],
                  union(l, r$prime)
                ];
        } else if (c2 < c2$prime) {
          _l$prime = /* :: */[
            /* tuple */[
              c1 < c1$prime ? c1 : c1$prime,
              c2$prime
            ],
            r$prime
          ];
          _l = r;
          continue ;
        } else {
          _l$prime = r$prime;
          _l = /* :: */[
            /* tuple */[
              c1 < c1$prime ? c1 : c1$prime,
              c2
            ],
            r
          ];
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
    if (l$prime && l) {
      var r$prime = l$prime[1];
      var match = l$prime[0];
      var c2$prime = match[1];
      var c1$prime = match[0];
      var r = l[1];
      var match$1 = l[0];
      var c2 = match$1[1];
      var c1 = match$1[0];
      if (Caml_obj.caml_lessthan(c2, c1$prime)) {
        _l = r;
        continue ;
      } else if (Caml_obj.caml_lessthan(c2$prime, c1)) {
        _l$prime = r$prime;
        continue ;
      } else if (Caml_obj.caml_lessthan(c2, c2$prime)) {
        return /* :: */[
                /* tuple */[
                  Caml_obj.caml_max(c1, c1$prime),
                  c2
                ],
                inter(r, l$prime)
              ];
      } else {
        return /* :: */[
                /* tuple */[
                  Caml_obj.caml_max(c1, c1$prime),
                  c2$prime
                ],
                inter(l, r$prime)
              ];
      }
    } else {
      return /* [] */0;
    }
  };
}

function diff(_l, _l$prime) {
  while(true) {
    var l$prime = _l$prime;
    var l = _l;
    if (l$prime) {
      if (l) {
        var r$prime = l$prime[1];
        var match = l$prime[0];
        var c2$prime = match[1];
        var c1$prime = match[0];
        var r = l[1];
        var match$1 = l[0];
        var c2 = match$1[1];
        var c1 = match$1[0];
        if (c2 < c1$prime) {
          return /* :: */[
                  /* tuple */[
                    c1,
                    c2
                  ],
                  diff(r, l$prime)
                ];
        } else if (c2$prime < c1) {
          _l$prime = r$prime;
          continue ;
        } else {
          var r$prime$prime = c2$prime < c2 ? /* :: */[
              /* tuple */[
                c2$prime + 1 | 0,
                c2
              ],
              r
            ] : r;
          if (c1 < c1$prime) {
            return /* :: */[
                    /* tuple */[
                      c1,
                      c1$prime - 1 | 0
                    ],
                    diff(r$prime$prime, r$prime)
                  ];
          } else {
            _l$prime = r$prime;
            _l = r$prime$prime;
            continue ;
          }
        }
      } else {
        return /* [] */0;
      }
    } else {
      return l;
    }
  };
}

function single(c) {
  return /* :: */[
          /* tuple */[
            c,
            c
          ],
          /* [] */0
        ];
}

function seq(c, c$prime) {
  if (Caml_obj.caml_lessequal(c, c$prime)) {
    return /* :: */[
            /* tuple */[
              c,
              c$prime
            ],
            /* [] */0
          ];
  } else {
    return /* :: */[
            /* tuple */[
              c$prime,
              c
            ],
            /* [] */0
          ];
  }
}

function offset(o, l) {
  if (l) {
    var match = l[0];
    return /* :: */[
            /* tuple */[
              match[0] + o | 0,
              match[1] + o | 0
            ],
            offset(o, l[1])
          ];
  } else {
    return /* [] */0;
  }
}

function mem(c, _s) {
  while(true) {
    var s = _s;
    if (s) {
      var match = s[0];
      if (c <= match[1]) {
        return c >= match[0];
      } else {
        _s = s[1];
        continue ;
      }
    } else {
      return false;
    }
  };
}

function hash_rec(param) {
  if (param) {
    var match = param[0];
    return (match[0] + Caml_int32.imul(13, match[1]) | 0) + Caml_int32.imul(257, hash_rec(param[1])) | 0;
  } else {
    return 0;
  }
}

function one_char(param) {
  if (param && !param[1]) {
    var match = param[0];
    var i = match[0];
    if (Caml_obj.caml_equal(i, match[1])) {
      return Caml_option.some(i);
    } else {
      return undefined;
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
  if (param) {
    return param[/* h */4];
  } else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* Node */[
          /* l */l,
          /* v */x,
          /* d */d,
          /* r */r,
          /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function bal(l, x, d, r) {
  var hl = l ? l[/* h */4] : 0;
  var hr = r ? r[/* h */4] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[/* r */3];
      var ld = l[/* d */2];
      var lv = l[/* v */1];
      var ll = l[/* l */0];
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      } else if (lr) {
        return create(create(ll, lv, ld, lr[/* l */0]), lr[/* v */1], lr[/* d */2], create(lr[/* r */3], x, d, r));
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
    if (r) {
      var rr = r[/* r */3];
      var rd = r[/* d */2];
      var rv = r[/* v */1];
      var rl = r[/* l */0];
      if (height(rr) >= height(rl)) {
        return create(create(l, x, d, rl), rv, rd, rr);
      } else if (rl) {
        return create(create(l, x, d, rl[/* l */0]), rl[/* v */1], rl[/* d */2], create(rl[/* r */3], rv, rd, rr));
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
    return /* Node */[
            /* l */l,
            /* v */x,
            /* d */d,
            /* r */r,
            /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function add(x, data, m) {
  if (m) {
    var r = m[/* r */3];
    var d = m[/* d */2];
    var v = m[/* v */1];
    var l = m[/* l */0];
    var c = compare(x, v);
    if (c === 0) {
      if (d === data) {
        return m;
      } else {
        return /* Node */[
                /* l */l,
                /* v */x,
                /* d */data,
                /* r */r,
                /* h */m[/* h */4]
              ];
      }
    } else if (c < 0) {
      var ll = add(x, data, l);
      if (l === ll) {
        return m;
      } else {
        return bal(ll, v, d, r);
      }
    } else {
      var rr = add(x, data, r);
      if (r === rr) {
        return m;
      } else {
        return bal(l, v, d, rr);
      }
    }
  } else {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* d */data,
            /* r : Empty */0,
            /* h */1
          ];
  }
}

var cany = /* :: */[
  /* tuple */[
    0,
    255
  ],
  /* [] */0
];

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
  if (param) {
    return param[/* h */3];
  } else {
    return 0;
  }
}

function create$1(l, v, r) {
  var hl = l ? l[/* h */3] : 0;
  var hr = r ? r[/* h */3] : 0;
  return /* Node */[
          /* l */l,
          /* v */v,
          /* r */r,
          /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function bal$1(l, v, r) {
  var hl = l ? l[/* h */3] : 0;
  var hr = r ? r[/* h */3] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[/* r */2];
      var lv = l[/* v */1];
      var ll = l[/* l */0];
      if (height$1(ll) >= height$1(lr)) {
        return create$1(ll, lv, create$1(lr, v, r));
      } else if (lr) {
        return create$1(create$1(ll, lv, lr[/* l */0]), lr[/* v */1], create$1(lr[/* r */2], v, r));
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
    if (r) {
      var rr = r[/* r */2];
      var rv = r[/* v */1];
      var rl = r[/* l */0];
      if (height$1(rr) >= height$1(rl)) {
        return create$1(create$1(l, v, rl), rv, rr);
      } else if (rl) {
        return create$1(create$1(l, v, rl[/* l */0]), rl[/* v */1], create$1(rl[/* r */2], rv, rr));
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
    return /* Node */[
            /* l */l,
            /* v */v,
            /* r */r,
            /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function add$1(x, t) {
  if (t) {
    var r = t[/* r */2];
    var v = t[/* v */1];
    var l = t[/* l */0];
    var c = Caml_primitive.caml_int_compare(x, v);
    if (c === 0) {
      return t;
    } else if (c < 0) {
      var ll = add$1(x, l);
      if (l === ll) {
        return t;
      } else {
        return bal$1(ll, v, r);
      }
    } else {
      var rr = add$1(x, r);
      if (r === rr) {
        return t;
      } else {
        return bal$1(l, v, rr);
      }
    }
  } else {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* r : Empty */0,
            /* h */1
          ];
  }
}

function hash_combine(h, accu) {
  return Caml_int32.imul(accu, 65599) + h | 0;
}

var empty = /* record */[
  /* marks : [] */0,
  /* pmarks : Empty */0
];

function hash(m, accu) {
  var _l = m[/* marks */0];
  var _accu = hash_combine(Hashtbl.hash(m[/* pmarks */1]), accu);
  while(true) {
    var accu$1 = _accu;
    var l = _l;
    if (l) {
      var match = l[0];
      _accu = hash_combine(match[0], hash_combine(match[1], accu$1));
      _l = l[1];
      continue ;
    } else {
      return accu$1;
    }
  };
}

function marks_set_idx(idx, marks) {
  if (marks) {
    var match = marks[0];
    if (match[1] !== -1) {
      return marks;
    } else {
      return /* :: */[
              /* tuple */[
                match[0],
                idx
              ],
              marks_set_idx(idx, marks[1])
            ];
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
    if (param) {
      var res = Curry._1(f, param[0]);
      if (res !== undefined) {
        return res;
      } else {
        _param = param[1];
        continue ;
      }
    } else {
      return undefined;
    }
  };
}

var eps_expr = /* record */[
  /* id */0,
  /* def : Eps */0
];

function mk_expr(ids, def) {
  ids[0] = ids[0] + 1 | 0;
  return /* record */[
          /* id */ids[0],
          /* def */def
        ];
}

function cst(ids, s) {
  if (s ? false : true) {
    return mk_expr(ids, /* Alt */Block.__(1, [/* [] */0]));
  } else {
    return mk_expr(ids, /* Cst */Block.__(0, [s]));
  }
}

function alt(ids, l) {
  if (l) {
    if (l[1]) {
      return mk_expr(ids, /* Alt */Block.__(1, [l]));
    } else {
      return l[0];
    }
  } else {
    return mk_expr(ids, /* Alt */Block.__(1, [/* [] */0]));
  }
}

function seq$1(ids, kind, x, y) {
  var match = x[/* def */1];
  var match$1 = y[/* def */1];
  var exit = 0;
  var exit$1 = 0;
  if (typeof match === "number") {
    return y;
  } else if (match.tag === 1 && !match[0]) {
    return x;
  } else {
    exit$1 = 2;
  }
  if (exit$1 === 2) {
    if (typeof match$1 === "number") {
      if (kind === /* First */332064784) {
        return x;
      } else {
        exit = 1;
      }
    } else if (match$1.tag === 1 && !match$1[0]) {
      return y;
    } else {
      exit = 1;
    }
  }
  if (exit === 1) {
    return mk_expr(ids, /* Seq */Block.__(2, [
                  kind,
                  x,
                  y
                ]));
  }
  
}

function is_eps(expr) {
  var match = expr[/* def */1];
  if (typeof match === "number") {
    return true;
  } else {
    return false;
  }
}

function rep(ids, kind, sem, x) {
  return mk_expr(ids, /* Rep */Block.__(3, [
                kind,
                sem,
                x
              ]));
}

function erase(ids, m, m$prime) {
  return mk_expr(ids, /* Erase */Block.__(5, [
                m,
                m$prime
              ]));
}

function rename(ids, x) {
  var match = x[/* def */1];
  if (typeof match === "number") {
    return mk_expr(ids, x[/* def */1]);
  } else {
    switch (match.tag | 0) {
      case 1 : 
          return mk_expr(ids, /* Alt */Block.__(1, [List.map((function (param) {
                                return rename(ids, param);
                              }), match[0])]));
      case 2 : 
          return mk_expr(ids, /* Seq */Block.__(2, [
                        match[0],
                        rename(ids, match[1]),
                        rename(ids, match[2])
                      ]));
      case 3 : 
          return mk_expr(ids, /* Rep */Block.__(3, [
                        match[0],
                        match[1],
                        rename(ids, match[2])
                      ]));
      default:
        return mk_expr(ids, x[/* def */1]);
    }
  }
}

function equal(_l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      var match = l1[0];
      switch (match.tag | 0) {
        case 0 : 
            if (l2) {
              var match$1 = l2[0];
              switch (match$1.tag | 0) {
                case 0 : 
                    if (match[1][/* id */0] === match$1[1][/* id */0] && equal(match[0], match$1[0])) {
                      _l2 = l2[1];
                      _l1 = l1[1];
                      continue ;
                    } else {
                      return false;
                    }
                case 1 : 
                case 2 : 
                    return false;
                
              }
            } else {
              return false;
            }
        case 1 : 
            if (l2) {
              var match$2 = l2[0];
              switch (match$2.tag | 0) {
                case 1 : 
                    if (match[1][/* id */0] === match$2[1][/* id */0] && Caml_obj.caml_equal(match[0], match$2[0])) {
                      _l2 = l2[1];
                      _l1 = l1[1];
                      continue ;
                    } else {
                      return false;
                    }
                case 0 : 
                case 2 : 
                    return false;
                
              }
            } else {
              return false;
            }
        case 2 : 
            if (l2) {
              var match$3 = l2[0];
              switch (match$3.tag | 0) {
                case 0 : 
                case 1 : 
                    return false;
                case 2 : 
                    if (Caml_obj.caml_equal(match[0], match$3[0])) {
                      _l2 = l2[1];
                      _l1 = l1[1];
                      continue ;
                    } else {
                      return false;
                    }
                
              }
            } else {
              return false;
            }
        
      }
    } else if (l2) {
      return false;
    } else {
      return true;
    }
  };
}

function hash$1(_l, _accu) {
  while(true) {
    var accu = _accu;
    var l = _l;
    if (l) {
      var match = l[0];
      switch (match.tag | 0) {
        case 0 : 
            _accu = hash_combine(388635598, hash_combine(match[1][/* id */0], hash$1(match[0], accu)));
            _l = l[1];
            continue ;
        case 1 : 
            _accu = hash_combine(726404471, hash_combine(match[1][/* id */0], hash(match[0], accu)));
            _l = l[1];
            continue ;
        case 2 : 
            _accu = hash_combine(471882453, hash(match[0], accu));
            _l = l[1];
            continue ;
        
      }
    } else {
      return accu;
    }
  };
}

function tseq(kind, x, y, rem) {
  var exit = 0;
  if (x) {
    var match = x[0];
    switch (match.tag | 0) {
      case 1 : 
          if (typeof match[1][/* def */1] === "number" && !x[1]) {
            return /* :: */[
                    /* TExp */Block.__(1, [
                        match[0],
                        y
                      ]),
                    rem
                  ];
          } else {
            exit = 1;
          }
          break;
      case 0 : 
      case 2 : 
          exit = 1;
          break;
      
    }
  } else {
    return rem;
  }
  if (exit === 1) {
    return /* :: */[
            /* TSeq */Block.__(0, [
                x,
                y,
                kind
              ]),
            rem
          ];
  }
  
}

var dummy = /* record */[
  /* idx */-1,
  /* category */-1,
  /* desc : [] */0,
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
  return mk(0, cat, /* :: */[
              /* TExp */Block.__(1, [
                  empty,
                  e
                ]),
              /* [] */0
            ]);
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

var Table = Hashtbl.Make(/* module */[
      /* equal */equal$1,
      /* hash */hash$3
    ]);

function reset_table(a) {
  return $$Array.fill(a, 0, a.length, false);
}

function mark_used_indices(tbl) {
  return (function (param) {
      return List.iter((function (param) {
                    var exit = 0;
                    switch (param.tag | 0) {
                      case 0 : 
                          return mark_used_indices(tbl)(param[0]);
                      case 1 : 
                      case 2 : 
                          exit = 1;
                          break;
                      
                    }
                    if (exit === 1) {
                      return List.iter((function (param) {
                                    var i = param[1];
                                    if (i >= 0) {
                                      return Caml_array.caml_array_set(tbl, i, true);
                                    } else {
                                      return 0;
                                    }
                                  }), param[0][/* marks */0]);
                    }
                    
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
        switch (param.tag | 0) {
          case 0 : 
          case 1 : 
              return true;
          case 2 : 
              return false;
          
        }
      }));

function split_at_match_rec(_l$prime, _param) {
  while(true) {
    var param = _param;
    var l$prime = _l$prime;
    if (param) {
      var x = param[0];
      switch (x.tag | 0) {
        case 0 : 
        case 1 : 
            _param = param[1];
            _l$prime = /* :: */[
              x,
              l$prime
            ];
            continue ;
        case 2 : 
            return /* tuple */[
                    List.rev(l$prime),
                    Curry._1(remove_matches, param[1])
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
    if (l) {
      var x = l[0];
      switch (x.tag | 0) {
        case 0 : 
            var x$1 = x[1];
            var match = remove_duplicates(prev, x[0], x$1);
            var match$1 = remove_duplicates(match[1], l[1], y);
            return /* tuple */[
                    tseq(x[2], match[0], x$1, match$1[0]),
                    match$1[1]
                  ];
        case 1 : 
            var x$2 = x[1];
            if (typeof x$2[/* def */1] === "number") {
              var r = l[1];
              if (List.memq(y[/* id */0], prev)) {
                _l = r;
                continue ;
              } else {
                var match$2 = remove_duplicates(/* :: */[
                      y[/* id */0],
                      prev
                    ], r, y);
                return /* tuple */[
                        /* :: */[
                          x,
                          match$2[0]
                        ],
                        match$2[1]
                      ];
              }
            } else {
              var r$1 = l[1];
              if (List.memq(x$2[/* id */0], prev)) {
                _l = r$1;
                continue ;
              } else {
                var match$3 = remove_duplicates(/* :: */[
                      x$2[/* id */0],
                      prev
                    ], r$1, y);
                return /* tuple */[
                        /* :: */[
                          x,
                          match$3[0]
                        ],
                        match$3[1]
                      ];
              }
            }
        case 2 : 
            return /* tuple */[
                    /* :: */[
                      x,
                      /* [] */0
                    ],
                    prev
                  ];
        
      }
    } else {
      return /* tuple */[
              /* [] */0,
              prev
            ];
    }
  };
}

function set_idx(idx, param) {
  if (param) {
    var match = param[0];
    switch (match.tag | 0) {
      case 0 : 
          return /* :: */[
                  /* TSeq */Block.__(0, [
                      set_idx(idx, match[0]),
                      match[1],
                      match[2]
                    ]),
                  set_idx(idx, param[1])
                ];
      case 1 : 
          return /* :: */[
                  /* TExp */Block.__(1, [
                      marks_set_idx$1(match[0], idx),
                      match[1]
                    ]),
                  set_idx(idx, param[1])
                ];
      case 2 : 
          return /* :: */[
                  /* TMatch */Block.__(2, [marks_set_idx$1(match[0], idx)]),
                  set_idx(idx, param[1])
                ];
      
    }
  } else {
    return /* [] */0;
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
  if (typeof match === "number") {
    return /* :: */[
            /* TMatch */Block.__(2, [marks]),
            rem
          ];
  } else {
    switch (match.tag | 0) {
      case 0 : 
          if (mem(c, match[0])) {
            return /* :: */[
                    /* TExp */Block.__(1, [
                        marks,
                        eps_expr
                      ]),
                    rem
                  ];
          } else {
            return rem;
          }
      case 1 : 
          return delta_2(marks, c, next_cat, prev_cat, match[0], rem);
      case 2 : 
          var y$prime = delta_1(marks, c, next_cat, prev_cat, match[1], /* [] */0);
          return delta_seq(c, next_cat, prev_cat, match[0], y$prime, match[2], rem);
      case 3 : 
          var kind = match[1];
          var y$prime$1 = delta_1(marks, c, next_cat, prev_cat, match[2], /* [] */0);
          var match$1 = first((function (param) {
                  switch (param.tag | 0) {
                    case 0 : 
                    case 1 : 
                        return undefined;
                    case 2 : 
                        return param[0];
                    
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
          if (match[0] >= 620821490) {
            return /* :: */[
                    /* TMatch */Block.__(2, [marks]),
                    tseq(kind, y$prime$prime, x, rem)
                  ];
          } else {
            return tseq(kind, y$prime$prime, x, /* :: */[
                        /* TMatch */Block.__(2, [match$2[1]]),
                        rem
                      ]);
          }
      case 4 : 
          var i = match[0];
          var marks_000 = /* marks : :: */[
            /* tuple */[
              i,
              -1
            ],
            List.remove_assq(i, marks[/* marks */0])
          ];
          var marks_001 = /* pmarks */marks[/* pmarks */1];
          var marks$1 = /* record */[
            marks_000,
            marks_001
          ];
          return /* :: */[
                  /* TMatch */Block.__(2, [marks$1]),
                  rem
                ];
      case 5 : 
          return /* :: */[
                  /* TMatch */Block.__(2, [filter_marks(match[0], match[1], marks)]),
                  rem
                ];
      case 6 : 
          if (intersect(next_cat, match[0])) {
            return /* :: */[
                    /* TMatch */Block.__(2, [marks]),
                    rem
                  ];
          } else {
            return rem;
          }
      case 7 : 
          if (intersect(prev_cat, match[0])) {
            return /* :: */[
                    /* TMatch */Block.__(2, [marks]),
                    rem
                  ];
          } else {
            return rem;
          }
      case 8 : 
          var marks_000$1 = /* marks */marks[/* marks */0];
          var marks_001$1 = /* pmarks */add$1(match[0], marks[/* pmarks */1]);
          var marks$2 = /* record */[
            marks_000$1,
            marks_001$1
          ];
          return /* :: */[
                  /* TMatch */Block.__(2, [marks$2]),
                  rem
                ];
      
    }
  }
}

function delta_2(marks, c, next_cat, prev_cat, l, rem) {
  if (l) {
    return delta_1(marks, c, next_cat, prev_cat, l[0], delta_2(marks, c, next_cat, prev_cat, l[1], rem));
  } else {
    return rem;
  }
}

function delta_seq(c, next_cat, prev_cat, kind, y, z, rem) {
  var match = first((function (param) {
          switch (param.tag | 0) {
            case 0 : 
            case 1 : 
                return undefined;
            case 2 : 
                return param[0];
            
          }
        }), y);
  if (match !== undefined) {
    var marks = match;
    if (kind !== -730718166) {
      if (kind >= 332064784) {
        var match$1 = split_at_match_rec(/* [] */0, y);
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
  if (l) {
    var c$1 = c;
    var next_cat$1 = next_cat;
    var prev_cat$1 = prev_cat;
    var x = l[0];
    var rem$1 = delta_4(c, next_cat, prev_cat, l[1], rem);
    switch (x.tag | 0) {
      case 0 : 
          var y$prime = delta_4(c$1, next_cat$1, prev_cat$1, x[0], /* [] */0);
          return delta_seq(c$1, next_cat$1, prev_cat$1, x[2], y$prime, x[1], rem$1);
      case 1 : 
          return delta_1(x[0], c$1, next_cat$1, prev_cat$1, x[1], rem$1);
      case 2 : 
          return /* :: */[
                  x,
                  rem$1
                ];
      
    }
  } else {
    return rem;
  }
}

function delta(tbl_ref, next_cat, $$char, st) {
  var prev_cat = st[/* category */1];
  var match = remove_duplicates(/* [] */0, delta_4($$char, next_cat, prev_cat, st[/* desc */2], /* [] */0), eps_expr);
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
    if (match$1) {
      var match$2 = match$1[0];
      switch (match$2.tag | 0) {
        case 0 : 
        case 1 : 
            st = /* Running */1;
            break;
        case 2 : 
            var m = match$2[0];
            st = /* Match */[
              flatten_match(m[/* marks */0]),
              m[/* pmarks */1]
            ];
            break;
        
      }
    } else {
      st = /* Failed */0;
    }
    s[/* status */3] = st;
    return st;
  }
}

var Re_automata_000 = /* Category */[
  $plus$plus,
  from_char,
  1,
  2,
  4,
  8,
  16,
  32
];

var Re_automata_022 = /* State */[
  dummy,
  create$2,
  Table
];

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
    return Re_automata_000[/* inexistant */2];
  } else if (c === re[/* lnl */5]) {
    return Curry._2(Re_automata_000[/* ++ */0], Curry._2(Re_automata_000[/* ++ */0], Re_automata_000[/* lastnewline */6], Re_automata_000[/* newline */5]), Re_automata_000[/* not_letter */4]);
  } else {
    return Curry._1(Re_automata_000[/* from_char */1], Caml_bytes.get(re[/* col_repr */3], c));
  }
}

var dummy_next = /* array */[];

var unknown_state = /* record */[
  /* idx */-2,
  /* real_idx */0,
  /* next */dummy_next,
  /* final : [] */0,
  /* desc */Re_automata_022[/* dummy */0]
];

function mk_state(ncol, desc) {
  var match = status(desc);
  var break_state = typeof match === "number" ? match === 0 : true;
  return /* record */[
          /* idx */break_state ? -3 : desc[/* idx */0],
          /* real_idx */desc[/* idx */0],
          /* next */break_state ? dummy_next : Caml_array.caml_make_vect(ncol, unknown_state),
          /* final : [] */0,
          /* desc */desc
        ];
}

function find_state(re, desc) {
  try {
    return Curry._2(Re_automata_022[/* Table */2][/* find */6], re[/* states */7], desc);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      var st = mk_state(re[/* ncol */4], desc);
      Curry._3(Re_automata_022[/* Table */2][/* add */4], re[/* states */7], desc, st);
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
      st[/* final */3] = /* :: */[
        /* tuple */[
          cat,
          res
        ],
        st[/* final */3]
      ];
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
      var st = find_state(re, Curry._2(Re_automata_022[/* create */1], cat, re[/* initial */0]));
      re[/* initial_states */1] = /* :: */[
        /* tuple */[
          cat,
          st
        ],
        re[/* initial_states */1]
      ];
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
        if (param) {
          var c = compare(x, param[/* v */1]);
          if (c === 0) {
            return param[/* d */2];
          } else {
            _param = c < 0 ? param[/* l */0] : param[/* r */3];
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
              }), s, /* [] */0);
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
    if (typeof param === "number") {
      return false;
    } else {
      switch (param.tag | 0) {
        case 0 : 
            return true;
        case 4 : 
        case 5 : 
            _param = param[1];
            continue ;
        case 7 : 
        case 9 : 
        case 10 : 
            _param = param[0];
            continue ;
        case 2 : 
        case 11 : 
        case 12 : 
            return List.for_all(is_charset, param[0]);
        case 13 : 
            if (is_charset(param[0])) {
              _param = param[1];
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
    if (t) {
      var match = t[0];
      Curry._2(f, match[0], match[1]);
      _t = t[1];
      continue ;
    } else {
      return /* () */0;
    }
  };
}

var cupper = union(seq(/* "A" */65, /* "Z" */90), union(seq(/* "\192" */192, /* "\214" */214), seq(/* "\216" */216, /* "\222" */222)));

var clower = offset(32, cupper);

var calpha = List.fold_right(cadd, /* :: */[
      /* "\170" */170,
      /* :: */[
        /* "\181" */181,
        /* :: */[
          /* "\186" */186,
          /* :: */[
            /* "\223" */223,
            /* :: */[
              /* "\255" */255,
              /* [] */0
            ]
          ]
        ]
      ]
    ], union(clower, cupper));

var cdigit = seq(/* "0" */48, /* "9" */57);

var calnum = union(calpha, cdigit);

var cword = union(/* :: */[
      /* tuple */[
        /* "_" */95,
        /* "_" */95
      ],
      /* [] */0
    ], calnum);

function colorize(c, regexp) {
  var lnl = /* record */[/* contents */false];
  var colorize$1 = function (_regexp) {
    while(true) {
      var regexp = _regexp;
      if (typeof regexp === "number") {
        switch (regexp) {
          case 0 : 
          case 1 : 
              return split(/* :: */[
                          /* tuple */[
                            /* "\n" */10,
                            /* "\n" */10
                          ],
                          /* [] */0
                        ], c);
          case 2 : 
          case 3 : 
          case 4 : 
              return split(cword, c);
          case 7 : 
              lnl[0] = true;
              return /* () */0;
          case 5 : 
          case 6 : 
          case 8 : 
          case 9 : 
              return /* () */0;
          
        }
      } else {
        switch (regexp.tag | 0) {
          case 0 : 
              return split(regexp[0], c);
          case 1 : 
          case 2 : 
              return List.iter(colorize$1, regexp[0]);
          case 3 : 
          case 6 : 
          case 7 : 
          case 8 : 
              _regexp = regexp[0];
              continue ;
          case 4 : 
          case 5 : 
          case 14 : 
              _regexp = regexp[1];
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
    if (typeof x1 === "number") {
      switch (x1) {
        case 0 : 
            if (typeof x2 === "number") {
              return x2 === 0;
            } else {
              return false;
            }
        case 1 : 
            if (typeof x2 === "number") {
              return x2 === 1;
            } else {
              return false;
            }
        case 2 : 
            if (typeof x2 === "number") {
              return x2 === 2;
            } else {
              return false;
            }
        case 3 : 
            if (typeof x2 === "number") {
              return x2 === 3;
            } else {
              return false;
            }
        case 4 : 
            if (typeof x2 === "number") {
              return x2 === 4;
            } else {
              return false;
            }
        case 5 : 
            if (typeof x2 === "number") {
              return x2 === 5;
            } else {
              return false;
            }
        case 6 : 
            if (typeof x2 === "number") {
              return x2 === 6;
            } else {
              return false;
            }
        case 7 : 
            if (typeof x2 === "number") {
              return x2 === 7;
            } else {
              return false;
            }
        case 8 : 
            if (typeof x2 === "number") {
              return x2 === 8;
            } else {
              return false;
            }
        case 9 : 
            if (typeof x2 === "number") {
              return x2 >= 9;
            } else {
              return false;
            }
        
      }
    } else {
      switch (x1.tag | 0) {
        case 0 : 
            if (typeof x2 === "number" || x2.tag) {
              return false;
            } else {
              return Caml_obj.caml_equal(x1[0], x2[0]);
            }
        case 1 : 
            if (typeof x2 === "number" || x2.tag !== 1) {
              return false;
            } else {
              return eq_list(x1[0], x2[0]);
            }
        case 2 : 
            if (typeof x2 === "number" || x2.tag !== 2) {
              return false;
            } else {
              return eq_list(x1[0], x2[0]);
            }
        case 3 : 
            if (typeof x2 === "number" || !(x2.tag === 3 && x1[1] === x2[1] && Caml_obj.caml_equal(x1[2], x2[2]))) {
              return false;
            } else {
              _x2 = x2[0];
              _x1 = x1[0];
              continue ;
            }
        case 4 : 
            if (typeof x2 === "number" || !(x2.tag === 4 && x1[0] === x2[0])) {
              return false;
            } else {
              _x2 = x2[1];
              _x1 = x1[1];
              continue ;
            }
        case 5 : 
            if (typeof x2 === "number" || !(x2.tag === 5 && x1[0] === x2[0])) {
              return false;
            } else {
              _x2 = x2[1];
              _x1 = x1[1];
              continue ;
            }
        case 6 : 
            return false;
        case 7 : 
            if (typeof x2 === "number" || x2.tag !== 7) {
              return false;
            } else {
              _x2 = x2[0];
              _x1 = x1[0];
              continue ;
            }
        case 8 : 
            if (typeof x2 === "number" || x2.tag !== 8) {
              return false;
            } else {
              _x2 = x2[0];
              _x1 = x1[0];
              continue ;
            }
        case 9 : 
            if (typeof x2 === "number" || x2.tag !== 9) {
              return false;
            } else {
              _x2 = x2[0];
              _x1 = x1[0];
              continue ;
            }
        case 10 : 
            if (typeof x2 === "number" || x2.tag !== 10) {
              return false;
            } else {
              _x2 = x2[0];
              _x1 = x1[0];
              continue ;
            }
        case 11 : 
            if (typeof x2 === "number" || x2.tag !== 11) {
              return false;
            } else {
              return eq_list(x1[0], x2[0]);
            }
        case 12 : 
            if (typeof x2 === "number" || x2.tag !== 12) {
              return false;
            } else {
              return eq_list(x1[0], x2[0]);
            }
        case 13 : 
            if (typeof x2 === "number" || !(x2.tag === 13 && equal$2(x1[0], x2[0]))) {
              return false;
            } else {
              _x2 = x2[1];
              _x1 = x1[1];
              continue ;
            }
        case 14 : 
            if (typeof x2 === "number" || !(x2.tag === 14 && x1[0] === x2[0])) {
              return false;
            } else {
              _x2 = x2[1];
              _x1 = x1[1];
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
    if (l1) {
      if (l2 && equal$2(l1[0], l2[0])) {
        _l2 = l2[1];
        _l1 = l1[1];
        continue ;
      } else {
        return false;
      }
    } else if (l2) {
      return false;
    } else {
      return true;
    }
  };
}

function sequence(l) {
  if (l && !l[1]) {
    return l[0];
  } else {
    return /* Sequence */Block.__(1, [l]);
  }
}

function merge_sequences(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var x = param[0];
      var exit = 0;
      if (typeof x === "number") {
        exit = 1;
      } else {
        switch (x.tag | 0) {
          case 1 : 
              var match = x[0];
              if (match) {
                var y = match[1];
                var x$1 = match[0];
                var r$prime = merge_sequences(param[1]);
                var exit$1 = 0;
                if (r$prime) {
                  var match$1 = r$prime[0];
                  if (typeof match$1 === "number" || match$1.tag !== 1) {
                    exit$1 = 2;
                  } else {
                    var match$2 = match$1[0];
                    if (match$2 && equal$2(x$1, match$2[0])) {
                      return /* :: */[
                              /* Sequence */Block.__(1, [/* :: */[
                                    x$1,
                                    /* :: */[
                                      /* Alternative */Block.__(2, [/* :: */[
                                            sequence(y),
                                            /* :: */[
                                              sequence(match$2[1]),
                                              /* [] */0
                                            ]
                                          ]]),
                                      /* [] */0
                                    ]
                                  ]]),
                              r$prime[1]
                            ];
                    } else {
                      exit$1 = 2;
                    }
                  }
                } else {
                  exit$1 = 2;
                }
                if (exit$1 === 2) {
                  return /* :: */[
                          /* Sequence */Block.__(1, [/* :: */[
                                x$1,
                                y
                              ]]),
                          r$prime
                        ];
                }
                
              } else {
                exit = 1;
              }
              break;
          case 2 : 
              _param = Pervasives.$at(x[0], param[1]);
              continue ;
          default:
            exit = 1;
        }
      }
      if (exit === 1) {
        return /* :: */[
                x,
                merge_sequences(param[1])
              ];
      }
      
    } else {
      return /* [] */0;
    }
  };
}

function enforce_kind(ids, kind, kind$prime, cr) {
  if (kind !== 332064784 || kind$prime === 332064784) {
    return cr;
  } else {
    return seq$1(ids, kind$prime, cr, mk_expr(ids, /* Eps */0));
  }
}

function translate(ids, kind, _ign_group, ign_case, _greedy, pos, cache, c, _param) {
  while(true) {
    var param = _param;
    var greedy = _greedy;
    var ign_group = _ign_group;
    if (typeof param === "number") {
      switch (param) {
        case 0 : 
            var c$1 = Curry._2(Re_automata_000[/* ++ */0], Re_automata_000[/* inexistant */2], Re_automata_000[/* newline */5]);
            return /* tuple */[
                    mk_expr(ids, /* After */Block.__(7, [c$1])),
                    kind
                  ];
        case 1 : 
            var c$2 = Curry._2(Re_automata_000[/* ++ */0], Re_automata_000[/* inexistant */2], Re_automata_000[/* newline */5]);
            return /* tuple */[
                    mk_expr(ids, /* Before */Block.__(6, [c$2])),
                    kind
                  ];
        case 2 : 
            var c$3 = Curry._2(Re_automata_000[/* ++ */0], Re_automata_000[/* inexistant */2], Re_automata_000[/* not_letter */4]);
            var c$4 = Curry._2(Re_automata_000[/* ++ */0], Re_automata_000[/* inexistant */2], Re_automata_000[/* letter */3]);
            return /* tuple */[
                    seq$1(ids, /* First */332064784, mk_expr(ids, /* After */Block.__(7, [c$3])), mk_expr(ids, /* Before */Block.__(6, [c$4]))),
                    kind
                  ];
        case 3 : 
            var c$5 = Curry._2(Re_automata_000[/* ++ */0], Re_automata_000[/* inexistant */2], Re_automata_000[/* letter */3]);
            var c$6 = Curry._2(Re_automata_000[/* ++ */0], Re_automata_000[/* inexistant */2], Re_automata_000[/* not_letter */4]);
            return /* tuple */[
                    seq$1(ids, /* First */332064784, mk_expr(ids, /* After */Block.__(7, [c$5])), mk_expr(ids, /* Before */Block.__(6, [c$6]))),
                    kind
                  ];
        case 4 : 
            return /* tuple */[
                    alt(ids, /* :: */[
                          seq$1(ids, /* First */332064784, mk_expr(ids, /* After */Block.__(7, [Re_automata_000[/* letter */3]])), mk_expr(ids, /* Before */Block.__(6, [Re_automata_000[/* letter */3]]))),
                          /* :: */[
                            seq$1(ids, /* First */332064784, mk_expr(ids, /* After */Block.__(7, [Re_automata_000[/* letter */3]])), mk_expr(ids, /* Before */Block.__(6, [Re_automata_000[/* letter */3]]))),
                            /* [] */0
                          ]
                        ]),
                    kind
                  ];
        case 5 : 
            return /* tuple */[
                    mk_expr(ids, /* After */Block.__(7, [Re_automata_000[/* inexistant */2]])),
                    kind
                  ];
        case 6 : 
            return /* tuple */[
                    mk_expr(ids, /* Before */Block.__(6, [Re_automata_000[/* inexistant */2]])),
                    kind
                  ];
        case 7 : 
            var c$7 = Curry._2(Re_automata_000[/* ++ */0], Re_automata_000[/* inexistant */2], Re_automata_000[/* lastnewline */6]);
            return /* tuple */[
                    mk_expr(ids, /* Before */Block.__(6, [c$7])),
                    kind
                  ];
        case 8 : 
            return /* tuple */[
                    mk_expr(ids, /* After */Block.__(7, [Re_automata_000[/* search_boundary */7]])),
                    kind
                  ];
        case 9 : 
            return /* tuple */[
                    mk_expr(ids, /* Before */Block.__(6, [Re_automata_000[/* search_boundary */7]])),
                    kind
                  ];
        
      }
    } else {
      switch (param.tag | 0) {
        case 0 : 
            return /* tuple */[
                    cst(ids, trans_set(cache, c, param[0])),
                    kind
                  ];
        case 1 : 
            return /* tuple */[
                    trans_seq(ids, kind, ign_group, ign_case, greedy, pos, cache, c, param[0]),
                    kind
                  ];
        case 2 : 
            var merged_sequences = merge_sequences(param[0]);
            var exit = 0;
            if (merged_sequences && !merged_sequences[1]) {
              var match = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, merged_sequences[0]);
              return /* tuple */[
                      enforce_kind(ids, kind, match[1], match[0]),
                      kind
                    ];
            } else {
              exit = 1;
            }
            if (exit === 1) {
              return /* tuple */[
                      alt(ids, List.map((function(ign_group,greedy){
                              return function (r$prime) {
                                var match = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r$prime);
                                return enforce_kind(ids, kind, match[1], match[0]);
                              }
                              }(ign_group,greedy)), merged_sequences)),
                      kind
                    ];
            }
            break;
        case 3 : 
            var j = param[2];
            var i = param[1];
            var match$1 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, param[0]);
            var kind$prime = match$1[1];
            var cr = match$1[0];
            var rem;
            if (j !== undefined) {
              var f = greedy >= 620821490 ? (function(cr,kind$prime){
                return function (rem) {
                  return alt(ids, /* :: */[
                              mk_expr(ids, /* Eps */0),
                              /* :: */[
                                seq$1(ids, kind$prime, rename(ids, cr), rem),
                                /* [] */0
                              ]
                            ]);
                }
                }(cr,kind$prime)) : (function(cr,kind$prime){
                return function (rem) {
                  return alt(ids, /* :: */[
                              seq$1(ids, kind$prime, rename(ids, cr), rem),
                              /* :: */[
                                mk_expr(ids, /* Eps */0),
                                /* [] */0
                              ]
                            ]);
                }
                }(cr,kind$prime));
              rem = iter(j - i | 0, f, mk_expr(ids, /* Eps */0));
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
        case 4 : 
            var kind$prime$1 = param[0];
            var match$2 = translate(ids, kind$prime$1, ign_group, ign_case, greedy, pos, cache, c, param[1]);
            return /* tuple */[
                    enforce_kind(ids, kind$prime$1, match$2[1], match$2[0]),
                    kind$prime$1
                  ];
        case 5 : 
            _param = param[1];
            _greedy = param[0];
            continue ;
        case 6 : 
            var r$prime = param[0];
            if (ign_group) {
              _param = r$prime;
              continue ;
            } else {
              var p = pos[0];
              pos[0] = pos[0] + 2 | 0;
              var match$3 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r$prime);
              return /* tuple */[
                      seq$1(ids, /* First */332064784, mk_expr(ids, /* Mark */Block.__(4, [p])), seq$1(ids, /* First */332064784, match$3[0], mk_expr(ids, /* Mark */Block.__(4, [p + 1 | 0])))),
                      match$3[1]
                    ];
            }
        case 7 : 
            _param = param[0];
            _ign_group = true;
            continue ;
        case 8 : 
            var b = pos[0];
            var match$4 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, param[0]);
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
        case 14 : 
            var match$5 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, param[1]);
            return /* tuple */[
                    seq$1(ids, /* First */332064784, mk_expr(ids, /* Pmark */Block.__(8, [param[0]])), match$5[0]),
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
  if (param) {
    var rem = param[1];
    var r = param[0];
    if (rem) {
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
    return mk_expr(ids, /* Eps */0);
  }
}

function case_insens(s) {
  return union(s, union(offset(32, inter(s, cupper)), offset(-32, inter(s, clower))));
}

function as_set(param) {
  if (typeof param === "number") {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "re.ml",
            747,
            13
          ]
        ];
  } else if (param.tag) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "re.ml",
            747,
            13
          ]
        ];
  } else {
    return param[0];
  }
}

function handle_case(_ign_case, _r) {
  while(true) {
    var r = _r;
    var ign_case = _ign_case;
    if (typeof r === "number") {
      return r;
    } else {
      switch (r.tag | 0) {
        case 0 : 
            var s = r[0];
            return /* Set */Block.__(0, [ign_case ? case_insens(s) : s]);
        case 1 : 
            return /* Sequence */Block.__(1, [List.map((function(ign_case){
                          return function (param) {
                            return handle_case(ign_case, param);
                          }
                          }(ign_case)), r[0])]);
        case 2 : 
            var l$prime = List.map((function(ign_case){
                return function (param) {
                  return handle_case(ign_case, param);
                }
                }(ign_case)), r[0]);
            if (is_charset(/* Alternative */Block.__(2, [l$prime]))) {
              return /* Set */Block.__(0, [List.fold_left((function (s, r) {
                                return union(s, as_set(r));
                              }), /* [] */0, l$prime)]);
            } else {
              return /* Alternative */Block.__(2, [l$prime]);
            }
        case 3 : 
            return /* Repeat */Block.__(3, [
                      handle_case(ign_case, r[0]),
                      r[1],
                      r[2]
                    ]);
        case 4 : 
            var r$prime = handle_case(ign_case, r[1]);
            if (is_charset(r$prime)) {
              return r$prime;
            } else {
              return /* Sem */Block.__(4, [
                        r[0],
                        r$prime
                      ]);
            }
        case 5 : 
            var r$prime$1 = handle_case(ign_case, r[1]);
            if (is_charset(r$prime$1)) {
              return r$prime$1;
            } else {
              return /* Sem_greedy */Block.__(5, [
                        r[0],
                        r$prime$1
                      ]);
            }
        case 6 : 
            return /* Group */Block.__(6, [handle_case(ign_case, r[0])]);
        case 7 : 
            var r$prime$2 = handle_case(ign_case, r[0]);
            if (is_charset(r$prime$2)) {
              return r$prime$2;
            } else {
              return /* No_group */Block.__(7, [r$prime$2]);
            }
        case 8 : 
            var r$prime$3 = handle_case(ign_case, r[0]);
            if (is_charset(r$prime$3)) {
              return r$prime$3;
            } else {
              return /* Nest */Block.__(8, [r$prime$3]);
            }
        case 9 : 
            _r = r[0];
            _ign_case = false;
            continue ;
        case 10 : 
            _r = r[0];
            _ign_case = true;
            continue ;
        case 11 : 
            var l$prime$1 = List.map((function(ign_case){
                return function (r) {
                  return handle_case(ign_case, r);
                }
                }(ign_case)), r[0]);
            return /* Set */Block.__(0, [List.fold_left((function (s, r) {
                              return inter(s, as_set(r));
                            }), cany, l$prime$1)]);
        case 12 : 
            var l$prime$2 = List.map((function(ign_case){
                return function (r) {
                  return handle_case(ign_case, r);
                }
                }(ign_case)), r[0]);
            return /* Set */Block.__(0, [diff(cany, List.fold_left((function (s, r) {
                                  return union(s, as_set(r));
                                }), /* [] */0, l$prime$2))]);
        case 13 : 
            return /* Set */Block.__(0, [inter(as_set(handle_case(ign_case, r[0])), diff(cany, as_set(handle_case(ign_case, r[1]))))]);
        case 14 : 
            return /* Pmark */Block.__(14, [
                      r[0],
                      handle_case(ign_case, r[1])
                    ]);
        
      }
    }
  };
}

function anchored(_param) {
  while(true) {
    var param = _param;
    if (typeof param === "number") {
      switch (param) {
        case 5 : 
        case 8 : 
            return true;
        default:
          return false;
      }
    } else {
      switch (param.tag | 0) {
        case 1 : 
            return List.exists(anchored, param[0]);
        case 2 : 
            return List.for_all(anchored, param[0]);
        case 3 : 
            if (param[1] > 0) {
              _param = param[0];
              continue ;
            } else {
              return false;
            }
        case 6 : 
        case 7 : 
        case 8 : 
        case 9 : 
        case 10 : 
            _param = param[0];
            continue ;
        case 4 : 
        case 5 : 
        case 14 : 
            _param = param[1];
            continue ;
        default:
          return false;
      }
    }
  };
}

function alt$1(l) {
  if (l && !l[1]) {
    return l[0];
  } else {
    return /* Alternative */Block.__(2, [l]);
  }
}

function seq$2(l) {
  if (l && !l[1]) {
    return l[0];
  } else {
    return /* Sequence */Block.__(1, [l]);
  }
}

var epsilon = /* Sequence */Block.__(1, [/* [] */0]);

function repn(r, i, j) {
  if (i < 0) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Re.repn"
        ];
  }
  if (j !== undefined) {
    if (j < i) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Re.repn"
          ];
    }
    
  }
  return /* Repeat */Block.__(3, [
            r,
            i,
            j
          ]);
}

function set(str) {
  var s = /* [] */0;
  for(var i = 0 ,i_finish = str.length - 1 | 0; i <= i_finish; ++i){
    s = union(single(Caml_string.get(str, i)), s);
  }
  return /* Set */Block.__(0, [s]);
}

function compl(l) {
  var r = /* Complement */Block.__(12, [l]);
  if (is_charset(r)) {
    return r;
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Re.compl"
        ];
  }
}

var any = /* Set */Block.__(0, [cany]);

var notnl = /* Set */Block.__(0, [diff(cany, /* :: */[
          /* tuple */[
            /* "\n" */10,
            /* "\n" */10
          ],
          /* [] */0
        ])]);

var lower = alt$1(/* :: */[
      /* Set */Block.__(0, [seq(/* "a" */97, /* "z" */122)]),
      /* :: */[
        /* Set */Block.__(0, [/* :: */[
              /* tuple */[
                /* "\181" */181,
                /* "\181" */181
              ],
              /* [] */0
            ]]),
        /* :: */[
          /* Set */Block.__(0, [seq(/* "\223" */223, /* "\246" */246)]),
          /* :: */[
            /* Set */Block.__(0, [seq(/* "\248" */248, /* "\255" */255)]),
            /* [] */0
          ]
        ]
      ]
    ]);

var upper = alt$1(/* :: */[
      /* Set */Block.__(0, [seq(/* "A" */65, /* "Z" */90)]),
      /* :: */[
        /* Set */Block.__(0, [seq(/* "\192" */192, /* "\214" */214)]),
        /* :: */[
          /* Set */Block.__(0, [seq(/* "\216" */216, /* "\222" */222)]),
          /* [] */0
        ]
      ]
    ]);

var alpha = alt$1(/* :: */[
      lower,
      /* :: */[
        upper,
        /* :: */[
          /* Set */Block.__(0, [/* :: */[
                /* tuple */[
                  /* "\170" */170,
                  /* "\170" */170
                ],
                /* [] */0
              ]]),
          /* :: */[
            /* Set */Block.__(0, [/* :: */[
                  /* tuple */[
                    /* "\186" */186,
                    /* "\186" */186
                  ],
                  /* [] */0
                ]]),
            /* [] */0
          ]
        ]
      ]
    ]);

var digit = /* Set */Block.__(0, [seq(/* "0" */48, /* "9" */57)]);

var alnum = alt$1(/* :: */[
      alpha,
      /* :: */[
        digit,
        /* [] */0
      ]
    ]);

var wordc = alt$1(/* :: */[
      alnum,
      /* :: */[
        /* Set */Block.__(0, [/* :: */[
              /* tuple */[
                /* "_" */95,
                /* "_" */95
              ],
              /* [] */0
            ]]),
        /* [] */0
      ]
    ]);

var ascii = /* Set */Block.__(0, [seq(/* "\000" */0, /* "\127" */127)]);

var blank = set("\t ");

var cntrl = alt$1(/* :: */[
      /* Set */Block.__(0, [seq(/* "\000" */0, /* "\031" */31)]),
      /* :: */[
        /* Set */Block.__(0, [seq(/* "\127" */127, /* "\159" */159)]),
        /* [] */0
      ]
    ]);

var graph = alt$1(/* :: */[
      /* Set */Block.__(0, [seq(/* "!" */33, /* "~" */126)]),
      /* :: */[
        /* Set */Block.__(0, [seq(/* "\160" */160, /* "\255" */255)]),
        /* [] */0
      ]
    ]);

var print = alt$1(/* :: */[
      /* Set */Block.__(0, [seq(/* " " */32, /* "~" */126)]),
      /* :: */[
        /* Set */Block.__(0, [seq(/* "\160" */160, /* "\255" */255)]),
        /* [] */0
      ]
    ]);

var punct = alt$1(/* :: */[
      /* Set */Block.__(0, [seq(/* "!" */33, /* "/" */47)]),
      /* :: */[
        /* Set */Block.__(0, [seq(/* ":" */58, /* "@" */64)]),
        /* :: */[
          /* Set */Block.__(0, [seq(/* "[" */91, /* "`" */96)]),
          /* :: */[
            /* Set */Block.__(0, [seq(/* "{" */123, /* "~" */126)]),
            /* :: */[
              /* Set */Block.__(0, [seq(/* "\160" */160, /* "\169" */169)]),
              /* :: */[
                /* Set */Block.__(0, [seq(/* "\171" */171, /* "\180" */180)]),
                /* :: */[
                  /* Set */Block.__(0, [seq(/* "\182" */182, /* "\185" */185)]),
                  /* :: */[
                    /* Set */Block.__(0, [seq(/* "\187" */187, /* "\191" */191)]),
                    /* :: */[
                      /* Set */Block.__(0, [/* :: */[
                            /* tuple */[
                              /* "\215" */215,
                              /* "\215" */215
                            ],
                            /* [] */0
                          ]]),
                      /* :: */[
                        /* Set */Block.__(0, [/* :: */[
                              /* tuple */[
                                /* "\247" */247,
                                /* "\247" */247
                              ],
                              /* [] */0
                            ]]),
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
    ]);

var space = alt$1(/* :: */[
      /* Set */Block.__(0, [/* :: */[
            /* tuple */[
              /* " " */32,
              /* " " */32
            ],
            /* [] */0
          ]]),
      /* :: */[
        /* Set */Block.__(0, [seq(/* "\t" */9, /* "\r" */13)]),
        /* [] */0
      ]
    ]);

var xdigit = alt$1(/* :: */[
      digit,
      /* :: */[
        /* Set */Block.__(0, [seq(/* "a" */97, /* "f" */102)]),
        /* :: */[
          /* Set */Block.__(0, [seq(/* "A" */65, /* "F" */70)]),
          /* [] */0
        ]
      ]
    ]);

function compile(r) {
  var regexp = anchored(r) ? /* Group */Block.__(6, [r]) : seq$2(/* :: */[
          /* Sem */Block.__(4, [
              /* Shortest */-1034406550,
              repn(any, 0, undefined)
            ]),
          /* :: */[
            /* Group */Block.__(6, [r]),
            /* [] */0
          ]
        ]);
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
  var match$1 = translate(ids, /* First */332064784, false, false, /* Greedy */-904640576, pos, /* record */[/* contents : Empty */0], col, regexp$1);
  var r$1 = enforce_kind(ids, /* First */332064784, match$1[1], match$1[0]);
  var init = r$1;
  var cols = col;
  var col_repr = match[1];
  var ncol$2 = ncol$1;
  var lnl$1 = lnl;
  var group_count = pos[0] / 2 | 0;
  return /* record */[
          /* initial */init,
          /* initial_states : [] */0,
          /* cols */cols,
          /* col_repr */col_repr,
          /* ncol */ncol$2,
          /* lnl */lnl$1,
          /* tbl : record */[/* contents : array */[false]],
          /* states */Curry._1(Re_automata_022[/* Table */2][/* create */0], 97),
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
  var initial_cat = pos$1 === 0 ? Curry._2(Re_automata_000[/* ++ */0], Re_automata_000[/* search_boundary */7], Re_automata_000[/* inexistant */2]) : Curry._2(Re_automata_000[/* ++ */0], Re_automata_000[/* search_boundary */7], category(re$1, get_color(re$1, s$1, pos$1 - 1 | 0)));
  var initial_state = find_initial_state(re$1, initial_cat);
  var st = scan_str(info, s$1, initial_state, groups$1);
  var res;
  if (st[/* idx */0] === -3 || partial) {
    res = status(st[/* desc */4]);
  } else {
    var final_cat = last === slen ? Curry._2(Re_automata_000[/* ++ */0], Re_automata_000[/* search_boundary */7], Re_automata_000[/* inexistant */2]) : Curry._2(Re_automata_000[/* ++ */0], Re_automata_000[/* search_boundary */7], category(re$1, get_color(re$1, s$1, last)));
    var match = $$final(info, st, final_cat);
    if (groups$1) {
      Caml_array.caml_array_set(info[/* positions */2], match[0], last + 1 | 0);
    }
    res = match[1];
  }
  if (typeof res === "number") {
    if (res !== 0) {
      return /* Running */1;
    } else {
      return /* Failed */0;
    }
  } else {
    return /* Match */[/* record */[
              /* s */s$1,
              /* marks */res[0],
              /* pmarks */res[1],
              /* gpos */info[/* positions */2],
              /* gcount */re$1[/* group_count */8]
            ]];
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
      return /* Sem_greedy */Block.__(5, [
                /* Non_greedy */620821490,
                r
              ]);
    } else {
      return /* Sem_greedy */Block.__(5, [
                /* Greedy */-904640576,
                r
              ]);
    }
  };
  var regexp$prime = function (_left) {
    while(true) {
      var left = _left;
      if (accept(/* "|" */124)) {
        _left = alt$1(/* :: */[
              left,
              /* :: */[
                branch$prime(/* [] */0),
                /* [] */0
              ]
            ]);
        continue ;
      } else {
        return left;
      }
    };
  };
  var branch$prime = function (_left) {
    while(true) {
      var left = _left;
      if (i[0] === l || test(/* "|" */124) || test(/* ")" */41)) {
        return seq$2(List.rev(left));
      } else {
        _left = /* :: */[
          piece(/* () */0),
          left
        ];
        continue ;
      }
    };
  };
  var bracket = function (_s) {
    while(true) {
      var s = _s;
      if (s !== /* [] */0 && accept(/* "]" */93)) {
        return s;
      } else {
        var match = $$char(/* () */0);
        if (match[0] >= 748194550) {
          var c = match[1];
          if (accept(/* "-" */45)) {
            if (accept(/* "]" */93)) {
              return /* :: */[
                      /* Set */Block.__(0, [single(c)]),
                      /* :: */[
                        /* Set */Block.__(0, [/* :: */[
                              /* tuple */[
                                /* "-" */45,
                                /* "-" */45
                              ],
                              /* [] */0
                            ]]),
                        s
                      ]
                    ];
            } else {
              var match$1 = $$char(/* () */0);
              if (match$1[0] >= 748194550) {
                _s = /* :: */[
                  /* Set */Block.__(0, [seq(c, match$1[1])]),
                  s
                ];
                continue ;
              } else {
                return /* :: */[
                        /* Set */Block.__(0, [single(c)]),
                        /* :: */[
                          /* Set */Block.__(0, [/* :: */[
                                /* tuple */[
                                  /* "-" */45,
                                  /* "-" */45
                                ],
                                /* [] */0
                              ]]),
                          /* :: */[
                            match$1[1],
                            s
                          ]
                        ]
                      ];
              }
            }
          } else {
            _s = /* :: */[
              /* Set */Block.__(0, [single(c)]),
              s
            ];
            continue ;
          }
        } else {
          _s = /* :: */[
            match[1],
            s
          ];
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
        if (j !== undefined) {
          if (j < i$1) {
            throw Parse_error;
          }
          
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
          cls = List.find(accept_s, /* :: */[
                "alnum",
                /* :: */[
                  "ascii",
                  /* :: */[
                    "blank",
                    /* :: */[
                      "cntrl",
                      /* :: */[
                        "digit",
                        /* :: */[
                          "lower",
                          /* :: */[
                            "print",
                            /* :: */[
                              "space",
                              /* :: */[
                                "upper",
                                /* :: */[
                                  "word",
                                  /* :: */[
                                    "punct",
                                    /* :: */[
                                      "graph",
                                      /* :: */[
                                        "xdigit",
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
              ]);
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
        var re = compl$1 ? compl(/* :: */[
                posix_class,
                /* [] */0
              ]) : posix_class;
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
                        compl(/* :: */[
                              digit,
                              /* [] */0
                            ])
                      ];
            case 25 : 
                return /* `Set */[
                        4150146,
                        compl(/* :: */[
                              space,
                              /* [] */0
                            ])
                      ];
            case 29 : 
                return /* `Set */[
                        4150146,
                        compl(/* :: */[
                              alnum,
                              /* :: */[
                                /* Set */Block.__(0, [/* :: */[
                                      /* tuple */[
                                        /* "_" */95,
                                        /* "_" */95
                                      ],
                                      /* [] */0
                                    ]]),
                                /* [] */0
                              ]
                            ])
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
                        alt$1(/* :: */[
                              alnum,
                              /* :: */[
                                /* Set */Block.__(0, [/* :: */[
                                      /* tuple */[
                                        /* "_" */95,
                                        /* "_" */95
                                      ],
                                      /* [] */0
                                    ]]),
                                /* [] */0
                              ]
                            ])
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
          var r = regexp$prime(branch$prime(/* [] */0));
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
        var r$1 = regexp$prime(branch$prime(/* [] */0));
        if (!accept(/* ")" */41)) {
          throw Parse_error;
        }
        return /* Group */Block.__(6, [r$1]);
      }
    } else if (accept(/* "^" */94)) {
      if (multiline) {
        return /* Beg_of_line */0;
      } else {
        return /* Beg_of_str */5;
      }
    } else if (accept(/* "$" */36)) {
      if (multiline) {
        return /* End_of_line */1;
      } else if (dollar_endonly) {
        return /* Last_end_of_line */7;
      } else {
        return /* End_of_str */6;
      }
    } else if (accept(/* "[" */91)) {
      if (accept(/* "^" */94)) {
        return compl(bracket(/* [] */0));
      } else {
        return alt$1(bracket(/* [] */0));
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
            return /* Beg_of_str */5;
        case 66 : 
            return /* Not_bound */4;
        case 68 : 
            return compl(/* :: */[
                        digit,
                        /* [] */0
                      ]);
        case 71 : 
            return /* Start */8;
        case 83 : 
            return compl(/* :: */[
                        space,
                        /* [] */0
                      ]);
        case 87 : 
            return compl(/* :: */[
                        alnum,
                        /* :: */[
                          /* Set */Block.__(0, [/* :: */[
                                /* tuple */[
                                  /* "_" */95,
                                  /* "_" */95
                                ],
                                /* [] */0
                              ]]),
                          /* [] */0
                        ]
                      ]);
        case 90 : 
            return /* Last_end_of_line */7;
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
            return /* Set */Block.__(0, [single(c)]);
        case 98 : 
            return alt$1(/* :: */[
                        /* Beg_of_word */2,
                        /* :: */[
                          /* End_of_word */3,
                          /* [] */0
                        ]
                      ]);
        case 100 : 
            return digit;
        case 115 : 
            return space;
        case 119 : 
            return alt$1(/* :: */[
                        alnum,
                        /* :: */[
                          /* Set */Block.__(0, [/* :: */[
                                /* tuple */[
                                  /* "_" */95,
                                  /* "_" */95
                                ],
                                /* [] */0
                              ]]),
                          /* [] */0
                        ]
                      ]);
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
            return /* End_of_str */6;
        default:
          return /* Set */Block.__(0, [single(c)]);
      }
    } else {
      if (i[0] === l) {
        throw Parse_error;
      }
      var c$1 = get(/* () */0);
      if (c$1 >= 64) {
        if (c$1 !== 92) {
          if (c$1 !== 123) {
            return /* Set */Block.__(0, [single(c$1)]);
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
        return /* Set */Block.__(0, [single(c$1)]);
      } else {
        if (c$1 >= 42) {
          throw Parse_error;
        }
        return /* Set */Block.__(0, [single(c$1)]);
      }
    }
  };
  var integer = function (param) {
    if (i[0] === l) {
      return undefined;
    } else {
      var d = get(/* () */0);
      if (d > 57 || d < 48) {
        i[0] = i[0] - 1 | 0;
        return undefined;
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
  var res = regexp$prime(branch$prime(/* [] */0));
  if (i[0] !== l) {
    throw Parse_error;
  }
  return res;
}

function re($staropt$star, pat) {
  var flags = $staropt$star !== undefined ? $staropt$star : /* [] */0;
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
  var opts$1 = $staropt$star$1 !== undefined ? $staropt$star$1 : /* [] */0;
  var r = parse(List.memq(/* Multiline */1071952589, opts$1), List.memq(/* Dollar_endonly */-712595228, opts$1), List.memq(/* Dotall */-424303016, opts$1), List.memq(/* Ungreedy */-243745063, opts$1), s);
  var r$1 = List.memq(/* Anchored */616470068, opts$1) ? seq$2(/* :: */[
          /* Start */8,
          /* :: */[
            r,
            /* [] */0
          ]
        ]) : r;
  if (List.memq(/* Caseless */604571177, opts$1)) {
    return /* No_case */Block.__(10, [r$1]);
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
  if (typeof match === "number") {
    throw Caml_builtin_exceptions.not_found;
  }
  return match[0];
}

var s = Caml_bytes.bytes_to_string(Bytes.make(1048575, /* "a" */97)) + "b";

eq("File \"xx.ml\", line 7, characters 3-10", get(exec(compile(re(undefined, "aa?b")), undefined, s), 0), "aab");

Mt.from_pair_suites("Ocaml_re_test", suites[0]);

/* Table Not a pure module */
