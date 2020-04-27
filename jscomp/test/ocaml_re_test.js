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
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  
}

function union(_l, _l$prime) {
  while(true) {
    var l$prime = _l$prime;
    var l = _l;
    if (!l$prime) {
      return l;
    }
    if (!l) {
      return l$prime;
    }
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
    }
    if ((c2$prime + 1 | 0) < c1) {
      return /* :: */[
              /* tuple */[
                c1$prime,
                c2$prime
              ],
              union(l, r$prime)
            ];
    }
    if (c2 < c2$prime) {
      _l$prime = /* :: */[
        /* tuple */[
          c1 < c1$prime ? c1 : c1$prime,
          c2$prime
        ],
        r$prime
      ];
      _l = r;
      continue ;
    }
    _l$prime = r$prime;
    _l = /* :: */[
      /* tuple */[
        c1 < c1$prime ? c1 : c1$prime,
        c2
      ],
      r
    ];
    continue ;
  };
}

function inter(_l, _l$prime) {
  while(true) {
    var l$prime = _l$prime;
    var l = _l;
    if (!l$prime) {
      return /* [] */0;
    }
    if (!l) {
      return /* [] */0;
    }
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
    }
    if (!Caml_obj.caml_lessthan(c2$prime, c1)) {
      if (Caml_obj.caml_lessthan(c2, c2$prime)) {
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
    }
    _l$prime = r$prime;
    continue ;
  };
}

function diff(_l, _l$prime) {
  while(true) {
    var l$prime = _l$prime;
    var l = _l;
    if (!l$prime) {
      return l;
    }
    if (!l) {
      return /* [] */0;
    }
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
    }
    if (c2$prime < c1) {
      _l$prime = r$prime;
      continue ;
    }
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
    }
    _l$prime = r$prime;
    _l = r$prime$prime;
    continue ;
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
  if (!l) {
    return /* [] */0;
  }
  var match = l[0];
  return /* :: */[
          /* tuple */[
            match[0] + o | 0,
            match[1] + o | 0
          ],
          offset(o, l[1])
        ];
}

function mem(c, _s) {
  while(true) {
    var s = _s;
    if (!s) {
      return false;
    }
    var match = s[0];
    if (c <= match[1]) {
      return c >= match[0];
    }
    _s = s[1];
    continue ;
  };
}

function hash_rec(param) {
  if (!param) {
    return 0;
  }
  var match = param[0];
  return (match[0] + Caml_int32.imul(13, match[1]) | 0) + Caml_int32.imul(257, hash_rec(param[1])) | 0;
}

function one_char(param) {
  if (!param) {
    return ;
  }
  if (param[1]) {
    return ;
  }
  var match = param[0];
  var i = match[0];
  if (Caml_obj.caml_equal(i, match[1])) {
    return Caml_option.some(i);
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
      }
      if (lr) {
        return create(create(ll, lv, ld, lr[/* l */0]), lr[/* v */1], lr[/* d */2], create(lr[/* r */3], x, d, r));
      }
      throw {
            CamlExt: Caml_builtin_exceptions.invalid_argument,
            _1: "Map.bal"
          };
    }
    throw {
          CamlExt: Caml_builtin_exceptions.invalid_argument,
          _1: "Map.bal"
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */[
            /* l */l,
            /* v */x,
            /* d */d,
            /* r */r,
            /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
  if (r) {
    var rr = r[/* r */3];
    var rd = r[/* d */2];
    var rv = r[/* v */1];
    var rl = r[/* l */0];
    if (height(rr) >= height(rl)) {
      return create(create(l, x, d, rl), rv, rd, rr);
    }
    if (rl) {
      return create(create(l, x, d, rl[/* l */0]), rl[/* v */1], rl[/* d */2], create(rl[/* r */3], rv, rd, rr));
    }
    throw {
          CamlExt: Caml_builtin_exceptions.invalid_argument,
          _1: "Map.bal"
        };
  }
  throw {
        CamlExt: Caml_builtin_exceptions.invalid_argument,
        _1: "Map.bal"
      };
}

function add(x, data, m) {
  if (!m) {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* d */data,
            /* r : Empty */0,
            /* h */1
          ];
  }
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
  }
  if (c < 0) {
    var ll = add(x, data, l);
    if (l === ll) {
      return m;
    } else {
      return bal(ll, v, d, r);
    }
  }
  var rr = add(x, data, r);
  if (r === rr) {
    return m;
  } else {
    return bal(l, v, d, rr);
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
    }
    var switcher$1 = param - 171 | 0;
    if (!(switcher$1 > 14 || switcher$1 < 0) && switcher$1 !== 10) {
      return 4;
    } else {
      return 2;
    }
  }
  if (param < 65) {
    if (param >= 48) {
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
      }
      if (lr) {
        return create$1(create$1(ll, lv, lr[/* l */0]), lr[/* v */1], create$1(lr[/* r */2], v, r));
      }
      throw {
            CamlExt: Caml_builtin_exceptions.invalid_argument,
            _1: "Set.bal"
          };
    }
    throw {
          CamlExt: Caml_builtin_exceptions.invalid_argument,
          _1: "Set.bal"
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */[
            /* l */l,
            /* v */v,
            /* r */r,
            /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
  if (r) {
    var rr = r[/* r */2];
    var rv = r[/* v */1];
    var rl = r[/* l */0];
    if (height$1(rr) >= height$1(rl)) {
      return create$1(create$1(l, v, rl), rv, rr);
    }
    if (rl) {
      return create$1(create$1(l, v, rl[/* l */0]), rl[/* v */1], create$1(rl[/* r */2], rv, rr));
    }
    throw {
          CamlExt: Caml_builtin_exceptions.invalid_argument,
          _1: "Set.bal"
        };
  }
  throw {
        CamlExt: Caml_builtin_exceptions.invalid_argument,
        _1: "Set.bal"
      };
}

function add$1(x, t) {
  if (!t) {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* r : Empty */0,
            /* h */1
          ];
  }
  var r = t[/* r */2];
  var v = t[/* v */1];
  var l = t[/* l */0];
  var c = Caml_primitive.caml_int_compare(x, v);
  if (c === 0) {
    return t;
  }
  if (c < 0) {
    var ll = add$1(x, l);
    if (l === ll) {
      return t;
    } else {
      return bal$1(ll, v, r);
    }
  }
  var rr = add$1(x, r);
  if (r === rr) {
    return t;
  } else {
    return bal$1(l, v, rr);
  }
}

function hash_combine(h, accu) {
  return Caml_int32.imul(accu, 65599) + h | 0;
}

var empty = {
  marks: /* [] */0,
  pmarks: /* Empty */0
};

function hash(m, accu) {
  var _l = m.marks;
  var _accu = hash_combine(Hashtbl.hash(m.pmarks), accu);
  while(true) {
    var accu$1 = _accu;
    var l = _l;
    if (!l) {
      return accu$1;
    }
    var match = l[0];
    _accu = hash_combine(match[0], hash_combine(match[1], accu$1));
    _l = l[1];
    continue ;
  };
}

function marks_set_idx(idx, marks) {
  if (!marks) {
    return marks;
  }
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
}

function marks_set_idx$1(marks, idx) {
  return {
          marks: marks_set_idx(idx, marks.marks),
          pmarks: marks.pmarks
        };
}

function first(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var res = Curry._1(f, param[0]);
    if (res !== undefined) {
      return res;
    }
    _param = param[1];
    continue ;
  };
}

var eps_expr = {
  id: 0,
  def: /* Eps */0
};

function mk_expr(ids, def) {
  ids.contents = ids.contents + 1 | 0;
  return {
          id: ids.contents,
          def: def
        };
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
  var match = x.def;
  var match$1 = y.def;
  var exit = 0;
  if (typeof match === "number") {
    return y;
  }
  if (match.tag === /* Alt */1) {
    if (!match[0]) {
      return x;
    }
    exit = 2;
  } else {
    exit = 2;
  }
  if (exit === 2) {
    if (typeof match$1 === "number") {
      if (kind === /* First */332064784) {
        return x;
      }
      
    } else if (match$1.tag === /* Alt */1 && !match$1[0]) {
      return y;
    }
    
  }
  return mk_expr(ids, /* Seq */Block.__(2, [
                kind,
                x,
                y
              ]));
}

function is_eps(expr) {
  var match = expr.def;
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
  var l = x.def;
  if (typeof l === "number") {
    return mk_expr(ids, x.def);
  }
  switch (l.tag | 0) {
    case /* Alt */1 :
        return mk_expr(ids, /* Alt */Block.__(1, [List.map((function (param) {
                              return rename(ids, param);
                            }), l[0])]));
    case /* Seq */2 :
        return mk_expr(ids, /* Seq */Block.__(2, [
                      l[0],
                      rename(ids, l[1]),
                      rename(ids, l[2])
                    ]));
    case /* Rep */3 :
        return mk_expr(ids, /* Rep */Block.__(3, [
                      l[0],
                      l[1],
                      rename(ids, l[2])
                    ]));
    default:
      return mk_expr(ids, x.def);
  }
}

function equal(_l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (!l1) {
      if (l2) {
        return false;
      } else {
        return true;
      }
    }
    var marks1 = l1[0];
    switch (marks1.tag | 0) {
      case /* TSeq */0 :
          if (!l2) {
            return false;
          }
          var match = l2[0];
          switch (match.tag | 0) {
            case /* TSeq */0 :
                if (marks1[1].id !== match[1].id) {
                  return false;
                }
                if (!equal(marks1[0], match[0])) {
                  return false;
                }
                _l2 = l2[1];
                _l1 = l1[1];
                continue ;
            case /* TExp */1 :
            case /* TMatch */2 :
                return false;
            
          }
      case /* TExp */1 :
          if (!l2) {
            return false;
          }
          var match$1 = l2[0];
          switch (match$1.tag | 0) {
            case /* TExp */1 :
                if (marks1[1].id !== match$1[1].id) {
                  return false;
                }
                if (!Caml_obj.caml_equal(marks1[0], match$1[0])) {
                  return false;
                }
                _l2 = l2[1];
                _l1 = l1[1];
                continue ;
            case /* TSeq */0 :
            case /* TMatch */2 :
                return false;
            
          }
      case /* TMatch */2 :
          if (!l2) {
            return false;
          }
          var marks2 = l2[0];
          switch (marks2.tag | 0) {
            case /* TSeq */0 :
            case /* TExp */1 :
                return false;
            case /* TMatch */2 :
                if (!Caml_obj.caml_equal(marks1[0], marks2[0])) {
                  return false;
                }
                _l2 = l2[1];
                _l1 = l1[1];
                continue ;
            
          }
      
    }
  };
}

function hash$1(_l, _accu) {
  while(true) {
    var accu = _accu;
    var l = _l;
    if (!l) {
      return accu;
    }
    var marks = l[0];
    switch (marks.tag | 0) {
      case /* TSeq */0 :
          _accu = hash_combine(388635598, hash_combine(marks[1].id, hash$1(marks[0], accu)));
          _l = l[1];
          continue ;
      case /* TExp */1 :
          _accu = hash_combine(726404471, hash_combine(marks[1].id, hash(marks[0], accu)));
          _l = l[1];
          continue ;
      case /* TMatch */2 :
          _accu = hash_combine(471882453, hash(marks[0], accu));
          _l = l[1];
          continue ;
      
    }
  };
}

function tseq(kind, x, y, rem) {
  if (!x) {
    return rem;
  }
  var match = x[0];
  switch (match.tag | 0) {
    case /* TExp */1 :
        if (typeof match[1].def === "number" && !x[1]) {
          return /* :: */[
                  /* TExp */Block.__(1, [
                      match[0],
                      y
                    ]),
                  rem
                ];
        }
        break;
    case /* TSeq */0 :
    case /* TMatch */2 :
        break;
    
  }
  return /* :: */[
          /* TSeq */Block.__(0, [
              x,
              y,
              kind
            ]),
          rem
        ];
}

var dummy = {
  idx: -1,
  category: -1,
  desc: /* [] */0,
  status: undefined,
  hash: -1
};

function hash$2(idx, cat, desc) {
  return hash$1(desc, hash_combine(idx, hash_combine(cat, 0))) & 1073741823;
}

function mk(idx, cat, desc) {
  return {
          idx: idx,
          category: cat,
          desc: desc,
          status: undefined,
          hash: hash$2(idx, cat, desc)
        };
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
  if (x.hash === y.hash && x.idx === y.idx && x.category === y.category) {
    return equal(x.desc, y.desc);
  } else {
    return false;
  }
}

function hash$3(t) {
  return t.hash;
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
                    switch (param.tag | 0) {
                      case /* TSeq */0 :
                          return mark_used_indices(tbl)(param[0]);
                      case /* TExp */1 :
                      case /* TMatch */2 :
                          break;
                      
                    }
                    return List.iter((function (param) {
                                  var i = param[1];
                                  if (i >= 0) {
                                    return Caml_array.caml_array_set(tbl, i, true);
                                  }
                                  
                                }), param[0].marks);
                  }), param);
    });
}

function find_free(tbl, _idx, len) {
  while(true) {
    var idx = _idx;
    if (idx === len || !Caml_array.caml_array_get(tbl, idx)) {
      return idx;
    }
    _idx = idx + 1 | 0;
    continue ;
  };
}

function free_index(tbl_ref, l) {
  var tbl = tbl_ref.contents;
  reset_table(tbl);
  mark_used_indices(tbl)(l);
  var len = tbl.length;
  var idx = find_free(tbl, 0, len);
  if (idx === len) {
    tbl_ref.contents = Caml_array.caml_make_vect((len << 1), false);
  }
  return idx;
}

var remove_matches = List.filter((function (param) {
        switch (param.tag | 0) {
          case /* TSeq */0 :
          case /* TExp */1 :
              return true;
          case /* TMatch */2 :
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
        case /* TSeq */0 :
        case /* TExp */1 :
            _param = param[1];
            _l$prime = /* :: */[
              x,
              l$prime
            ];
            continue ;
        case /* TMatch */2 :
            return /* tuple */[
                    List.rev(l$prime),
                    Curry._1(remove_matches, param[1])
                  ];
        
      }
    } else {
      throw {
            CamlExt: Caml_builtin_exceptions.assert_failure,
            _1: /* tuple */[
              "re_automata.ml",
              429,
              21
            ]
          };
    }
  };
}

function remove_duplicates(prev, _l, y) {
  while(true) {
    var l = _l;
    if (!l) {
      return /* tuple */[
              /* [] */0,
              prev
            ];
    }
    var x = l[0];
    switch (x.tag | 0) {
      case /* TSeq */0 :
          var x$1 = x[1];
          var match = remove_duplicates(prev, x[0], x$1);
          var match$1 = remove_duplicates(match[1], l[1], y);
          return /* tuple */[
                  tseq(x[2], match[0], x$1, match$1[0]),
                  match$1[1]
                ];
      case /* TExp */1 :
          var x$2 = x[1];
          if (typeof x$2.def === "number") {
            var r = l[1];
            if (List.memq(y.id, prev)) {
              _l = r;
              continue ;
            }
            var match$2 = remove_duplicates(/* :: */[
                  y.id,
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
          var r$1 = l[1];
          if (List.memq(x$2.id, prev)) {
            _l = r$1;
            continue ;
          }
          var match$3 = remove_duplicates(/* :: */[
                x$2.id,
                prev
              ], r$1, y);
          return /* tuple */[
                  /* :: */[
                    x,
                    match$3[0]
                  ],
                  match$3[1]
                ];
      case /* TMatch */2 :
          return /* tuple */[
                  /* :: */[
                    x,
                    /* [] */0
                  ],
                  prev
                ];
      
    }
  };
}

function set_idx(idx, param) {
  if (!param) {
    return /* [] */0;
  }
  var marks = param[0];
  switch (marks.tag | 0) {
    case /* TSeq */0 :
        return /* :: */[
                /* TSeq */Block.__(0, [
                    set_idx(idx, marks[0]),
                    marks[1],
                    marks[2]
                  ]),
                set_idx(idx, param[1])
              ];
    case /* TExp */1 :
        return /* :: */[
                /* TExp */Block.__(1, [
                    marks_set_idx$1(marks[0], idx),
                    marks[1]
                  ]),
                set_idx(idx, param[1])
              ];
    case /* TMatch */2 :
        return /* :: */[
                /* TMatch */Block.__(2, [marks_set_idx$1(marks[0], idx)]),
                set_idx(idx, param[1])
              ];
    
  }
}

function filter_marks(b, e, marks) {
  return {
          marks: List.filter((function (param) {
                    var i = param[0];
                    if (i < b) {
                      return true;
                    } else {
                      return i > e;
                    }
                  }))(marks.marks),
          pmarks: marks.pmarks
        };
}

function delta_1(marks, c, next_cat, prev_cat, x, rem) {
  var s = x.def;
  if (typeof s === "number") {
    return /* :: */[
            /* TMatch */Block.__(2, [marks]),
            rem
          ];
  }
  switch (s.tag | 0) {
    case /* Cst */0 :
        if (mem(c, s[0])) {
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
    case /* Alt */1 :
        return delta_2(marks, c, next_cat, prev_cat, s[0], rem);
    case /* Seq */2 :
        var y$prime = delta_1(marks, c, next_cat, prev_cat, s[1], /* [] */0);
        return delta_seq(c, next_cat, prev_cat, s[0], y$prime, s[2], rem);
    case /* Rep */3 :
        var kind = s[1];
        var y$prime$1 = delta_1(marks, c, next_cat, prev_cat, s[2], /* [] */0);
        var marks$prime = first((function (marks) {
                switch (marks.tag | 0) {
                  case /* TSeq */0 :
                  case /* TExp */1 :
                      return ;
                  case /* TMatch */2 :
                      return marks[0];
                  
                }
              }), y$prime$1);
        var match = marks$prime !== undefined ? /* tuple */[
            Curry._1(remove_matches, y$prime$1),
            marks$prime
          ] : /* tuple */[
            y$prime$1,
            marks
          ];
        var y$prime$prime = match[0];
        if (s[0] >= 620821490) {
          return /* :: */[
                  /* TMatch */Block.__(2, [marks]),
                  tseq(kind, y$prime$prime, x, rem)
                ];
        } else {
          return tseq(kind, y$prime$prime, x, /* :: */[
                      /* TMatch */Block.__(2, [match[1]]),
                      rem
                    ]);
        }
    case /* Mark */4 :
        var i = s[0];
        var marks_marks = /* :: */[
          /* tuple */[
            i,
            -1
          ],
          List.remove_assq(i, marks.marks)
        ];
        var marks_pmarks = marks.pmarks;
        var marks$1 = {
          marks: marks_marks,
          pmarks: marks_pmarks
        };
        return /* :: */[
                /* TMatch */Block.__(2, [marks$1]),
                rem
              ];
    case /* Erase */5 :
        return /* :: */[
                /* TMatch */Block.__(2, [filter_marks(s[0], s[1], marks)]),
                rem
              ];
    case /* Before */6 :
        if (intersect(next_cat, s[0])) {
          return /* :: */[
                  /* TMatch */Block.__(2, [marks]),
                  rem
                ];
        } else {
          return rem;
        }
    case /* After */7 :
        if (intersect(prev_cat, s[0])) {
          return /* :: */[
                  /* TMatch */Block.__(2, [marks]),
                  rem
                ];
        } else {
          return rem;
        }
    case /* Pmark */8 :
        var marks_marks$1 = marks.marks;
        var marks_pmarks$1 = add$1(s[0], marks.pmarks);
        var marks$2 = {
          marks: marks_marks$1,
          pmarks: marks_pmarks$1
        };
        return /* :: */[
                /* TMatch */Block.__(2, [marks$2]),
                rem
              ];
    
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
  var marks = first((function (marks) {
          switch (marks.tag | 0) {
            case /* TSeq */0 :
            case /* TExp */1 :
                return ;
            case /* TMatch */2 :
                return marks[0];
            
          }
        }), y);
  if (marks === undefined) {
    return tseq(kind, y, z, rem);
  }
  if (kind === -730718166) {
    return tseq(kind, Curry._1(remove_matches, y), z, delta_1(marks, c, next_cat, prev_cat, z, rem));
  }
  if (kind < 332064784) {
    return delta_1(marks, c, next_cat, prev_cat, z, tseq(kind, Curry._1(remove_matches, y), z, rem));
  }
  var match = split_at_match_rec(/* [] */0, y);
  return tseq(kind, match[0], z, delta_1(marks, c, next_cat, prev_cat, z, tseq(kind, match[1], z, rem)));
}

function delta_4(c, next_cat, prev_cat, l, rem) {
  if (l) {
    var x = l[0];
    var rem$1 = delta_4(c, next_cat, prev_cat, l[1], rem);
    switch (x.tag | 0) {
      case /* TSeq */0 :
          var y$prime = delta_4(c, next_cat, prev_cat, x[0], /* [] */0);
          return delta_seq(c, next_cat, prev_cat, x[2], y$prime, x[1], rem$1);
      case /* TExp */1 :
          return delta_1(x[0], c, next_cat, prev_cat, x[1], rem$1);
      case /* TMatch */2 :
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
  var prev_cat = st.category;
  var match = remove_duplicates(/* [] */0, delta_4($$char, next_cat, prev_cat, st.desc, /* [] */0), eps_expr);
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
  var st = s.status;
  if (st !== undefined) {
    return st;
  }
  var match = s.desc;
  var st$1;
  if (match) {
    var m = match[0];
    switch (m.tag | 0) {
      case /* TSeq */0 :
      case /* TExp */1 :
          st$1 = /* Running */1;
          break;
      case /* TMatch */2 :
          var m$1 = m[0];
          st$1 = /* Match */[
            flatten_match(m$1.marks),
            m$1.pmarks
          ];
          break;
      
    }
  } else {
    st$1 = /* Failed */0;
  }
  s.status = st$1;
  return st$1;
}

var Re_automata_Category = {
  $plus$plus: $plus$plus,
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
    }
    _v = Curry._1(f, v);
    _n = n - 1 | 0;
    continue ;
  };
}

function category(re, c) {
  if (c === -1) {
    return Re_automata_Category.inexistant;
  } else if (c === re.lnl) {
    return Curry._2(Re_automata_Category.$plus$plus, Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.lastnewline, Re_automata_Category.newline), Re_automata_Category.not_letter);
  } else {
    return Curry._1(Re_automata_Category.from_char, Caml_bytes.get(re.col_repr, c));
  }
}

var dummy_next = [];

var unknown_state = {
  idx: -2,
  real_idx: 0,
  next: dummy_next,
  final: /* [] */0,
  desc: Re_automata_State.dummy
};

function mk_state(ncol, desc) {
  var match = status(desc);
  var break_state = typeof match === "number" ? match === 0 : true;
  return {
          idx: break_state ? -3 : desc.idx,
          real_idx: desc.idx,
          next: break_state ? dummy_next : Caml_array.caml_make_vect(ncol, unknown_state),
          final: /* [] */0,
          desc: desc
        };
}

function find_state(re, desc) {
  try {
    return Curry._2(Re_automata_State.Table.find, re.states, desc);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.CamlExt === Caml_builtin_exceptions.not_found) {
      var st = mk_state(re.ncol, desc);
      Curry._3(Re_automata_State.Table.add, re.states, desc, st);
      return st;
    }
    throw exn;
  }
}

function delta$1(info, cat, c, st) {
  var desc = delta(info.re.tbl, cat, c, st.desc);
  var len = info.positions.length;
  if (desc.idx === len && len > 0) {
    var pos = info.positions;
    info.positions = Caml_array.caml_make_vect((len << 1), 0);
    $$Array.blit(pos, 0, info.positions, 0, len);
  }
  return desc;
}

function validate(info, s, pos, st) {
  var c = Caml_bytes.get(info.i_cols, Caml_string.get(s, pos));
  var cat = category(info.re, c);
  var desc$prime = delta$1(info, cat, c, st);
  var st$prime = find_state(info.re, desc$prime);
  return Caml_array.caml_array_set(st.next, c, st$prime);
}

function loop(info, s, pos, st) {
  if (pos >= info.last) {
    return st;
  }
  var st$prime = Caml_array.caml_array_get(st.next, Caml_bytes.get(info.i_cols, Caml_string.get(s, pos)));
  var _pos = pos;
  var _st = st;
  var _st$prime = st$prime;
  while(true) {
    var st$prime$1 = _st$prime;
    var st$1 = _st;
    var pos$1 = _pos;
    if (st$prime$1.idx < 0) {
      if (st$prime$1.idx === -3) {
        Caml_array.caml_array_set(info.positions, st$prime$1.real_idx, pos$1 + 1 | 0);
        return st$prime$1;
      } else {
        validate(info, s, pos$1, st$1);
        return loop(info, s, pos$1, st$1);
      }
    }
    var pos$2 = pos$1 + 1 | 0;
    if (pos$2 < info.last) {
      var st$prime$prime = Caml_array.caml_array_get(st$prime$1.next, Caml_bytes.get(info.i_cols, Caml_string.get(s, pos$2)));
      Caml_array.caml_array_set(info.positions, st$prime$1.idx, pos$2);
      _st$prime = st$prime$prime;
      _st = st$prime$1;
      _pos = pos$2;
      continue ;
    }
    Caml_array.caml_array_set(info.positions, st$prime$1.idx, pos$2);
    return st$prime$1;
  };
}

function $$final(info, st, cat) {
  try {
    return List.assq(cat, st.final);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.CamlExt === Caml_builtin_exceptions.not_found) {
      var st$prime = delta$1(info, cat, -1, st);
      var res_000 = st$prime.idx;
      var res_001 = status(st$prime);
      var res = /* tuple */[
        res_000,
        res_001
      ];
      st.final = /* :: */[
        /* tuple */[
          cat,
          res
        ],
        st.final
      ];
      return res;
    }
    throw exn;
  }
}

function find_initial_state(re, cat) {
  try {
    return List.assq(cat, re.initial_states);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.CamlExt === Caml_builtin_exceptions.not_found) {
      var st = find_state(re, Curry._2(Re_automata_State.create, cat, re.initial));
      re.initial_states = /* :: */[
        /* tuple */[
          cat,
          st
        ],
        re.initial_states
      ];
      return st;
    }
    throw exn;
  }
}

function get_color(re, s, pos) {
  if (pos < 0) {
    return -1;
  }
  var slen = s.length;
  if (pos >= slen) {
    return -1;
  } else if (pos === (slen - 1 | 0) && re.lnl !== -1 && Caml_string.get(s, pos) === /* "\n" */10) {
    return re.lnl;
  } else {
    return Caml_bytes.get(re.cols, Caml_string.get(s, pos));
  }
}

function scan_str(info, s, initial_state, groups) {
  var pos = info.pos;
  var last = info.last;
  if (!(last === s.length && info.re.lnl !== -1 && last > pos && Caml_string.get(s, last - 1 | 0) === /* "\n" */10)) {
    if (groups) {
      return loop(info, s, pos, initial_state);
    } else {
      var _pos = pos;
      var _st = initial_state;
      while(true) {
        var st = _st;
        var pos$1 = _pos;
        if (pos$1 >= last) {
          return st;
        }
        var st$prime = Caml_array.caml_array_get(st.next, Caml_bytes.get(info.i_cols, Caml_string.get(s, pos$1)));
        if (st$prime.idx >= 0) {
          _st = st$prime;
          _pos = pos$1 + 1 | 0;
          continue ;
        }
        if (st$prime.idx === -3) {
          return st$prime;
        }
        validate(info, s, pos$1, st);
        continue ;
      };
    }
  }
  var info$1 = {
    re: info.re,
    i_cols: info.i_cols,
    positions: info.positions,
    pos: info.pos,
    last: last - 1 | 0
  };
  var st$1 = scan_str(info$1, s, initial_state, groups);
  if (st$1.idx === -3) {
    return st$1;
  } else {
    var pos$2 = last - 1 | 0;
    while(true) {
      var st$prime$1 = Caml_array.caml_array_get(st$1.next, info$1.re.lnl);
      if (st$prime$1.idx >= 0) {
        if (groups) {
          Caml_array.caml_array_set(info$1.positions, st$prime$1.idx, pos$2 + 1 | 0);
        }
        return st$prime$1;
      }
      if (st$prime$1.idx === -3) {
        if (groups) {
          Caml_array.caml_array_set(info$1.positions, st$prime$1.real_idx, pos$2 + 1 | 0);
        }
        return st$prime$1;
      }
      var c = info$1.re.lnl;
      var real_c = Caml_bytes.get(info$1.i_cols, /* "\n" */10);
      var cat = category(info$1.re, c);
      var desc$prime = delta$1(info$1, cat, real_c, st$1);
      var st$prime$2 = find_state(info$1.re, desc$prime);
      Caml_array.caml_array_set(st$1.next, c, st$prime$2);
      continue ;
    };
  }
}

function cadd(c, s) {
  return union(single(c), s);
}

function trans_set(cache, cm, s) {
  var i = one_char(s);
  if (i !== undefined) {
    return single(Caml_bytes.get(cm, i));
  }
  var v_000 = hash_rec(s);
  var v = /* tuple */[
    v_000,
    s
  ];
  try {
    var _param = cache.contents;
    while(true) {
      var param = _param;
      if (param) {
        var c = compare(v, param[/* v */1]);
        if (c === 0) {
          return param[/* d */2];
        }
        _param = c < 0 ? param[/* l */0] : param[/* r */3];
        continue ;
      }
      throw {
            CamlExt: Caml_builtin_exceptions.not_found
          };
    };
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.CamlExt === Caml_builtin_exceptions.not_found) {
      var l = List.fold_right((function (param, l) {
              return union(seq(Caml_bytes.get(cm, param[0]), Caml_bytes.get(cm, param[1])), l);
            }), s, /* [] */0);
      cache.contents = add(v, l, cache.contents);
      return l;
    }
    throw exn;
  }
}

function is_charset(_param) {
  while(true) {
    var param = _param;
    if (typeof param === "number") {
      return false;
    }
    switch (param.tag | 0) {
      case /* Set */0 :
          return true;
      case /* Sem */4 :
      case /* Sem_greedy */5 :
          _param = param[1];
          continue ;
      case /* No_group */7 :
      case /* Case */9 :
      case /* No_case */10 :
          _param = param[0];
          continue ;
      case /* Alternative */2 :
      case /* Intersection */11 :
      case /* Complement */12 :
          return List.for_all(is_charset, param[0]);
      case /* Difference */13 :
          if (!is_charset(param[0])) {
            return false;
          }
          _param = param[1];
          continue ;
      default:
        return false;
    }
  };
}

function split(s, cm) {
  var _t = s;
  var f = function (i, j) {
    cm[i] = /* "\001" */1;
    cm[j + 1 | 0] = /* "\001" */1;
    
  };
  while(true) {
    var t = _t;
    if (!t) {
      return ;
    }
    var match = t[0];
    Curry._2(f, match[0], match[1]);
    _t = t[1];
    continue ;
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
  var lnl = {
    contents: false
  };
  var colorize$1 = function (_regexp) {
    while(true) {
      var regexp = _regexp;
      if (typeof regexp === "number") {
        switch (regexp) {
          case /* Beg_of_line */0 :
          case /* End_of_line */1 :
              return split(/* :: */[
                          /* tuple */[
                            /* "\n" */10,
                            /* "\n" */10
                          ],
                          /* [] */0
                        ], c);
          case /* Beg_of_word */2 :
          case /* End_of_word */3 :
          case /* Not_bound */4 :
              return split(cword, c);
          case /* Last_end_of_line */7 :
              lnl.contents = true;
              return ;
          case /* Beg_of_str */5 :
          case /* End_of_str */6 :
          case /* Start */8 :
          case /* Stop */9 :
              return ;
          
        }
      } else {
        switch (regexp.tag | 0) {
          case /* Set */0 :
              return split(regexp[0], c);
          case /* Sequence */1 :
          case /* Alternative */2 :
              return List.iter(colorize$1, regexp[0]);
          case /* Repeat */3 :
          case /* Group */6 :
          case /* No_group */7 :
          case /* Nest */8 :
              _regexp = regexp[0];
              continue ;
          case /* Sem */4 :
          case /* Sem_greedy */5 :
          case /* Pmark */14 :
              _regexp = regexp[1];
              continue ;
          default:
            throw {
                  CamlExt: Caml_builtin_exceptions.assert_failure,
                  _1: /* tuple */[
                    "re.ml",
                    502,
                    35
                  ]
                };
        }
      }
    };
  };
  colorize$1(regexp);
  return lnl.contents;
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
        case /* Beg_of_line */0 :
            if (typeof x2 === "number") {
              return x2 === 0;
            } else {
              return false;
            }
        case /* End_of_line */1 :
            if (typeof x2 === "number") {
              return x2 === 1;
            } else {
              return false;
            }
        case /* Beg_of_word */2 :
            if (typeof x2 === "number") {
              return x2 === 2;
            } else {
              return false;
            }
        case /* End_of_word */3 :
            if (typeof x2 === "number") {
              return x2 === 3;
            } else {
              return false;
            }
        case /* Not_bound */4 :
            if (typeof x2 === "number") {
              return x2 === 4;
            } else {
              return false;
            }
        case /* Beg_of_str */5 :
            if (typeof x2 === "number") {
              return x2 === 5;
            } else {
              return false;
            }
        case /* End_of_str */6 :
            if (typeof x2 === "number") {
              return x2 === 6;
            } else {
              return false;
            }
        case /* Last_end_of_line */7 :
            if (typeof x2 === "number") {
              return x2 === 7;
            } else {
              return false;
            }
        case /* Start */8 :
            if (typeof x2 === "number") {
              return x2 === 8;
            } else {
              return false;
            }
        case /* Stop */9 :
            if (typeof x2 === "number") {
              return x2 >= 9;
            } else {
              return false;
            }
        
      }
    } else {
      switch (x1.tag | 0) {
        case /* Set */0 :
            if (typeof x2 === "number" || x2.tag) {
              return false;
            } else {
              return Caml_obj.caml_equal(x1[0], x2[0]);
            }
        case /* Sequence */1 :
            if (typeof x2 === "number" || x2.tag !== /* Sequence */1) {
              return false;
            } else {
              return eq_list(x1[0], x2[0]);
            }
        case /* Alternative */2 :
            if (typeof x2 === "number" || x2.tag !== /* Alternative */2) {
              return false;
            } else {
              return eq_list(x1[0], x2[0]);
            }
        case /* Repeat */3 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.tag !== /* Repeat */3) {
              return false;
            }
            if (x1[1] !== x2[1]) {
              return false;
            }
            if (!Caml_obj.caml_equal(x1[2], x2[2])) {
              return false;
            }
            _x2 = x2[0];
            _x1 = x1[0];
            continue ;
        case /* Sem */4 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.tag !== /* Sem */4) {
              return false;
            }
            if (x1[0] !== x2[0]) {
              return false;
            }
            _x2 = x2[1];
            _x1 = x1[1];
            continue ;
        case /* Sem_greedy */5 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.tag !== /* Sem_greedy */5) {
              return false;
            }
            if (x1[0] !== x2[0]) {
              return false;
            }
            _x2 = x2[1];
            _x1 = x1[1];
            continue ;
        case /* Group */6 :
            return false;
        case /* No_group */7 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.tag !== /* No_group */7) {
              return false;
            }
            _x2 = x2[0];
            _x1 = x1[0];
            continue ;
        case /* Nest */8 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.tag !== /* Nest */8) {
              return false;
            }
            _x2 = x2[0];
            _x1 = x1[0];
            continue ;
        case /* Case */9 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.tag !== /* Case */9) {
              return false;
            }
            _x2 = x2[0];
            _x1 = x1[0];
            continue ;
        case /* No_case */10 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.tag !== /* No_case */10) {
              return false;
            }
            _x2 = x2[0];
            _x1 = x1[0];
            continue ;
        case /* Intersection */11 :
            if (typeof x2 === "number" || x2.tag !== /* Intersection */11) {
              return false;
            } else {
              return eq_list(x1[0], x2[0]);
            }
        case /* Complement */12 :
            if (typeof x2 === "number" || x2.tag !== /* Complement */12) {
              return false;
            } else {
              return eq_list(x1[0], x2[0]);
            }
        case /* Difference */13 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.tag !== /* Difference */13) {
              return false;
            }
            if (!equal$2(x1[0], x2[0])) {
              return false;
            }
            _x2 = x2[1];
            _x1 = x1[1];
            continue ;
        case /* Pmark */14 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.tag !== /* Pmark */14) {
              return false;
            }
            if (x1[0] !== x2[0]) {
              return false;
            }
            _x2 = x2[1];
            _x1 = x1[1];
            continue ;
        
      }
    }
  };
}

function eq_list(_l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (!l1) {
      if (l2) {
        return false;
      } else {
        return true;
      }
    }
    if (!l2) {
      return false;
    }
    if (!equal$2(l1[0], l2[0])) {
      return false;
    }
    _l2 = l2[1];
    _l1 = l1[1];
    continue ;
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
    if (!param) {
      return /* [] */0;
    }
    var l$prime = param[0];
    if (typeof l$prime !== "number") {
      switch (l$prime.tag | 0) {
        case /* Sequence */1 :
            var match = l$prime[0];
            if (match) {
              var y = match[1];
              var x = match[0];
              var r$prime = merge_sequences(param[1]);
              var exit = 0;
              if (r$prime) {
                var match$1 = r$prime[0];
                if (typeof match$1 === "number" || match$1.tag !== /* Sequence */1) {
                  exit = 2;
                } else {
                  var match$2 = match$1[0];
                  if (match$2) {
                    if (equal$2(x, match$2[0])) {
                      return /* :: */[
                              /* Sequence */Block.__(1, [/* :: */[
                                    x,
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
                    }
                    exit = 2;
                  } else {
                    exit = 2;
                  }
                }
              } else {
                exit = 2;
              }
              if (exit === 2) {
                return /* :: */[
                        /* Sequence */Block.__(1, [/* :: */[
                              x,
                              y
                            ]]),
                        r$prime
                      ];
              }
              
            }
            break;
        case /* Alternative */2 :
            _param = Pervasives.$at(l$prime[0], param[1]);
            continue ;
        default:
          
      }
    }
    return /* :: */[
            l$prime,
            merge_sequences(param[1])
          ];
  };
}

function enforce_kind(ids, kind, kind$prime, cr) {
  if (kind !== 332064784 || kind$prime === 332064784) {
    return cr;
  } else {
    return seq$1(ids, kind$prime, cr, mk_expr(ids, /* Eps */0));
  }
}

function translate(ids, kind, _ign_group, ign_case, _greedy, pos, cache, c, _s) {
  while(true) {
    var s = _s;
    var greedy = _greedy;
    var ign_group = _ign_group;
    if (typeof s === "number") {
      switch (s) {
        case /* Beg_of_line */0 :
            var c$1 = Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.inexistant, Re_automata_Category.newline);
            return /* tuple */[
                    mk_expr(ids, /* After */Block.__(7, [c$1])),
                    kind
                  ];
        case /* End_of_line */1 :
            var c$2 = Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.inexistant, Re_automata_Category.newline);
            return /* tuple */[
                    mk_expr(ids, /* Before */Block.__(6, [c$2])),
                    kind
                  ];
        case /* Beg_of_word */2 :
            var c$3 = Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.inexistant, Re_automata_Category.not_letter);
            var c$4 = Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.inexistant, Re_automata_Category.letter);
            return /* tuple */[
                    seq$1(ids, /* First */332064784, mk_expr(ids, /* After */Block.__(7, [c$3])), mk_expr(ids, /* Before */Block.__(6, [c$4]))),
                    kind
                  ];
        case /* End_of_word */3 :
            var c$5 = Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.inexistant, Re_automata_Category.letter);
            var c$6 = Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.inexistant, Re_automata_Category.not_letter);
            return /* tuple */[
                    seq$1(ids, /* First */332064784, mk_expr(ids, /* After */Block.__(7, [c$5])), mk_expr(ids, /* Before */Block.__(6, [c$6]))),
                    kind
                  ];
        case /* Not_bound */4 :
            return /* tuple */[
                    alt(ids, /* :: */[
                          seq$1(ids, /* First */332064784, mk_expr(ids, /* After */Block.__(7, [Re_automata_Category.letter])), mk_expr(ids, /* Before */Block.__(6, [Re_automata_Category.letter]))),
                          /* :: */[
                            seq$1(ids, /* First */332064784, mk_expr(ids, /* After */Block.__(7, [Re_automata_Category.letter])), mk_expr(ids, /* Before */Block.__(6, [Re_automata_Category.letter]))),
                            /* [] */0
                          ]
                        ]),
                    kind
                  ];
        case /* Beg_of_str */5 :
            return /* tuple */[
                    mk_expr(ids, /* After */Block.__(7, [Re_automata_Category.inexistant])),
                    kind
                  ];
        case /* End_of_str */6 :
            return /* tuple */[
                    mk_expr(ids, /* Before */Block.__(6, [Re_automata_Category.inexistant])),
                    kind
                  ];
        case /* Last_end_of_line */7 :
            var c$7 = Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.inexistant, Re_automata_Category.lastnewline);
            return /* tuple */[
                    mk_expr(ids, /* Before */Block.__(6, [c$7])),
                    kind
                  ];
        case /* Start */8 :
            return /* tuple */[
                    mk_expr(ids, /* After */Block.__(7, [Re_automata_Category.search_boundary])),
                    kind
                  ];
        case /* Stop */9 :
            return /* tuple */[
                    mk_expr(ids, /* Before */Block.__(6, [Re_automata_Category.search_boundary])),
                    kind
                  ];
        
      }
    } else {
      switch (s.tag | 0) {
        case /* Set */0 :
            return /* tuple */[
                    cst(ids, trans_set(cache, c, s[0])),
                    kind
                  ];
        case /* Sequence */1 :
            return /* tuple */[
                    trans_seq(ids, kind, ign_group, ign_case, greedy, pos, cache, c, s[0]),
                    kind
                  ];
        case /* Alternative */2 :
            var merged_sequences = merge_sequences(s[0]);
            if (merged_sequences && !merged_sequences[1]) {
              var match = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, merged_sequences[0]);
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
        case /* Repeat */3 :
            var j = s[2];
            var i = s[1];
            var match$1 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, s[0]);
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
        case /* Sem */4 :
            var kind$prime$1 = s[0];
            var match$2 = translate(ids, kind$prime$1, ign_group, ign_case, greedy, pos, cache, c, s[1]);
            return /* tuple */[
                    enforce_kind(ids, kind$prime$1, match$2[1], match$2[0]),
                    kind$prime$1
                  ];
        case /* Sem_greedy */5 :
            _s = s[1];
            _greedy = s[0];
            continue ;
        case /* Group */6 :
            var r$prime = s[0];
            if (ign_group) {
              _s = r$prime;
              continue ;
            }
            var p = pos.contents;
            pos.contents = pos.contents + 2 | 0;
            var match$3 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r$prime);
            return /* tuple */[
                    seq$1(ids, /* First */332064784, mk_expr(ids, /* Mark */Block.__(4, [p])), seq$1(ids, /* First */332064784, match$3[0], mk_expr(ids, /* Mark */Block.__(4, [p + 1 | 0])))),
                    match$3[1]
                  ];
        case /* No_group */7 :
            _s = s[0];
            _ign_group = true;
            continue ;
        case /* Nest */8 :
            var b = pos.contents;
            var match$4 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, s[0]);
            var kind$prime$2 = match$4[1];
            var cr$1 = match$4[0];
            var e = pos.contents - 1 | 0;
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
        case /* Pmark */14 :
            var match$5 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, s[1]);
            return /* tuple */[
                    seq$1(ids, /* First */332064784, mk_expr(ids, /* Pmark */Block.__(8, [s[0]])), match$5[0]),
                    match$5[1]
                  ];
        default:
          throw {
                CamlExt: Caml_builtin_exceptions.assert_failure,
                _1: /* tuple */[
                  "re.ml",
                  714,
                  4
                ]
              };
      }
    }
  };
}

function trans_seq(ids, kind, ign_group, ign_case, greedy, pos, cache, c, param) {
  if (!param) {
    return mk_expr(ids, /* Eps */0);
  }
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
  }
  var match$1 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r);
  return enforce_kind(ids, kind, match$1[1], match$1[0]);
}

function case_insens(s) {
  return union(s, union(offset(32, inter(s, cupper)), offset(-32, inter(s, clower))));
}

function as_set(s) {
  if (typeof s === "number") {
    throw {
          CamlExt: Caml_builtin_exceptions.assert_failure,
          _1: /* tuple */[
            "re.ml",
            747,
            13
          ]
        };
  }
  if (!s.tag) {
    return s[0];
  }
  throw {
        CamlExt: Caml_builtin_exceptions.assert_failure,
        _1: /* tuple */[
          "re.ml",
          747,
          13
        ]
      };
}

function handle_case(_ign_case, _s) {
  while(true) {
    var s = _s;
    var ign_case = _ign_case;
    if (typeof s === "number") {
      return s;
    }
    switch (s.tag | 0) {
      case /* Set */0 :
          var s$1 = s[0];
          return /* Set */Block.__(0, [ign_case ? case_insens(s$1) : s$1]);
      case /* Sequence */1 :
          return /* Sequence */Block.__(1, [List.map((function(ign_case){
                        return function (param) {
                          return handle_case(ign_case, param);
                        }
                        }(ign_case)), s[0])]);
      case /* Alternative */2 :
          var l$prime = List.map((function(ign_case){
              return function (param) {
                return handle_case(ign_case, param);
              }
              }(ign_case)), s[0]);
          if (is_charset(/* Alternative */Block.__(2, [l$prime]))) {
            return /* Set */Block.__(0, [List.fold_left((function (s, r) {
                              return union(s, as_set(r));
                            }), /* [] */0, l$prime)]);
          } else {
            return /* Alternative */Block.__(2, [l$prime]);
          }
      case /* Repeat */3 :
          return /* Repeat */Block.__(3, [
                    handle_case(ign_case, s[0]),
                    s[1],
                    s[2]
                  ]);
      case /* Sem */4 :
          var r$prime = handle_case(ign_case, s[1]);
          if (is_charset(r$prime)) {
            return r$prime;
          } else {
            return /* Sem */Block.__(4, [
                      s[0],
                      r$prime
                    ]);
          }
      case /* Sem_greedy */5 :
          var r$prime$1 = handle_case(ign_case, s[1]);
          if (is_charset(r$prime$1)) {
            return r$prime$1;
          } else {
            return /* Sem_greedy */Block.__(5, [
                      s[0],
                      r$prime$1
                    ]);
          }
      case /* Group */6 :
          return /* Group */Block.__(6, [handle_case(ign_case, s[0])]);
      case /* No_group */7 :
          var r$prime$2 = handle_case(ign_case, s[0]);
          if (is_charset(r$prime$2)) {
            return r$prime$2;
          } else {
            return /* No_group */Block.__(7, [r$prime$2]);
          }
      case /* Nest */8 :
          var r$prime$3 = handle_case(ign_case, s[0]);
          if (is_charset(r$prime$3)) {
            return r$prime$3;
          } else {
            return /* Nest */Block.__(8, [r$prime$3]);
          }
      case /* Case */9 :
          _s = s[0];
          _ign_case = false;
          continue ;
      case /* No_case */10 :
          _s = s[0];
          _ign_case = true;
          continue ;
      case /* Intersection */11 :
          var l$prime$1 = List.map((function(ign_case){
              return function (r) {
                return handle_case(ign_case, r);
              }
              }(ign_case)), s[0]);
          return /* Set */Block.__(0, [List.fold_left((function (s, r) {
                            return inter(s, as_set(r));
                          }), cany, l$prime$1)]);
      case /* Complement */12 :
          var l$prime$2 = List.map((function(ign_case){
              return function (r) {
                return handle_case(ign_case, r);
              }
              }(ign_case)), s[0]);
          return /* Set */Block.__(0, [diff(cany, List.fold_left((function (s, r) {
                                return union(s, as_set(r));
                              }), /* [] */0, l$prime$2))]);
      case /* Difference */13 :
          return /* Set */Block.__(0, [inter(as_set(handle_case(ign_case, s[0])), diff(cany, as_set(handle_case(ign_case, s[1]))))]);
      case /* Pmark */14 :
          return /* Pmark */Block.__(14, [
                    s[0],
                    handle_case(ign_case, s[1])
                  ]);
      
    }
  };
}

function anchored(_l) {
  while(true) {
    var l = _l;
    if (typeof l === "number") {
      switch (l) {
        case /* Beg_of_str */5 :
        case /* Start */8 :
            return true;
        default:
          return false;
      }
    } else {
      switch (l.tag | 0) {
        case /* Sequence */1 :
            return List.exists(anchored, l[0]);
        case /* Alternative */2 :
            return List.for_all(anchored, l[0]);
        case /* Repeat */3 :
            if (l[1] <= 0) {
              return false;
            }
            _l = l[0];
            continue ;
        case /* Group */6 :
        case /* No_group */7 :
        case /* Nest */8 :
        case /* Case */9 :
        case /* No_case */10 :
            _l = l[0];
            continue ;
        case /* Sem */4 :
        case /* Sem_greedy */5 :
        case /* Pmark */14 :
            _l = l[1];
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
    throw {
          CamlExt: Caml_builtin_exceptions.invalid_argument,
          _1: "Re.repn"
        };
  }
  if (j !== undefined && j < i) {
    throw {
          CamlExt: Caml_builtin_exceptions.invalid_argument,
          _1: "Re.repn"
        };
  }
  return /* Repeat */Block.__(3, [
            r,
            i,
            j
          ]);
}

function set(str) {
  var s = /* [] */0;
  for(var i = 0 ,i_finish = str.length; i < i_finish; ++i){
    s = union(single(Caml_string.get(str, i)), s);
  }
  return /* Set */Block.__(0, [s]);
}

function compl(l) {
  var r = /* Complement */Block.__(12, [l]);
  if (is_charset(r)) {
    return r;
  }
  throw {
        CamlExt: Caml_builtin_exceptions.invalid_argument,
        _1: "Re.compl"
      };
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
  var ids = {
    contents: 0
  };
  var pos = {
    contents: 0
  };
  var match$1 = translate(ids, /* First */332064784, false, false, /* Greedy */-904640576, pos, {
        contents: /* Empty */0
      }, col, regexp$1);
  var r$1 = enforce_kind(ids, /* First */332064784, match$1[1], match$1[0]);
  var col_repr = match[1];
  var group_count = pos.contents / 2 | 0;
  return {
          initial: r$1,
          initial_states: /* [] */0,
          cols: col,
          col_repr: col_repr,
          ncol: ncol$1,
          lnl: lnl,
          tbl: {
            contents: [false]
          },
          states: Curry._1(Re_automata_State.Table.create, 97),
          group_count: group_count
        };
}

function exec_internal(name, posOpt, lenOpt, groups, re, s) {
  var pos = posOpt !== undefined ? posOpt : 0;
  var len = lenOpt !== undefined ? lenOpt : -1;
  if (pos < 0 || len < -1 || (pos + len | 0) > s.length) {
    throw {
          CamlExt: Caml_builtin_exceptions.invalid_argument,
          _1: name
        };
  }
  var partial = false;
  var slen = s.length;
  var last = len === -1 ? slen : pos + len | 0;
  var tmp;
  if (groups) {
    var n = re.tbl.contents.length + 1 | 0;
    tmp = n <= 10 ? [
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
    tmp = [];
  }
  var info = {
    re: re,
    i_cols: re.cols,
    positions: tmp,
    pos: pos,
    last: last
  };
  var initial_cat = pos === 0 ? Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.search_boundary, Re_automata_Category.inexistant) : Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.search_boundary, category(re, get_color(re, s, pos - 1 | 0)));
  var initial_state = find_initial_state(re, initial_cat);
  var st = scan_str(info, s, initial_state, groups);
  var res;
  if (st.idx === -3 || partial) {
    res = status(st.desc);
  } else {
    var final_cat = last === slen ? Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.search_boundary, Re_automata_Category.inexistant) : Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.search_boundary, category(re, get_color(re, s, last)));
    var match = $$final(info, st, final_cat);
    if (groups) {
      Caml_array.caml_array_set(info.positions, match[0], last + 1 | 0);
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
    return /* Match */[{
              s: s,
              marks: res[0],
              pmarks: res[1],
              gpos: info.positions,
              gcount: re.group_count
            }];
  }
}

function offset$1(t, i) {
  if (((i << 1) + 1 | 0) >= t.marks.length) {
    throw {
          CamlExt: Caml_builtin_exceptions.not_found
        };
  }
  var m1 = Caml_array.caml_array_get(t.marks, (i << 1));
  if (m1 === -1) {
    throw {
          CamlExt: Caml_builtin_exceptions.not_found
        };
  }
  var p1 = Caml_array.caml_array_get(t.gpos, m1) - 1 | 0;
  var p2 = Caml_array.caml_array_get(t.gpos, Caml_array.caml_array_get(t.marks, (i << 1) + 1 | 0)) - 1 | 0;
  return /* tuple */[
          p1,
          p2
        ];
}

function get(t, i) {
  var match = offset$1(t, i);
  var p1 = match[0];
  return $$String.sub(t.s, p1, match[1] - p1 | 0);
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
      throw {
            CamlExt: Caml_builtin_exceptions.invalid_argument,
            _1: s
          };
  }
}

function parse(multiline, dollar_endonly, dotall, ungreedy, s) {
  var i = {
    contents: 0
  };
  var l = s.length;
  var test = function (c) {
    if (i.contents !== l) {
      return Caml_string.get(s, i.contents) === c;
    } else {
      return false;
    }
  };
  var accept = function (c) {
    var r = test(c);
    if (r) {
      i.contents = i.contents + 1 | 0;
    }
    return r;
  };
  var accept_s = function (s$prime) {
    var len = s$prime.length;
    try {
      for(var j = 0; j < len; ++j){
        try {
          if (Caml_string.get(s$prime, j) !== Caml_string.get(s, i.contents + j | 0)) {
            throw {
                  CamlExt: Pervasives.Exit
                };
          }
          
        }
        catch (exn){
          throw {
                CamlExt: Pervasives.Exit
              };
        }
      }
      i.contents = i.contents + len | 0;
      return true;
    }
    catch (raw_exn){
      var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn$1.CamlExt === Pervasives.Exit) {
        return false;
      }
      throw exn$1;
    }
  };
  var get = function (param) {
    var r = Caml_string.get(s, i.contents);
    i.contents = i.contents + 1 | 0;
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
  var branch$prime = function (_left) {
    while(true) {
      var left = _left;
      if (i.contents === l || test(/* "|" */124) || test(/* ")" */41)) {
        return seq$2(List.rev(left));
      }
      _left = /* :: */[
        piece(undefined),
        left
      ];
      continue ;
    };
  };
  var piece = function (param) {
    var r = atom(undefined);
    if (accept(/* "*" */42)) {
      return greedy_mod(repn(r, 0, undefined));
    }
    if (accept(/* "+" */43)) {
      return greedy_mod(repn(r, 1, undefined));
    }
    if (accept(/* "?" */63)) {
      return greedy_mod(repn(r, 0, 1));
    }
    if (!accept(/* "{" */123)) {
      return r;
    }
    var i$1 = integer(undefined);
    if (i$1 !== undefined) {
      var j = accept(/* "," */44) ? integer(undefined) : i$1;
      if (!accept(/* "}" */125)) {
        throw {
              CamlExt: Parse_error
            };
      }
      if (j !== undefined && j < i$1) {
        throw {
              CamlExt: Parse_error
            };
      }
      return greedy_mod(repn(r, i$1, j));
    }
    i.contents = i.contents - 1 | 0;
    return r;
  };
  var regexp$prime = function (_left) {
    while(true) {
      var left = _left;
      if (!accept(/* "|" */124)) {
        return left;
      }
      _left = alt$1(/* :: */[
            left,
            /* :: */[
              branch$prime(/* [] */0),
              /* [] */0
            ]
          ]);
      continue ;
    };
  };
  var atom = function (param) {
    if (accept(/* "." */46)) {
      if (dotall) {
        return any;
      } else {
        return notnl;
      }
    }
    if (accept(/* "(" */40)) {
      if (accept(/* "?" */63)) {
        if (accept(/* ":" */58)) {
          var r = regexp$prime(branch$prime(/* [] */0));
          if (!accept(/* ")" */41)) {
            throw {
                  CamlExt: Parse_error
                };
          }
          return r;
        }
        if (accept(/* "#" */35)) {
          var _param;
          while(true) {
            if (accept(/* ")" */41)) {
              return epsilon;
            }
            i.contents = i.contents + 1 | 0;
            _param = undefined;
            continue ;
          };
        }
        throw {
              CamlExt: Parse_error
            };
      }
      var r$1 = regexp$prime(branch$prime(/* [] */0));
      if (!accept(/* ")" */41)) {
        throw {
              CamlExt: Parse_error
            };
      }
      return /* Group */Block.__(6, [r$1]);
    }
    if (accept(/* "^" */94)) {
      if (multiline) {
        return /* Beg_of_line */0;
      } else {
        return /* Beg_of_str */5;
      }
    }
    if (accept(/* "$" */36)) {
      if (multiline) {
        return /* End_of_line */1;
      } else if (dollar_endonly) {
        return /* Last_end_of_line */7;
      } else {
        return /* End_of_str */6;
      }
    }
    if (accept(/* "[" */91)) {
      if (accept(/* "^" */94)) {
        return compl(bracket(/* [] */0));
      } else {
        return alt$1(bracket(/* [] */0));
      }
    }
    if (accept(/* "\\" */92)) {
      if (i.contents === l) {
        throw {
              CamlExt: Parse_error
            };
      }
      var c = get(undefined);
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
            throw {
                  CamlExt: Not_supported
                };
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
            throw {
                  CamlExt: Parse_error
                };
        case 122 :
            return /* End_of_str */6;
        default:
          return /* Set */Block.__(0, [single(c)]);
      }
    } else {
      if (i.contents === l) {
        throw {
              CamlExt: Parse_error
            };
      }
      var c$1 = get(undefined);
      if (c$1 >= 64) {
        if (c$1 !== 92) {
          if (c$1 !== 123) {
            return /* Set */Block.__(0, [single(c$1)]);
          }
          throw {
                CamlExt: Parse_error
              };
        }
        throw {
              CamlExt: Parse_error
            };
      }
      if (c$1 >= 44) {
        if (c$1 >= 63) {
          throw {
                CamlExt: Parse_error
              };
        }
        return /* Set */Block.__(0, [single(c$1)]);
      }
      if (c$1 >= 42) {
        throw {
              CamlExt: Parse_error
            };
      }
      return /* Set */Block.__(0, [single(c$1)]);
    }
  };
  var integer = function (param) {
    if (i.contents === l) {
      return ;
    }
    var d = get(undefined);
    if (d > 57 || d < 48) {
      i.contents = i.contents - 1 | 0;
      return ;
    } else {
      var _i = d - /* "0" */48 | 0;
      while(true) {
        var i$1 = _i;
        if (i.contents === l) {
          return i$1;
        }
        var d$1 = get(undefined);
        if (d$1 > 57 || d$1 < 48) {
          i.contents = i.contents - 1 | 0;
          return i$1;
        }
        var i$prime = Caml_int32.imul(10, i$1) + (d$1 - /* "0" */48 | 0) | 0;
        if (i$prime < i$1) {
          throw {
                CamlExt: Parse_error
              };
        }
        _i = i$prime;
        continue ;
      };
    }
  };
  var $$char = function (param) {
    if (i.contents === l) {
      throw {
            CamlExt: Parse_error
          };
    }
    var c = get(undefined);
    if (c === /* "[" */91) {
      if (accept(/* "=" */61)) {
        throw {
              CamlExt: Not_supported
            };
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
        catch (raw_exn){
          var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
          if (exn.CamlExt === Caml_builtin_exceptions.not_found) {
            throw {
                  CamlExt: Parse_error
                };
          }
          throw exn;
        }
        if (!accept_s(":]")) {
          throw {
                CamlExt: Parse_error
              };
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
      }
      if (!accept(/* "." */46)) {
        return /* `Char */[
                748194550,
                c
              ];
      }
      if (i.contents === l) {
        throw {
              CamlExt: Parse_error
            };
      }
      var c$1 = get(undefined);
      if (!accept(/* "." */46)) {
        throw {
              CamlExt: Not_supported
            };
      }
      if (!accept(/* "]" */93)) {
        throw {
              CamlExt: Parse_error
            };
      }
      return /* `Char */[
              748194550,
              c$1
            ];
    }
    if (c !== /* "\\" */92) {
      return /* `Char */[
              748194550,
              c
            ];
    }
    var c$2 = get(undefined);
    if (c$2 >= 58) {
      if (c$2 >= 123) {
        return /* `Char */[
                748194550,
                c$2
              ];
      }
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
            throw {
                  CamlExt: Parse_error
                };
        
      }
    } else {
      if (c$2 >= 48) {
        throw {
              CamlExt: Not_supported
            };
      }
      return /* `Char */[
              748194550,
              c$2
            ];
    }
  };
  var bracket = function (_s) {
    while(true) {
      var s = _s;
      if (s !== /* [] */0 && accept(/* "]" */93)) {
        return s;
      }
      var match = $$char(undefined);
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
          }
          var match$1 = $$char(undefined);
          if (match$1[0] < 748194550) {
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
          _s = /* :: */[
            /* Set */Block.__(0, [seq(c, match$1[1])]),
            s
          ];
          continue ;
        }
        _s = /* :: */[
          /* Set */Block.__(0, [single(c)]),
          s
        ];
        continue ;
      }
      _s = /* :: */[
        match[1],
        s
      ];
      continue ;
    };
  };
  var res = regexp$prime(branch$prime(/* [] */0));
  if (i.contents !== l) {
    throw {
          CamlExt: Parse_error
        };
  }
  return res;
}

function re(flagsOpt, pat) {
  var flags = flagsOpt !== undefined ? flagsOpt : /* [] */0;
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
  var optsOpt = opts;
  var opts$1 = optsOpt !== undefined ? optsOpt : /* [] */0;
  var r = parse(List.memq(/* Multiline */1071952589, opts$1), List.memq(/* Dollar_endonly */-712595228, opts$1), List.memq(/* Dotall */-424303016, opts$1), List.memq(/* Ungreedy */-243745063, opts$1), pat);
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
  var len;
  var substr = exec_internal("Re.exec", pos, len, true, rex, s);
  if (typeof substr === "number") {
    throw {
          CamlExt: Caml_builtin_exceptions.not_found
        };
  }
  return substr[0];
}

var s = Caml_bytes.bytes_to_string(Bytes.make(1048575, /* "a" */97)) + "b";

eq("File \"xx.ml\", line 7, characters 3-10", get(exec(compile(re(undefined, "aa?b")), undefined, s), 0), "aab");

Mt.from_pair_suites("Ocaml_re_test", suites.contents);

/* Table Not a pure module */
