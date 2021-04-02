'use strict';

var Mt = require("./mt.js");
var Caml = require("../../lib/js/caml.js");
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
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
  
}

function union(_l, _l$p) {
  while(true) {
    var l$p = _l$p;
    var l = _l;
    if (!l$p) {
      return l;
    }
    if (!l) {
      return l$p;
    }
    var r$p = l$p.tl;
    var match = l$p.hd;
    var c2$p = match[1];
    var c1$p = match[0];
    var r = l.tl;
    var match$1 = l.hd;
    var c2 = match$1[1];
    var c1 = match$1[0];
    if ((c2 + 1 | 0) < c1$p) {
      return {
              hd: [
                c1,
                c2
              ],
              tl: union(r, l$p)
            };
    }
    if ((c2$p + 1 | 0) < c1) {
      return {
              hd: [
                c1$p,
                c2$p
              ],
              tl: union(l, r$p)
            };
    }
    if (c2 < c2$p) {
      _l$p = {
        hd: [
          c1 < c1$p ? c1 : c1$p,
          c2$p
        ],
        tl: r$p
      };
      _l = r;
      continue ;
    }
    _l$p = r$p;
    _l = {
      hd: [
        c1 < c1$p ? c1 : c1$p,
        c2
      ],
      tl: r
    };
    continue ;
  };
}

function inter(_l, _l$p) {
  while(true) {
    var l$p = _l$p;
    var l = _l;
    if (!l$p) {
      return /* [] */0;
    }
    if (!l) {
      return /* [] */0;
    }
    var r$p = l$p.tl;
    var match = l$p.hd;
    var c2$p = match[1];
    var c1$p = match[0];
    var r = l.tl;
    var match$1 = l.hd;
    var c2 = match$1[1];
    var c1 = match$1[0];
    if (Caml_obj.caml_lessthan(c2, c1$p)) {
      _l = r;
      continue ;
    }
    if (!Caml_obj.caml_lessthan(c2$p, c1)) {
      if (Caml_obj.caml_lessthan(c2, c2$p)) {
        return {
                hd: [
                  Caml_obj.caml_max(c1, c1$p),
                  c2
                ],
                tl: inter(r, l$p)
              };
      } else {
        return {
                hd: [
                  Caml_obj.caml_max(c1, c1$p),
                  c2$p
                ],
                tl: inter(l, r$p)
              };
      }
    }
    _l$p = r$p;
    continue ;
  };
}

function diff(_l, _l$p) {
  while(true) {
    var l$p = _l$p;
    var l = _l;
    if (!l$p) {
      return l;
    }
    if (!l) {
      return /* [] */0;
    }
    var r$p = l$p.tl;
    var match = l$p.hd;
    var c2$p = match[1];
    var c1$p = match[0];
    var r = l.tl;
    var match$1 = l.hd;
    var c2 = match$1[1];
    var c1 = match$1[0];
    if (c2 < c1$p) {
      return {
              hd: [
                c1,
                c2
              ],
              tl: diff(r, l$p)
            };
    }
    if (c2$p < c1) {
      _l$p = r$p;
      continue ;
    }
    var r$p$p = c2$p < c2 ? ({
          hd: [
            c2$p + 1 | 0,
            c2
          ],
          tl: r
        }) : r;
    if (c1 < c1$p) {
      return {
              hd: [
                c1,
                c1$p - 1 | 0
              ],
              tl: diff(r$p$p, r$p)
            };
    }
    _l$p = r$p;
    _l = r$p$p;
    continue ;
  };
}

function single(c) {
  return {
          hd: [
            c,
            c
          ],
          tl: /* [] */0
        };
}

function seq(c, c$p) {
  if (Caml_obj.caml_lessequal(c, c$p)) {
    return {
            hd: [
              c,
              c$p
            ],
            tl: /* [] */0
          };
  } else {
    return {
            hd: [
              c$p,
              c
            ],
            tl: /* [] */0
          };
  }
}

function offset(o, l) {
  if (!l) {
    return /* [] */0;
  }
  var match = l.hd;
  return {
          hd: [
            match[0] + o | 0,
            match[1] + o | 0
          ],
          tl: offset(o, l.tl)
        };
}

function mem(c, _s) {
  while(true) {
    var s = _s;
    if (!s) {
      return false;
    }
    var match = s.hd;
    if (c <= match[1]) {
      return c >= match[0];
    }
    _s = s.tl;
    continue ;
  };
}

function hash_rec(param) {
  if (!param) {
    return 0;
  }
  var match = param.hd;
  return (match[0] + Math.imul(13, match[1]) | 0) + Math.imul(257, hash_rec(param.tl)) | 0;
}

function one_char(param) {
  if (!param) {
    return ;
  }
  if (param.tl) {
    return ;
  }
  var match = param.hd;
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
    return param.h;
  } else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* Node */{
          l: l,
          v: x,
          d: d,
          r: r,
          h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal(l, x, d, r) {
  var hl = l ? l.h : 0;
  var hr = r ? r.h : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l.r;
      var ld = l.d;
      var lv = l.v;
      var ll = l.l;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      }
      if (lr) {
        return create(create(ll, lv, ld, lr.l), lr.v, lr.d, create(lr.r, x, d, r));
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal",
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */{
            l: l,
            v: x,
            d: d,
            r: r,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (r) {
    var rr = r.r;
    var rd = r.d;
    var rv = r.v;
    var rl = r.l;
    if (height(rr) >= height(rl)) {
      return create(create(l, x, d, rl), rv, rd, rr);
    }
    if (rl) {
      return create(create(l, x, d, rl.l), rl.v, rl.d, create(rl.r, rv, rd, rr));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Map.bal",
        Error: new Error()
      };
}

function add(x, data, m) {
  if (!m) {
    return /* Node */{
            l: /* Empty */0,
            v: x,
            d: data,
            r: /* Empty */0,
            h: 1
          };
  }
  var r = m.r;
  var d = m.d;
  var v = m.v;
  var l = m.l;
  var c = compare(x, v);
  if (c === 0) {
    if (d === data) {
      return m;
    } else {
      return /* Node */{
              l: l,
              v: x,
              d: data,
              r: r,
              h: m.h
            };
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

var cany = {
  hd: [
    0,
    255
  ],
  tl: /* [] */0
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
      if (param > 246 || param < 192) {
        if (param >= 248) {
          return 2;
        } else {
          return 4;
        }
      } else if (param !== 215) {
        return 2;
      } else {
        return 4;
      }
    } else if (!(param > 185 || param < 171) && param !== 181) {
      return 4;
    } else {
      return 2;
    }
  } else if (param >= 65) {
    if (param > 96 || param < 91) {
      if (param >= 123) {
        return 4;
      } else {
        return 2;
      }
    } else if (param !== 95) {
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
    return param.h;
  } else {
    return 0;
  }
}

function create$1(l, v, r) {
  var hl = l ? l.h : 0;
  var hr = r ? r.h : 0;
  return /* Node */{
          l: l,
          v: v,
          r: r,
          h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal$1(l, v, r) {
  var hl = l ? l.h : 0;
  var hr = r ? r.h : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l.r;
      var lv = l.v;
      var ll = l.l;
      if (height$1(ll) >= height$1(lr)) {
        return create$1(ll, lv, create$1(lr, v, r));
      }
      if (lr) {
        return create$1(create$1(ll, lv, lr.l), lr.v, create$1(lr.r, v, r));
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Set.bal",
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Set.bal",
          Error: new Error()
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */{
            l: l,
            v: v,
            r: r,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (r) {
    var rr = r.r;
    var rv = r.v;
    var rl = r.l;
    if (height$1(rr) >= height$1(rl)) {
      return create$1(create$1(l, v, rl), rv, rr);
    }
    if (rl) {
      return create$1(create$1(l, v, rl.l), rl.v, create$1(rl.r, rv, rr));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Set.bal",
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Set.bal",
        Error: new Error()
      };
}

function add$1(x, t) {
  if (!t) {
    return /* Node */{
            l: /* Empty */0,
            v: x,
            r: /* Empty */0,
            h: 1
          };
  }
  var r = t.r;
  var v = t.v;
  var l = t.l;
  var c = Caml.caml_int_compare(x, v);
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
  return Math.imul(accu, 65599) + h | 0;
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
    var match = l.hd;
    _accu = hash_combine(match[0], hash_combine(match[1], accu$1));
    _l = l.tl;
    continue ;
  };
}

function marks_set_idx(idx, marks) {
  if (!marks) {
    return marks;
  }
  var match = marks.hd;
  if (match[1] !== -1) {
    return marks;
  } else {
    return {
            hd: [
              match[0],
              idx
            ],
            tl: marks_set_idx(idx, marks.tl)
          };
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
    var res = Curry._1(f, param.hd);
    if (res !== undefined) {
      return res;
    }
    _param = param.tl;
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
    return mk_expr(ids, {
                TAG: /* Alt */1,
                _0: /* [] */0
              });
  } else {
    return mk_expr(ids, {
                TAG: /* Cst */0,
                _0: s
              });
  }
}

function alt(ids, l) {
  if (l) {
    if (l.tl) {
      return mk_expr(ids, {
                  TAG: /* Alt */1,
                  _0: l
                });
    } else {
      return l.hd;
    }
  } else {
    return mk_expr(ids, {
                TAG: /* Alt */1,
                _0: /* [] */0
              });
  }
}

function seq$1(ids, kind, x, y) {
  var match = x.def;
  var match$1 = y.def;
  var exit = 0;
  if (typeof match === "number") {
    return y;
  }
  if (match.TAG === /* Alt */1) {
    if (!match._0) {
      return x;
    }
    exit = 2;
  } else {
    exit = 2;
  }
  if (exit === 2) {
    if (typeof match$1 === "number") {
      if (kind === "First") {
        return x;
      }
      
    } else if (match$1.TAG === /* Alt */1 && !match$1._0) {
      return y;
    }
    
  }
  return mk_expr(ids, {
              TAG: /* Seq */2,
              _0: kind,
              _1: x,
              _2: y
            });
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
  return mk_expr(ids, {
              TAG: /* Rep */3,
              _0: kind,
              _1: sem,
              _2: x
            });
}

function erase(ids, m, m$p) {
  return mk_expr(ids, {
              TAG: /* Erase */5,
              _0: m,
              _1: m$p
            });
}

function rename(ids, x) {
  var l = x.def;
  if (typeof l === "number") {
    return mk_expr(ids, x.def);
  }
  switch (l.TAG | 0) {
    case /* Alt */1 :
        return mk_expr(ids, {
                    TAG: /* Alt */1,
                    _0: List.map((function (param) {
                            return rename(ids, param);
                          }), l._0)
                  });
    case /* Seq */2 :
        return mk_expr(ids, {
                    TAG: /* Seq */2,
                    _0: l._0,
                    _1: rename(ids, l._1),
                    _2: rename(ids, l._2)
                  });
    case /* Rep */3 :
        return mk_expr(ids, {
                    TAG: /* Rep */3,
                    _0: l._0,
                    _1: l._1,
                    _2: rename(ids, l._2)
                  });
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
    var marks1 = l1.hd;
    switch (marks1.TAG | 0) {
      case /* TSeq */0 :
          if (!l2) {
            return false;
          }
          var match = l2.hd;
          switch (match.TAG | 0) {
            case /* TSeq */0 :
                if (marks1._1.id !== match._1.id) {
                  return false;
                }
                if (!equal(marks1._0, match._0)) {
                  return false;
                }
                _l2 = l2.tl;
                _l1 = l1.tl;
                continue ;
            case /* TExp */1 :
            case /* TMatch */2 :
                return false;
            
          }
      case /* TExp */1 :
          if (!l2) {
            return false;
          }
          var match$1 = l2.hd;
          switch (match$1.TAG | 0) {
            case /* TExp */1 :
                if (marks1._1.id !== match$1._1.id) {
                  return false;
                }
                if (!Caml_obj.caml_equal(marks1._0, match$1._0)) {
                  return false;
                }
                _l2 = l2.tl;
                _l1 = l1.tl;
                continue ;
            case /* TSeq */0 :
            case /* TMatch */2 :
                return false;
            
          }
      case /* TMatch */2 :
          if (!l2) {
            return false;
          }
          var marks2 = l2.hd;
          switch (marks2.TAG | 0) {
            case /* TSeq */0 :
            case /* TExp */1 :
                return false;
            case /* TMatch */2 :
                if (!Caml_obj.caml_equal(marks1._0, marks2._0)) {
                  return false;
                }
                _l2 = l2.tl;
                _l1 = l1.tl;
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
    var marks = l.hd;
    switch (marks.TAG | 0) {
      case /* TSeq */0 :
          _accu = hash_combine(388635598, hash_combine(marks._1.id, hash$1(marks._0, accu)));
          _l = l.tl;
          continue ;
      case /* TExp */1 :
          _accu = hash_combine(726404471, hash_combine(marks._1.id, hash(marks._0, accu)));
          _l = l.tl;
          continue ;
      case /* TMatch */2 :
          _accu = hash_combine(471882453, hash(marks._0, accu));
          _l = l.tl;
          continue ;
      
    }
  };
}

function tseq(kind, x, y, rem) {
  if (!x) {
    return rem;
  }
  var match = x.hd;
  switch (match.TAG | 0) {
    case /* TExp */1 :
        if (typeof match._1.def === "number" && !x.tl) {
          return {
                  hd: {
                    TAG: /* TExp */1,
                    _0: match._0,
                    _1: y
                  },
                  tl: rem
                };
        }
        break;
    case /* TSeq */0 :
    case /* TMatch */2 :
        break;
    
  }
  return {
          hd: {
            TAG: /* TSeq */0,
            _0: x,
            _1: y,
            _2: kind
          },
          tl: rem
        };
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
  return mk(0, cat, {
              hd: {
                TAG: /* TExp */1,
                _0: empty,
                _1: e
              },
              tl: /* [] */0
            });
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
  return function (param) {
    return List.iter((function (param) {
                  switch (param.TAG | 0) {
                    case /* TSeq */0 :
                        return mark_used_indices(tbl)(param._0);
                    case /* TExp */1 :
                    case /* TMatch */2 :
                        break;
                    
                  }
                  return List.iter((function (param) {
                                var i = param[1];
                                if (i >= 0) {
                                  return Caml_array.set(tbl, i, true);
                                }
                                
                              }), param._0.marks);
                }), param);
  };
}

function find_free(tbl, _idx, len) {
  while(true) {
    var idx = _idx;
    if (idx === len || !Caml_array.get(tbl, idx)) {
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
    tbl_ref.contents = Caml_array.make((len << 1), false);
  }
  return idx;
}

var remove_matches = List.filter(function (param) {
      switch (param.TAG | 0) {
        case /* TSeq */0 :
        case /* TExp */1 :
            return true;
        case /* TMatch */2 :
            return false;
        
      }
    });

function split_at_match_rec(_l$p, _param) {
  while(true) {
    var param = _param;
    var l$p = _l$p;
    if (param) {
      var x = param.hd;
      switch (x.TAG | 0) {
        case /* TSeq */0 :
        case /* TExp */1 :
            _param = param.tl;
            _l$p = {
              hd: x,
              tl: l$p
            };
            continue ;
        case /* TMatch */2 :
            return [
                    List.rev(l$p),
                    Curry._1(remove_matches, param.tl)
                  ];
        
      }
    } else {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "re_automata.ml",
              429,
              21
            ],
            Error: new Error()
          };
    }
  };
}

function remove_duplicates(prev, _l, y) {
  while(true) {
    var l = _l;
    if (!l) {
      return [
              /* [] */0,
              prev
            ];
    }
    var x = l.hd;
    switch (x.TAG | 0) {
      case /* TSeq */0 :
          var x$1 = x._1;
          var match = remove_duplicates(prev, x._0, x$1);
          var match$1 = remove_duplicates(match[1], l.tl, y);
          return [
                  tseq(x._2, match[0], x$1, match$1[0]),
                  match$1[1]
                ];
      case /* TExp */1 :
          var x$2 = x._1;
          if (typeof x$2.def === "number") {
            var r = l.tl;
            if (List.memq(y.id, prev)) {
              _l = r;
              continue ;
            }
            var match$2 = remove_duplicates({
                  hd: y.id,
                  tl: prev
                }, r, y);
            return [
                    {
                      hd: x,
                      tl: match$2[0]
                    },
                    match$2[1]
                  ];
          }
          var r$1 = l.tl;
          if (List.memq(x$2.id, prev)) {
            _l = r$1;
            continue ;
          }
          var match$3 = remove_duplicates({
                hd: x$2.id,
                tl: prev
              }, r$1, y);
          return [
                  {
                    hd: x,
                    tl: match$3[0]
                  },
                  match$3[1]
                ];
      case /* TMatch */2 :
          return [
                  {
                    hd: x,
                    tl: /* [] */0
                  },
                  prev
                ];
      
    }
  };
}

function set_idx(idx, param) {
  if (!param) {
    return /* [] */0;
  }
  var marks = param.hd;
  switch (marks.TAG | 0) {
    case /* TSeq */0 :
        return {
                hd: {
                  TAG: /* TSeq */0,
                  _0: set_idx(idx, marks._0),
                  _1: marks._1,
                  _2: marks._2
                },
                tl: set_idx(idx, param.tl)
              };
    case /* TExp */1 :
        return {
                hd: {
                  TAG: /* TExp */1,
                  _0: marks_set_idx$1(marks._0, idx),
                  _1: marks._1
                },
                tl: set_idx(idx, param.tl)
              };
    case /* TMatch */2 :
        return {
                hd: {
                  TAG: /* TMatch */2,
                  _0: marks_set_idx$1(marks._0, idx)
                },
                tl: set_idx(idx, param.tl)
              };
    
  }
}

function filter_marks(b, e, marks) {
  return {
          marks: List.filter(function (param) {
                  var i = param[0];
                  if (i < b) {
                    return true;
                  } else {
                    return i > e;
                  }
                })(marks.marks),
          pmarks: marks.pmarks
        };
}

function delta_1(marks, c, next_cat, prev_cat, x, rem) {
  var s = x.def;
  if (typeof s === "number") {
    return {
            hd: {
              TAG: /* TMatch */2,
              _0: marks
            },
            tl: rem
          };
  }
  switch (s.TAG | 0) {
    case /* Cst */0 :
        if (mem(c, s._0)) {
          return {
                  hd: {
                    TAG: /* TExp */1,
                    _0: marks,
                    _1: eps_expr
                  },
                  tl: rem
                };
        } else {
          return rem;
        }
    case /* Alt */1 :
        return delta_2(marks, c, next_cat, prev_cat, s._0, rem);
    case /* Seq */2 :
        var y$p = delta_1(marks, c, next_cat, prev_cat, s._1, /* [] */0);
        return delta_seq(c, next_cat, prev_cat, s._0, y$p, s._2, rem);
    case /* Rep */3 :
        var kind = s._1;
        var y$p$1 = delta_1(marks, c, next_cat, prev_cat, s._2, /* [] */0);
        var marks$p = first((function (marks) {
                switch (marks.TAG | 0) {
                  case /* TSeq */0 :
                  case /* TExp */1 :
                      return ;
                  case /* TMatch */2 :
                      return marks._0;
                  
                }
              }), y$p$1);
        var match = marks$p !== undefined ? [
            Curry._1(remove_matches, y$p$1),
            marks$p
          ] : [
            y$p$1,
            marks
          ];
        var y$p$p = match[0];
        if (s._0 === "Non_greedy") {
          return {
                  hd: {
                    TAG: /* TMatch */2,
                    _0: marks
                  },
                  tl: tseq(kind, y$p$p, x, rem)
                };
        } else {
          return tseq(kind, y$p$p, x, {
                      hd: {
                        TAG: /* TMatch */2,
                        _0: match[1]
                      },
                      tl: rem
                    });
        }
    case /* Mark */4 :
        var i = s._0;
        var marks_marks = {
          hd: [
            i,
            -1
          ],
          tl: List.remove_assq(i, marks.marks)
        };
        var marks_pmarks = marks.pmarks;
        var marks$1 = {
          marks: marks_marks,
          pmarks: marks_pmarks
        };
        return {
                hd: {
                  TAG: /* TMatch */2,
                  _0: marks$1
                },
                tl: rem
              };
    case /* Erase */5 :
        return {
                hd: {
                  TAG: /* TMatch */2,
                  _0: filter_marks(s._0, s._1, marks)
                },
                tl: rem
              };
    case /* Before */6 :
        if (intersect(next_cat, s._0)) {
          return {
                  hd: {
                    TAG: /* TMatch */2,
                    _0: marks
                  },
                  tl: rem
                };
        } else {
          return rem;
        }
    case /* After */7 :
        if (intersect(prev_cat, s._0)) {
          return {
                  hd: {
                    TAG: /* TMatch */2,
                    _0: marks
                  },
                  tl: rem
                };
        } else {
          return rem;
        }
    case /* Pmark */8 :
        var marks_marks$1 = marks.marks;
        var marks_pmarks$1 = add$1(s._0, marks.pmarks);
        var marks$2 = {
          marks: marks_marks$1,
          pmarks: marks_pmarks$1
        };
        return {
                hd: {
                  TAG: /* TMatch */2,
                  _0: marks$2
                },
                tl: rem
              };
    
  }
}

function delta_2(marks, c, next_cat, prev_cat, l, rem) {
  if (l) {
    return delta_1(marks, c, next_cat, prev_cat, l.hd, delta_2(marks, c, next_cat, prev_cat, l.tl, rem));
  } else {
    return rem;
  }
}

function delta_seq(c, next_cat, prev_cat, kind, y, z, rem) {
  var marks = first((function (marks) {
          switch (marks.TAG | 0) {
            case /* TSeq */0 :
            case /* TExp */1 :
                return ;
            case /* TMatch */2 :
                return marks._0;
            
          }
        }), y);
  if (marks === undefined) {
    return tseq(kind, y, z, rem);
  }
  if (kind === "Longest") {
    return tseq(kind, Curry._1(remove_matches, y), z, delta_1(marks, c, next_cat, prev_cat, z, rem));
  }
  if (kind !== "First") {
    return delta_1(marks, c, next_cat, prev_cat, z, tseq(kind, Curry._1(remove_matches, y), z, rem));
  }
  var match = split_at_match_rec(/* [] */0, y);
  return tseq(kind, match[0], z, delta_1(marks, c, next_cat, prev_cat, z, tseq(kind, match[1], z, rem)));
}

function delta_4(c, next_cat, prev_cat, l, rem) {
  if (l) {
    var x = l.hd;
    var rem$1 = delta_4(c, next_cat, prev_cat, l.tl, rem);
    switch (x.TAG | 0) {
      case /* TSeq */0 :
          var y$p = delta_4(c, next_cat, prev_cat, x._0, /* [] */0);
          return delta_seq(c, next_cat, prev_cat, x._2, y$p, x._1, rem$1);
      case /* TExp */1 :
          return delta_1(x._0, c, next_cat, prev_cat, x._1, rem$1);
      case /* TMatch */2 :
          return {
                  hd: x,
                  tl: rem$1
                };
      
    }
  } else {
    return rem;
  }
}

function delta(tbl_ref, next_cat, $$char, st) {
  var prev_cat = st.category;
  var match = remove_duplicates(/* [] */0, delta_4($$char, next_cat, prev_cat, st.desc, /* [] */0), eps_expr);
  var expr$p = match[0];
  var idx = free_index(tbl_ref, expr$p);
  var expr$p$p = set_idx(idx, expr$p);
  return mk(idx, next_cat, expr$p$p);
}

function flatten_match(m) {
  var ma = List.fold_left((function (ma, param) {
          return Caml.caml_int_max(ma, param[0]);
        }), -1, m);
  var res = Caml_array.make(ma + 1 | 0, -1);
  List.iter((function (param) {
          return Caml_array.set(res, param[0], param[1]);
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
    var m = match.hd;
    switch (m.TAG | 0) {
      case /* TSeq */0 :
      case /* TExp */1 :
          st$1 = /* Running */1;
          break;
      case /* TMatch */2 :
          var m$1 = m._0;
          st$1 = /* Match */{
            _0: flatten_match(m$1.marks),
            _1: m$1.pmarks
          };
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
          next: break_state ? dummy_next : Caml_array.make(ncol, unknown_state),
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
    if (exn.RE_EXN_ID === "Not_found") {
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
    info.positions = Caml_array.make((len << 1), 0);
    $$Array.blit(pos, 0, info.positions, 0, len);
  }
  return desc;
}

function validate(info, s, pos, st) {
  var c = Caml_bytes.get(info.i_cols, Caml_string.get(s, pos));
  var cat = category(info.re, c);
  var desc$p = delta$1(info, cat, c, st);
  var st$p = find_state(info.re, desc$p);
  return Caml_array.set(st.next, c, st$p);
}

function loop(info, s, pos, st) {
  if (pos >= info.last) {
    return st;
  }
  var st$p = Caml_array.get(st.next, Caml_bytes.get(info.i_cols, Caml_string.get(s, pos)));
  var _pos = pos;
  var _st = st;
  var _st$p = st$p;
  while(true) {
    var st$p$1 = _st$p;
    var st$1 = _st;
    var pos$1 = _pos;
    if (st$p$1.idx < 0) {
      if (st$p$1.idx === -3) {
        Caml_array.set(info.positions, st$p$1.real_idx, pos$1 + 1 | 0);
        return st$p$1;
      } else {
        validate(info, s, pos$1, st$1);
        return loop(info, s, pos$1, st$1);
      }
    }
    var pos$2 = pos$1 + 1 | 0;
    if (pos$2 < info.last) {
      var st$p$p = Caml_array.get(st$p$1.next, Caml_bytes.get(info.i_cols, Caml_string.get(s, pos$2)));
      Caml_array.set(info.positions, st$p$1.idx, pos$2);
      _st$p = st$p$p;
      _st = st$p$1;
      _pos = pos$2;
      continue ;
    }
    Caml_array.set(info.positions, st$p$1.idx, pos$2);
    return st$p$1;
  };
}

function $$final(info, st, cat) {
  try {
    return List.assq(cat, st.final);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      var st$p = delta$1(info, cat, -1, st);
      var res_0 = st$p.idx;
      var res_1 = status(st$p);
      var res = [
        res_0,
        res_1
      ];
      st.final = {
        hd: [
          cat,
          res
        ],
        tl: st.final
      };
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
    if (exn.RE_EXN_ID === "Not_found") {
      var st = find_state(re, Curry._2(Re_automata_State.create, cat, re.initial));
      re.initial_states = {
        hd: [
          cat,
          st
        ],
        tl: re.initial_states
      };
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
  } else if (pos === (slen - 1 | 0) && re.lnl !== -1 && Caml_string.get(s, pos) === /* '\n' */10) {
    return re.lnl;
  } else {
    return Caml_bytes.get(re.cols, Caml_string.get(s, pos));
  }
}

function scan_str(info, s, initial_state, groups) {
  var pos = info.pos;
  var last = info.last;
  if (!(last === s.length && info.re.lnl !== -1 && last > pos && Caml_string.get(s, last - 1 | 0) === /* '\n' */10)) {
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
        var st$p = Caml_array.get(st.next, Caml_bytes.get(info.i_cols, Caml_string.get(s, pos$1)));
        if (st$p.idx >= 0) {
          _st = st$p;
          _pos = pos$1 + 1 | 0;
          continue ;
        }
        if (st$p.idx === -3) {
          return st$p;
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
      var st$p$1 = Caml_array.get(st$1.next, info$1.re.lnl);
      if (st$p$1.idx >= 0) {
        if (groups) {
          Caml_array.set(info$1.positions, st$p$1.idx, pos$2 + 1 | 0);
        }
        return st$p$1;
      }
      if (st$p$1.idx === -3) {
        if (groups) {
          Caml_array.set(info$1.positions, st$p$1.real_idx, pos$2 + 1 | 0);
        }
        return st$p$1;
      }
      var c = info$1.re.lnl;
      var real_c = Caml_bytes.get(info$1.i_cols, /* '\n' */10);
      var cat = category(info$1.re, c);
      var desc$p = delta$1(info$1, cat, real_c, st$1);
      var st$p$2 = find_state(info$1.re, desc$p);
      Caml_array.set(st$1.next, c, st$p$2);
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
  var v_0 = hash_rec(s);
  var v = [
    v_0,
    s
  ];
  try {
    var _param = cache.contents;
    while(true) {
      var param = _param;
      if (param) {
        var c = compare(v, param.v);
        if (c === 0) {
          return param.d;
        }
        _param = c < 0 ? param.l : param.r;
        continue ;
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    };
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
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
    switch (param.TAG | 0) {
      case /* Set */0 :
          return true;
      case /* Sem */4 :
      case /* Sem_greedy */5 :
          _param = param._1;
          continue ;
      case /* No_group */7 :
      case /* Case */9 :
      case /* No_case */10 :
          _param = param._0;
          continue ;
      case /* Alternative */2 :
      case /* Intersection */11 :
      case /* Complement */12 :
          return List.for_all(is_charset, param._0);
      case /* Difference */13 :
          if (!is_charset(param._0)) {
            return false;
          }
          _param = param._1;
          continue ;
      default:
        return false;
    }
  };
}

function split(s, cm) {
  var _t = s;
  var f = function (i, j) {
    Caml_bytes.set(cm, i, /* '\001' */1);
    return Caml_bytes.set(cm, j + 1 | 0, /* '\001' */1);
  };
  while(true) {
    var t = _t;
    if (!t) {
      return ;
    }
    var match = t.hd;
    Curry._2(f, match[0], match[1]);
    _t = t.tl;
    continue ;
  };
}

var cupper = union(seq(/* 'A' */65, /* 'Z' */90), union(seq(/* '\192' */192, /* '\214' */214), seq(/* '\216' */216, /* '\222' */222)));

var clower = offset(32, cupper);

var calpha = List.fold_right(cadd, {
      hd: /* '\170' */170,
      tl: {
        hd: /* '\181' */181,
        tl: {
          hd: /* '\186' */186,
          tl: {
            hd: /* '\223' */223,
            tl: {
              hd: /* '\255' */255,
              tl: /* [] */0
            }
          }
        }
      }
    }, union(clower, cupper));

var cdigit = seq(/* '0' */48, /* '9' */57);

var calnum = union(calpha, cdigit);

var cword = union({
      hd: [
        /* '_' */95,
        /* '_' */95
      ],
      tl: /* [] */0
    }, calnum);

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
              return split({
                          hd: [
                            /* '\n' */10,
                            /* '\n' */10
                          ],
                          tl: /* [] */0
                        }, c);
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
        switch (regexp.TAG | 0) {
          case /* Set */0 :
              return split(regexp._0, c);
          case /* Sequence */1 :
          case /* Alternative */2 :
              return List.iter(colorize$1, regexp._0);
          case /* Repeat */3 :
          case /* Group */6 :
          case /* No_group */7 :
          case /* Nest */8 :
              _regexp = regexp._0;
              continue ;
          case /* Sem */4 :
          case /* Sem_greedy */5 :
          case /* Pmark */14 :
              _regexp = regexp._1;
              continue ;
          default:
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "re.ml",
                    502,
                    35
                  ],
                  Error: new Error()
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
  Caml_bytes.set(c, 0, /* '\000' */0);
  Caml_bytes.set(col_repr, 0, /* '\000' */0);
  for(var i = 1; i <= 255; ++i){
    if (Caml_bytes.get(cm, i) !== /* '\000' */0) {
      v = v + 1 | 0;
    }
    Caml_bytes.set(c, i, Char.chr(v));
    Caml_bytes.set(col_repr, v, Char.chr(i));
  }
  return [
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
            return x2 === 0;
        case /* End_of_line */1 :
            return x2 === 1;
        case /* Beg_of_word */2 :
            return x2 === 2;
        case /* End_of_word */3 :
            return x2 === 3;
        case /* Not_bound */4 :
            return x2 === 4;
        case /* Beg_of_str */5 :
            return x2 === 5;
        case /* End_of_str */6 :
            return x2 === 6;
        case /* Last_end_of_line */7 :
            return x2 === 7;
        case /* Start */8 :
            return x2 === 8;
        case /* Stop */9 :
            if (typeof x2 === "number") {
              return x2 >= 9;
            } else {
              return false;
            }
        
      }
    } else {
      switch (x1.TAG | 0) {
        case /* Set */0 :
            if (typeof x2 === "number" || x2.TAG !== /* Set */0) {
              return false;
            } else {
              return Caml_obj.caml_equal(x1._0, x2._0);
            }
        case /* Sequence */1 :
            if (typeof x2 === "number" || x2.TAG !== /* Sequence */1) {
              return false;
            } else {
              return eq_list(x1._0, x2._0);
            }
        case /* Alternative */2 :
            if (typeof x2 === "number" || x2.TAG !== /* Alternative */2) {
              return false;
            } else {
              return eq_list(x1._0, x2._0);
            }
        case /* Repeat */3 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.TAG !== /* Repeat */3) {
              return false;
            }
            if (x1._1 !== x2._1) {
              return false;
            }
            if (!Caml_obj.caml_equal(x1._2, x2._2)) {
              return false;
            }
            _x2 = x2._0;
            _x1 = x1._0;
            continue ;
        case /* Sem */4 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.TAG !== /* Sem */4) {
              return false;
            }
            if (x1._0 !== x2._0) {
              return false;
            }
            _x2 = x2._1;
            _x1 = x1._1;
            continue ;
        case /* Sem_greedy */5 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.TAG !== /* Sem_greedy */5) {
              return false;
            }
            if (x1._0 !== x2._0) {
              return false;
            }
            _x2 = x2._1;
            _x1 = x1._1;
            continue ;
        case /* Group */6 :
            return false;
        case /* No_group */7 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.TAG !== /* No_group */7) {
              return false;
            }
            _x2 = x2._0;
            _x1 = x1._0;
            continue ;
        case /* Nest */8 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.TAG !== /* Nest */8) {
              return false;
            }
            _x2 = x2._0;
            _x1 = x1._0;
            continue ;
        case /* Case */9 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.TAG !== /* Case */9) {
              return false;
            }
            _x2 = x2._0;
            _x1 = x1._0;
            continue ;
        case /* No_case */10 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.TAG !== /* No_case */10) {
              return false;
            }
            _x2 = x2._0;
            _x1 = x1._0;
            continue ;
        case /* Intersection */11 :
            if (typeof x2 === "number" || x2.TAG !== /* Intersection */11) {
              return false;
            } else {
              return eq_list(x1._0, x2._0);
            }
        case /* Complement */12 :
            if (typeof x2 === "number" || x2.TAG !== /* Complement */12) {
              return false;
            } else {
              return eq_list(x1._0, x2._0);
            }
        case /* Difference */13 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.TAG !== /* Difference */13) {
              return false;
            }
            if (!equal$2(x1._0, x2._0)) {
              return false;
            }
            _x2 = x2._1;
            _x1 = x1._1;
            continue ;
        case /* Pmark */14 :
            if (typeof x2 === "number") {
              return false;
            }
            if (x2.TAG !== /* Pmark */14) {
              return false;
            }
            if (x1._0 !== x2._0) {
              return false;
            }
            _x2 = x2._1;
            _x1 = x1._1;
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
    if (!equal$2(l1.hd, l2.hd)) {
      return false;
    }
    _l2 = l2.tl;
    _l1 = l1.tl;
    continue ;
  };
}

function sequence(l) {
  if (l && !l.tl) {
    return l.hd;
  } else {
    return {
            TAG: /* Sequence */1,
            _0: l
          };
  }
}

function merge_sequences(_param) {
  while(true) {
    var param = _param;
    if (!param) {
      return /* [] */0;
    }
    var l$p = param.hd;
    if (typeof l$p !== "number") {
      switch (l$p.TAG | 0) {
        case /* Sequence */1 :
            var match = l$p._0;
            if (match) {
              var y = match.tl;
              var x = match.hd;
              var r$p = merge_sequences(param.tl);
              var exit = 0;
              if (r$p) {
                var match$1 = r$p.hd;
                if (typeof match$1 === "number" || match$1.TAG !== /* Sequence */1) {
                  exit = 2;
                } else {
                  var match$2 = match$1._0;
                  if (match$2) {
                    if (equal$2(x, match$2.hd)) {
                      return {
                              hd: {
                                TAG: /* Sequence */1,
                                _0: {
                                  hd: x,
                                  tl: {
                                    hd: {
                                      TAG: /* Alternative */2,
                                      _0: {
                                        hd: sequence(y),
                                        tl: {
                                          hd: sequence(match$2.tl),
                                          tl: /* [] */0
                                        }
                                      }
                                    },
                                    tl: /* [] */0
                                  }
                                }
                              },
                              tl: r$p.tl
                            };
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
                return {
                        hd: {
                          TAG: /* Sequence */1,
                          _0: {
                            hd: x,
                            tl: y
                          }
                        },
                        tl: r$p
                      };
              }
              
            }
            break;
        case /* Alternative */2 :
            _param = Pervasives.$at(l$p._0, param.tl);
            continue ;
        default:
          
      }
    }
    return {
            hd: l$p,
            tl: merge_sequences(param.tl)
          };
  };
}

function enforce_kind(ids, kind, kind$p, cr) {
  if (kind === "First" && kind$p !== "First") {
    return seq$1(ids, kind$p, cr, mk_expr(ids, /* Eps */0));
  } else {
    return cr;
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
            return [
                    mk_expr(ids, {
                          TAG: /* After */7,
                          _0: c$1
                        }),
                    kind
                  ];
        case /* End_of_line */1 :
            var c$2 = Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.inexistant, Re_automata_Category.newline);
            return [
                    mk_expr(ids, {
                          TAG: /* Before */6,
                          _0: c$2
                        }),
                    kind
                  ];
        case /* Beg_of_word */2 :
            var c$3 = Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.inexistant, Re_automata_Category.not_letter);
            var c$4 = Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.inexistant, Re_automata_Category.letter);
            return [
                    seq$1(ids, "First", mk_expr(ids, {
                              TAG: /* After */7,
                              _0: c$3
                            }), mk_expr(ids, {
                              TAG: /* Before */6,
                              _0: c$4
                            })),
                    kind
                  ];
        case /* End_of_word */3 :
            var c$5 = Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.inexistant, Re_automata_Category.letter);
            var c$6 = Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.inexistant, Re_automata_Category.not_letter);
            return [
                    seq$1(ids, "First", mk_expr(ids, {
                              TAG: /* After */7,
                              _0: c$5
                            }), mk_expr(ids, {
                              TAG: /* Before */6,
                              _0: c$6
                            })),
                    kind
                  ];
        case /* Not_bound */4 :
            return [
                    alt(ids, {
                          hd: seq$1(ids, "First", mk_expr(ids, {
                                    TAG: /* After */7,
                                    _0: Re_automata_Category.letter
                                  }), mk_expr(ids, {
                                    TAG: /* Before */6,
                                    _0: Re_automata_Category.letter
                                  })),
                          tl: {
                            hd: seq$1(ids, "First", mk_expr(ids, {
                                      TAG: /* After */7,
                                      _0: Re_automata_Category.letter
                                    }), mk_expr(ids, {
                                      TAG: /* Before */6,
                                      _0: Re_automata_Category.letter
                                    })),
                            tl: /* [] */0
                          }
                        }),
                    kind
                  ];
        case /* Beg_of_str */5 :
            return [
                    mk_expr(ids, {
                          TAG: /* After */7,
                          _0: Re_automata_Category.inexistant
                        }),
                    kind
                  ];
        case /* End_of_str */6 :
            return [
                    mk_expr(ids, {
                          TAG: /* Before */6,
                          _0: Re_automata_Category.inexistant
                        }),
                    kind
                  ];
        case /* Last_end_of_line */7 :
            var c$7 = Curry._2(Re_automata_Category.$plus$plus, Re_automata_Category.inexistant, Re_automata_Category.lastnewline);
            return [
                    mk_expr(ids, {
                          TAG: /* Before */6,
                          _0: c$7
                        }),
                    kind
                  ];
        case /* Start */8 :
            return [
                    mk_expr(ids, {
                          TAG: /* After */7,
                          _0: Re_automata_Category.search_boundary
                        }),
                    kind
                  ];
        case /* Stop */9 :
            return [
                    mk_expr(ids, {
                          TAG: /* Before */6,
                          _0: Re_automata_Category.search_boundary
                        }),
                    kind
                  ];
        
      }
    } else {
      switch (s.TAG | 0) {
        case /* Set */0 :
            return [
                    cst(ids, trans_set(cache, c, s._0)),
                    kind
                  ];
        case /* Sequence */1 :
            return [
                    trans_seq(ids, kind, ign_group, ign_case, greedy, pos, cache, c, s._0),
                    kind
                  ];
        case /* Alternative */2 :
            var merged_sequences = merge_sequences(s._0);
            if (merged_sequences && !merged_sequences.tl) {
              var match = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, merged_sequences.hd);
              return [
                      enforce_kind(ids, kind, match[1], match[0]),
                      kind
                    ];
            }
            return [
                    alt(ids, List.map((function(ign_group,greedy){
                            return function (r$p) {
                              var match = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r$p);
                              return enforce_kind(ids, kind, match[1], match[0]);
                            }
                            }(ign_group,greedy)), merged_sequences)),
                    kind
                  ];
        case /* Repeat */3 :
            var j = s._2;
            var i = s._1;
            var match$1 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, s._0);
            var kind$p = match$1[1];
            var cr = match$1[0];
            var rem;
            if (j !== undefined) {
              var f = greedy === "Non_greedy" ? (function(cr,kind$p){
                return function (rem) {
                  return alt(ids, {
                              hd: mk_expr(ids, /* Eps */0),
                              tl: {
                                hd: seq$1(ids, kind$p, rename(ids, cr), rem),
                                tl: /* [] */0
                              }
                            });
                }
                }(cr,kind$p)) : (function(cr,kind$p){
                return function (rem) {
                  return alt(ids, {
                              hd: seq$1(ids, kind$p, rename(ids, cr), rem),
                              tl: {
                                hd: mk_expr(ids, /* Eps */0),
                                tl: /* [] */0
                              }
                            });
                }
                }(cr,kind$p));
              rem = iter(j - i | 0, f, mk_expr(ids, /* Eps */0));
            } else {
              rem = rep(ids, greedy, kind$p, cr);
            }
            return [
                    iter(i, (function(cr,kind$p){
                        return function (rem) {
                          return seq$1(ids, kind$p, rename(ids, cr), rem);
                        }
                        }(cr,kind$p)), rem),
                    kind
                  ];
        case /* Sem */4 :
            var kind$p$1 = s._0;
            var match$2 = translate(ids, kind$p$1, ign_group, ign_case, greedy, pos, cache, c, s._1);
            return [
                    enforce_kind(ids, kind$p$1, match$2[1], match$2[0]),
                    kind$p$1
                  ];
        case /* Sem_greedy */5 :
            _s = s._1;
            _greedy = s._0;
            continue ;
        case /* Group */6 :
            var r$p = s._0;
            if (ign_group) {
              _s = r$p;
              continue ;
            }
            var p = pos.contents;
            pos.contents = pos.contents + 2 | 0;
            var match$3 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r$p);
            return [
                    seq$1(ids, "First", mk_expr(ids, {
                              TAG: /* Mark */4,
                              _0: p
                            }), seq$1(ids, "First", match$3[0], mk_expr(ids, {
                                  TAG: /* Mark */4,
                                  _0: p + 1 | 0
                                }))),
                    match$3[1]
                  ];
        case /* No_group */7 :
            _s = s._0;
            _ign_group = true;
            continue ;
        case /* Nest */8 :
            var b = pos.contents;
            var match$4 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, s._0);
            var kind$p$2 = match$4[1];
            var cr$1 = match$4[0];
            var e = pos.contents - 1 | 0;
            if (e < b) {
              return [
                      cr$1,
                      kind$p$2
                    ];
            } else {
              return [
                      seq$1(ids, "First", erase(ids, b, e), cr$1),
                      kind$p$2
                    ];
            }
        case /* Pmark */14 :
            var match$5 = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, s._1);
            return [
                    seq$1(ids, "First", mk_expr(ids, {
                              TAG: /* Pmark */8,
                              _0: s._0
                            }), match$5[0]),
                    match$5[1]
                  ];
        default:
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "re.ml",
                  714,
                  4
                ],
                Error: new Error()
              };
      }
    }
  };
}

function trans_seq(ids, kind, ign_group, ign_case, greedy, pos, cache, c, param) {
  if (!param) {
    return mk_expr(ids, /* Eps */0);
  }
  var rem = param.tl;
  var r = param.hd;
  if (rem) {
    var match = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r);
    var cr$p = match[0];
    var cr$p$p = trans_seq(ids, kind, ign_group, ign_case, greedy, pos, cache, c, rem);
    if (is_eps(cr$p$p)) {
      return cr$p;
    } else if (is_eps(cr$p)) {
      return cr$p$p;
    } else {
      return seq$1(ids, match[1], cr$p, cr$p$p);
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
          RE_EXN_ID: "Assert_failure",
          _1: [
            "re.ml",
            747,
            13
          ],
          Error: new Error()
        };
  }
  if (s.TAG === /* Set */0) {
    return s._0;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "re.ml",
          747,
          13
        ],
        Error: new Error()
      };
}

function handle_case(_ign_case, _s) {
  while(true) {
    var s = _s;
    var ign_case = _ign_case;
    if (typeof s === "number") {
      return s;
    }
    switch (s.TAG | 0) {
      case /* Set */0 :
          var s$1 = s._0;
          return {
                  TAG: /* Set */0,
                  _0: ign_case ? case_insens(s$1) : s$1
                };
      case /* Sequence */1 :
          return {
                  TAG: /* Sequence */1,
                  _0: List.map((function(ign_case){
                      return function (param) {
                        return handle_case(ign_case, param);
                      }
                      }(ign_case)), s._0)
                };
      case /* Alternative */2 :
          var l$p = List.map((function(ign_case){
              return function (param) {
                return handle_case(ign_case, param);
              }
              }(ign_case)), s._0);
          if (is_charset({
                  TAG: /* Alternative */2,
                  _0: l$p
                })) {
            return {
                    TAG: /* Set */0,
                    _0: List.fold_left((function (s, r) {
                            return union(s, as_set(r));
                          }), /* [] */0, l$p)
                  };
          } else {
            return {
                    TAG: /* Alternative */2,
                    _0: l$p
                  };
          }
      case /* Repeat */3 :
          return {
                  TAG: /* Repeat */3,
                  _0: handle_case(ign_case, s._0),
                  _1: s._1,
                  _2: s._2
                };
      case /* Sem */4 :
          var r$p = handle_case(ign_case, s._1);
          if (is_charset(r$p)) {
            return r$p;
          } else {
            return {
                    TAG: /* Sem */4,
                    _0: s._0,
                    _1: r$p
                  };
          }
      case /* Sem_greedy */5 :
          var r$p$1 = handle_case(ign_case, s._1);
          if (is_charset(r$p$1)) {
            return r$p$1;
          } else {
            return {
                    TAG: /* Sem_greedy */5,
                    _0: s._0,
                    _1: r$p$1
                  };
          }
      case /* Group */6 :
          return {
                  TAG: /* Group */6,
                  _0: handle_case(ign_case, s._0)
                };
      case /* No_group */7 :
          var r$p$2 = handle_case(ign_case, s._0);
          if (is_charset(r$p$2)) {
            return r$p$2;
          } else {
            return {
                    TAG: /* No_group */7,
                    _0: r$p$2
                  };
          }
      case /* Nest */8 :
          var r$p$3 = handle_case(ign_case, s._0);
          if (is_charset(r$p$3)) {
            return r$p$3;
          } else {
            return {
                    TAG: /* Nest */8,
                    _0: r$p$3
                  };
          }
      case /* Case */9 :
          _s = s._0;
          _ign_case = false;
          continue ;
      case /* No_case */10 :
          _s = s._0;
          _ign_case = true;
          continue ;
      case /* Intersection */11 :
          var l$p$1 = List.map((function(ign_case){
              return function (r) {
                return handle_case(ign_case, r);
              }
              }(ign_case)), s._0);
          return {
                  TAG: /* Set */0,
                  _0: List.fold_left((function (s, r) {
                          return inter(s, as_set(r));
                        }), cany, l$p$1)
                };
      case /* Complement */12 :
          var l$p$2 = List.map((function(ign_case){
              return function (r) {
                return handle_case(ign_case, r);
              }
              }(ign_case)), s._0);
          return {
                  TAG: /* Set */0,
                  _0: diff(cany, List.fold_left((function (s, r) {
                              return union(s, as_set(r));
                            }), /* [] */0, l$p$2))
                };
      case /* Difference */13 :
          return {
                  TAG: /* Set */0,
                  _0: inter(as_set(handle_case(ign_case, s._0)), diff(cany, as_set(handle_case(ign_case, s._1))))
                };
      case /* Pmark */14 :
          return {
                  TAG: /* Pmark */14,
                  _0: s._0,
                  _1: handle_case(ign_case, s._1)
                };
      
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
      switch (l.TAG | 0) {
        case /* Sequence */1 :
            return List.exists(anchored, l._0);
        case /* Alternative */2 :
            return List.for_all(anchored, l._0);
        case /* Repeat */3 :
            if (l._1 <= 0) {
              return false;
            }
            _l = l._0;
            continue ;
        case /* Group */6 :
        case /* No_group */7 :
        case /* Nest */8 :
        case /* Case */9 :
        case /* No_case */10 :
            _l = l._0;
            continue ;
        case /* Sem */4 :
        case /* Sem_greedy */5 :
        case /* Pmark */14 :
            _l = l._1;
            continue ;
        default:
          return false;
      }
    }
  };
}

function alt$1(l) {
  if (l && !l.tl) {
    return l.hd;
  } else {
    return {
            TAG: /* Alternative */2,
            _0: l
          };
  }
}

function seq$2(l) {
  if (l && !l.tl) {
    return l.hd;
  } else {
    return {
            TAG: /* Sequence */1,
            _0: l
          };
  }
}

var epsilon = {
  TAG: /* Sequence */1,
  _0: /* [] */0
};

function repn(r, i, j) {
  if (i < 0) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Re.repn",
          Error: new Error()
        };
  }
  if (j !== undefined && j < i) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Re.repn",
          Error: new Error()
        };
  }
  return {
          TAG: /* Repeat */3,
          _0: r,
          _1: i,
          _2: j
        };
}

function set(str) {
  var s = /* [] */0;
  for(var i = 0 ,i_finish = str.length; i < i_finish; ++i){
    s = union(single(Caml_string.get(str, i)), s);
  }
  return {
          TAG: /* Set */0,
          _0: s
        };
}

function compl(l) {
  var r = {
    TAG: /* Complement */12,
    _0: l
  };
  if (is_charset(r)) {
    return r;
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Re.compl",
        Error: new Error()
      };
}

var any = {
  TAG: /* Set */0,
  _0: cany
};

var notnl = {
  TAG: /* Set */0,
  _0: diff(cany, {
        hd: [
          /* '\n' */10,
          /* '\n' */10
        ],
        tl: /* [] */0
      })
};

var lower = alt$1({
      hd: {
        TAG: /* Set */0,
        _0: seq(/* 'a' */97, /* 'z' */122)
      },
      tl: {
        hd: {
          TAG: /* Set */0,
          _0: {
            hd: [
              /* '\181' */181,
              /* '\181' */181
            ],
            tl: /* [] */0
          }
        },
        tl: {
          hd: {
            TAG: /* Set */0,
            _0: seq(/* '\223' */223, /* '\246' */246)
          },
          tl: {
            hd: {
              TAG: /* Set */0,
              _0: seq(/* '\248' */248, /* '\255' */255)
            },
            tl: /* [] */0
          }
        }
      }
    });

var upper = alt$1({
      hd: {
        TAG: /* Set */0,
        _0: seq(/* 'A' */65, /* 'Z' */90)
      },
      tl: {
        hd: {
          TAG: /* Set */0,
          _0: seq(/* '\192' */192, /* '\214' */214)
        },
        tl: {
          hd: {
            TAG: /* Set */0,
            _0: seq(/* '\216' */216, /* '\222' */222)
          },
          tl: /* [] */0
        }
      }
    });

var alpha = alt$1({
      hd: lower,
      tl: {
        hd: upper,
        tl: {
          hd: {
            TAG: /* Set */0,
            _0: {
              hd: [
                /* '\170' */170,
                /* '\170' */170
              ],
              tl: /* [] */0
            }
          },
          tl: {
            hd: {
              TAG: /* Set */0,
              _0: {
                hd: [
                  /* '\186' */186,
                  /* '\186' */186
                ],
                tl: /* [] */0
              }
            },
            tl: /* [] */0
          }
        }
      }
    });

var digit = {
  TAG: /* Set */0,
  _0: seq(/* '0' */48, /* '9' */57)
};

var alnum = alt$1({
      hd: alpha,
      tl: {
        hd: digit,
        tl: /* [] */0
      }
    });

var wordc = alt$1({
      hd: alnum,
      tl: {
        hd: {
          TAG: /* Set */0,
          _0: {
            hd: [
              /* '_' */95,
              /* '_' */95
            ],
            tl: /* [] */0
          }
        },
        tl: /* [] */0
      }
    });

var ascii = {
  TAG: /* Set */0,
  _0: seq(/* '\000' */0, /* '\127' */127)
};

var blank = set("\t ");

var cntrl = alt$1({
      hd: {
        TAG: /* Set */0,
        _0: seq(/* '\000' */0, /* '\031' */31)
      },
      tl: {
        hd: {
          TAG: /* Set */0,
          _0: seq(/* '\127' */127, /* '\159' */159)
        },
        tl: /* [] */0
      }
    });

var graph = alt$1({
      hd: {
        TAG: /* Set */0,
        _0: seq(/* '!' */33, /* '~' */126)
      },
      tl: {
        hd: {
          TAG: /* Set */0,
          _0: seq(/* '\160' */160, /* '\255' */255)
        },
        tl: /* [] */0
      }
    });

var print = alt$1({
      hd: {
        TAG: /* Set */0,
        _0: seq(/* ' ' */32, /* '~' */126)
      },
      tl: {
        hd: {
          TAG: /* Set */0,
          _0: seq(/* '\160' */160, /* '\255' */255)
        },
        tl: /* [] */0
      }
    });

var punct = alt$1({
      hd: {
        TAG: /* Set */0,
        _0: seq(/* '!' */33, /* '/' */47)
      },
      tl: {
        hd: {
          TAG: /* Set */0,
          _0: seq(/* ':' */58, /* '@' */64)
        },
        tl: {
          hd: {
            TAG: /* Set */0,
            _0: seq(/* '[' */91, /* '`' */96)
          },
          tl: {
            hd: {
              TAG: /* Set */0,
              _0: seq(/* '{' */123, /* '~' */126)
            },
            tl: {
              hd: {
                TAG: /* Set */0,
                _0: seq(/* '\160' */160, /* '\169' */169)
              },
              tl: {
                hd: {
                  TAG: /* Set */0,
                  _0: seq(/* '\171' */171, /* '\180' */180)
                },
                tl: {
                  hd: {
                    TAG: /* Set */0,
                    _0: seq(/* '\182' */182, /* '\185' */185)
                  },
                  tl: {
                    hd: {
                      TAG: /* Set */0,
                      _0: seq(/* '\187' */187, /* '\191' */191)
                    },
                    tl: {
                      hd: {
                        TAG: /* Set */0,
                        _0: {
                          hd: [
                            /* '\215' */215,
                            /* '\215' */215
                          ],
                          tl: /* [] */0
                        }
                      },
                      tl: {
                        hd: {
                          TAG: /* Set */0,
                          _0: {
                            hd: [
                              /* '\247' */247,
                              /* '\247' */247
                            ],
                            tl: /* [] */0
                          }
                        },
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
    });

var space = alt$1({
      hd: {
        TAG: /* Set */0,
        _0: {
          hd: [
            /* ' ' */32,
            /* ' ' */32
          ],
          tl: /* [] */0
        }
      },
      tl: {
        hd: {
          TAG: /* Set */0,
          _0: seq(/* '\t' */9, /* '\r' */13)
        },
        tl: /* [] */0
      }
    });

var xdigit = alt$1({
      hd: digit,
      tl: {
        hd: {
          TAG: /* Set */0,
          _0: seq(/* 'a' */97, /* 'f' */102)
        },
        tl: {
          hd: {
            TAG: /* Set */0,
            _0: seq(/* 'A' */65, /* 'F' */70)
          },
          tl: /* [] */0
        }
      }
    });

function compile(r) {
  var regexp = anchored(r) ? ({
        TAG: /* Group */6,
        _0: r
      }) : seq$2({
          hd: {
            TAG: /* Sem */4,
            _0: "Shortest",
            _1: repn(any, 0, undefined)
          },
          tl: {
            hd: {
              TAG: /* Group */6,
              _0: r
            },
            tl: /* [] */0
          }
        });
  var regexp$1 = handle_case(false, regexp);
  var c = Bytes.make(257, /* '\000' */0);
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
  var match$1 = translate(ids, "First", false, false, "Greedy", pos, {
        contents: /* Empty */0
      }, col, regexp$1);
  var r$1 = enforce_kind(ids, "First", match$1[1], match$1[0]);
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
          RE_EXN_ID: "Invalid_argument",
          _1: name,
          Error: new Error()
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
      ] : Caml_array.make(n, 0);
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
      Caml_array.set(info.positions, match[0], last + 1 | 0);
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
    return /* Match */{
            _0: {
              s: s,
              marks: res._0,
              pmarks: res._1,
              gpos: info.positions,
              gcount: re.group_count
            }
          };
  }
}

function offset$1(t, i) {
  if (((i << 1) + 1 | 0) >= t.marks.length) {
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  }
  var m1 = Caml_array.get(t.marks, (i << 1));
  if (m1 === -1) {
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  }
  var p1 = Caml_array.get(t.gpos, m1) - 1 | 0;
  var p2 = Caml_array.get(t.gpos, Caml_array.get(t.marks, (i << 1) + 1 | 0)) - 1 | 0;
  return [
          p1,
          p2
        ];
}

function get(t, i) {
  var match = offset$1(t, i);
  var p1 = match[0];
  return $$String.sub(t.s, p1, match[1] - p1 | 0);
}

var Parse_error = /* @__PURE__ */Caml_exceptions.create("Parse_error");

var Not_supported = /* @__PURE__ */Caml_exceptions.create("Not_supported");

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
            RE_EXN_ID: "Invalid_argument",
            _1: s,
            Error: new Error()
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
  var accept_s = function (s$p) {
    var len = s$p.length;
    try {
      for(var j = 0; j < len; ++j){
        try {
          if (Caml_string.get(s$p, j) !== Caml_string.get(s, i.contents + j | 0)) {
            throw {
                  RE_EXN_ID: Pervasives.Exit,
                  Error: new Error()
                };
          }
          
        }
        catch (exn){
          throw {
                RE_EXN_ID: Pervasives.Exit,
                Error: new Error()
              };
        }
      }
      i.contents = i.contents + len | 0;
      return true;
    }
    catch (raw_exn){
      var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn$1.RE_EXN_ID === Pervasives.Exit) {
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
    var gr = accept(/* '?' */63);
    var gr$1 = ungreedy ? !gr : gr;
    if (gr$1) {
      return {
              TAG: /* Sem_greedy */5,
              _0: "Non_greedy",
              _1: r
            };
    } else {
      return {
              TAG: /* Sem_greedy */5,
              _0: "Greedy",
              _1: r
            };
    }
  };
  var regexp$p = function (_left) {
    while(true) {
      var left = _left;
      if (!accept(/* '|' */124)) {
        return left;
      }
      _left = alt$1({
            hd: left,
            tl: {
              hd: branch$p(/* [] */0),
              tl: /* [] */0
            }
          });
      continue ;
    };
  };
  var branch$p = function (_left) {
    while(true) {
      var left = _left;
      if (i.contents === l || test(/* '|' */124) || test(/* ')' */41)) {
        return seq$2(List.rev(left));
      }
      _left = {
        hd: piece(undefined),
        tl: left
      };
      continue ;
    };
  };
  var bracket = function (_s) {
    while(true) {
      var s = _s;
      if (s !== /* [] */0 && accept(/* ']' */93)) {
        return s;
      }
      var match = $$char(undefined);
      if (match.NAME === "Char") {
        var c = match.VAL;
        if (accept(/* '-' */45)) {
          if (accept(/* ']' */93)) {
            return {
                    hd: {
                      TAG: /* Set */0,
                      _0: single(c)
                    },
                    tl: {
                      hd: {
                        TAG: /* Set */0,
                        _0: {
                          hd: [
                            /* '-' */45,
                            /* '-' */45
                          ],
                          tl: /* [] */0
                        }
                      },
                      tl: s
                    }
                  };
          }
          var match$1 = $$char(undefined);
          if (match$1.NAME !== "Char") {
            return {
                    hd: {
                      TAG: /* Set */0,
                      _0: single(c)
                    },
                    tl: {
                      hd: {
                        TAG: /* Set */0,
                        _0: {
                          hd: [
                            /* '-' */45,
                            /* '-' */45
                          ],
                          tl: /* [] */0
                        }
                      },
                      tl: {
                        hd: match$1.VAL,
                        tl: s
                      }
                    }
                  };
          }
          _s = {
            hd: {
              TAG: /* Set */0,
              _0: seq(c, match$1.VAL)
            },
            tl: s
          };
          continue ;
        }
        _s = {
          hd: {
            TAG: /* Set */0,
            _0: single(c)
          },
          tl: s
        };
        continue ;
      }
      _s = {
        hd: match.VAL,
        tl: s
      };
      continue ;
    };
  };
  var piece = function (param) {
    var r = atom(undefined);
    if (accept(/* '*' */42)) {
      return greedy_mod(repn(r, 0, undefined));
    }
    if (accept(/* '+' */43)) {
      return greedy_mod(repn(r, 1, undefined));
    }
    if (accept(/* '?' */63)) {
      return greedy_mod(repn(r, 0, 1));
    }
    if (!accept(/* '{' */123)) {
      return r;
    }
    var i$1 = integer(undefined);
    if (i$1 !== undefined) {
      var j = accept(/* ',' */44) ? integer(undefined) : i$1;
      if (!accept(/* '}' */125)) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      if (j !== undefined && j < i$1) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      return greedy_mod(repn(r, i$1, j));
    }
    i.contents = i.contents - 1 | 0;
    return r;
  };
  var $$char = function (param) {
    if (i.contents === l) {
      throw {
            RE_EXN_ID: Parse_error,
            Error: new Error()
          };
    }
    var c = get(undefined);
    if (c === /* '[' */91) {
      if (accept(/* '=' */61)) {
        throw {
              RE_EXN_ID: Not_supported,
              Error: new Error()
            };
      }
      if (accept(/* ':' */58)) {
        var compl$1 = accept(/* '^' */94);
        var cls;
        try {
          cls = List.find(accept_s, {
                hd: "alnum",
                tl: {
                  hd: "ascii",
                  tl: {
                    hd: "blank",
                    tl: {
                      hd: "cntrl",
                      tl: {
                        hd: "digit",
                        tl: {
                          hd: "lower",
                          tl: {
                            hd: "print",
                            tl: {
                              hd: "space",
                              tl: {
                                hd: "upper",
                                tl: {
                                  hd: "word",
                                  tl: {
                                    hd: "punct",
                                    tl: {
                                      hd: "graph",
                                      tl: {
                                        hd: "xdigit",
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
              });
        }
        catch (raw_exn){
          var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
          if (exn.RE_EXN_ID === "Not_found") {
            throw {
                  RE_EXN_ID: Parse_error,
                  Error: new Error()
                };
          }
          throw exn;
        }
        if (!accept_s(":]")) {
          throw {
                RE_EXN_ID: Parse_error,
                Error: new Error()
              };
        }
        var posix_class = posix_class_of_string(cls);
        var re = compl$1 ? compl({
                hd: posix_class,
                tl: /* [] */0
              }) : posix_class;
        return {
                NAME: "Set",
                VAL: re
              };
      }
      if (!accept(/* '.' */46)) {
        return {
                NAME: "Char",
                VAL: c
              };
      }
      if (i.contents === l) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      var c$1 = get(undefined);
      if (!accept(/* '.' */46)) {
        throw {
              RE_EXN_ID: Not_supported,
              Error: new Error()
            };
      }
      if (!accept(/* ']' */93)) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      return {
              NAME: "Char",
              VAL: c$1
            };
    }
    if (c !== /* '\\' */92) {
      return {
              NAME: "Char",
              VAL: c
            };
    }
    var c$2 = get(undefined);
    if (c$2 >= 58) {
      if (c$2 >= 123) {
        return {
                NAME: "Char",
                VAL: c$2
              };
      }
      switch (c$2) {
        case 68 :
            return {
                    NAME: "Set",
                    VAL: compl({
                          hd: digit,
                          tl: /* [] */0
                        })
                  };
        case 83 :
            return {
                    NAME: "Set",
                    VAL: compl({
                          hd: space,
                          tl: /* [] */0
                        })
                  };
        case 87 :
            return {
                    NAME: "Set",
                    VAL: compl({
                          hd: alnum,
                          tl: {
                            hd: {
                              TAG: /* Set */0,
                              _0: {
                                hd: [
                                  /* '_' */95,
                                  /* '_' */95
                                ],
                                tl: /* [] */0
                              }
                            },
                            tl: /* [] */0
                          }
                        })
                  };
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
            return {
                    NAME: "Char",
                    VAL: c$2
                  };
        case 98 :
            return {
                    NAME: "Char",
                    VAL: /* '\b' */8
                  };
        case 100 :
            return {
                    NAME: "Set",
                    VAL: digit
                  };
        case 110 :
            return {
                    NAME: "Char",
                    VAL: /* '\n' */10
                  };
        case 114 :
            return {
                    NAME: "Char",
                    VAL: /* '\r' */13
                  };
        case 115 :
            return {
                    NAME: "Set",
                    VAL: space
                  };
        case 116 :
            return {
                    NAME: "Char",
                    VAL: /* '\t' */9
                  };
        case 119 :
            return {
                    NAME: "Set",
                    VAL: alt$1({
                          hd: alnum,
                          tl: {
                            hd: {
                              TAG: /* Set */0,
                              _0: {
                                hd: [
                                  /* '_' */95,
                                  /* '_' */95
                                ],
                                tl: /* [] */0
                              }
                            },
                            tl: /* [] */0
                          }
                        })
                  };
        case 65 :
        case 66 :
        case 67 :
        case 69 :
        case 70 :
        case 71 :
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
        case 90 :
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
        case 111 :
        case 112 :
        case 113 :
        case 117 :
        case 118 :
        case 120 :
        case 121 :
        case 122 :
            throw {
                  RE_EXN_ID: Parse_error,
                  Error: new Error()
                };
        
      }
    } else {
      if (c$2 >= 48) {
        throw {
              RE_EXN_ID: Not_supported,
              Error: new Error()
            };
      }
      return {
              NAME: "Char",
              VAL: c$2
            };
    }
  };
  var atom = function (param) {
    if (accept(/* '.' */46)) {
      if (dotall) {
        return any;
      } else {
        return notnl;
      }
    }
    if (accept(/* '(' */40)) {
      if (accept(/* '?' */63)) {
        if (accept(/* ':' */58)) {
          var r = regexp$p(branch$p(/* [] */0));
          if (!accept(/* ')' */41)) {
            throw {
                  RE_EXN_ID: Parse_error,
                  Error: new Error()
                };
          }
          return r;
        }
        if (accept(/* '#' */35)) {
          var _param;
          while(true) {
            if (accept(/* ')' */41)) {
              return epsilon;
            }
            i.contents = i.contents + 1 | 0;
            _param = undefined;
            continue ;
          };
        }
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      var r$1 = regexp$p(branch$p(/* [] */0));
      if (!accept(/* ')' */41)) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      return {
              TAG: /* Group */6,
              _0: r$1
            };
    }
    if (accept(/* '^' */94)) {
      if (multiline) {
        return /* Beg_of_line */0;
      } else {
        return /* Beg_of_str */5;
      }
    }
    if (accept(/* '$' */36)) {
      if (multiline) {
        return /* End_of_line */1;
      } else if (dollar_endonly) {
        return /* Last_end_of_line */7;
      } else {
        return /* End_of_str */6;
      }
    }
    if (accept(/* '[' */91)) {
      if (accept(/* '^' */94)) {
        return compl(bracket(/* [] */0));
      } else {
        return alt$1(bracket(/* [] */0));
      }
    }
    if (accept(/* '\\' */92)) {
      if (i.contents === l) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
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
                  RE_EXN_ID: Not_supported,
                  Error: new Error()
                };
        case 65 :
            return /* Beg_of_str */5;
        case 66 :
            return /* Not_bound */4;
        case 68 :
            return compl({
                        hd: digit,
                        tl: /* [] */0
                      });
        case 71 :
            return /* Start */8;
        case 83 :
            return compl({
                        hd: space,
                        tl: /* [] */0
                      });
        case 87 :
            return compl({
                        hd: alnum,
                        tl: {
                          hd: {
                            TAG: /* Set */0,
                            _0: {
                              hd: [
                                /* '_' */95,
                                /* '_' */95
                              ],
                              tl: /* [] */0
                            }
                          },
                          tl: /* [] */0
                        }
                      });
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
            return {
                    TAG: /* Set */0,
                    _0: single(c)
                  };
        case 98 :
            return alt$1({
                        hd: /* Beg_of_word */2,
                        tl: {
                          hd: /* End_of_word */3,
                          tl: /* [] */0
                        }
                      });
        case 100 :
            return digit;
        case 115 :
            return space;
        case 119 :
            return alt$1({
                        hd: alnum,
                        tl: {
                          hd: {
                            TAG: /* Set */0,
                            _0: {
                              hd: [
                                /* '_' */95,
                                /* '_' */95
                              ],
                              tl: /* [] */0
                            }
                          },
                          tl: /* [] */0
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
            throw {
                  RE_EXN_ID: Parse_error,
                  Error: new Error()
                };
        case 122 :
            return /* End_of_str */6;
        default:
          return {
                  TAG: /* Set */0,
                  _0: single(c)
                };
      }
    } else {
      if (i.contents === l) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      var c$1 = get(undefined);
      if (c$1 >= 64) {
        if (c$1 !== 92) {
          if (c$1 !== 123) {
            return {
                    TAG: /* Set */0,
                    _0: single(c$1)
                  };
          }
          throw {
                RE_EXN_ID: Parse_error,
                Error: new Error()
              };
        }
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      if (c$1 >= 44) {
        if (c$1 >= 63) {
          throw {
                RE_EXN_ID: Parse_error,
                Error: new Error()
              };
        }
        return {
                TAG: /* Set */0,
                _0: single(c$1)
              };
      }
      if (c$1 >= 42) {
        throw {
              RE_EXN_ID: Parse_error,
              Error: new Error()
            };
      }
      return {
              TAG: /* Set */0,
              _0: single(c$1)
            };
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
      var _i = d - /* '0' */48 | 0;
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
        var i$p = Math.imul(10, i$1) + (d$1 - /* '0' */48 | 0) | 0;
        if (i$p < i$1) {
          throw {
                RE_EXN_ID: Parse_error,
                Error: new Error()
              };
        }
        _i = i$p;
        continue ;
      };
    }
  };
  var res = regexp$p(branch$p(/* [] */0));
  if (i.contents !== l) {
    throw {
          RE_EXN_ID: Parse_error,
          Error: new Error()
        };
  }
  return res;
}

function re(flagsOpt, pat) {
  var flags = flagsOpt !== undefined ? flagsOpt : /* [] */0;
  var opts = List.map((function (param) {
          if (param === "CASELESS") {
            return "Caseless";
          } else if (param === "ANCHORED") {
            return "Anchored";
          } else {
            return "Multiline";
          }
        }), flags);
  var optsOpt = opts;
  var opts$1 = optsOpt !== undefined ? optsOpt : /* [] */0;
  var r = parse(List.memq("Multiline", opts$1), List.memq("Dollar_endonly", opts$1), List.memq("Dotall", opts$1), List.memq("Ungreedy", opts$1), pat);
  var r$1 = List.memq("Anchored", opts$1) ? seq$2({
          hd: /* Start */8,
          tl: {
            hd: r,
            tl: /* [] */0
          }
        }) : r;
  if (List.memq("Caseless", opts$1)) {
    return {
            TAG: /* No_case */10,
            _0: r$1
          };
  } else {
    return r$1;
  }
}

function exec(rex, pos, s) {
  var len;
  var substr = exec_internal("Re.exec", pos, len, true, rex, s);
  if (typeof substr === "number") {
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  }
  return substr._0;
}

var s = "a".repeat(1048575) + "b";

eq("File \"xx.ml\", line 7, characters 3-10", get(exec(compile(re(undefined, "aa?b")), undefined, s), 0), "aab");

Mt.from_pair_suites("Ocaml_re_test", suites.contents);

/* Table Not a pure module */
