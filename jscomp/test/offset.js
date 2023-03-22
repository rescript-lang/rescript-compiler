'use strict';

var Caml = require("../../lib/js/caml.js");
var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var $$String = require("../../lib/js/string.js");
var Caml_option = require("../../lib/js/caml_option.js");

function height(param) {
  if (typeof param !== "object") {
    return 0;
  } else {
    return param.h;
  }
}

function create(l, v, r) {
  var hl;
  hl = typeof l !== "object" ? 0 : l.h;
  var hr;
  hr = typeof r !== "object" ? 0 : r.h;
  return {
          TAG: "Node",
          l: l,
          v: v,
          r: r,
          h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal(l, v, r) {
  var hl;
  hl = typeof l !== "object" ? 0 : l.h;
  var hr;
  hr = typeof r !== "object" ? 0 : r.h;
  if (hl > (hr + 2 | 0)) {
    if (typeof l !== "object") {
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Set.bal",
            Error: new Error()
          };
    }
    var lr = l.r;
    var lv = l.v;
    var ll = l.l;
    if (height(ll) >= height(lr)) {
      return create(ll, lv, create(lr, v, r));
    }
    if (typeof lr === "object") {
      return create(create(ll, lv, lr.l), lr.v, create(lr.r, v, r));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Set.bal",
          Error: new Error()
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return {
            TAG: "Node",
            l: l,
            v: v,
            r: r,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (typeof r !== "object") {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Set.bal",
          Error: new Error()
        };
  }
  var rr = r.r;
  var rv = r.v;
  var rl = r.l;
  if (height(rr) >= height(rl)) {
    return create(create(l, v, rl), rv, rr);
  }
  if (typeof rl === "object") {
    return create(create(l, v, rl.l), rl.v, create(rl.r, rv, rr));
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Set.bal",
        Error: new Error()
      };
}

function add(x, t) {
  if (typeof t !== "object") {
    return {
            TAG: "Node",
            l: "Empty",
            v: x,
            r: "Empty",
            h: 1
          };
  }
  var r = t.r;
  var v = t.v;
  var l = t.l;
  var c = Caml.string_compare(x, v);
  if (c === 0) {
    return t;
  }
  if (c < 0) {
    var ll = add(x, l);
    if (l === ll) {
      return t;
    } else {
      return bal(ll, v, r);
    }
  }
  var rr = add(x, r);
  if (r === rr) {
    return t;
  } else {
    return bal(l, v, rr);
  }
}

function singleton(x) {
  return {
          TAG: "Node",
          l: "Empty",
          v: x,
          r: "Empty",
          h: 1
        };
}

function add_min_element(x, param) {
  if (typeof param !== "object") {
    return singleton(x);
  } else {
    return bal(add_min_element(x, param.l), param.v, param.r);
  }
}

function add_max_element(x, param) {
  if (typeof param !== "object") {
    return singleton(x);
  } else {
    return bal(param.l, param.v, add_max_element(x, param.r));
  }
}

function join(l, v, r) {
  if (typeof l !== "object") {
    return add_min_element(v, r);
  }
  var lh = l.h;
  if (typeof r !== "object") {
    return add_max_element(v, l);
  }
  var rh = r.h;
  if (lh > (rh + 2 | 0)) {
    return bal(l.l, l.v, join(l.r, v, r));
  } else if (rh > (lh + 2 | 0)) {
    return bal(join(l, v, r.l), r.v, r.r);
  } else {
    return create(l, v, r);
  }
}

function min_elt(_param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    var l = param.l;
    if (typeof l !== "object") {
      return param.v;
    }
    _param = l;
    continue ;
  };
}

function min_elt_opt(_param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      return ;
    }
    var l = param.l;
    if (typeof l !== "object") {
      return Caml_option.some(param.v);
    }
    _param = l;
    continue ;
  };
}

function max_elt(_param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    var r = param.r;
    if (typeof r !== "object") {
      return param.v;
    }
    _param = r;
    continue ;
  };
}

function max_elt_opt(_param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      return ;
    }
    var r = param.r;
    if (typeof r !== "object") {
      return Caml_option.some(param.v);
    }
    _param = r;
    continue ;
  };
}

function remove_min_elt(param) {
  if (typeof param !== "object") {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Set.remove_min_elt",
          Error: new Error()
        };
  }
  var l = param.l;
  if (typeof l !== "object") {
    return param.r;
  } else {
    return bal(remove_min_elt(l), param.v, param.r);
  }
}

function concat(t1, t2) {
  if (typeof t1 !== "object") {
    return t2;
  } else if (typeof t2 !== "object") {
    return t1;
  } else {
    return join(t1, min_elt(t2), remove_min_elt(t2));
  }
}

function split(x, param) {
  if (typeof param !== "object") {
    return [
            "Empty",
            false,
            "Empty"
          ];
  }
  var r = param.r;
  var v = param.v;
  var l = param.l;
  var c = Caml.string_compare(x, v);
  if (c === 0) {
    return [
            l,
            true,
            r
          ];
  }
  if (c < 0) {
    var match = split(x, l);
    return [
            match[0],
            match[1],
            join(match[2], v, r)
          ];
  }
  var match$1 = split(x, r);
  return [
          join(l, v, match$1[0]),
          match$1[1],
          match$1[2]
        ];
}

function is_empty(param) {
  if (typeof param !== "object") {
    return true;
  } else {
    return false;
  }
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      return false;
    }
    var c = Caml.string_compare(x, param.v);
    if (c === 0) {
      return true;
    }
    _param = c < 0 ? param.l : param.r;
    continue ;
  };
}

function remove(x, t) {
  if (typeof t !== "object") {
    return "Empty";
  }
  var r = t.r;
  var v = t.v;
  var l = t.l;
  var c = Caml.string_compare(x, v);
  if (c === 0) {
    if (typeof l !== "object") {
      return r;
    } else if (typeof r !== "object") {
      return l;
    } else {
      return bal(l, min_elt(r), remove_min_elt(r));
    }
  }
  if (c < 0) {
    var ll = remove(x, l);
    if (l === ll) {
      return t;
    } else {
      return bal(ll, v, r);
    }
  }
  var rr = remove(x, r);
  if (r === rr) {
    return t;
  } else {
    return bal(l, v, rr);
  }
}

function union(s1, s2) {
  if (typeof s1 !== "object") {
    return s2;
  }
  var h1 = s1.h;
  var v1 = s1.v;
  if (typeof s2 !== "object") {
    return s1;
  }
  var h2 = s2.h;
  var v2 = s2.v;
  if (h1 >= h2) {
    if (h2 === 1) {
      return add(v2, s1);
    }
    var match = split(v1, s2);
    return join(union(s1.l, match[0]), v1, union(s1.r, match[2]));
  }
  if (h1 === 1) {
    return add(v1, s2);
  }
  var match$1 = split(v2, s1);
  return join(union(match$1[0], s2.l), v2, union(match$1[2], s2.r));
}

function inter(s1, s2) {
  if (typeof s1 !== "object") {
    return "Empty";
  }
  if (typeof s2 !== "object") {
    return "Empty";
  }
  var r1 = s1.r;
  var v1 = s1.v;
  var l1 = s1.l;
  var match = split(v1, s2);
  var l2 = match[0];
  if (match[1]) {
    return join(inter(l1, l2), v1, inter(r1, match[2]));
  } else {
    return concat(inter(l1, l2), inter(r1, match[2]));
  }
}

function diff(s1, s2) {
  if (typeof s1 !== "object") {
    return "Empty";
  }
  if (typeof s2 !== "object") {
    return s1;
  }
  var r1 = s1.r;
  var v1 = s1.v;
  var l1 = s1.l;
  var match = split(v1, s2);
  var l2 = match[0];
  if (match[1]) {
    return concat(diff(l1, l2), diff(r1, match[2]));
  } else {
    return join(diff(l1, l2), v1, diff(r1, match[2]));
  }
}

function cons_enum(_s, _e) {
  while(true) {
    var e = _e;
    var s = _s;
    if (typeof s !== "object") {
      return e;
    }
    _e = {
      TAG: "More",
      _0: s.v,
      _1: s.r,
      _2: e
    };
    _s = s.l;
    continue ;
  };
}

function compare(s1, s2) {
  var _e1 = cons_enum(s1, "End");
  var _e2 = cons_enum(s2, "End");
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (typeof e1 !== "object") {
      if (typeof e2 !== "object") {
        return 0;
      } else {
        return -1;
      }
    }
    if (typeof e2 !== "object") {
      return 1;
    }
    var c = Caml.string_compare(e1._0, e2._0);
    if (c !== 0) {
      return c;
    }
    _e2 = cons_enum(e2._1, e2._2);
    _e1 = cons_enum(e1._1, e1._2);
    continue ;
  };
}

function equal(s1, s2) {
  return compare(s1, s2) === 0;
}

function subset(_s1, _s2) {
  while(true) {
    var s2 = _s2;
    var s1 = _s1;
    if (typeof s1 !== "object") {
      return true;
    }
    var r1 = s1.r;
    var v1 = s1.v;
    var l1 = s1.l;
    if (typeof s2 !== "object") {
      return false;
    }
    var r2 = s2.r;
    var l2 = s2.l;
    var c = Caml.string_compare(v1, s2.v);
    if (c === 0) {
      if (!subset(l1, l2)) {
        return false;
      }
      _s2 = r2;
      _s1 = r1;
      continue ;
    }
    if (c < 0) {
      if (!subset({
              TAG: "Node",
              l: l1,
              v: v1,
              r: "Empty",
              h: 0
            }, l2)) {
        return false;
      }
      _s1 = r1;
      continue ;
    }
    if (!subset({
            TAG: "Node",
            l: "Empty",
            v: v1,
            r: r1,
            h: 0
          }, r2)) {
      return false;
    }
    _s1 = l1;
    continue ;
  };
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      return ;
    }
    iter(f, param.l);
    Curry._1(f, param.v);
    _param = param.r;
    continue ;
  };
}

function fold(f, _s, _accu) {
  while(true) {
    var accu = _accu;
    var s = _s;
    if (typeof s !== "object") {
      return accu;
    }
    _accu = Curry._2(f, s.v, fold(f, s.l, accu));
    _s = s.r;
    continue ;
  };
}

function for_all(p, _param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      return true;
    }
    if (!Curry._1(p, param.v)) {
      return false;
    }
    if (!for_all(p, param.l)) {
      return false;
    }
    _param = param.r;
    continue ;
  };
}

function exists(p, _param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      return false;
    }
    if (Curry._1(p, param.v)) {
      return true;
    }
    if (exists(p, param.l)) {
      return true;
    }
    _param = param.r;
    continue ;
  };
}

function filter(p, t) {
  if (typeof t !== "object") {
    return "Empty";
  }
  var r = t.r;
  var v = t.v;
  var l = t.l;
  var l$p = filter(p, l);
  var pv = Curry._1(p, v);
  var r$p = filter(p, r);
  if (pv) {
    if (l === l$p && r === r$p) {
      return t;
    } else {
      return join(l$p, v, r$p);
    }
  } else {
    return concat(l$p, r$p);
  }
}

function partition(p, param) {
  if (typeof param !== "object") {
    return [
            "Empty",
            "Empty"
          ];
  }
  var v = param.v;
  var match = partition(p, param.l);
  var lf = match[1];
  var lt = match[0];
  var pv = Curry._1(p, v);
  var match$1 = partition(p, param.r);
  var rf = match$1[1];
  var rt = match$1[0];
  if (pv) {
    return [
            join(lt, v, rt),
            concat(lf, rf)
          ];
  } else {
    return [
            concat(lt, rt),
            join(lf, v, rf)
          ];
  }
}

function cardinal(param) {
  if (typeof param !== "object") {
    return 0;
  } else {
    return (cardinal(param.l) + 1 | 0) + cardinal(param.r) | 0;
  }
}

function elements_aux(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (typeof param !== "object") {
      return accu;
    }
    _param = param.l;
    _accu = {
      hd: param.v,
      tl: elements_aux(accu, param.r)
    };
    continue ;
  };
}

function elements(s) {
  return elements_aux(/* [] */0, s);
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    var v = param.v;
    var c = Caml.string_compare(x, v);
    if (c === 0) {
      return v;
    }
    _param = c < 0 ? param.l : param.r;
    continue ;
  };
}

function find_first(f, _param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    var v = param.v;
    if (Curry._1(f, v)) {
      var _v0 = v;
      var _param$1 = param.l;
      while(true) {
        var param$1 = _param$1;
        var v0 = _v0;
        if (typeof param$1 !== "object") {
          return v0;
        }
        var v$1 = param$1.v;
        if (Curry._1(f, v$1)) {
          _param$1 = param$1.l;
          _v0 = v$1;
          continue ;
        }
        _param$1 = param$1.r;
        continue ;
      };
    }
    _param = param.r;
    continue ;
  };
}

function find_first_opt(f, _param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      return ;
    }
    var v = param.v;
    if (Curry._1(f, v)) {
      var _v0 = v;
      var _param$1 = param.l;
      while(true) {
        var param$1 = _param$1;
        var v0 = _v0;
        if (typeof param$1 !== "object") {
          return Caml_option.some(v0);
        }
        var v$1 = param$1.v;
        if (Curry._1(f, v$1)) {
          _param$1 = param$1.l;
          _v0 = v$1;
          continue ;
        }
        _param$1 = param$1.r;
        continue ;
      };
    }
    _param = param.r;
    continue ;
  };
}

function find_last(f, _param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    var v = param.v;
    if (Curry._1(f, v)) {
      var _v0 = v;
      var _param$1 = param.r;
      while(true) {
        var param$1 = _param$1;
        var v0 = _v0;
        if (typeof param$1 !== "object") {
          return v0;
        }
        var v$1 = param$1.v;
        if (Curry._1(f, v$1)) {
          _param$1 = param$1.r;
          _v0 = v$1;
          continue ;
        }
        _param$1 = param$1.l;
        continue ;
      };
    }
    _param = param.l;
    continue ;
  };
}

function find_last_opt(f, _param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      return ;
    }
    var v = param.v;
    if (Curry._1(f, v)) {
      var _v0 = v;
      var _param$1 = param.r;
      while(true) {
        var param$1 = _param$1;
        var v0 = _v0;
        if (typeof param$1 !== "object") {
          return Caml_option.some(v0);
        }
        var v$1 = param$1.v;
        if (Curry._1(f, v$1)) {
          _param$1 = param$1.r;
          _v0 = v$1;
          continue ;
        }
        _param$1 = param$1.l;
        continue ;
      };
    }
    _param = param.l;
    continue ;
  };
}

function find_opt(x, _param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      return ;
    }
    var v = param.v;
    var c = Caml.string_compare(x, v);
    if (c === 0) {
      return Caml_option.some(v);
    }
    _param = c < 0 ? param.l : param.r;
    continue ;
  };
}

function map(f, t) {
  if (typeof t !== "object") {
    return "Empty";
  }
  var r = t.r;
  var v = t.v;
  var l = t.l;
  var l$p = map(f, l);
  var v$p = Curry._1(f, v);
  var r$p = map(f, r);
  if (l === l$p && v === v$p && r === r$p) {
    return t;
  } else if ((l$p === "Empty" || Caml.string_compare(max_elt(l$p), v$p) < 0) && (r$p === "Empty" || Caml.string_compare(v$p, min_elt(r$p)) < 0)) {
    return join(l$p, v$p, r$p);
  } else {
    return union(l$p, add(v$p, r$p));
  }
}

function of_list(l) {
  if (!l) {
    return "Empty";
  }
  var match = l.tl;
  var x0 = l.hd;
  if (!match) {
    return singleton(x0);
  }
  var match$1 = match.tl;
  var x1 = match.hd;
  if (!match$1) {
    return add(x1, singleton(x0));
  }
  var match$2 = match$1.tl;
  var x2 = match$1.hd;
  if (!match$2) {
    return add(x2, add(x1, singleton(x0)));
  }
  var match$3 = match$2.tl;
  var x3 = match$2.hd;
  if (match$3) {
    if (match$3.tl) {
      var l$1 = List.sort_uniq($$String.compare, l);
      var sub = function (n, l) {
        switch (n) {
          case 0 :
              return [
                      "Empty",
                      l
                    ];
          case 1 :
              if (l) {
                return [
                        {
                          TAG: "Node",
                          l: "Empty",
                          v: l.hd,
                          r: "Empty",
                          h: 1
                        },
                        l.tl
                      ];
              }
              break;
          case 2 :
              if (l) {
                var match = l.tl;
                if (match) {
                  return [
                          {
                            TAG: "Node",
                            l: {
                              TAG: "Node",
                              l: "Empty",
                              v: l.hd,
                              r: "Empty",
                              h: 1
                            },
                            v: match.hd,
                            r: "Empty",
                            h: 2
                          },
                          match.tl
                        ];
                }
                
              }
              break;
          case 3 :
              if (l) {
                var match$1 = l.tl;
                if (match$1) {
                  var match$2 = match$1.tl;
                  if (match$2) {
                    return [
                            {
                              TAG: "Node",
                              l: {
                                TAG: "Node",
                                l: "Empty",
                                v: l.hd,
                                r: "Empty",
                                h: 1
                              },
                              v: match$1.hd,
                              r: {
                                TAG: "Node",
                                l: "Empty",
                                v: match$2.hd,
                                r: "Empty",
                                h: 1
                              },
                              h: 2
                            },
                            match$2.tl
                          ];
                  }
                  
                }
                
              }
              break;
          default:
            
        }
        var nl = n / 2 | 0;
        var match$3 = sub(nl, l);
        var l$1 = match$3[1];
        if (l$1) {
          var match$4 = sub((n - nl | 0) - 1 | 0, l$1.tl);
          return [
                  create(match$3[0], l$1.hd, match$4[0]),
                  match$4[1]
                ];
        }
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "set.ml",
                510,
                18
              ],
              Error: new Error()
            };
      };
      return sub(List.length(l$1), l$1)[0];
    } else {
      return add(match$3.hd, add(x3, add(x2, add(x1, singleton(x0)))));
    }
  } else {
    return add(x3, add(x2, add(x1, singleton(x0))));
  }
}

var $$Set = {
  empty: "Empty",
  is_empty: is_empty,
  mem: mem,
  add: add,
  singleton: singleton,
  remove: remove,
  union: union,
  inter: inter,
  diff: diff,
  compare: compare,
  equal: equal,
  subset: subset,
  iter: iter,
  map: map,
  fold: fold,
  for_all: for_all,
  exists: exists,
  filter: filter,
  partition: partition,
  cardinal: cardinal,
  elements: elements,
  min_elt: min_elt,
  min_elt_opt: min_elt_opt,
  max_elt: max_elt,
  max_elt_opt: max_elt_opt,
  choose: min_elt,
  choose_opt: min_elt_opt,
  split: split,
  find: find,
  find_opt: find_opt,
  find_first: find_first,
  find_first_opt: find_first_opt,
  find_last: find_last,
  find_last_opt: find_last_opt,
  of_list: of_list
};

var M = {
  x: 1,
  $$Set: $$Set
};

var x = 1;

exports.M = M;
exports.x = x;
exports.$$Set = $$Set;
/* No side effect */
