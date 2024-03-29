// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");

function Make(Ord) {
  var height = function (x) {
    if (typeof x !== "object") {
      return 0;
    } else {
      return x._3;
    }
  };
  var create = function (l, v, r) {
    var hl;
    hl = typeof l !== "object" ? 0 : l._3;
    var hr;
    hr = typeof r !== "object" ? 0 : r._3;
    return {
            TAG: "Node",
            _0: l,
            _1: v,
            _2: r,
            _3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  };
  var bal = function (l, v, r) {
    var hl;
    hl = typeof l !== "object" ? 0 : l._3;
    var hr;
    hr = typeof r !== "object" ? 0 : r._3;
    if (hl > (hr + 2 | 0)) {
      if (typeof l !== "object") {
        throw {
              RE_EXN_ID: "Invalid_argument",
              _1: "Set.bal",
              Error: new Error()
            };
      }
      var lr = l._2;
      var lv = l._1;
      var ll = l._0;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, create(lr, v, r));
      }
      if (typeof lr === "object") {
        return create(create(ll, lv, lr._0), lr._1, create(lr._2, v, r));
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
              _0: l,
              _1: v,
              _2: r,
              _3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
            };
    }
    if (typeof r !== "object") {
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Set.bal",
            Error: new Error()
          };
    }
    var rr = r._2;
    var rv = r._1;
    var rl = r._0;
    if (height(rr) >= height(rl)) {
      return create(create(l, v, rl), rv, rr);
    }
    if (typeof rl === "object") {
      return create(create(l, v, rl._0), rl._1, create(rl._2, rv, rr));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Set.bal",
          Error: new Error()
        };
  };
  var add = function (x, x_) {
    if (typeof x_ !== "object") {
      return {
              TAG: "Node",
              _0: "Empty",
              _1: x,
              _2: "Empty",
              _3: 1
            };
    }
    var r = x_._2;
    var v = x_._1;
    var l = x_._0;
    var c = Curry._2(Ord.compare, x, v);
    if (c === 0) {
      return x_;
    } else if (c < 0) {
      return bal(add(x, l), v, r);
    } else {
      return bal(l, v, add(x, r));
    }
  };
  var singleton = function (x) {
    return {
            TAG: "Node",
            _0: "Empty",
            _1: x,
            _2: "Empty",
            _3: 1
          };
  };
  var add_min_element = function (v, x) {
    if (typeof x !== "object") {
      return singleton(v);
    } else {
      return bal(add_min_element(v, x._0), x._1, x._2);
    }
  };
  var add_max_element = function (v, x) {
    if (typeof x !== "object") {
      return singleton(v);
    } else {
      return bal(x._0, x._1, add_max_element(v, x._2));
    }
  };
  var join = function (l, v, r) {
    if (typeof l !== "object") {
      return add_min_element(v, r);
    }
    var lh = l._3;
    if (typeof r !== "object") {
      return add_max_element(v, l);
    }
    var rh = r._3;
    if (lh > (rh + 2 | 0)) {
      return bal(l._0, l._1, join(l._2, v, r));
    } else if (rh > (lh + 2 | 0)) {
      return bal(join(l, v, r._0), r._1, r._2);
    } else {
      return create(l, v, r);
    }
  };
  var min_elt = function (_x) {
    while(true) {
      var x = _x;
      if (typeof x !== "object") {
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
      }
      var l = x._0;
      if (typeof l !== "object") {
        return x._1;
      }
      _x = l;
      continue ;
    };
  };
  var max_elt = function (_x) {
    while(true) {
      var x = _x;
      if (typeof x !== "object") {
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
      }
      var r = x._2;
      if (typeof r !== "object") {
        return x._1;
      }
      _x = r;
      continue ;
    };
  };
  var remove_min_elt = function (x) {
    if (typeof x !== "object") {
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Set.remove_min_elt",
            Error: new Error()
          };
    }
    var l = x._0;
    if (typeof l !== "object") {
      return x._2;
    } else {
      return bal(remove_min_elt(l), x._1, x._2);
    }
  };
  var merge = function (t1, t2) {
    if (typeof t1 !== "object") {
      return t2;
    } else if (typeof t2 !== "object") {
      return t1;
    } else {
      return bal(t1, min_elt(t2), remove_min_elt(t2));
    }
  };
  var concat = function (t1, t2) {
    if (typeof t1 !== "object") {
      return t2;
    } else if (typeof t2 !== "object") {
      return t1;
    } else {
      return join(t1, min_elt(t2), remove_min_elt(t2));
    }
  };
  var split = function (x, x_) {
    if (typeof x_ !== "object") {
      return [
              "Empty",
              false,
              "Empty"
            ];
    }
    var r = x_._2;
    var v = x_._1;
    var l = x_._0;
    var c = Curry._2(Ord.compare, x, v);
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
  };
  var is_empty = function (x) {
    if (typeof x !== "object") {
      return true;
    } else {
      return false;
    }
  };
  var mem = function (x, _x_) {
    while(true) {
      var x_ = _x_;
      if (typeof x_ !== "object") {
        return false;
      }
      var c = Curry._2(Ord.compare, x, x_._1);
      if (c === 0) {
        return true;
      }
      _x_ = c < 0 ? x_._0 : x_._2;
      continue ;
    };
  };
  var remove = function (x, x_) {
    if (typeof x_ !== "object") {
      return "Empty";
    }
    var r = x_._2;
    var v = x_._1;
    var l = x_._0;
    var c = Curry._2(Ord.compare, x, v);
    if (c === 0) {
      return merge(l, r);
    } else if (c < 0) {
      return bal(remove(x, l), v, r);
    } else {
      return bal(l, v, remove(x, r));
    }
  };
  var union = function (s1, s2) {
    if (typeof s1 !== "object") {
      return s2;
    }
    var h1 = s1._3;
    var v1 = s1._1;
    if (typeof s2 !== "object") {
      return s1;
    }
    var h2 = s2._3;
    var v2 = s2._1;
    if (h1 >= h2) {
      if (h2 === 1) {
        return add(v2, s1);
      }
      var match = split(v1, s2);
      return join(union(s1._0, match[0]), v1, union(s1._2, match[2]));
    }
    if (h1 === 1) {
      return add(v1, s2);
    }
    var match$1 = split(v2, s1);
    return join(union(match$1[0], s2._0), v2, union(match$1[2], s2._2));
  };
  var inter = function (s1, s2) {
    if (typeof s1 !== "object") {
      return "Empty";
    }
    if (typeof s2 !== "object") {
      return "Empty";
    }
    var r1 = s1._2;
    var v1 = s1._1;
    var l1 = s1._0;
    var match = split(v1, s2);
    var l2 = match[0];
    if (match[1]) {
      return join(inter(l1, l2), v1, inter(r1, match[2]));
    } else {
      return concat(inter(l1, l2), inter(r1, match[2]));
    }
  };
  var diff = function (s1, s2) {
    if (typeof s1 !== "object") {
      return "Empty";
    }
    if (typeof s2 !== "object") {
      return s1;
    }
    var r1 = s1._2;
    var v1 = s1._1;
    var l1 = s1._0;
    var match = split(v1, s2);
    var l2 = match[0];
    if (match[1]) {
      return concat(diff(l1, l2), diff(r1, match[2]));
    } else {
      return join(diff(l1, l2), v1, diff(r1, match[2]));
    }
  };
  var cons_enum = function (_s, _e) {
    while(true) {
      var e = _e;
      var s = _s;
      if (typeof s !== "object") {
        return e;
      }
      _e = {
        TAG: "More",
        _0: s._1,
        _1: s._2,
        _2: e
      };
      _s = s._0;
      continue ;
    };
  };
  var compare_aux = function (_e1, _e2) {
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
      var c = Curry._2(Ord.compare, e1._0, e2._0);
      if (c !== 0) {
        return c;
      }
      _e2 = cons_enum(e2._1, e2._2);
      _e1 = cons_enum(e1._1, e1._2);
      continue ;
    };
  };
  var compare = function (s1, s2) {
    return compare_aux(cons_enum(s1, "End"), cons_enum(s2, "End"));
  };
  var equal = function (s1, s2) {
    return compare(s1, s2) === 0;
  };
  var subset = function (_s1, _s2) {
    while(true) {
      var s2 = _s2;
      var s1 = _s1;
      if (typeof s1 !== "object") {
        return true;
      }
      var r1 = s1._2;
      var v1 = s1._1;
      var l1 = s1._0;
      if (typeof s2 !== "object") {
        return false;
      }
      var r2 = s2._2;
      var l2 = s2._0;
      var c = Curry._2(Ord.compare, v1, s2._1);
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
                _0: l1,
                _1: v1,
                _2: "Empty",
                _3: 0
              }, l2)) {
          return false;
        }
        _s1 = r1;
        continue ;
      }
      if (!subset({
              TAG: "Node",
              _0: "Empty",
              _1: v1,
              _2: r1,
              _3: 0
            }, r2)) {
        return false;
      }
      _s1 = l1;
      continue ;
    };
  };
  var iter = function (f, _x_) {
    while(true) {
      var x_ = _x_;
      if (typeof x_ !== "object") {
        return ;
      }
      iter(f, x_._0);
      Curry._1(f, x_._1);
      _x_ = x_._2;
      continue ;
    };
  };
  var fold = function (f, _s, _accu) {
    while(true) {
      var accu = _accu;
      var s = _s;
      if (typeof s !== "object") {
        return accu;
      }
      _accu = Curry._2(f, s._1, fold(f, s._0, accu));
      _s = s._2;
      continue ;
    };
  };
  var for_all = function (p, _x) {
    while(true) {
      var x = _x;
      if (typeof x !== "object") {
        return true;
      }
      if (!Curry._1(p, x._1)) {
        return false;
      }
      if (!for_all(p, x._0)) {
        return false;
      }
      _x = x._2;
      continue ;
    };
  };
  var exists = function (p, _x) {
    while(true) {
      var x = _x;
      if (typeof x !== "object") {
        return false;
      }
      if (Curry._1(p, x._1)) {
        return true;
      }
      if (exists(p, x._0)) {
        return true;
      }
      _x = x._2;
      continue ;
    };
  };
  var filter = function (p, x) {
    if (typeof x !== "object") {
      return "Empty";
    }
    var v = x._1;
    var l$p = filter(p, x._0);
    var pv = Curry._1(p, v);
    var r$p = filter(p, x._2);
    if (pv) {
      return join(l$p, v, r$p);
    } else {
      return concat(l$p, r$p);
    }
  };
  var partition = function (p, x) {
    if (typeof x !== "object") {
      return [
              "Empty",
              "Empty"
            ];
    }
    var v = x._1;
    var match = partition(p, x._0);
    var lf = match[1];
    var lt = match[0];
    var pv = Curry._1(p, v);
    var match$1 = partition(p, x._2);
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
  };
  var cardinal = function (x) {
    if (typeof x !== "object") {
      return 0;
    } else {
      return (cardinal(x._0) + 1 | 0) + cardinal(x._2) | 0;
    }
  };
  var elements_aux = function (_accu, _x) {
    while(true) {
      var x = _x;
      var accu = _accu;
      if (typeof x !== "object") {
        return accu;
      }
      _x = x._0;
      _accu = {
        hd: x._1,
        tl: elements_aux(accu, x._2)
      };
      continue ;
    };
  };
  var elements = function (s) {
    return elements_aux(/* [] */0, s);
  };
  var find = function (x, _x_) {
    while(true) {
      var x_ = _x_;
      if (typeof x_ !== "object") {
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
      }
      var v = x_._1;
      var c = Curry._2(Ord.compare, x, v);
      if (c === 0) {
        return v;
      }
      _x_ = c < 0 ? x_._0 : x_._2;
      continue ;
    };
  };
  var of_sorted_list = function (l) {
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
                        _0: "Empty",
                        _1: l.hd,
                        _2: "Empty",
                        _3: 1
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
                          _0: {
                            TAG: "Node",
                            _0: "Empty",
                            _1: l.hd,
                            _2: "Empty",
                            _3: 1
                          },
                          _1: match.hd,
                          _2: "Empty",
                          _3: 2
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
                            _0: {
                              TAG: "Node",
                              _0: "Empty",
                              _1: l.hd,
                              _2: "Empty",
                              _3: 1
                            },
                            _1: match$1.hd,
                            _2: {
                              TAG: "Node",
                              _0: "Empty",
                              _1: match$2.hd,
                              _2: "Empty",
                              _3: 1
                            },
                            _3: 2
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
              "test_set.res",
              497,
              20
            ],
            Error: new Error()
          };
    };
    return sub(List.length(l), l)[0];
  };
  var of_list = function (l) {
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
        return of_sorted_list(List.sort_uniq(Ord.compare, l));
      } else {
        return add(match$3.hd, add(x3, add(x2, add(x1, singleton(x0)))));
      }
    } else {
      return add(x3, add(x2, add(x1, singleton(x0))));
    }
  };
  return {
          height: height,
          create: create,
          bal: bal,
          add: add,
          singleton: singleton,
          add_min_element: add_min_element,
          add_max_element: add_max_element,
          join: join,
          min_elt: min_elt,
          max_elt: max_elt,
          remove_min_elt: remove_min_elt,
          merge: merge,
          concat: concat,
          split: split,
          empty: "Empty",
          is_empty: is_empty,
          mem: mem,
          remove: remove,
          union: union,
          inter: inter,
          diff: diff,
          cons_enum: cons_enum,
          compare_aux: compare_aux,
          compare: compare,
          equal: equal,
          subset: subset,
          iter: iter,
          fold: fold,
          for_all: for_all,
          exists: exists,
          filter: filter,
          partition: partition,
          cardinal: cardinal,
          elements_aux: elements_aux,
          elements: elements,
          choose: min_elt,
          find: find,
          of_sorted_list: of_sorted_list,
          of_list: of_list
        };
}

var N = {
  a: 3
};

exports.Make = Make;
exports.N = N;
/* No side effect */
