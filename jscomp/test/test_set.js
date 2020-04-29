'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");

function Make(Ord) {
  var height = function (param) {
    if (param) {
      return param[3];
    } else {
      return 0;
    }
  };
  var create = function (l, v, r) {
    var hl = l ? l[3] : 0;
    var hr = r ? r[3] : 0;
    return /* Node */[
            l,
            v,
            r,
            hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  };
  var bal = function (l, v, r) {
    var hl = l ? l[3] : 0;
    var hr = r ? r[3] : 0;
    if (hl > (hr + 2 | 0)) {
      if (l) {
        var lr = l[2];
        var lv = l[1];
        var ll = l[0];
        if (height(ll) >= height(lr)) {
          return create(ll, lv, create(lr, v, r));
        }
        if (lr) {
          return create(create(ll, lv, lr[0]), lr[1], create(lr[2], v, r));
        }
        throw {
              ExceptionID: -3,
              _1: "Set.bal",
              Debug: "Invalid_argument"
            };
      }
      throw {
            ExceptionID: -3,
            _1: "Set.bal",
            Debug: "Invalid_argument"
          };
    }
    if (hr <= (hl + 2 | 0)) {
      return /* Node */[
              l,
              v,
              r,
              hl >= hr ? hl + 1 | 0 : hr + 1 | 0
            ];
    }
    if (r) {
      var rr = r[2];
      var rv = r[1];
      var rl = r[0];
      if (height(rr) >= height(rl)) {
        return create(create(l, v, rl), rv, rr);
      }
      if (rl) {
        return create(create(l, v, rl[0]), rl[1], create(rl[2], rv, rr));
      }
      throw {
            ExceptionID: -3,
            _1: "Set.bal",
            Debug: "Invalid_argument"
          };
    }
    throw {
          ExceptionID: -3,
          _1: "Set.bal",
          Debug: "Invalid_argument"
        };
  };
  var add = function (x, t) {
    if (!t) {
      return /* Node */[
              /* Empty */0,
              x,
              /* Empty */0,
              1
            ];
    }
    var r = t[2];
    var v = t[1];
    var l = t[0];
    var c = Curry._2(Ord.compare, x, v);
    if (c === 0) {
      return t;
    } else if (c < 0) {
      return bal(add(x, l), v, r);
    } else {
      return bal(l, v, add(x, r));
    }
  };
  var singleton = function (x) {
    return /* Node */[
            /* Empty */0,
            x,
            /* Empty */0,
            1
          ];
  };
  var add_min_element = function (v, param) {
    if (param) {
      return bal(add_min_element(v, param[0]), param[1], param[2]);
    } else {
      return singleton(v);
    }
  };
  var add_max_element = function (v, param) {
    if (param) {
      return bal(param[0], param[1], add_max_element(v, param[2]));
    } else {
      return singleton(v);
    }
  };
  var join = function (l, v, r) {
    if (!l) {
      return add_min_element(v, r);
    }
    if (!r) {
      return add_max_element(v, l);
    }
    var rh = r[3];
    var lh = l[3];
    if (lh > (rh + 2 | 0)) {
      return bal(l[0], l[1], join(l[2], v, r));
    } else if (rh > (lh + 2 | 0)) {
      return bal(join(l, v, r[0]), r[1], r[2]);
    } else {
      return create(l, v, r);
    }
  };
  var min_elt = function (_param) {
    while(true) {
      var param = _param;
      if (param) {
        var l = param[0];
        if (!l) {
          return param[1];
        }
        _param = l;
        continue ;
      }
      throw {
            ExceptionID: -6,
            Debug: "Not_found"
          };
    };
  };
  var max_elt = function (_param) {
    while(true) {
      var param = _param;
      if (param) {
        var r = param[2];
        if (!r) {
          return param[1];
        }
        _param = r;
        continue ;
      }
      throw {
            ExceptionID: -6,
            Debug: "Not_found"
          };
    };
  };
  var remove_min_elt = function (param) {
    if (param) {
      var l = param[0];
      if (l) {
        return bal(remove_min_elt(l), param[1], param[2]);
      } else {
        return param[2];
      }
    }
    throw {
          ExceptionID: -3,
          _1: "Set.remove_min_elt",
          Debug: "Invalid_argument"
        };
  };
  var merge = function (t1, t2) {
    if (t1) {
      if (t2) {
        return bal(t1, min_elt(t2), remove_min_elt(t2));
      } else {
        return t1;
      }
    } else {
      return t2;
    }
  };
  var concat = function (t1, t2) {
    if (t1) {
      if (t2) {
        return join(t1, min_elt(t2), remove_min_elt(t2));
      } else {
        return t1;
      }
    } else {
      return t2;
    }
  };
  var split = function (x, param) {
    if (!param) {
      return /* tuple */[
              /* Empty */0,
              false,
              /* Empty */0
            ];
    }
    var r = param[2];
    var v = param[1];
    var l = param[0];
    var c = Curry._2(Ord.compare, x, v);
    if (c === 0) {
      return /* tuple */[
              l,
              true,
              r
            ];
    }
    if (c < 0) {
      var match = split(x, l);
      return /* tuple */[
              match[0],
              match[1],
              join(match[2], v, r)
            ];
    }
    var match$1 = split(x, r);
    return /* tuple */[
            join(l, v, match$1[0]),
            match$1[1],
            match$1[2]
          ];
  };
  var is_empty = function (param) {
    if (param) {
      return false;
    } else {
      return true;
    }
  };
  var mem = function (x, _param) {
    while(true) {
      var param = _param;
      if (!param) {
        return false;
      }
      var c = Curry._2(Ord.compare, x, param[1]);
      if (c === 0) {
        return true;
      }
      _param = c < 0 ? param[0] : param[2];
      continue ;
    };
  };
  var remove = function (x, param) {
    if (!param) {
      return /* Empty */0;
    }
    var r = param[2];
    var v = param[1];
    var l = param[0];
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
    if (!s1) {
      return s2;
    }
    if (!s2) {
      return s1;
    }
    var h2 = s2[3];
    var v2 = s2[1];
    var h1 = s1[3];
    var v1 = s1[1];
    if (h1 >= h2) {
      if (h2 === 1) {
        return add(v2, s1);
      }
      var match = split(v1, s2);
      return join(union(s1[0], match[0]), v1, union(s1[2], match[2]));
    }
    if (h1 === 1) {
      return add(v1, s2);
    }
    var match$1 = split(v2, s1);
    return join(union(match$1[0], s2[0]), v2, union(match$1[2], s2[2]));
  };
  var inter = function (s1, s2) {
    if (!s1) {
      return /* Empty */0;
    }
    if (!s2) {
      return /* Empty */0;
    }
    var r1 = s1[2];
    var v1 = s1[1];
    var l1 = s1[0];
    var match = split(v1, s2);
    var l2 = match[0];
    if (match[1]) {
      return join(inter(l1, l2), v1, inter(r1, match[2]));
    } else {
      return concat(inter(l1, l2), inter(r1, match[2]));
    }
  };
  var diff = function (s1, s2) {
    if (!s1) {
      return /* Empty */0;
    }
    if (!s2) {
      return s1;
    }
    var r1 = s1[2];
    var v1 = s1[1];
    var l1 = s1[0];
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
      if (!s) {
        return e;
      }
      _e = /* More */[
        s[1],
        s[2],
        e
      ];
      _s = s[0];
      continue ;
    };
  };
  var compare_aux = function (_e1, _e2) {
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (!e1) {
        if (e2) {
          return -1;
        } else {
          return 0;
        }
      }
      if (!e2) {
        return 1;
      }
      var c = Curry._2(Ord.compare, e1[0], e2[0]);
      if (c !== 0) {
        return c;
      }
      _e2 = cons_enum(e2[1], e2[2]);
      _e1 = cons_enum(e1[1], e1[2]);
      continue ;
    };
  };
  var compare = function (s1, s2) {
    return compare_aux(cons_enum(s1, /* End */0), cons_enum(s2, /* End */0));
  };
  var equal = function (s1, s2) {
    return compare(s1, s2) === 0;
  };
  var subset = function (_s1, _s2) {
    while(true) {
      var s2 = _s2;
      var s1 = _s1;
      if (!s1) {
        return true;
      }
      if (!s2) {
        return false;
      }
      var r2 = s2[2];
      var l2 = s2[0];
      var r1 = s1[2];
      var v1 = s1[1];
      var l1 = s1[0];
      var c = Curry._2(Ord.compare, v1, s2[1]);
      if (c === 0) {
        if (!subset(l1, l2)) {
          return false;
        }
        _s2 = r2;
        _s1 = r1;
        continue ;
      }
      if (c < 0) {
        if (!subset(/* Node */[
                l1,
                v1,
                /* Empty */0,
                0
              ], l2)) {
          return false;
        }
        _s1 = r1;
        continue ;
      }
      if (!subset(/* Node */[
              /* Empty */0,
              v1,
              r1,
              0
            ], r2)) {
        return false;
      }
      _s1 = l1;
      continue ;
    };
  };
  var iter = function (f, _param) {
    while(true) {
      var param = _param;
      if (!param) {
        return ;
      }
      iter(f, param[0]);
      Curry._1(f, param[1]);
      _param = param[2];
      continue ;
    };
  };
  var fold = function (f, _s, _accu) {
    while(true) {
      var accu = _accu;
      var s = _s;
      if (!s) {
        return accu;
      }
      _accu = Curry._2(f, s[1], fold(f, s[0], accu));
      _s = s[2];
      continue ;
    };
  };
  var for_all = function (p, _param) {
    while(true) {
      var param = _param;
      if (!param) {
        return true;
      }
      if (!Curry._1(p, param[1])) {
        return false;
      }
      if (!for_all(p, param[0])) {
        return false;
      }
      _param = param[2];
      continue ;
    };
  };
  var exists = function (p, _param) {
    while(true) {
      var param = _param;
      if (!param) {
        return false;
      }
      if (Curry._1(p, param[1])) {
        return true;
      }
      if (exists(p, param[0])) {
        return true;
      }
      _param = param[2];
      continue ;
    };
  };
  var filter = function (p, param) {
    if (!param) {
      return /* Empty */0;
    }
    var v = param[1];
    var l$prime = filter(p, param[0]);
    var pv = Curry._1(p, v);
    var r$prime = filter(p, param[2]);
    if (pv) {
      return join(l$prime, v, r$prime);
    } else {
      return concat(l$prime, r$prime);
    }
  };
  var partition = function (p, param) {
    if (!param) {
      return /* tuple */[
              /* Empty */0,
              /* Empty */0
            ];
    }
    var v = param[1];
    var match = partition(p, param[0]);
    var lf = match[1];
    var lt = match[0];
    var pv = Curry._1(p, v);
    var match$1 = partition(p, param[2]);
    var rf = match$1[1];
    var rt = match$1[0];
    if (pv) {
      return /* tuple */[
              join(lt, v, rt),
              concat(lf, rf)
            ];
    } else {
      return /* tuple */[
              concat(lt, rt),
              join(lf, v, rf)
            ];
    }
  };
  var cardinal = function (param) {
    if (param) {
      return (cardinal(param[0]) + 1 | 0) + cardinal(param[2]) | 0;
    } else {
      return 0;
    }
  };
  var elements_aux = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (!param) {
        return accu;
      }
      _param = param[0];
      _accu = /* :: */[
        param[1],
        elements_aux(accu, param[2])
      ];
      continue ;
    };
  };
  var elements = function (s) {
    return elements_aux(/* [] */0, s);
  };
  var find = function (x, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var v = param[1];
        var c = Curry._2(Ord.compare, x, v);
        if (c === 0) {
          return v;
        }
        _param = c < 0 ? param[0] : param[2];
        continue ;
      }
      throw {
            ExceptionID: -6,
            Debug: "Not_found"
          };
    };
  };
  var of_sorted_list = function (l) {
    var sub = function (n, l) {
      switch (n) {
        case 0 :
            return /* tuple */[
                    /* Empty */0,
                    l
                  ];
        case 1 :
            if (l) {
              return /* tuple */[
                      /* Node */[
                        /* Empty */0,
                        l[0],
                        /* Empty */0,
                        1
                      ],
                      l[1]
                    ];
            }
            break;
        case 2 :
            if (l) {
              var match = l[1];
              if (match) {
                return /* tuple */[
                        /* Node */[
                          /* Node */[
                            /* Empty */0,
                            l[0],
                            /* Empty */0,
                            1
                          ],
                          match[0],
                          /* Empty */0,
                          2
                        ],
                        match[1]
                      ];
              }
              
            }
            break;
        case 3 :
            if (l) {
              var match$1 = l[1];
              if (match$1) {
                var match$2 = match$1[1];
                if (match$2) {
                  return /* tuple */[
                          /* Node */[
                            /* Node */[
                              /* Empty */0,
                              l[0],
                              /* Empty */0,
                              1
                            ],
                            match$1[0],
                            /* Node */[
                              /* Empty */0,
                              match$2[0],
                              /* Empty */0,
                              1
                            ],
                            2
                          ],
                          match$2[1]
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
        var match$4 = sub((n - nl | 0) - 1 | 0, l$1[1]);
        return /* tuple */[
                create(match$3[0], l$1[0], match$4[0]),
                match$4[1]
              ];
      }
      throw {
            ExceptionID: -9,
            _1: /* tuple */[
              "test_set.ml",
              372,
              18
            ],
            Debug: "Assert_failure"
          };
    };
    return sub(List.length(l), l)[0];
  };
  var of_list = function (l) {
    if (!l) {
      return /* Empty */0;
    }
    var match = l[1];
    var x0 = l[0];
    if (!match) {
      return singleton(x0);
    }
    var match$1 = match[1];
    var x1 = match[0];
    if (!match$1) {
      return add(x1, singleton(x0));
    }
    var match$2 = match$1[1];
    var x2 = match$1[0];
    if (!match$2) {
      return add(x2, add(x1, singleton(x0)));
    }
    var match$3 = match$2[1];
    var x3 = match$2[0];
    if (match$3) {
      if (match$3[1]) {
        return of_sorted_list(List.sort_uniq(Ord.compare, l));
      } else {
        return add(match$3[0], add(x3, add(x2, add(x1, singleton(x0)))));
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
          empty: /* Empty */0,
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
