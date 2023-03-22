'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

function cons_enum(_s, _e) {
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
}

function height(param) {
  if (typeof param !== "object") {
    return 0;
  } else {
    return param._3;
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
    var l = param._0;
    if (typeof l !== "object") {
      return param._1;
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
    var r = param._2;
    if (typeof r !== "object") {
      return param._1;
    }
    _param = r;
    continue ;
  };
}

function is_empty(param) {
  if (typeof param !== "object") {
    return true;
  } else {
    return false;
  }
}

function cardinal_aux(_acc, _param) {
  while(true) {
    var param = _param;
    var acc = _acc;
    if (typeof param !== "object") {
      return acc;
    }
    _param = param._0;
    _acc = cardinal_aux(acc + 1 | 0, param._2);
    continue ;
  };
}

function cardinal(s) {
  return cardinal_aux(0, s);
}

function elements_aux(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (typeof param !== "object") {
      return accu;
    }
    _param = param._0;
    _accu = {
      hd: param._1,
      tl: elements_aux(accu, param._2)
    };
    continue ;
  };
}

function elements(s) {
  return elements_aux(/* [] */0, s);
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      return ;
    }
    iter(f, param._0);
    Curry._1(f, param._1);
    _param = param._2;
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
    _accu = Curry._2(f, s._1, fold(f, s._0, accu));
    _s = s._2;
    continue ;
  };
}

function for_all(p, _param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      return true;
    }
    if (!Curry._1(p, param._1)) {
      return false;
    }
    if (!for_all(p, param._0)) {
      return false;
    }
    _param = param._2;
    continue ;
  };
}

function exists(p, _param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      return false;
    }
    if (Curry._1(p, param._1)) {
      return true;
    }
    if (exists(p, param._0)) {
      return true;
    }
    _param = param._2;
    continue ;
  };
}

function max_int3(a, b, c) {
  if (a >= b) {
    if (a >= c) {
      return a;
    } else {
      return c;
    }
  } else if (b >= c) {
    return b;
  } else {
    return c;
  }
}

function max_int_2(a, b) {
  if (a >= b) {
    return a;
  } else {
    return b;
  }
}

var Height_invariant_broken = /* @__PURE__ */Caml_exceptions.create("Set_gen.Height_invariant_broken");

var Height_diff_borken = /* @__PURE__ */Caml_exceptions.create("Set_gen.Height_diff_borken");

function check_height_and_diff(param) {
  if (typeof param !== "object") {
    return 0;
  }
  var h = param._3;
  var hl = check_height_and_diff(param._0);
  var hr = check_height_and_diff(param._2);
  if (h !== (max_int_2(hl, hr) + 1 | 0)) {
    throw {
          RE_EXN_ID: Height_invariant_broken,
          Error: new Error()
        };
  }
  var diff = Pervasives.abs(hl - hr | 0);
  if (diff > 2) {
    throw {
          RE_EXN_ID: Height_diff_borken,
          Error: new Error()
        };
  }
  return h;
}

function check(tree) {
  check_height_and_diff(tree);
}

function create(l, v, r) {
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
}

function internal_bal(l, v, r) {
  var hl;
  hl = typeof l !== "object" ? 0 : l._3;
  var hr;
  hr = typeof r !== "object" ? 0 : r._3;
  if (hl > (hr + 2 | 0)) {
    if (typeof l !== "object") {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "set_gen.ml",
              225,
              15
            ],
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
          RE_EXN_ID: "Assert_failure",
          _1: [
            "set_gen.ml",
            235,
            19
          ],
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
          RE_EXN_ID: "Assert_failure",
          _1: [
            "set_gen.ml",
            245,
            15
          ],
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
        RE_EXN_ID: "Assert_failure",
        _1: [
          "set_gen.ml",
          251,
          19
        ],
        Error: new Error()
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
  var l = param._0;
  if (typeof l !== "object") {
    return param._2;
  } else {
    return internal_bal(remove_min_elt(l), param._1, param._2);
  }
}

function singleton(x) {
  return {
          TAG: "Node",
          _0: "Empty",
          _1: x,
          _2: "Empty",
          _3: 1
        };
}

function internal_merge(l, r) {
  if (typeof l !== "object") {
    return r;
  } else if (typeof r !== "object") {
    return l;
  } else {
    return internal_bal(l, min_elt(r), remove_min_elt(r));
  }
}

function add_min_element(v, param) {
  if (typeof param !== "object") {
    return singleton(v);
  } else {
    return internal_bal(add_min_element(v, param._0), param._1, param._2);
  }
}

function add_max_element(v, param) {
  if (typeof param !== "object") {
    return singleton(v);
  } else {
    return internal_bal(param._0, param._1, add_max_element(v, param._2));
  }
}

function internal_join(l, v, r) {
  if (typeof l !== "object") {
    return add_min_element(v, r);
  }
  var lh = l._3;
  if (typeof r !== "object") {
    return add_max_element(v, l);
  }
  var rh = r._3;
  if (lh > (rh + 2 | 0)) {
    return internal_bal(l._0, l._1, internal_join(l._2, v, r));
  } else if (rh > (lh + 2 | 0)) {
    return internal_bal(internal_join(l, v, r._0), r._1, r._2);
  } else {
    return create(l, v, r);
  }
}

function internal_concat(t1, t2) {
  if (typeof t1 !== "object") {
    return t2;
  } else if (typeof t2 !== "object") {
    return t1;
  } else {
    return internal_join(t1, min_elt(t2), remove_min_elt(t2));
  }
}

function filter(p, param) {
  if (typeof param !== "object") {
    return "Empty";
  }
  var v = param._1;
  var l$p = filter(p, param._0);
  var pv = Curry._1(p, v);
  var r$p = filter(p, param._2);
  if (pv) {
    return internal_join(l$p, v, r$p);
  } else {
    return internal_concat(l$p, r$p);
  }
}

function partition(p, param) {
  if (typeof param !== "object") {
    return [
            "Empty",
            "Empty"
          ];
  }
  var v = param._1;
  var match = partition(p, param._0);
  var lf = match[1];
  var lt = match[0];
  var pv = Curry._1(p, v);
  var match$1 = partition(p, param._2);
  var rf = match$1[1];
  var rt = match$1[0];
  if (pv) {
    return [
            internal_join(lt, v, rt),
            internal_concat(lf, rf)
          ];
  } else {
    return [
            internal_concat(lt, rt),
            internal_join(lf, v, rf)
          ];
  }
}

function of_sorted_list(l) {
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
            "set_gen.ml",
            361,
            14
          ],
          Error: new Error()
        };
  };
  return sub(List.length(l), l)[0];
}

function of_sorted_array(l) {
  var sub = function (start, n, l) {
    if (n === 0) {
      return "Empty";
    }
    if (n === 1) {
      var x0 = l[start];
      return {
              TAG: "Node",
              _0: "Empty",
              _1: x0,
              _2: "Empty",
              _3: 1
            };
    }
    if (n === 2) {
      var x0$1 = l[start];
      var x1 = l[start + 1 | 0];
      return {
              TAG: "Node",
              _0: {
                TAG: "Node",
                _0: "Empty",
                _1: x0$1,
                _2: "Empty",
                _3: 1
              },
              _1: x1,
              _2: "Empty",
              _3: 2
            };
    }
    if (n === 3) {
      var x0$2 = l[start];
      var x1$1 = l[start + 1 | 0];
      var x2 = l[start + 2 | 0];
      return {
              TAG: "Node",
              _0: {
                TAG: "Node",
                _0: "Empty",
                _1: x0$2,
                _2: "Empty",
                _3: 1
              },
              _1: x1$1,
              _2: {
                TAG: "Node",
                _0: "Empty",
                _1: x2,
                _2: "Empty",
                _3: 1
              },
              _3: 2
            };
    }
    var nl = n / 2 | 0;
    var left = sub(start, nl, l);
    var mid = start + nl | 0;
    var v = l[mid];
    var right = sub(mid + 1 | 0, (n - nl | 0) - 1 | 0, l);
    return create(left, v, right);
  };
  return sub(0, l.length, l);
}

function is_ordered(cmp, tree) {
  var is_ordered_min_max = function (tree) {
    if (typeof tree !== "object") {
      return "Empty";
    }
    var r = tree._2;
    var v = tree._1;
    var match = is_ordered_min_max(tree._0);
    if (typeof match === "object") {
      var match$1 = match.VAL;
      var max_v = match$1[1];
      var min_v = match$1[0];
      var match$2 = is_ordered_min_max(r);
      if (typeof match$2 !== "object") {
        if (match$2 === "Empty" && Curry._2(cmp, max_v, v) < 0) {
          return {
                  NAME: "V",
                  VAL: [
                    min_v,
                    v
                  ]
                };
        } else {
          return "No";
        }
      }
      var match$3 = match$2.VAL;
      if (Curry._2(cmp, max_v, match$3[0]) < 0) {
        return {
                NAME: "V",
                VAL: [
                  min_v,
                  match$3[1]
                ]
              };
      } else {
        return "No";
      }
    }
    if (match !== "Empty") {
      return "No";
    }
    var match$4 = is_ordered_min_max(r);
    if (typeof match$4 !== "object") {
      if (match$4 === "Empty") {
        return {
                NAME: "V",
                VAL: [
                  v,
                  v
                ]
              };
      } else {
        return "No";
      }
    }
    var match$5 = match$4.VAL;
    if (Curry._2(cmp, v, match$5[0]) < 0) {
      return {
              NAME: "V",
              VAL: [
                v,
                match$5[1]
              ]
            };
    } else {
      return "No";
    }
  };
  return is_ordered_min_max(tree) !== "No";
}

function invariant(cmp, t) {
  check_height_and_diff(t);
  return is_ordered(cmp, t);
}

function compare_aux(cmp, _e1, _e2) {
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
    var c = Curry._2(cmp, e1._0, e2._0);
    if (c !== 0) {
      return c;
    }
    _e2 = cons_enum(e2._1, e2._2);
    _e1 = cons_enum(e1._1, e1._2);
    continue ;
  };
}

function compare(cmp, s1, s2) {
  return compare_aux(cmp, cons_enum(s1, "End"), cons_enum(s2, "End"));
}

var empty = "Empty";

var choose = min_elt;

exports.cons_enum = cons_enum;
exports.height = height;
exports.min_elt = min_elt;
exports.max_elt = max_elt;
exports.empty = empty;
exports.is_empty = is_empty;
exports.cardinal_aux = cardinal_aux;
exports.cardinal = cardinal;
exports.elements_aux = elements_aux;
exports.elements = elements;
exports.choose = choose;
exports.iter = iter;
exports.fold = fold;
exports.for_all = for_all;
exports.exists = exists;
exports.max_int3 = max_int3;
exports.max_int_2 = max_int_2;
exports.Height_invariant_broken = Height_invariant_broken;
exports.Height_diff_borken = Height_diff_borken;
exports.check_height_and_diff = check_height_and_diff;
exports.check = check;
exports.create = create;
exports.internal_bal = internal_bal;
exports.remove_min_elt = remove_min_elt;
exports.singleton = singleton;
exports.internal_merge = internal_merge;
exports.add_min_element = add_min_element;
exports.add_max_element = add_max_element;
exports.internal_join = internal_join;
exports.internal_concat = internal_concat;
exports.filter = filter;
exports.partition = partition;
exports.of_sorted_list = of_sorted_list;
exports.of_sorted_array = of_sorted_array;
exports.is_ordered = is_ordered;
exports.invariant = invariant;
exports.compare_aux = compare_aux;
exports.compare = compare;
/* No side effect */
