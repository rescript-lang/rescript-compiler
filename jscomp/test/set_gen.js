'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function cons_enum(_s, _e) {
  while(true) {
    var e = _e;
    var s = _s;
    if (s !== "Empty") {
      _e = /* constructor */{
        tag: "More",
        Arg0: s.Arg1,
        Arg1: s.Arg2,
        Arg2: e
      };
      _s = s.Arg0;
      continue ;
    } else {
      return e;
    }
  };
}

function height(param) {
  if (param !== "Empty") {
    return param.Arg3;
  } else {
    return 0;
  }
}

function min_elt(_param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var l = param.Arg0;
      if (l !== "Empty") {
        _param = l;
        continue ;
      } else {
        return param.Arg1;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function max_elt(_param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var r = param.Arg2;
      if (r !== "Empty") {
        _param = r;
        continue ;
      } else {
        return param.Arg1;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function is_empty(param) {
  return param === "Empty";
}

function cardinal_aux(_acc, _param) {
  while(true) {
    var param = _param;
    var acc = _acc;
    if (param !== "Empty") {
      _param = param.Arg0;
      _acc = cardinal_aux(acc + 1 | 0, param.Arg2);
      continue ;
    } else {
      return acc;
    }
  };
}

function cardinal(s) {
  return cardinal_aux(0, s);
}

function elements_aux(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param !== "Empty") {
      _param = param.Arg0;
      _accu = /* constructor */{
        tag: "::",
        Arg0: param.Arg1,
        Arg1: elements_aux(accu, param.Arg2)
      };
      continue ;
    } else {
      return accu;
    }
  };
}

function elements(s) {
  return elements_aux("[]", s);
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      iter(f, param.Arg0);
      Curry._1(f, param.Arg1);
      _param = param.Arg2;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function fold(f, _s, _accu) {
  while(true) {
    var accu = _accu;
    var s = _s;
    if (s !== "Empty") {
      _accu = Curry._2(f, s.Arg1, fold(f, s.Arg0, accu));
      _s = s.Arg2;
      continue ;
    } else {
      return accu;
    }
  };
}

function for_all(p, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      if (Curry._1(p, param.Arg1) && for_all(p, param.Arg0)) {
        _param = param.Arg2;
        continue ;
      } else {
        return false;
      }
    } else {
      return true;
    }
  };
}

function exists(p, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      if (Curry._1(p, param.Arg1) || exists(p, param.Arg0)) {
        return true;
      } else {
        _param = param.Arg2;
        continue ;
      }
    } else {
      return false;
    }
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

var Height_invariant_broken = Caml_exceptions.create("Set_gen.Height_invariant_broken");

var Height_diff_borken = Caml_exceptions.create("Set_gen.Height_diff_borken");

function check_height_and_diff(param) {
  if (param !== "Empty") {
    var h = param.Arg3;
    var hl = check_height_and_diff(param.Arg0);
    var hr = check_height_and_diff(param.Arg2);
    if (h !== (max_int_2(hl, hr) + 1 | 0)) {
      throw Height_invariant_broken;
    }
    var diff = Pervasives.abs(hl - hr | 0);
    if (diff > 2) {
      throw Height_diff_borken;
    }
    return h;
  } else {
    return 0;
  }
}

function check(tree) {
  check_height_and_diff(tree);
  return /* () */0;
}

function create(l, v, r) {
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

function internal_bal(l, v, r) {
  var hl = l !== "Empty" ? l.Arg3 : 0;
  var hr = r !== "Empty" ? r.Arg3 : 0;
  if (hl > (hr + 2 | 0)) {
    if (l !== "Empty") {
      var lr = l.Arg2;
      var lv = l.Arg1;
      var ll = l.Arg0;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, create(lr, v, r));
      } else if (lr !== "Empty") {
        return create(create(ll, lv, lr.Arg0), lr.Arg1, create(lr.Arg2, v, r));
      } else {
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "set_gen.ml",
                235,
                19
              ]
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            /* tuple */[
              "set_gen.ml",
              225,
              15
            ]
          ];
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r !== "Empty") {
      var rr = r.Arg2;
      var rv = r.Arg1;
      var rl = r.Arg0;
      if (height(rr) >= height(rl)) {
        return create(create(l, v, rl), rv, rr);
      } else if (rl !== "Empty") {
        return create(create(l, v, rl.Arg0), rl.Arg1, create(rl.Arg2, rv, rr));
      } else {
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "set_gen.ml",
                251,
                19
              ]
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            /* tuple */[
              "set_gen.ml",
              245,
              15
            ]
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

function remove_min_elt(param) {
  if (param !== "Empty") {
    var l = param.Arg0;
    if (l !== "Empty") {
      return internal_bal(remove_min_elt(l), param.Arg1, param.Arg2);
    } else {
      return param.Arg2;
    }
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Set.remove_min_elt"
        ];
  }
}

function singleton(x) {
  return /* constructor */{
          tag: "Node",
          Arg0: "Empty",
          Arg1: x,
          Arg2: "Empty",
          Arg3: 1
        };
}

function internal_merge(l, r) {
  if (l !== "Empty") {
    if (r !== "Empty") {
      return internal_bal(l, min_elt(r), remove_min_elt(r));
    } else {
      return l;
    }
  } else {
    return r;
  }
}

function add_min_element(v, param) {
  if (param !== "Empty") {
    return internal_bal(add_min_element(v, param.Arg0), param.Arg1, param.Arg2);
  } else {
    return singleton(v);
  }
}

function add_max_element(v, param) {
  if (param !== "Empty") {
    return internal_bal(param.Arg0, param.Arg1, add_max_element(v, param.Arg2));
  } else {
    return singleton(v);
  }
}

function internal_join(l, v, r) {
  if (l !== "Empty") {
    if (r !== "Empty") {
      var rh = r.Arg3;
      var lh = l.Arg3;
      if (lh > (rh + 2 | 0)) {
        return internal_bal(l.Arg0, l.Arg1, internal_join(l.Arg2, v, r));
      } else if (rh > (lh + 2 | 0)) {
        return internal_bal(internal_join(l, v, r.Arg0), r.Arg1, r.Arg2);
      } else {
        return create(l, v, r);
      }
    } else {
      return add_max_element(v, l);
    }
  } else {
    return add_min_element(v, r);
  }
}

function internal_concat(t1, t2) {
  if (t1 !== "Empty") {
    if (t2 !== "Empty") {
      return internal_join(t1, min_elt(t2), remove_min_elt(t2));
    } else {
      return t1;
    }
  } else {
    return t2;
  }
}

function filter(p, param) {
  if (param !== "Empty") {
    var v = param.Arg1;
    var l$prime = filter(p, param.Arg0);
    var pv = Curry._1(p, v);
    var r$prime = filter(p, param.Arg2);
    if (pv) {
      return internal_join(l$prime, v, r$prime);
    } else {
      return internal_concat(l$prime, r$prime);
    }
  } else {
    return "Empty";
  }
}

function partition(p, param) {
  if (param !== "Empty") {
    var v = param.Arg1;
    var match = partition(p, param.Arg0);
    var lf = match[1];
    var lt = match[0];
    var pv = Curry._1(p, v);
    var match$1 = partition(p, param.Arg2);
    var rf = match$1[1];
    var rt = match$1[0];
    if (pv) {
      return /* tuple */[
              internal_join(lt, v, rt),
              internal_concat(lf, rf)
            ];
    } else {
      return /* tuple */[
              internal_concat(lt, rt),
              internal_join(lf, v, rf)
            ];
    }
  } else {
    return /* tuple */[
            "Empty",
            "Empty"
          ];
  }
}

function of_sorted_list(l) {
  var sub = function (n, l) {
    switch (n) {
      case 0 :
          return /* tuple */[
                  "Empty",
                  l
                ];
      case 1 :
          if (l !== "[]") {
            return /* tuple */[
                    /* constructor */{
                      tag: "Node",
                      Arg0: "Empty",
                      Arg1: l.Arg0,
                      Arg2: "Empty",
                      Arg3: 1
                    },
                    l.Arg1
                  ];
          }
          break;
      case 2 :
          if (l !== "[]") {
            var match = l.Arg1;
            if (match !== "[]") {
              return /* tuple */[
                      /* constructor */{
                        tag: "Node",
                        Arg0: /* constructor */{
                          tag: "Node",
                          Arg0: "Empty",
                          Arg1: l.Arg0,
                          Arg2: "Empty",
                          Arg3: 1
                        },
                        Arg1: match.Arg0,
                        Arg2: "Empty",
                        Arg3: 2
                      },
                      match.Arg1
                    ];
            }
            
          }
          break;
      case 3 :
          if (l !== "[]") {
            var match$1 = l.Arg1;
            if (match$1 !== "[]") {
              var match$2 = match$1.Arg1;
              if (match$2 !== "[]") {
                return /* tuple */[
                        /* constructor */{
                          tag: "Node",
                          Arg0: /* constructor */{
                            tag: "Node",
                            Arg0: "Empty",
                            Arg1: l.Arg0,
                            Arg2: "Empty",
                            Arg3: 1
                          },
                          Arg1: match$1.Arg0,
                          Arg2: /* constructor */{
                            tag: "Node",
                            Arg0: "Empty",
                            Arg1: match$2.Arg0,
                            Arg2: "Empty",
                            Arg3: 1
                          },
                          Arg3: 2
                        },
                        match$2.Arg1
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
    if (l$1 !== "[]") {
      var match$4 = sub((n - nl | 0) - 1 | 0, l$1.Arg1);
      return /* tuple */[
              create(match$3[0], l$1.Arg0, match$4[0]),
              match$4[1]
            ];
    } else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            /* tuple */[
              "set_gen.ml",
              361,
              14
            ]
          ];
    }
  };
  return sub(List.length(l), l)[0];
}

function of_sorted_array(l) {
  var sub = function (start, n, l) {
    if (n === 0) {
      return "Empty";
    } else if (n === 1) {
      var x0 = l[start];
      return /* constructor */{
              tag: "Node",
              Arg0: "Empty",
              Arg1: x0,
              Arg2: "Empty",
              Arg3: 1
            };
    } else if (n === 2) {
      var x0$1 = l[start];
      var x1 = l[start + 1 | 0];
      return /* constructor */{
              tag: "Node",
              Arg0: /* constructor */{
                tag: "Node",
                Arg0: "Empty",
                Arg1: x0$1,
                Arg2: "Empty",
                Arg3: 1
              },
              Arg1: x1,
              Arg2: "Empty",
              Arg3: 2
            };
    } else if (n === 3) {
      var x0$2 = l[start];
      var x1$1 = l[start + 1 | 0];
      var x2 = l[start + 2 | 0];
      return /* constructor */{
              tag: "Node",
              Arg0: /* constructor */{
                tag: "Node",
                Arg0: "Empty",
                Arg1: x0$2,
                Arg2: "Empty",
                Arg3: 1
              },
              Arg1: x1$1,
              Arg2: /* constructor */{
                tag: "Node",
                Arg0: "Empty",
                Arg1: x2,
                Arg2: "Empty",
                Arg3: 1
              },
              Arg3: 2
            };
    } else {
      var nl = n / 2 | 0;
      var left = sub(start, nl, l);
      var mid = start + nl | 0;
      var v = l[mid];
      var right = sub(mid + 1 | 0, (n - nl | 0) - 1 | 0, l);
      return create(left, v, right);
    }
  };
  return sub(0, l.length, l);
}

function is_ordered(cmp, tree) {
  var is_ordered_min_max = function (tree) {
    if (tree !== "Empty") {
      var r = tree.Arg2;
      var v = tree.Arg1;
      var match = is_ordered_min_max(tree.Arg0);
      if (typeof match === "number") {
        if (match >= 50834029) {
          var match$1 = is_ordered_min_max(r);
          if (typeof match$1 === "number") {
            if (match$1 >= 50834029) {
              return /* `V */[
                      86,
                      /* tuple */[
                        v,
                        v
                      ]
                    ];
            } else {
              return /* No */17505;
            }
          } else {
            var match$2 = match$1[1];
            if (Curry._2(cmp, v, match$2[0]) < 0) {
              return /* `V */[
                      86,
                      /* tuple */[
                        v,
                        match$2[1]
                      ]
                    ];
            } else {
              return /* No */17505;
            }
          }
        } else {
          return /* No */17505;
        }
      } else {
        var match$3 = match[1];
        var max_v = match$3[1];
        var min_v = match$3[0];
        var match$4 = is_ordered_min_max(r);
        if (typeof match$4 === "number") {
          if (match$4 >= 50834029 && Curry._2(cmp, max_v, v) < 0) {
            return /* `V */[
                    86,
                    /* tuple */[
                      min_v,
                      v
                    ]
                  ];
          } else {
            return /* No */17505;
          }
        } else {
          var match$5 = match$4[1];
          if (Curry._2(cmp, max_v, match$5[0]) < 0) {
            return /* `V */[
                    86,
                    /* tuple */[
                      min_v,
                      match$5[1]
                    ]
                  ];
          } else {
            return /* No */17505;
          }
        }
      }
    } else {
      return /* Empty */50834029;
    }
  };
  return is_ordered_min_max(tree) !== /* No */17505;
}

function invariant(cmp, t) {
  check_height_and_diff(t);
  return is_ordered(cmp, t);
}

function compare_aux(cmp, _e1, _e2) {
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1 !== "End") {
      if (e2 !== "End") {
        var c = Curry._2(cmp, e1.Arg0, e2.Arg0);
        if (c !== 0) {
          return c;
        } else {
          _e2 = cons_enum(e2.Arg1, e2.Arg2);
          _e1 = cons_enum(e1.Arg1, e1.Arg2);
          continue ;
        }
      } else {
        return 1;
      }
    } else if (e2 !== "End") {
      return -1;
    } else {
      return 0;
    }
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
