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
    if (s) {
      _e = /* More */[
        s[1],
        s[2],
        e
      ];
      _s = s[0];
      continue ;
      
    } else {
      return e;
    }
  };
}

function height(param) {
  if (param) {
    return param[3];
  } else {
    return 0;
  }
}

function min_elt(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param[0];
      if (l) {
        _param = l;
        continue ;
        
      } else {
        return param[1];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function max_elt(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var r = param[2];
      if (r) {
        _param = r;
        continue ;
        
      } else {
        return param[1];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function is_empty(param) {
  if (param) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

function cardinal_aux(_acc, _param) {
  while(true) {
    var param = _param;
    var acc = _acc;
    if (param) {
      _param = param[0];
      _acc = cardinal_aux(acc + 1 | 0, param[2]);
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
    if (param) {
      _param = param[0];
      _accu = /* :: */[
        param[1],
        elements_aux(accu, param[2])
      ];
      continue ;
      
    } else {
      return accu;
    }
  };
}

function elements(s) {
  return elements_aux(/* [] */0, s);
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      iter(f, param[0]);
      Curry._1(f, param[1]);
      _param = param[2];
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
    if (s) {
      _accu = Curry._2(f, s[1], fold(f, s[0], accu));
      _s = s[2];
      continue ;
      
    } else {
      return accu;
    }
  };
}

function for_all(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (Curry._1(p, param[1])) {
        if (for_all(p, param[0])) {
          _param = param[2];
          continue ;
          
        } else {
          return /* false */0;
        }
      } else {
        return /* false */0;
      }
    } else {
      return /* true */1;
    }
  };
}

function exists(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (Curry._1(p, param[1])) {
        return /* true */1;
      } else if (exists(p, param[0])) {
        return /* true */1;
      } else {
        _param = param[2];
        continue ;
        
      }
    } else {
      return /* false */0;
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
  if (param) {
    var h = param[3];
    var hl = check_height_and_diff(param[0]);
    var hr = check_height_and_diff(param[2]);
    if (h !== (max_int_2(hl, hr) + 1 | 0)) {
      throw Height_invariant_broken;
    } else {
      var diff = Pervasives.abs(hl - hr | 0);
      if (diff > 2) {
        throw Height_diff_borken;
      } else {
        return h;
      }
    }
  } else {
    return 0;
  }
}

function check(tree) {
  check_height_and_diff(tree);
  return /* () */0;
}

function create(l, v, r) {
  var hl = l ? l[3] : 0;
  var hr = r ? r[3] : 0;
  return /* Node */[
          l,
          v,
          r,
          hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function internal_bal(l, v, r) {
  var hl = l ? l[3] : 0;
  var hr = r ? r[3] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[2];
      var lv = l[1];
      var ll = l[0];
      if (height(ll) >= height(lr)) {
        return create(ll, lv, create(lr, v, r));
      } else if (lr) {
        return create(create(ll, lv, lr[0]), lr[1], create(lr[2], v, r));
      } else {
        throw [
              Caml_builtin_exceptions.assert_failure,
              [
                "set_gen.ml",
                235,
                19
              ]
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "set_gen.ml",
              225,
              15
            ]
          ];
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r) {
      var rr = r[2];
      var rv = r[1];
      var rl = r[0];
      if (height(rr) >= height(rl)) {
        return create(create(l, v, rl), rv, rr);
      } else if (rl) {
        return create(create(l, v, rl[0]), rl[1], create(rl[2], rv, rr));
      } else {
        throw [
              Caml_builtin_exceptions.assert_failure,
              [
                "set_gen.ml",
                251,
                19
              ]
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "set_gen.ml",
              245,
              15
            ]
          ];
    }
  } else {
    return /* Node */[
            l,
            v,
            r,
            hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function remove_min_elt(param) {
  if (param) {
    var l = param[0];
    if (l) {
      return internal_bal(remove_min_elt(l), param[1], param[2]);
    } else {
      return param[2];
    }
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Set.remove_min_elt"
        ];
  }
}

function singleton(x) {
  return /* Node */[
          /* Empty */0,
          x,
          /* Empty */0,
          1
        ];
}

function internal_merge(l, r) {
  if (l) {
    if (r) {
      return internal_bal(l, min_elt(r), remove_min_elt(r));
    } else {
      return l;
    }
  } else {
    return r;
  }
}

function add_min_element(v, param) {
  if (param) {
    return internal_bal(add_min_element(v, param[0]), param[1], param[2]);
  } else {
    return singleton(v);
  }
}

function add_max_element(v, param) {
  if (param) {
    return internal_bal(param[0], param[1], add_max_element(v, param[2]));
  } else {
    return singleton(v);
  }
}

function internal_join(l, v, r) {
  if (l) {
    if (r) {
      var rh = r[3];
      var lh = l[3];
      if (lh > (rh + 2 | 0)) {
        return internal_bal(l[0], l[1], internal_join(l[2], v, r));
      } else if (rh > (lh + 2 | 0)) {
        return internal_bal(internal_join(l, v, r[0]), r[1], r[2]);
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
  if (t1) {
    if (t2) {
      return internal_join(t1, min_elt(t2), remove_min_elt(t2));
    } else {
      return t1;
    }
  } else {
    return t2;
  }
}

function filter(p, param) {
  if (param) {
    var v = param[1];
    var l$prime = filter(p, param[0]);
    var pv = Curry._1(p, v);
    var r$prime = filter(p, param[2]);
    if (pv) {
      return internal_join(l$prime, v, r$prime);
    } else {
      return internal_concat(l$prime, r$prime);
    }
  } else {
    return /* Empty */0;
  }
}

function partition(p, param) {
  if (param) {
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
            /* Empty */0,
            /* Empty */0
          ];
  }
}

function of_sorted_list(l) {
  var sub = function (n, l) {
    var exit = 0;
    if (n > 3 || n < 0) {
      exit = 1;
    } else {
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
            } else {
              exit = 1;
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
              } else {
                exit = 1;
              }
            } else {
              exit = 1;
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
                } else {
                  exit = 1;
                }
              } else {
                exit = 1;
              }
            } else {
              exit = 1;
            }
            break;
        
      }
    }
    if (exit === 1) {
      var nl = n / 2 | 0;
      var match$3 = sub(nl, l);
      var l$1 = match$3[1];
      if (l$1) {
        var match$4 = sub((n - nl | 0) - 1 | 0, l$1[1]);
        return /* tuple */[
                create(match$3[0], l$1[0], match$4[0]),
                match$4[1]
              ];
      } else {
        throw [
              Caml_builtin_exceptions.assert_failure,
              [
                "set_gen.ml",
                361,
                14
              ]
            ];
      }
    }
    
  };
  return sub(List.length(l), l)[0];
}

function of_sorted_array(l) {
  var sub = function (start, n, l) {
    if (n) {
      if (n === 1) {
        var x0 = l[start];
        return /* Node */[
                /* Empty */0,
                x0,
                /* Empty */0,
                1
              ];
      } else if (n === 2) {
        var x0$1 = l[start];
        var x1 = l[start + 1 | 0];
        return /* Node */[
                /* Node */[
                  /* Empty */0,
                  x0$1,
                  /* Empty */0,
                  1
                ],
                x1,
                /* Empty */0,
                2
              ];
      } else if (n === 3) {
        var x0$2 = l[start];
        var x1$1 = l[start + 1 | 0];
        var x2 = l[start + 2 | 0];
        return /* Node */[
                /* Node */[
                  /* Empty */0,
                  x0$2,
                  /* Empty */0,
                  1
                ],
                x1$1,
                /* Node */[
                  /* Empty */0,
                  x2,
                  /* Empty */0,
                  1
                ],
                2
              ];
      } else {
        var nl = n / 2 | 0;
        var left = sub(start, nl, l);
        var mid = start + nl | 0;
        var v = l[mid];
        var right = sub(mid + 1 | 0, (n - nl | 0) - 1 | 0, l);
        return create(left, v, right);
      }
    } else {
      return /* Empty */0;
    }
  };
  return sub(0, l.length, l);
}

function is_ordered(cmp, tree) {
  var is_ordered_min_max = function (tree) {
    if (tree) {
      var r = tree[2];
      var v = tree[1];
      var match = is_ordered_min_max(tree[0]);
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
  return +(is_ordered_min_max(tree) !== /* No */17505);
}

function invariant(cmp, t) {
  check_height_and_diff(t);
  return is_ordered(cmp, t);
}

function compare_aux(cmp, _e1, _e2) {
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var c = Curry._2(cmp, e1[0], e2[0]);
        if (c !== 0) {
          return c;
        } else {
          _e2 = cons_enum(e2[1], e2[2]);
          _e1 = cons_enum(e1[1], e1[2]);
          continue ;
          
        }
      } else {
        return 1;
      }
    } else if (e2) {
      return -1;
    } else {
      return 0;
    }
  };
}

function compare(cmp, s1, s2) {
  return compare_aux(cmp, cons_enum(s1, /* End */0), cons_enum(s2, /* End */0));
}

var empty = /* Empty */0;

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
