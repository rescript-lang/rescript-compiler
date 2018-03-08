'use strict';

var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function filter_map(f, _xs) {
  while(true) {
    var xs = _xs;
    if (xs) {
      var ys = xs[1];
      var match = Curry._1(f, xs[0]);
      if (match) {
        return /* :: */[
                match[0],
                filter_map(f, ys)
              ];
      } else {
        _xs = ys;
        continue ;
        
      }
    } else {
      return /* [] */0;
    }
  };
}

function excludes(p, l) {
  var excluded = [/* false */0];
  var aux = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (param) {
        var l = param[1];
        var x = param[0];
        if (Curry._1(p, x)) {
          excluded[0] = /* true */1;
          _param = l;
          continue ;
          
        } else {
          _param = l;
          _accu = /* :: */[
            x,
            accu
          ];
          continue ;
          
        }
      } else {
        return List.rev(accu);
      }
    };
  };
  var v = aux(/* [] */0, l);
  if (excluded[0]) {
    return /* tuple */[
            /* true */1,
            v
          ];
  } else {
    return /* tuple */[
            /* false */0,
            l
          ];
  }
}

function exclude_with_fact(p, l) {
  var excluded = [/* None */0];
  var aux = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (param) {
        var l = param[1];
        var x = param[0];
        if (Curry._1(p, x)) {
          excluded[0] = /* Some */[x];
          _param = l;
          continue ;
          
        } else {
          _param = l;
          _accu = /* :: */[
            x,
            accu
          ];
          continue ;
          
        }
      } else {
        return List.rev(accu);
      }
    };
  };
  var v = aux(/* [] */0, l);
  return /* tuple */[
          excluded[0],
          excluded[0] !== /* None */0 ? v : l
        ];
}

function exclude_with_fact2(p1, p2, l) {
  var excluded1 = [/* None */0];
  var excluded2 = [/* None */0];
  var aux = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (param) {
        var l = param[1];
        var x = param[0];
        if (Curry._1(p1, x)) {
          excluded1[0] = /* Some */[x];
          _param = l;
          continue ;
          
        } else if (Curry._1(p2, x)) {
          excluded2[0] = /* Some */[x];
          _param = l;
          continue ;
          
        } else {
          _param = l;
          _accu = /* :: */[
            x,
            accu
          ];
          continue ;
          
        }
      } else {
        return List.rev(accu);
      }
    };
  };
  var v = aux(/* [] */0, l);
  return /* tuple */[
          excluded1[0],
          excluded2[0],
          excluded1[0] !== /* None */0 && excluded2[0] !== /* None */0 ? v : l
        ];
}

function same_length(_xs, _ys) {
  while(true) {
    var ys = _ys;
    var xs = _xs;
    if (xs) {
      if (ys) {
        _ys = ys[1];
        _xs = xs[1];
        continue ;
        
      } else {
        return /* false */0;
      }
    } else if (ys) {
      return /* false */0;
    } else {
      return /* true */1;
    }
  };
}

function filter_mapi(f, xs) {
  var aux = function (_i, _xs) {
    while(true) {
      var xs = _xs;
      var i = _i;
      if (xs) {
        var ys = xs[1];
        var match = Curry._2(f, i, xs[0]);
        if (match) {
          return /* :: */[
                  match[0],
                  aux(i + 1 | 0, ys)
                ];
        } else {
          _xs = ys;
          _i = i + 1 | 0;
          continue ;
          
        }
      } else {
        return /* [] */0;
      }
    };
  };
  return aux(0, xs);
}

function filter_map2(f, _xs, _ys) {
  while(true) {
    var ys = _ys;
    var xs = _xs;
    if (xs) {
      if (ys) {
        var vs = ys[1];
        var us = xs[1];
        var match = Curry._2(f, xs[0], ys[0]);
        if (match) {
          return /* :: */[
                  match[0],
                  filter_map2(f, us, vs)
                ];
        } else {
          _ys = vs;
          _xs = us;
          continue ;
          
        }
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Ext_list_test.filter_map2"
            ];
      }
    } else if (ys) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Ext_list_test.filter_map2"
          ];
    } else {
      return /* [] */0;
    }
  };
}

function filter_map2i(f, xs, ys) {
  var aux = function (_i, _xs, _ys) {
    while(true) {
      var ys = _ys;
      var xs = _xs;
      var i = _i;
      if (xs) {
        if (ys) {
          var vs = ys[1];
          var us = xs[1];
          var match = Curry._3(f, i, xs[0], ys[0]);
          if (match) {
            return /* :: */[
                    match[0],
                    aux(i + 1 | 0, us, vs)
                  ];
          } else {
            _ys = vs;
            _xs = us;
            _i = i + 1 | 0;
            continue ;
            
          }
        } else {
          throw [
                Caml_builtin_exceptions.invalid_argument,
                "Ext_list_test.filter_map2i"
              ];
        }
      } else if (ys) {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Ext_list_test.filter_map2i"
            ];
      } else {
        return /* [] */0;
      }
    };
  };
  return aux(0, xs, ys);
}

function rev_map_append(f, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      _l2 = /* :: */[
        Curry._1(f, l1[0]),
        l2
      ];
      _l1 = l1[1];
      continue ;
      
    } else {
      return l2;
    }
  };
}

function flat_map2(f, lx, ly) {
  var _acc = /* [] */0;
  var _lx = lx;
  var _ly = ly;
  while(true) {
    var ly$1 = _ly;
    var lx$1 = _lx;
    var acc = _acc;
    if (lx$1) {
      if (ly$1) {
        _ly = ly$1[1];
        _lx = lx$1[1];
        _acc = List.rev_append(Curry._2(f, lx$1[0], ly$1[0]), acc);
        continue ;
        
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Ext_list_test.flat_map2"
            ];
      }
    } else if (ly$1) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Ext_list_test.flat_map2"
          ];
    } else {
      return List.rev(acc);
    }
  };
}

function flat_map_aux(f, _acc, append, _lx) {
  while(true) {
    var lx = _lx;
    var acc = _acc;
    if (lx) {
      _lx = lx[1];
      _acc = List.rev_append(Curry._1(f, lx[0]), acc);
      continue ;
      
    } else {
      return List.rev_append(acc, append);
    }
  };
}

function flat_map(f, lx) {
  return flat_map_aux(f, /* [] */0, /* [] */0, lx);
}

function flat_map_acc(f, append, lx) {
  return flat_map_aux(f, /* [] */0, append, lx);
}

function map2_last(f, l1, l2) {
  if (l1) {
    var l1$1 = l1[1];
    var u = l1[0];
    var exit = 0;
    if (l1$1) {
      exit = 1;
    } else if (l2) {
      if (l2[1]) {
        exit = 1;
      } else {
        return /* :: */[
                Curry._3(f, /* true */1, u, l2[0]),
                /* [] */0
              ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "List.map2_last"
          ];
    }
    if (exit === 1) {
      if (l2) {
        var r = Curry._3(f, /* false */0, u, l2[0]);
        return /* :: */[
                r,
                map2_last(f, l1$1, l2[1])
              ];
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "List.map2_last"
            ];
      }
    }
    
  } else if (l2) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "List.map2_last"
        ];
  } else {
    return /* [] */0;
  }
}

function map_last(f, l1) {
  if (l1) {
    var l1$1 = l1[1];
    var u = l1[0];
    if (l1$1) {
      var r = Curry._2(f, /* false */0, u);
      return /* :: */[
              r,
              map_last(f, l1$1)
            ];
    } else {
      return /* :: */[
              Curry._2(f, /* true */1, u),
              /* [] */0
            ];
    }
  } else {
    return /* [] */0;
  }
}

function fold_right2_last(f, l1, l2, accu) {
  if (l1) {
    var l1$1 = l1[1];
    var last1 = l1[0];
    var exit = 0;
    if (l1$1) {
      exit = 1;
    } else if (l2) {
      if (l2[1]) {
        exit = 1;
      } else {
        return Curry._4(f, /* true */1, last1, l2[0], accu);
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "List.fold_right2"
          ];
    }
    if (exit === 1) {
      if (l2) {
        return Curry._4(f, /* false */0, last1, l2[0], fold_right2_last(f, l1$1, l2[1], accu));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "List.fold_right2"
            ];
      }
    }
    
  } else if (l2) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "List.fold_right2"
        ];
  } else {
    return accu;
  }
}

function init(n, f) {
  return $$Array.to_list($$Array.init(n, f));
}

function take(n, l) {
  var arr = $$Array.of_list(l);
  var arr_length = arr.length;
  if (arr_length < n) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Ext_list_test.take"
        ];
  } else {
    return /* tuple */[
            $$Array.to_list($$Array.sub(arr, 0, n)),
            $$Array.to_list($$Array.sub(arr, n, arr_length - n | 0))
          ];
  }
}

function try_take(n, l) {
  var arr = $$Array.of_list(l);
  var arr_length = arr.length;
  if (arr_length <= n) {
    return /* tuple */[
            l,
            arr_length,
            /* [] */0
          ];
  } else {
    return /* tuple */[
            $$Array.to_list($$Array.sub(arr, 0, n)),
            n,
            $$Array.to_list($$Array.sub(arr, n, arr_length - n | 0))
          ];
  }
}

function length_compare(_l, _n) {
  while(true) {
    var n = _n;
    var l = _l;
    if (n < 0) {
      return /* Gt */15949;
    } else if (l) {
      _n = n - 1 | 0;
      _l = l[1];
      continue ;
      
    } else if (n === 0) {
      return /* Eq */15500;
    } else {
      return /* Lt */17064;
    }
  };
}

function length_larger_than_n(n, _xs, _ys) {
  while(true) {
    var ys = _ys;
    var xs = _xs;
    if (ys) {
      if (xs) {
        _ys = ys[1];
        _xs = xs[1];
        continue ;
        
      } else {
        return /* false */0;
      }
    } else {
      return +(length_compare(xs, n) === /* Eq */15500);
    }
  };
}

function exclude_tail(x) {
  var _acc = /* [] */0;
  var _x = x;
  while(true) {
    var x$1 = _x;
    var acc = _acc;
    if (x$1) {
      var ys = x$1[1];
      var x$2 = x$1[0];
      if (ys) {
        _x = ys;
        _acc = /* :: */[
          x$2,
          acc
        ];
        continue ;
        
      } else {
        return /* tuple */[
                x$2,
                List.rev(acc)
              ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Ext_list_test.exclude_tail"
          ];
    }
  };
}

function group(cmp, lst) {
  if (lst) {
    return aux(cmp, lst[0], group(cmp, lst[1]));
  } else {
    return /* [] */0;
  }
}

function aux(cmp, x, xss) {
  if (xss) {
    var ys = xss[1];
    var y = xss[0];
    if (Curry._2(cmp, x, List.hd(y))) {
      return /* :: */[
              /* :: */[
                x,
                y
              ],
              ys
            ];
    } else {
      return /* :: */[
              y,
              aux(cmp, x, ys)
            ];
    }
  } else {
    return /* :: */[
            /* :: */[
              x,
              /* [] */0
            ],
            /* [] */0
          ];
  }
}

function stable_group(cmp, lst) {
  return List.rev(group(cmp, lst));
}

function drop(_n, _h) {
  while(true) {
    var h = _h;
    var n = _n;
    if (n < 0) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Ext_list_test.drop"
          ];
    } else if (n === 0) {
      return h;
    } else if (h === /* [] */0) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Ext_list_test.drop"
          ];
    } else {
      _h = List.tl(h);
      _n = n - 1 | 0;
      continue ;
      
    }
  };
}

function find_first_not(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var a = param[0];
      if (Curry._1(p, a)) {
        _param = param[1];
        continue ;
        
      } else {
        return /* Some */[a];
      }
    } else {
      return /* None */0;
    }
  };
}

function for_all_opt(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = Curry._1(p, param[0]);
      if (v) {
        return v;
      } else {
        _param = param[1];
        continue ;
        
      }
    } else {
      return /* None */0;
    }
  };
}

function fold(f, l, init) {
  return List.fold_left((function (_, i) {
                return Curry._2(f, i, init);
              }), init, l);
}

function rev_map_acc(acc, f, l) {
  var _accu = acc;
  var _param = l;
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[1];
      _accu = /* :: */[
        Curry._1(f, param[0]),
        accu
      ];
      continue ;
      
    } else {
      return accu;
    }
  };
}

function map_acc(acc, f, l) {
  if (l) {
    return /* :: */[
            Curry._1(f, l[0]),
            map_acc(acc, f, l[1])
          ];
  } else {
    return acc;
  }
}

function rev_iter(f, xs) {
  if (xs) {
    rev_iter(f, xs[1]);
    return Curry._1(f, xs[0]);
  } else {
    return /* () */0;
  }
}

function for_all2_no_exn(p, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2 && Curry._2(p, l1[0], l2[0])) {
        _l2 = l2[1];
        _l1 = l1[1];
        continue ;
        
      } else {
        return /* false */0;
      }
    } else if (l2) {
      return /* false */0;
    } else {
      return /* true */1;
    }
  };
}

function find_no_exn(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var x = param[0];
      if (Curry._1(p, x)) {
        return /* Some */[x];
      } else {
        _param = param[1];
        continue ;
        
      }
    } else {
      return /* None */0;
    }
  };
}

function find_opt(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = Curry._1(p, param[0]);
      if (v) {
        return v;
      } else {
        _param = param[1];
        continue ;
        
      }
    } else {
      return /* None */0;
    }
  };
}

function split_map(f, xs) {
  var _bs = /* [] */0;
  var _cs = /* [] */0;
  var _xs = xs;
  while(true) {
    var xs$1 = _xs;
    var cs = _cs;
    var bs = _bs;
    if (xs$1) {
      var match = Curry._1(f, xs$1[0]);
      _xs = xs$1[1];
      _cs = /* :: */[
        match[1],
        cs
      ];
      _bs = /* :: */[
        match[0],
        bs
      ];
      continue ;
      
    } else {
      return /* tuple */[
              List.rev(bs),
              List.rev(cs)
            ];
    }
  };
}

function reduce_from_right(fn, lst) {
  var match = List.rev(lst);
  if (match) {
    return List.fold_left((function (x, y) {
                  return Curry._2(fn, y, x);
                }), match[0], match[1]);
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Ext_list_test.reduce"
        ];
  }
}

function reduce_from_left(fn, lst) {
  if (lst) {
    return List.fold_left(fn, lst[0], lst[1]);
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Ext_list_test.reduce_from_left"
        ];
  }
}

function create_ref_empty() {
  return [/* [] */0];
}

function ref_top(x) {
  var match = x[0];
  if (match) {
    return match[0];
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Ext_list_test.ref_top"
        ];
  }
}

function ref_empty(x) {
  var match = x[0];
  if (match) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

function ref_push(x, refs) {
  refs[0] = /* :: */[
    x,
    refs[0]
  ];
  return /* () */0;
}

function ref_pop(refs) {
  var match = refs[0];
  if (match) {
    refs[0] = match[1];
    return match[0];
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Ext_list_test.ref_pop"
        ];
  }
}

function rev_except_last(xs) {
  var _acc = /* [] */0;
  var _xs = xs;
  while(true) {
    var xs$1 = _xs;
    var acc = _acc;
    if (xs$1) {
      var xs$2 = xs$1[1];
      var x = xs$1[0];
      if (xs$2) {
        _xs = xs$2;
        _acc = /* :: */[
          x,
          acc
        ];
        continue ;
        
      } else {
        return /* tuple */[
                acc,
                x
              ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Ext_list_test.rev_except_last"
          ];
    }
  };
}

function sort_via_array(cmp, lst) {
  var arr = $$Array.of_list(lst);
  $$Array.sort(cmp, arr);
  return $$Array.to_list(arr);
}

function last(_xs) {
  while(true) {
    var xs = _xs;
    if (xs) {
      var tl = xs[1];
      if (tl) {
        _xs = tl;
        continue ;
        
      } else {
        return xs[0];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Ext_list_test.last"
          ];
    }
  };
}

function assoc_by_string(def, k, _lst) {
  while(true) {
    var lst = _lst;
    if (lst) {
      var match = lst[0];
      if (match[0] === k) {
        return match[1];
      } else {
        _lst = lst[1];
        continue ;
        
      }
    } else if (def) {
      return def[0];
    } else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "ext_list_test.ml",
              399,
              14
            ]
          ];
    }
  };
}

function assoc_by_int(def, k, _lst) {
  while(true) {
    var lst = _lst;
    if (lst) {
      var match = lst[0];
      if (match[0] === k) {
        return match[1];
      } else {
        _lst = lst[1];
        continue ;
        
      }
    } else if (def) {
      return def[0];
    } else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "ext_list_test.ml",
              409,
              14
            ]
          ];
    }
  };
}

exports.filter_map = filter_map;
exports.excludes = excludes;
exports.exclude_with_fact = exclude_with_fact;
exports.exclude_with_fact2 = exclude_with_fact2;
exports.same_length = same_length;
exports.filter_mapi = filter_mapi;
exports.filter_map2 = filter_map2;
exports.filter_map2i = filter_map2i;
exports.rev_map_append = rev_map_append;
exports.flat_map2 = flat_map2;
exports.flat_map_aux = flat_map_aux;
exports.flat_map = flat_map;
exports.flat_map_acc = flat_map_acc;
exports.map2_last = map2_last;
exports.map_last = map_last;
exports.fold_right2_last = fold_right2_last;
exports.init = init;
exports.take = take;
exports.try_take = try_take;
exports.length_compare = length_compare;
exports.length_larger_than_n = length_larger_than_n;
exports.exclude_tail = exclude_tail;
exports.group = group;
exports.aux = aux;
exports.stable_group = stable_group;
exports.drop = drop;
exports.find_first_not = find_first_not;
exports.for_all_opt = for_all_opt;
exports.fold = fold;
exports.rev_map_acc = rev_map_acc;
exports.map_acc = map_acc;
exports.rev_iter = rev_iter;
exports.for_all2_no_exn = for_all2_no_exn;
exports.find_no_exn = find_no_exn;
exports.find_opt = find_opt;
exports.split_map = split_map;
exports.reduce_from_right = reduce_from_right;
exports.reduce_from_left = reduce_from_left;
exports.create_ref_empty = create_ref_empty;
exports.ref_top = ref_top;
exports.ref_empty = ref_empty;
exports.ref_push = ref_push;
exports.ref_pop = ref_pop;
exports.rev_except_last = rev_except_last;
exports.sort_via_array = sort_via_array;
exports.last = last;
exports.assoc_by_string = assoc_by_string;
exports.assoc_by_int = assoc_by_int;
/* No side effect */
