// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var $$Array                 = require("../stdlib/array");
var Caml_curry              = require("../runtime/caml_curry");
var List                    = require("../stdlib/list");

function filter_map(f, _xs) {
  while(true) {
    var xs = _xs;
    if (xs) {
      var ys = xs[1];
      var match = Caml_curry.app1(f, xs[0]);
      if (match) {
        return /* :: */[
                match[0],
                filter_map(f, ys)
              ];
      }
      else {
        _xs = ys;
        continue ;
        
      }
    }
    else {
      return /* [] */0;
    }
  };
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
        
      }
      else {
        return /* false */0;
      }
    }
    else if (ys) {
      return /* false */0;
    }
    else {
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
        var match = Caml_curry.app2(f, i, xs[0]);
        if (match) {
          return /* :: */[
                  match[0],
                  aux(i + 1 | 0, ys)
                ];
        }
        else {
          _xs = ys;
          _i = i + 1 | 0;
          continue ;
          
        }
      }
      else {
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
        var match = Caml_curry.app2(f, xs[0], ys[0]);
        if (match) {
          return /* :: */[
                  match[0],
                  filter_map2(f, us, vs)
                ];
        }
        else {
          _ys = vs;
          _xs = us;
          continue ;
          
        }
      }
      else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Ext_list.filter_map2"
            ];
      }
    }
    else if (ys) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Ext_list.filter_map2"
          ];
    }
    else {
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
          var match = Caml_curry.app3(f, i, xs[0], ys[0]);
          if (match) {
            return /* :: */[
                    match[0],
                    aux(i + 1 | 0, us, vs)
                  ];
          }
          else {
            _ys = vs;
            _xs = us;
            _i = i + 1 | 0;
            continue ;
            
          }
        }
        else {
          throw [
                Caml_builtin_exceptions.invalid_argument,
                "Ext_list.filter_map2i"
              ];
        }
      }
      else if (ys) {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Ext_list.filter_map2i"
            ];
      }
      else {
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
        Caml_curry.app1(f, l1[0]),
        l2
      ];
      _l1 = l1[1];
      continue ;
      
    }
    else {
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
        _acc = List.rev_append(Caml_curry.app2(f, lx$1[0], ly$1[0]), acc);
        continue ;
        
      }
      else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Ext_list.flat_map2"
            ];
      }
    }
    else if (ly$1) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Ext_list.flat_map2"
          ];
    }
    else {
      return List.rev(acc);
    }
  };
}

function flat_map(f, lx) {
  var _acc = /* [] */0;
  var _lx = lx;
  while(true) {
    var lx$1 = _lx;
    var acc = _acc;
    if (lx$1) {
      _lx = lx$1[1];
      _acc = List.rev_append(Caml_curry.app1(f, lx$1[0]), acc);
      continue ;
      
    }
    else {
      return List.rev(acc);
    }
  };
}

function map2_last(f, l1, l2) {
  if (l1) {
    var l1$1 = l1[1];
    var u = l1[0];
    var exit = 0;
    if (l1$1) {
      exit = 1;
    }
    else if (l2) {
      if (l2[1]) {
        exit = 1;
      }
      else {
        return /* :: */[
                Caml_curry.app3(f, /* true */1, u, l2[0]),
                /* [] */0
              ];
      }
    }
    else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "List.map2_last"
          ];
    }
    if (exit === 1) {
      if (l2) {
        var r = Caml_curry.app3(f, /* false */0, u, l2[0]);
        return /* :: */[
                r,
                map2_last(f, l1$1, l2[1])
              ];
      }
      else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "List.map2_last"
            ];
      }
    }
    
  }
  else if (l2) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "List.map2_last"
        ];
  }
  else {
    return /* [] */0;
  }
}

function map_last(f, l1) {
  if (l1) {
    var l1$1 = l1[1];
    var u = l1[0];
    if (l1$1) {
      var r = Caml_curry.app2(f, /* false */0, u);
      return /* :: */[
              r,
              map_last(f, l1$1)
            ];
    }
    else {
      return /* :: */[
              Caml_curry.app2(f, /* true */1, u),
              /* [] */0
            ];
    }
  }
  else {
    return /* [] */0;
  }
}

function flat_map2_last(f, lx, ly) {
  return List.concat(map2_last(f, lx, ly));
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
          "Ext_list.take"
        ];
  }
  else {
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
  }
  else {
    return /* tuple */[
            $$Array.to_list($$Array.sub(arr, 0, n)),
            n,
            $$Array.to_list($$Array.sub(arr, n, arr_length - n | 0))
          ];
  }
}

function exclude_tail(x) {
  var _acc = /* [] */0;
  var _x = x;
  while(true) {
    var x$1 = _x;
    var acc = _acc;
    if (x$1) {
      var ys = x$1[1];
      if (ys) {
        _x = ys;
        _acc = /* :: */[
          x$1[0],
          acc
        ];
        continue ;
        
      }
      else {
        return List.rev(acc);
      }
    }
    else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Ext_list.exclude_tail"
          ];
    }
  };
}

function group(cmp, lst) {
  if (lst) {
    return aux(cmp, lst[0], group(cmp, lst[1]));
  }
  else {
    return /* [] */0;
  }
}

function aux(cmp, x, xss) {
  if (xss) {
    var ys = xss[1];
    var y = xss[0];
    if (Caml_curry.app2(cmp, x, List.hd(y))) {
      return /* :: */[
              /* :: */[
                x,
                y
              ],
              ys
            ];
    }
    else {
      return /* :: */[
              y,
              aux(cmp, x, ys)
            ];
    }
  }
  else {
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
            "Ext_list.drop"
          ];
    }
    else if (n) {
      if (h) {
        _h = List.tl(h);
        _n = n - 1 | 0;
        continue ;
        
      }
      else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Ext_list.drop"
            ];
      }
    }
    else {
      return h;
    }
  };
}

function for_all_ret(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var a = param[0];
      if (Caml_curry.app1(p, a)) {
        _param = param[1];
        continue ;
        
      }
      else {
        return /* Some */[a];
      }
    }
    else {
      return /* None */0;
    }
  };
}

function for_all_opt(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = Caml_curry.app1(p, param[0]);
      if (v) {
        return v;
      }
      else {
        _param = param[1];
        continue ;
        
      }
    }
    else {
      return /* None */0;
    }
  };
}

function fold(f, l, init) {
  return List.fold_left(function (_, i) {
              return Caml_curry.app2(f, i, init);
            }, init, l);
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
        Caml_curry.app1(f, param[0]),
        accu
      ];
      continue ;
      
    }
    else {
      return accu;
    }
  };
}

function rev_iter(f, xs) {
  if (xs) {
    rev_iter(f, xs[1]);
    return Caml_curry.app1(f, xs[0]);
  }
  else {
    return /* () */0;
  }
}

function for_all2_no_exn(p, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      if (l2) {
        if (Caml_curry.app2(p, l1[0], l2[0])) {
          _l2 = l2[1];
          _l1 = l1[1];
          continue ;
          
        }
        else {
          return /* false */0;
        }
      }
      else {
        return /* false */0;
      }
    }
    else if (l2) {
      return /* false */0;
    }
    else {
      return /* true */1;
    }
  };
}

function find_no_exn(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var x = param[0];
      if (Caml_curry.app1(p, x)) {
        return /* Some */[x];
      }
      else {
        _param = param[1];
        continue ;
        
      }
    }
    else {
      return /* None */0;
    }
  };
}

function find_opt(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = Caml_curry.app1(p, param[0]);
      if (v) {
        return v;
      }
      else {
        _param = param[1];
        continue ;
        
      }
    }
    else {
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
      var match = Caml_curry.app1(f, xs$1[0]);
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
      
    }
    else {
      return /* tuple */[
              List.rev(bs),
              List.rev(cs)
            ];
    }
  };
}

exports.filter_map      = filter_map;
exports.same_length     = same_length;
exports.filter_mapi     = filter_mapi;
exports.filter_map2     = filter_map2;
exports.filter_map2i    = filter_map2i;
exports.rev_map_append  = rev_map_append;
exports.flat_map2       = flat_map2;
exports.flat_map        = flat_map;
exports.map2_last       = map2_last;
exports.map_last        = map_last;
exports.flat_map2_last  = flat_map2_last;
exports.init            = init;
exports.take            = take;
exports.try_take        = try_take;
exports.exclude_tail    = exclude_tail;
exports.group           = group;
exports.aux             = aux;
exports.stable_group    = stable_group;
exports.drop            = drop;
exports.for_all_ret     = for_all_ret;
exports.for_all_opt     = for_all_opt;
exports.fold            = fold;
exports.rev_map_acc     = rev_map_acc;
exports.rev_iter        = rev_iter;
exports.for_all2_no_exn = for_all2_no_exn;
exports.find_no_exn     = find_no_exn;
exports.find_opt        = find_opt;
exports.split_map       = split_map;
/* No side effect */
