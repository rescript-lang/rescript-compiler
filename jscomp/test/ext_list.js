// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Pervasives = require("../stdlib/pervasives");
var $$Array = require("../stdlib/array");
var List = require("../stdlib/list");

function filter_map(f, _xs) {
  while(/* true */1) {
    var xs = _xs;
    if (xs) {
      var ys = xs[2];
      var match = f(xs[1]);
      if (match) {
        return [
                /* :: */0,
                match[1],
                filter_map(f, ys)
              ];
      }
      else {
        _xs = ys;
      }
    }
    else {
      return /* [] */0;
    }
  };
}

function same_length(_xs, _ys) {
  while(/* true */1) {
    var ys = _ys;
    var xs = _xs;
    if (xs) {
      if (ys) {
        _ys = ys[2];
        _xs = xs[2];
      }
      else {
        return /* false */0;
      }
    }
    else {
      return ys ? /* false */0 : /* true */1;
    }
  };
}

function filter_mapi(f, xs) {
  var aux = function (_i, _xs) {
    while(/* true */1) {
      var xs = _xs;
      var i = _i;
      if (xs) {
        var ys = xs[2];
        var match = f(i, xs[1]);
        if (match) {
          return [
                  /* :: */0,
                  match[1],
                  aux(i + 1, ys)
                ];
        }
        else {
          _xs = ys;
          _i = i + 1;
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
  while(/* true */1) {
    var ys = _ys;
    var xs = _xs;
    if (xs) {
      if (ys) {
        var vs = ys[2];
        var us = xs[2];
        var match = f(xs[1], ys[1]);
        if (match) {
          return [
                  /* :: */0,
                  match[1],
                  filter_map2(f, us, vs)
                ];
        }
        else {
          _ys = vs;
          _xs = us;
        }
      }
      else {
        return Pervasives.invalid_arg("Ext_list.filter_map2");
      }
    }
    else {
      return ys ? Pervasives.invalid_arg("Ext_list.filter_map2") : /* [] */0;
    }
  };
}

function filter_map2i(f, xs, ys) {
  var aux = function (_i, _xs, _ys) {
    while(/* true */1) {
      var ys = _ys;
      var xs = _xs;
      var i = _i;
      if (xs) {
        if (ys) {
          var vs = ys[2];
          var us = xs[2];
          var match = f(i, xs[1], ys[1]);
          if (match) {
            return [
                    /* :: */0,
                    match[1],
                    aux(i + 1, us, vs)
                  ];
          }
          else {
            _ys = vs;
            _xs = us;
            _i = i + 1;
          }
        }
        else {
          return Pervasives.invalid_arg("Ext_list.filter_map2i");
        }
      }
      else {
        return ys ? Pervasives.invalid_arg("Ext_list.filter_map2i") : /* [] */0;
      }
    };
  };
  return aux(0, xs, ys);
}

function rev_map_append(f, _l1, _l2) {
  while(/* true */1) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1) {
      _l2 = [
        /* :: */0,
        f(l1[1]),
        l2
      ];
      _l1 = l1[2];
    }
    else {
      return l2;
    }
  };
}

function flat_map2(f, lx, ly) {
  var aux = function (_acc, _lx, _ly) {
    while(/* true */1) {
      var ly = _ly;
      var lx = _lx;
      var acc = _acc;
      if (lx) {
        if (ly) {
          _ly = ly[2];
          _lx = lx[2];
          _acc = List.rev_append(f(lx[1], ly[1]), acc);
        }
        else {
          return Pervasives.invalid_arg("Ext_list.flat_map2");
        }
      }
      else {
        return ly ? Pervasives.invalid_arg("Ext_list.flat_map2") : List.rev(acc);
      }
    };
  };
  return aux(/* [] */0, lx, ly);
}

function flat_map(f, lx) {
  var aux = function (_acc, _lx) {
    while(/* true */1) {
      var lx = _lx;
      var acc = _acc;
      if (lx) {
        _lx = lx[2];
        _acc = List.rev_append(f(lx[1]), acc);
      }
      else {
        return List.rev(acc);
      }
    };
  };
  return aux(/* [] */0, lx);
}

function map2_last(f, l1, l2) {
  var exit = 0;
  if (l1) {
    var l1$1 = l1[2];
    var u = l1[1];
    var exit$1 = 0;
    if (l1$1) {
      exit$1 = 2;
    }
    else {
      if (l2) {
        if (l2[2]) {
          exit$1 = 2;
        }
        else {
          return [
                  /* :: */0,
                  f(/* true */1, u, l2[1]),
                  /* [] */0
                ];
        }
      }
      else {
        exit = 1;
      }
    }
    if (exit$1 === 2) {
      if (l2) {
        var r = f(/* false */0, u, l2[1]);
        return [
                /* :: */0,
                r,
                map2_last(f, l1$1, l2[2])
              ];
      }
      else {
        exit = 1;
      }
    }
    
  }
  else {
    if (l2) {
      exit = 1;
    }
    else {
      return /* [] */0;
    }
  }
  if (exit === 1) {
    return Pervasives.invalid_arg("List.map2_last");
  }
  
}

function map_last(f, l1) {
  if (l1) {
    var l1$1 = l1[2];
    var u = l1[1];
    if (l1$1) {
      var r = f(/* false */0, u);
      return [
              /* :: */0,
              r,
              map_last(f, l1$1)
            ];
    }
    else {
      return [
              /* :: */0,
              f(/* true */1, u),
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
  return arr_length < n ? Pervasives.invalid_arg("Ext_list.take") : [
            /* tuple */0,
            $$Array.to_list($$Array.sub(arr, 0, n)),
            $$Array.to_list($$Array.sub(arr, n, arr_length - n))
          ];
}

function exclude_tail(x) {
  var aux = function (_acc, _x) {
    while(/* true */1) {
      var x = _x;
      var acc = _acc;
      if (x) {
        var ys = x[2];
        if (ys) {
          _x = ys;
          _acc = [
            /* :: */0,
            x[1],
            acc
          ];
        }
        else {
          return List.rev(acc);
        }
      }
      else {
        return Pervasives.invalid_arg("Ext_list.exclude_tail");
      }
    };
  };
  return aux(/* [] */0, x);
}

function group(cmp, lst) {
  return lst ? aux(cmp, lst[1], group(cmp, lst[2])) : /* [] */0;
}

function aux(cmp, x, xss) {
  if (xss) {
    var ys = xss[2];
    var y = xss[1];
    return cmp(x, List.hd(y)) ? [
              /* :: */0,
              [
                /* :: */0,
                x,
                y
              ],
              ys
            ] : [
              /* :: */0,
              y,
              aux(cmp, x, ys)
            ];
  }
  else {
    return [
            /* :: */0,
            [
              /* :: */0,
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
  while(/* true */1) {
    var h = _h;
    var n = _n;
    if (n < 0) {
      return Pervasives.invalid_arg("Ext_list.drop");
    }
    else {
      if (n) {
        if (h) {
          _h = List.tl(h);
          _n = n - 1;
        }
        else {
          return Pervasives.invalid_arg("Ext_list.drop");
        }
      }
      else {
        return h;
      }
    }
  };
}

function for_all_ret(p, _param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      var a = param[1];
      if (p(a)) {
        _param = param[2];
      }
      else {
        return [
                /* Some */0,
                a
              ];
      }
    }
    else {
      return /* None */0;
    }
  };
}

function for_all_opt(p, _param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      var v = p(param[1]);
      if (v) {
        return v;
      }
      else {
        _param = param[2];
      }
    }
    else {
      return /* None */0;
    }
  };
}

function fold(f, l, init) {
  return List.fold_left(function (_, i) {
              return f(i, init);
            }, init, l);
}

function rev_map_acc(acc, f, l) {
  var rmap_f = function (_accu, _param) {
    while(/* true */1) {
      var param = _param;
      var accu = _accu;
      if (param) {
        _param = param[2];
        _accu = [
          /* :: */0,
          f(param[1]),
          accu
        ];
      }
      else {
        return accu;
      }
    };
  };
  return rmap_f(acc, l);
}

exports.filter_map = filter_map;
exports.same_length = same_length;
exports.filter_mapi = filter_mapi;
exports.filter_map2 = filter_map2;
exports.filter_map2i = filter_map2i;
exports.rev_map_append = rev_map_append;
exports.flat_map2 = flat_map2;
exports.flat_map = flat_map;
exports.map2_last = map2_last;
exports.map_last = map_last;
exports.flat_map2_last = flat_map2_last;
exports.init = init;
exports.take = take;
exports.exclude_tail = exclude_tail;
exports.group = group;
exports.aux = aux;
exports.stable_group = stable_group;
exports.drop = drop;
exports.for_all_ret = for_all_ret;
exports.for_all_opt = for_all_opt;
exports.fold = fold;
exports.rev_map_acc = rev_map_acc;
/* No side effect */
