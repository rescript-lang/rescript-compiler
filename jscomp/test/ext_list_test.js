'use strict';

var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Ext_string_test = require("./ext_string_test.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function filter_map(f, _xs) {
  while(true) {
    var xs = _xs;
    if (xs !== "[]") {
      var ys = xs.Arg1;
      var match = Curry._1(f, xs.Arg0);
      if (match !== undefined) {
        return /* constructor */{
                tag: "::",
                Arg0: Caml_option.valFromOption(match),
                Arg1: filter_map(f, ys)
              };
      } else {
        _xs = ys;
        continue ;
      }
    } else {
      return "[]";
    }
  };
}

function excludes(p, l) {
  var excluded = /* record */[/* contents */false];
  var aux = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (param !== "[]") {
        var l = param.Arg1;
        var x = param.Arg0;
        if (Curry._1(p, x)) {
          excluded[0] = true;
          _param = l;
          continue ;
        } else {
          _param = l;
          _accu = /* constructor */{
            tag: "::",
            Arg0: x,
            Arg1: accu
          };
          continue ;
        }
      } else {
        return List.rev(accu);
      }
    };
  };
  var v = aux("[]", l);
  if (excluded[0]) {
    return /* tuple */[
            true,
            v
          ];
  } else {
    return /* tuple */[
            false,
            l
          ];
  }
}

function exclude_with_fact(p, l) {
  var excluded = /* record */[/* contents */undefined];
  var aux = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (param !== "[]") {
        var l = param.Arg1;
        var x = param.Arg0;
        if (Curry._1(p, x)) {
          excluded[0] = Caml_option.some(x);
          _param = l;
          continue ;
        } else {
          _param = l;
          _accu = /* constructor */{
            tag: "::",
            Arg0: x,
            Arg1: accu
          };
          continue ;
        }
      } else {
        return List.rev(accu);
      }
    };
  };
  var v = aux("[]", l);
  return /* tuple */[
          excluded[0],
          excluded[0] !== undefined ? v : l
        ];
}

function exclude_with_fact2(p1, p2, l) {
  var excluded1 = /* record */[/* contents */undefined];
  var excluded2 = /* record */[/* contents */undefined];
  var aux = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (param !== "[]") {
        var l = param.Arg1;
        var x = param.Arg0;
        if (Curry._1(p1, x)) {
          excluded1[0] = Caml_option.some(x);
          _param = l;
          continue ;
        } else if (Curry._1(p2, x)) {
          excluded2[0] = Caml_option.some(x);
          _param = l;
          continue ;
        } else {
          _param = l;
          _accu = /* constructor */{
            tag: "::",
            Arg0: x,
            Arg1: accu
          };
          continue ;
        }
      } else {
        return List.rev(accu);
      }
    };
  };
  var v = aux("[]", l);
  return /* tuple */[
          excluded1[0],
          excluded2[0],
          excluded1[0] !== undefined && excluded2[0] !== undefined ? v : l
        ];
}

function same_length(_xs, _ys) {
  while(true) {
    var ys = _ys;
    var xs = _xs;
    if (xs !== "[]") {
      if (ys !== "[]") {
        _ys = ys.Arg1;
        _xs = xs.Arg1;
        continue ;
      } else {
        return false;
      }
    } else {
      return ys === "[]";
    }
  };
}

function filter_mapi(f, xs) {
  var aux = function (_i, _xs) {
    while(true) {
      var xs = _xs;
      var i = _i;
      if (xs !== "[]") {
        var ys = xs.Arg1;
        var match = Curry._2(f, i, xs.Arg0);
        if (match !== undefined) {
          return /* constructor */{
                  tag: "::",
                  Arg0: Caml_option.valFromOption(match),
                  Arg1: aux(i + 1 | 0, ys)
                };
        } else {
          _xs = ys;
          _i = i + 1 | 0;
          continue ;
        }
      } else {
        return "[]";
      }
    };
  };
  return aux(0, xs);
}

function filter_map2(f, _xs, _ys) {
  while(true) {
    var ys = _ys;
    var xs = _xs;
    if (xs !== "[]") {
      if (ys !== "[]") {
        var vs = ys.Arg1;
        var us = xs.Arg1;
        var match = Curry._2(f, xs.Arg0, ys.Arg0);
        if (match !== undefined) {
          return /* constructor */{
                  tag: "::",
                  Arg0: Caml_option.valFromOption(match),
                  Arg1: filter_map2(f, us, vs)
                };
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
    } else if (ys !== "[]") {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Ext_list_test.filter_map2"
          ];
    } else {
      return "[]";
    }
  };
}

function filter_map2i(f, xs, ys) {
  var aux = function (_i, _xs, _ys) {
    while(true) {
      var ys = _ys;
      var xs = _xs;
      var i = _i;
      if (xs !== "[]") {
        if (ys !== "[]") {
          var vs = ys.Arg1;
          var us = xs.Arg1;
          var match = Curry._3(f, i, xs.Arg0, ys.Arg0);
          if (match !== undefined) {
            return /* constructor */{
                    tag: "::",
                    Arg0: Caml_option.valFromOption(match),
                    Arg1: aux(i + 1 | 0, us, vs)
                  };
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
      } else if (ys !== "[]") {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Ext_list_test.filter_map2i"
            ];
      } else {
        return "[]";
      }
    };
  };
  return aux(0, xs, ys);
}

function rev_map_append(f, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1 !== "[]") {
      _l2 = /* constructor */{
        tag: "::",
        Arg0: Curry._1(f, l1.Arg0),
        Arg1: l2
      };
      _l1 = l1.Arg1;
      continue ;
    } else {
      return l2;
    }
  };
}

function flat_map2(f, lx, ly) {
  var _acc = "[]";
  var _lx = lx;
  var _ly = ly;
  while(true) {
    var ly$1 = _ly;
    var lx$1 = _lx;
    var acc = _acc;
    if (lx$1 !== "[]") {
      if (ly$1 !== "[]") {
        _ly = ly$1.Arg1;
        _lx = lx$1.Arg1;
        _acc = List.rev_append(Curry._2(f, lx$1.Arg0, ly$1.Arg0), acc);
        continue ;
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Ext_list_test.flat_map2"
            ];
      }
    } else {
      if (ly$1 !== "[]") {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Ext_list_test.flat_map2"
            ];
      }
      return List.rev(acc);
    }
  };
}

function flat_map_aux(f, _acc, append, _lx) {
  while(true) {
    var lx = _lx;
    var acc = _acc;
    if (lx !== "[]") {
      _lx = lx.Arg1;
      _acc = List.rev_append(Curry._1(f, lx.Arg0), acc);
      continue ;
    } else {
      return List.rev_append(acc, append);
    }
  };
}

function flat_map(f, lx) {
  return flat_map_aux(f, "[]", "[]", lx);
}

function flat_map_acc(f, append, lx) {
  return flat_map_aux(f, "[]", append, lx);
}

function map2_last(f, l1, l2) {
  if (l1 !== "[]") {
    var l1$1 = l1.Arg1;
    var u = l1.Arg0;
    if (l1$1 === "[]") {
      if (l2 !== "[]") {
        if (l2.Arg1 === "[]") {
          return /* constructor */{
                  tag: "::",
                  Arg0: Curry._3(f, true, u, l2.Arg0),
                  Arg1: "[]"
                };
        }
        
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "List.map2_last"
            ];
      }
    }
    if (l2 !== "[]") {
      var r = Curry._3(f, false, u, l2.Arg0);
      return /* constructor */{
              tag: "::",
              Arg0: r,
              Arg1: map2_last(f, l1$1, l2.Arg1)
            };
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "List.map2_last"
          ];
    }
  } else if (l2 !== "[]") {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "List.map2_last"
        ];
  } else {
    return "[]";
  }
}

function map_last(f, l1) {
  if (l1 !== "[]") {
    var l1$1 = l1.Arg1;
    var u = l1.Arg0;
    if (l1$1 !== "[]") {
      var r = Curry._2(f, false, u);
      return /* constructor */{
              tag: "::",
              Arg0: r,
              Arg1: map_last(f, l1$1)
            };
    } else {
      return /* constructor */{
              tag: "::",
              Arg0: Curry._2(f, true, u),
              Arg1: "[]"
            };
    }
  } else {
    return "[]";
  }
}

function fold_right2_last(f, l1, l2, accu) {
  if (l1 !== "[]") {
    var l1$1 = l1.Arg1;
    var last1 = l1.Arg0;
    if (l1$1 === "[]") {
      if (l2 !== "[]") {
        if (l2.Arg1 === "[]") {
          return Curry._4(f, true, last1, l2.Arg0, accu);
        }
        
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "List.fold_right2"
            ];
      }
    }
    if (l2 !== "[]") {
      return Curry._4(f, false, last1, l2.Arg0, fold_right2_last(f, l1$1, l2.Arg1, accu));
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "List.fold_right2"
          ];
    }
  } else {
    if (l2 !== "[]") {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "List.fold_right2"
          ];
    }
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
  }
  return /* tuple */[
          $$Array.to_list($$Array.sub(arr, 0, n)),
          $$Array.to_list($$Array.sub(arr, n, arr_length - n | 0))
        ];
}

function try_take(n, l) {
  var arr = $$Array.of_list(l);
  var arr_length = arr.length;
  if (arr_length <= n) {
    return /* tuple */[
            l,
            arr_length,
            "[]"
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
    } else if (l !== "[]") {
      _n = n - 1 | 0;
      _l = l.Arg1;
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
    if (ys !== "[]") {
      if (xs !== "[]") {
        _ys = ys.Arg1;
        _xs = xs.Arg1;
        continue ;
      } else {
        return false;
      }
    } else {
      return length_compare(xs, n) === /* Eq */15500;
    }
  };
}

function exclude_tail(x) {
  var _acc = "[]";
  var _x = x;
  while(true) {
    var x$1 = _x;
    var acc = _acc;
    if (x$1 !== "[]") {
      var ys = x$1.Arg1;
      var x$2 = x$1.Arg0;
      if (ys !== "[]") {
        _x = ys;
        _acc = /* constructor */{
          tag: "::",
          Arg0: x$2,
          Arg1: acc
        };
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
  if (lst !== "[]") {
    return aux(cmp, lst.Arg0, group(cmp, lst.Arg1));
  } else {
    return "[]";
  }
}

function aux(cmp, x, xss) {
  if (xss !== "[]") {
    var ys = xss.Arg1;
    var y = xss.Arg0;
    if (Curry._2(cmp, x, List.hd(y))) {
      return /* constructor */{
              tag: "::",
              Arg0: /* constructor */{
                tag: "::",
                Arg0: x,
                Arg1: y
              },
              Arg1: ys
            };
    } else {
      return /* constructor */{
              tag: "::",
              Arg0: y,
              Arg1: aux(cmp, x, ys)
            };
    }
  } else {
    return /* constructor */{
            tag: "::",
            Arg0: /* constructor */{
              tag: "::",
              Arg0: x,
              Arg1: "[]"
            },
            Arg1: "[]"
          };
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
    }
    if (n === 0) {
      return h;
    } else {
      if (h === "[]") {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Ext_list_test.drop"
            ];
      }
      _h = List.tl(h);
      _n = n - 1 | 0;
      continue ;
    }
  };
}

function find_first_not(p, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      var a = param.Arg0;
      if (Curry._1(p, a)) {
        _param = param.Arg1;
        continue ;
      } else {
        return Caml_option.some(a);
      }
    } else {
      return ;
    }
  };
}

function for_all_opt(p, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      var v = Curry._1(p, param.Arg0);
      if (v !== undefined) {
        return v;
      } else {
        _param = param.Arg1;
        continue ;
      }
    } else {
      return ;
    }
  };
}

function fold(f, l, init) {
  return List.fold_left((function (acc, i) {
                return Curry._2(f, i, init);
              }), init, l);
}

function rev_map_acc(acc, f, l) {
  var _accu = acc;
  var _param = l;
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param !== "[]") {
      _param = param.Arg1;
      _accu = /* constructor */{
        tag: "::",
        Arg0: Curry._1(f, param.Arg0),
        Arg1: accu
      };
      continue ;
    } else {
      return accu;
    }
  };
}

function map_acc(acc, f, l) {
  if (l !== "[]") {
    return /* constructor */{
            tag: "::",
            Arg0: Curry._1(f, l.Arg0),
            Arg1: map_acc(acc, f, l.Arg1)
          };
  } else {
    return acc;
  }
}

function rev_iter(f, xs) {
  if (xs !== "[]") {
    rev_iter(f, xs.Arg1);
    return Curry._1(f, xs.Arg0);
  } else {
    return /* () */0;
  }
}

function for_all2_no_exn(p, _l1, _l2) {
  while(true) {
    var l2 = _l2;
    var l1 = _l1;
    if (l1 !== "[]") {
      if (l2 !== "[]" && Curry._2(p, l1.Arg0, l2.Arg0)) {
        _l2 = l2.Arg1;
        _l1 = l1.Arg1;
        continue ;
      } else {
        return false;
      }
    } else {
      return l2 === "[]";
    }
  };
}

function find_no_exn(p, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      var x = param.Arg0;
      if (Curry._1(p, x)) {
        return Caml_option.some(x);
      } else {
        _param = param.Arg1;
        continue ;
      }
    } else {
      return ;
    }
  };
}

function find_opt(p, _param) {
  while(true) {
    var param = _param;
    if (param !== "[]") {
      var v = Curry._1(p, param.Arg0);
      if (v !== undefined) {
        return v;
      } else {
        _param = param.Arg1;
        continue ;
      }
    } else {
      return ;
    }
  };
}

function split_map(f, xs) {
  var _bs = "[]";
  var _cs = "[]";
  var _xs = xs;
  while(true) {
    var xs$1 = _xs;
    var cs = _cs;
    var bs = _bs;
    if (xs$1 !== "[]") {
      var match = Curry._1(f, xs$1.Arg0);
      _xs = xs$1.Arg1;
      _cs = /* constructor */{
        tag: "::",
        Arg0: match[1],
        Arg1: cs
      };
      _bs = /* constructor */{
        tag: "::",
        Arg0: match[0],
        Arg1: bs
      };
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
  if (match !== "[]") {
    return List.fold_left((function (x, y) {
                  return Curry._2(fn, y, x);
                }), match.Arg0, match.Arg1);
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Ext_list_test.reduce"
        ];
  }
}

function reduce_from_left(fn, lst) {
  if (lst !== "[]") {
    return List.fold_left(fn, lst.Arg0, lst.Arg1);
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Ext_list_test.reduce_from_left"
        ];
  }
}

function create_ref_empty(param) {
  return /* record */[/* contents */"[]"];
}

function ref_top(x) {
  var match = x[0];
  if (match !== "[]") {
    return match.Arg0;
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Ext_list_test.ref_top"
        ];
  }
}

function ref_empty(x) {
  var match = x[0];
  return match === "[]";
}

function ref_push(x, refs) {
  refs[0] = /* constructor */{
    tag: "::",
    Arg0: x,
    Arg1: refs[0]
  };
  return /* () */0;
}

function ref_pop(refs) {
  var match = refs[0];
  if (match !== "[]") {
    refs[0] = match.Arg1;
    return match.Arg0;
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Ext_list_test.ref_pop"
        ];
  }
}

function rev_except_last(xs) {
  var _acc = "[]";
  var _xs = xs;
  while(true) {
    var xs$1 = _xs;
    var acc = _acc;
    if (xs$1 !== "[]") {
      var xs$2 = xs$1.Arg1;
      var x = xs$1.Arg0;
      if (xs$2 !== "[]") {
        _xs = xs$2;
        _acc = /* constructor */{
          tag: "::",
          Arg0: x,
          Arg1: acc
        };
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
    if (xs !== "[]") {
      var tl = xs.Arg1;
      if (tl !== "[]") {
        _xs = tl;
        continue ;
      } else {
        return xs.Arg0;
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
    if (lst !== "[]") {
      var match = lst.Arg0;
      if (match[0] === k) {
        return match[1];
      } else {
        _lst = lst.Arg1;
        continue ;
      }
    } else if (def !== undefined) {
      return Caml_option.valFromOption(def);
    } else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            /* tuple */[
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
    if (lst !== "[]") {
      var match = lst.Arg0;
      if (match[0] === k) {
        return match[1];
      } else {
        _lst = lst.Arg1;
        continue ;
      }
    } else if (def !== undefined) {
      return Caml_option.valFromOption(def);
    } else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            /* tuple */[
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
/* Ext_string_test Not a pure module */
