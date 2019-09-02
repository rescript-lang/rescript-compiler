'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function Make(Ord) {
  var height = function (param) {
    if (param !== "Empty") {
      return param.Arg4;
    } else {
      return 0;
    }
  };
  var create = function (l, x, d, r) {
    var hl = height(l);
    var hr = height(r);
    return /* constructor */{
            tag: "Node",
            Arg0: l,
            Arg1: x,
            Arg2: d,
            Arg3: r,
            Arg4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  };
  var singleton = function (x, d) {
    return /* constructor */{
            tag: "Node",
            Arg0: "Empty",
            Arg1: x,
            Arg2: d,
            Arg3: "Empty",
            Arg4: 1
          };
  };
  var bal = function (l, x, d, r) {
    var hl = l !== "Empty" ? l.Arg4 : 0;
    var hr = r !== "Empty" ? r.Arg4 : 0;
    if (hl > (hr + 2 | 0)) {
      if (l !== "Empty") {
        var lr = l.Arg3;
        var ld = l.Arg2;
        var lv = l.Arg1;
        var ll = l.Arg0;
        if (height(ll) >= height(lr)) {
          return create(ll, lv, ld, create(lr, x, d, r));
        } else if (lr !== "Empty") {
          return create(create(ll, lv, ld, lr.Arg0), lr.Arg1, lr.Arg2, create(lr.Arg3, x, d, r));
        } else {
          throw [
                Caml_builtin_exceptions.invalid_argument,
                "Map.bal"
              ];
        }
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else if (hr > (hl + 2 | 0)) {
      if (r !== "Empty") {
        var rr = r.Arg3;
        var rd = r.Arg2;
        var rv = r.Arg1;
        var rl = r.Arg0;
        if (height(rr) >= height(rl)) {
          return create(create(l, x, d, rl), rv, rd, rr);
        } else if (rl !== "Empty") {
          return create(create(l, x, d, rl.Arg0), rl.Arg1, rl.Arg2, create(rl.Arg3, rv, rd, rr));
        } else {
          throw [
                Caml_builtin_exceptions.invalid_argument,
                "Map.bal"
              ];
        }
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      return /* constructor */{
              tag: "Node",
              Arg0: l,
              Arg1: x,
              Arg2: d,
              Arg3: r,
              Arg4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
            };
    }
  };
  var is_empty = function (param) {
    return param === "Empty";
  };
  var add = function (x, data, param) {
    if (param !== "Empty") {
      var r = param.Arg3;
      var d = param.Arg2;
      var v = param.Arg1;
      var l = param.Arg0;
      var c = Curry._2(Ord.compare, x, v);
      if (c === 0) {
        return /* constructor */{
                tag: "Node",
                Arg0: l,
                Arg1: x,
                Arg2: data,
                Arg3: r,
                Arg4: param.Arg4
              };
      } else if (c < 0) {
        return bal(add(x, data, l), v, d, r);
      } else {
        return bal(l, v, d, add(x, data, r));
      }
    } else {
      return /* constructor */{
              tag: "Node",
              Arg0: "Empty",
              Arg1: x,
              Arg2: data,
              Arg3: "Empty",
              Arg4: 1
            };
    }
  };
  var find = function (x, _param) {
    while(true) {
      var param = _param;
      if (param !== "Empty") {
        var c = Curry._2(Ord.compare, x, param.Arg1);
        if (c === 0) {
          return param.Arg2;
        } else {
          _param = c < 0 ? param.Arg0 : param.Arg3;
          continue ;
        }
      } else {
        throw Caml_builtin_exceptions.not_found;
      }
    };
  };
  var mem = function (x, _param) {
    while(true) {
      var param = _param;
      if (param !== "Empty") {
        var c = Curry._2(Ord.compare, x, param.Arg1);
        if (c === 0) {
          return true;
        } else {
          _param = c < 0 ? param.Arg0 : param.Arg3;
          continue ;
        }
      } else {
        return false;
      }
    };
  };
  var min_binding = function (_param) {
    while(true) {
      var param = _param;
      if (param !== "Empty") {
        var l = param.Arg0;
        if (l !== "Empty") {
          _param = l;
          continue ;
        } else {
          return /* tuple */[
                  param.Arg1,
                  param.Arg2
                ];
        }
      } else {
        throw Caml_builtin_exceptions.not_found;
      }
    };
  };
  var max_binding = function (_param) {
    while(true) {
      var param = _param;
      if (param !== "Empty") {
        var r = param.Arg3;
        if (r !== "Empty") {
          _param = r;
          continue ;
        } else {
          return /* tuple */[
                  param.Arg1,
                  param.Arg2
                ];
        }
      } else {
        throw Caml_builtin_exceptions.not_found;
      }
    };
  };
  var remove_min_binding = function (param) {
    if (param !== "Empty") {
      var l = param.Arg0;
      if (l !== "Empty") {
        return bal(remove_min_binding(l), param.Arg1, param.Arg2, param.Arg3);
      } else {
        return param.Arg3;
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.remove_min_elt"
          ];
    }
  };
  var remove = function (x, param) {
    if (param !== "Empty") {
      var r = param.Arg3;
      var d = param.Arg2;
      var v = param.Arg1;
      var l = param.Arg0;
      var c = Curry._2(Ord.compare, x, v);
      if (c === 0) {
        var t1 = l;
        var t2 = r;
        if (t1 !== "Empty") {
          if (t2 !== "Empty") {
            var match = min_binding(t2);
            return bal(t1, match[0], match[1], remove_min_binding(t2));
          } else {
            return t1;
          }
        } else {
          return t2;
        }
      } else if (c < 0) {
        return bal(remove(x, l), v, d, r);
      } else {
        return bal(l, v, d, remove(x, r));
      }
    } else {
      return "Empty";
    }
  };
  var iter = function (f, _param) {
    while(true) {
      var param = _param;
      if (param !== "Empty") {
        iter(f, param.Arg0);
        Curry._2(f, param.Arg1, param.Arg2);
        _param = param.Arg3;
        continue ;
      } else {
        return /* () */0;
      }
    };
  };
  var map = function (f, param) {
    if (param !== "Empty") {
      var l$prime = map(f, param.Arg0);
      var d$prime = Curry._1(f, param.Arg2);
      var r$prime = map(f, param.Arg3);
      return /* constructor */{
              tag: "Node",
              Arg0: l$prime,
              Arg1: param.Arg1,
              Arg2: d$prime,
              Arg3: r$prime,
              Arg4: param.Arg4
            };
    } else {
      return "Empty";
    }
  };
  var mapi = function (f, param) {
    if (param !== "Empty") {
      var v = param.Arg1;
      var l$prime = mapi(f, param.Arg0);
      var d$prime = Curry._2(f, v, param.Arg2);
      var r$prime = mapi(f, param.Arg3);
      return /* constructor */{
              tag: "Node",
              Arg0: l$prime,
              Arg1: v,
              Arg2: d$prime,
              Arg3: r$prime,
              Arg4: param.Arg4
            };
    } else {
      return "Empty";
    }
  };
  var fold = function (f, _m, _accu) {
    while(true) {
      var accu = _accu;
      var m = _m;
      if (m !== "Empty") {
        _accu = Curry._3(f, m.Arg1, m.Arg2, fold(f, m.Arg0, accu));
        _m = m.Arg3;
        continue ;
      } else {
        return accu;
      }
    };
  };
  var for_all = function (p, _param) {
    while(true) {
      var param = _param;
      if (param !== "Empty") {
        if (Curry._2(p, param.Arg1, param.Arg2) && for_all(p, param.Arg0)) {
          _param = param.Arg3;
          continue ;
        } else {
          return false;
        }
      } else {
        return true;
      }
    };
  };
  var exists = function (p, _param) {
    while(true) {
      var param = _param;
      if (param !== "Empty") {
        if (Curry._2(p, param.Arg1, param.Arg2) || exists(p, param.Arg0)) {
          return true;
        } else {
          _param = param.Arg3;
          continue ;
        }
      } else {
        return false;
      }
    };
  };
  var add_min_binding = function (k, v, param) {
    if (param !== "Empty") {
      return bal(add_min_binding(k, v, param.Arg0), param.Arg1, param.Arg2, param.Arg3);
    } else {
      return singleton(k, v);
    }
  };
  var add_max_binding = function (k, v, param) {
    if (param !== "Empty") {
      return bal(param.Arg0, param.Arg1, param.Arg2, add_max_binding(k, v, param.Arg3));
    } else {
      return singleton(k, v);
    }
  };
  var join = function (l, v, d, r) {
    if (l !== "Empty") {
      if (r !== "Empty") {
        var rh = r.Arg4;
        var lh = l.Arg4;
        if (lh > (rh + 2 | 0)) {
          return bal(l.Arg0, l.Arg1, l.Arg2, join(l.Arg3, v, d, r));
        } else if (rh > (lh + 2 | 0)) {
          return bal(join(l, v, d, r.Arg0), r.Arg1, r.Arg2, r.Arg3);
        } else {
          return create(l, v, d, r);
        }
      } else {
        return add_max_binding(v, d, l);
      }
    } else {
      return add_min_binding(v, d, r);
    }
  };
  var concat = function (t1, t2) {
    if (t1 !== "Empty") {
      if (t2 !== "Empty") {
        var match = min_binding(t2);
        return join(t1, match[0], match[1], remove_min_binding(t2));
      } else {
        return t1;
      }
    } else {
      return t2;
    }
  };
  var concat_or_join = function (t1, v, d, t2) {
    if (d !== undefined) {
      return join(t1, v, Caml_option.valFromOption(d), t2);
    } else {
      return concat(t1, t2);
    }
  };
  var split = function (x, param) {
    if (param !== "Empty") {
      var r = param.Arg3;
      var d = param.Arg2;
      var v = param.Arg1;
      var l = param.Arg0;
      var c = Curry._2(Ord.compare, x, v);
      if (c === 0) {
        return /* tuple */[
                l,
                Caml_option.some(d),
                r
              ];
      } else if (c < 0) {
        var match = split(x, l);
        return /* tuple */[
                match[0],
                match[1],
                join(match[2], v, d, r)
              ];
      } else {
        var match$1 = split(x, r);
        return /* tuple */[
                join(l, v, d, match$1[0]),
                match$1[1],
                match$1[2]
              ];
      }
    } else {
      return /* tuple */[
              "Empty",
              undefined,
              "Empty"
            ];
    }
  };
  var merge = function (f, s1, s2) {
    if (s1 !== "Empty") {
      var v1 = s1.Arg1;
      if (s1.Arg4 >= height(s2)) {
        var match = split(v1, s2);
        return concat_or_join(merge(f, s1.Arg0, match[0]), v1, Curry._3(f, v1, Caml_option.some(s1.Arg2), match[1]), merge(f, s1.Arg3, match[2]));
      }
      
    } else if (s2 === "Empty") {
      return "Empty";
    }
    if (s2 !== "Empty") {
      var v2 = s2.Arg1;
      var match$1 = split(v2, s1);
      return concat_or_join(merge(f, match$1[0], s2.Arg0), v2, Curry._3(f, v2, match$1[1], Caml_option.some(s2.Arg2)), merge(f, match$1[2], s2.Arg3));
    } else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            /* tuple */[
              "inline_map2_test.ml",
              270,
              10
            ]
          ];
    }
  };
  var filter = function (p, param) {
    if (param !== "Empty") {
      var d = param.Arg2;
      var v = param.Arg1;
      var l$prime = filter(p, param.Arg0);
      var pvd = Curry._2(p, v, d);
      var r$prime = filter(p, param.Arg3);
      if (pvd) {
        return join(l$prime, v, d, r$prime);
      } else {
        return concat(l$prime, r$prime);
      }
    } else {
      return "Empty";
    }
  };
  var partition = function (p, param) {
    if (param !== "Empty") {
      var d = param.Arg2;
      var v = param.Arg1;
      var match = partition(p, param.Arg0);
      var lf = match[1];
      var lt = match[0];
      var pvd = Curry._2(p, v, d);
      var match$1 = partition(p, param.Arg3);
      var rf = match$1[1];
      var rt = match$1[0];
      if (pvd) {
        return /* tuple */[
                join(lt, v, d, rt),
                concat(lf, rf)
              ];
      } else {
        return /* tuple */[
                concat(lt, rt),
                join(lf, v, d, rf)
              ];
      }
    } else {
      return /* tuple */[
              "Empty",
              "Empty"
            ];
    }
  };
  var cons_enum = function (_m, _e) {
    while(true) {
      var e = _e;
      var m = _m;
      if (m !== "Empty") {
        _e = /* constructor */{
          tag: "More",
          Arg0: m.Arg1,
          Arg1: m.Arg2,
          Arg2: m.Arg3,
          Arg3: e
        };
        _m = m.Arg0;
        continue ;
      } else {
        return e;
      }
    };
  };
  var compare = function (cmp, m1, m2) {
    var _e1 = cons_enum(m1, "End");
    var _e2 = cons_enum(m2, "End");
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1 !== "End") {
        if (e2 !== "End") {
          var c = Curry._2(Ord.compare, e1.Arg0, e2.Arg0);
          if (c !== 0) {
            return c;
          } else {
            var c$1 = Curry._2(cmp, e1.Arg1, e2.Arg1);
            if (c$1 !== 0) {
              return c$1;
            } else {
              _e2 = cons_enum(e2.Arg2, e2.Arg3);
              _e1 = cons_enum(e1.Arg2, e1.Arg3);
              continue ;
            }
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
  };
  var equal = function (cmp, m1, m2) {
    var _e1 = cons_enum(m1, "End");
    var _e2 = cons_enum(m2, "End");
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1 !== "End") {
        if (e2 !== "End" && Curry._2(Ord.compare, e1.Arg0, e2.Arg0) === 0 && Curry._2(cmp, e1.Arg1, e2.Arg1)) {
          _e2 = cons_enum(e2.Arg2, e2.Arg3);
          _e1 = cons_enum(e1.Arg2, e1.Arg3);
          continue ;
        } else {
          return false;
        }
      } else {
        return e2 === "End";
      }
    };
  };
  var cardinal = function (param) {
    if (param !== "Empty") {
      return (cardinal(param.Arg0) + 1 | 0) + cardinal(param.Arg3) | 0;
    } else {
      return 0;
    }
  };
  var bindings_aux = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (param !== "Empty") {
        _param = param.Arg0;
        _accu = /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            param.Arg1,
            param.Arg2
          ],
          Arg1: bindings_aux(accu, param.Arg3)
        };
        continue ;
      } else {
        return accu;
      }
    };
  };
  var bindings = function (s) {
    return bindings_aux("[]", s);
  };
  return {
          height: height,
          create: create,
          singleton: singleton,
          bal: bal,
          empty: "Empty",
          is_empty: is_empty,
          add: add,
          find: find,
          mem: mem,
          min_binding: min_binding,
          max_binding: max_binding,
          remove_min_binding: remove_min_binding,
          remove: remove,
          iter: iter,
          map: map,
          mapi: mapi,
          fold: fold,
          for_all: for_all,
          exists: exists,
          add_min_binding: add_min_binding,
          add_max_binding: add_max_binding,
          join: join,
          concat: concat,
          concat_or_join: concat_or_join,
          split: split,
          merge: merge,
          filter: filter,
          partition: partition,
          cons_enum: cons_enum,
          compare: compare,
          equal: equal,
          cardinal: cardinal,
          bindings_aux: bindings_aux,
          bindings: bindings,
          choose: min_binding
        };
}

function height(param) {
  if (param !== "Empty") {
    return param.Arg4;
  } else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* constructor */{
          tag: "Node",
          Arg0: l,
          Arg1: x,
          Arg2: d,
          Arg3: r,
          Arg4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function singleton(x, d) {
  return /* constructor */{
          tag: "Node",
          Arg0: "Empty",
          Arg1: x,
          Arg2: d,
          Arg3: "Empty",
          Arg4: 1
        };
}

function bal(l, x, d, r) {
  var hl = l !== "Empty" ? l.Arg4 : 0;
  var hr = r !== "Empty" ? r.Arg4 : 0;
  if (hl > (hr + 2 | 0)) {
    if (l !== "Empty") {
      var lr = l.Arg3;
      var ld = l.Arg2;
      var lv = l.Arg1;
      var ll = l.Arg0;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      } else if (lr !== "Empty") {
        return create(create(ll, lv, ld, lr.Arg0), lr.Arg1, lr.Arg2, create(lr.Arg3, x, d, r));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r !== "Empty") {
      var rr = r.Arg3;
      var rd = r.Arg2;
      var rv = r.Arg1;
      var rl = r.Arg0;
      if (height(rr) >= height(rl)) {
        return create(create(l, x, d, rl), rv, rd, rr);
      } else if (rl !== "Empty") {
        return create(create(l, x, d, rl.Arg0), rl.Arg1, rl.Arg2, create(rl.Arg3, rv, rd, rr));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: l,
            Arg1: x,
            Arg2: d,
            Arg3: r,
            Arg4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
}

function is_empty(param) {
  return param === "Empty";
}

function add(x, data, param) {
  if (param !== "Empty") {
    var r = param.Arg3;
    var d = param.Arg2;
    var v = param.Arg1;
    var l = param.Arg0;
    var c = Caml_primitive.caml_int_compare(x, v);
    if (c === 0) {
      return /* constructor */{
              tag: "Node",
              Arg0: l,
              Arg1: x,
              Arg2: data,
              Arg3: r,
              Arg4: param.Arg4
            };
    } else if (c < 0) {
      return bal(add(x, data, l), v, d, r);
    } else {
      return bal(l, v, d, add(x, data, r));
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: "Empty",
            Arg1: x,
            Arg2: data,
            Arg3: "Empty",
            Arg4: 1
          };
  }
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var c = Caml_primitive.caml_int_compare(x, param.Arg1);
      if (c === 0) {
        return param.Arg2;
      } else {
        _param = c < 0 ? param.Arg0 : param.Arg3;
        continue ;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var c = Caml_primitive.caml_int_compare(x, param.Arg1);
      if (c === 0) {
        return true;
      } else {
        _param = c < 0 ? param.Arg0 : param.Arg3;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function min_binding(_param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var l = param.Arg0;
      if (l !== "Empty") {
        _param = l;
        continue ;
      } else {
        return /* tuple */[
                param.Arg1,
                param.Arg2
              ];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function max_binding(_param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var r = param.Arg3;
      if (r !== "Empty") {
        _param = r;
        continue ;
      } else {
        return /* tuple */[
                param.Arg1,
                param.Arg2
              ];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function remove_min_binding(param) {
  if (param !== "Empty") {
    var l = param.Arg0;
    if (l !== "Empty") {
      return bal(remove_min_binding(l), param.Arg1, param.Arg2, param.Arg3);
    } else {
      return param.Arg3;
    }
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Map.remove_min_elt"
        ];
  }
}

function remove(x, param) {
  if (param !== "Empty") {
    var r = param.Arg3;
    var d = param.Arg2;
    var v = param.Arg1;
    var l = param.Arg0;
    var c = Caml_primitive.caml_int_compare(x, v);
    if (c === 0) {
      var t1 = l;
      var t2 = r;
      if (t1 !== "Empty") {
        if (t2 !== "Empty") {
          var match = min_binding(t2);
          return bal(t1, match[0], match[1], remove_min_binding(t2));
        } else {
          return t1;
        }
      } else {
        return t2;
      }
    } else if (c < 0) {
      return bal(remove(x, l), v, d, r);
    } else {
      return bal(l, v, d, remove(x, r));
    }
  } else {
    return "Empty";
  }
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      iter(f, param.Arg0);
      Curry._2(f, param.Arg1, param.Arg2);
      _param = param.Arg3;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function map(f, param) {
  if (param !== "Empty") {
    var l$prime = map(f, param.Arg0);
    var d$prime = Curry._1(f, param.Arg2);
    var r$prime = map(f, param.Arg3);
    return /* constructor */{
            tag: "Node",
            Arg0: l$prime,
            Arg1: param.Arg1,
            Arg2: d$prime,
            Arg3: r$prime,
            Arg4: param.Arg4
          };
  } else {
    return "Empty";
  }
}

function mapi(f, param) {
  if (param !== "Empty") {
    var v = param.Arg1;
    var l$prime = mapi(f, param.Arg0);
    var d$prime = Curry._2(f, v, param.Arg2);
    var r$prime = mapi(f, param.Arg3);
    return /* constructor */{
            tag: "Node",
            Arg0: l$prime,
            Arg1: v,
            Arg2: d$prime,
            Arg3: r$prime,
            Arg4: param.Arg4
          };
  } else {
    return "Empty";
  }
}

function fold(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (m !== "Empty") {
      _accu = Curry._3(f, m.Arg1, m.Arg2, fold(f, m.Arg0, accu));
      _m = m.Arg3;
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
      if (Curry._2(p, param.Arg1, param.Arg2) && for_all(p, param.Arg0)) {
        _param = param.Arg3;
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
      if (Curry._2(p, param.Arg1, param.Arg2) || exists(p, param.Arg0)) {
        return true;
      } else {
        _param = param.Arg3;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function add_min_binding(k, v, param) {
  if (param !== "Empty") {
    return bal(add_min_binding(k, v, param.Arg0), param.Arg1, param.Arg2, param.Arg3);
  } else {
    return singleton(k, v);
  }
}

function add_max_binding(k, v, param) {
  if (param !== "Empty") {
    return bal(param.Arg0, param.Arg1, param.Arg2, add_max_binding(k, v, param.Arg3));
  } else {
    return singleton(k, v);
  }
}

function join(l, v, d, r) {
  if (l !== "Empty") {
    if (r !== "Empty") {
      var rh = r.Arg4;
      var lh = l.Arg4;
      if (lh > (rh + 2 | 0)) {
        return bal(l.Arg0, l.Arg1, l.Arg2, join(l.Arg3, v, d, r));
      } else if (rh > (lh + 2 | 0)) {
        return bal(join(l, v, d, r.Arg0), r.Arg1, r.Arg2, r.Arg3);
      } else {
        return create(l, v, d, r);
      }
    } else {
      return add_max_binding(v, d, l);
    }
  } else {
    return add_min_binding(v, d, r);
  }
}

function concat(t1, t2) {
  if (t1 !== "Empty") {
    if (t2 !== "Empty") {
      var match = min_binding(t2);
      return join(t1, match[0], match[1], remove_min_binding(t2));
    } else {
      return t1;
    }
  } else {
    return t2;
  }
}

function concat_or_join(t1, v, d, t2) {
  if (d !== undefined) {
    return join(t1, v, Caml_option.valFromOption(d), t2);
  } else {
    return concat(t1, t2);
  }
}

function split(x, param) {
  if (param !== "Empty") {
    var r = param.Arg3;
    var d = param.Arg2;
    var v = param.Arg1;
    var l = param.Arg0;
    var c = Caml_primitive.caml_int_compare(x, v);
    if (c === 0) {
      return /* tuple */[
              l,
              Caml_option.some(d),
              r
            ];
    } else if (c < 0) {
      var match = split(x, l);
      return /* tuple */[
              match[0],
              match[1],
              join(match[2], v, d, r)
            ];
    } else {
      var match$1 = split(x, r);
      return /* tuple */[
              join(l, v, d, match$1[0]),
              match$1[1],
              match$1[2]
            ];
    }
  } else {
    return /* tuple */[
            "Empty",
            undefined,
            "Empty"
          ];
  }
}

function merge(f, s1, s2) {
  if (s1 !== "Empty") {
    var v1 = s1.Arg1;
    if (s1.Arg4 >= height(s2)) {
      var match = split(v1, s2);
      return concat_or_join(merge(f, s1.Arg0, match[0]), v1, Curry._3(f, v1, Caml_option.some(s1.Arg2), match[1]), merge(f, s1.Arg3, match[2]));
    }
    
  } else if (s2 === "Empty") {
    return "Empty";
  }
  if (s2 !== "Empty") {
    var v2 = s2.Arg1;
    var match$1 = split(v2, s1);
    return concat_or_join(merge(f, match$1[0], s2.Arg0), v2, Curry._3(f, v2, match$1[1], Caml_option.some(s2.Arg2)), merge(f, match$1[2], s2.Arg3));
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "inline_map2_test.ml",
            270,
            10
          ]
        ];
  }
}

function filter(p, param) {
  if (param !== "Empty") {
    var d = param.Arg2;
    var v = param.Arg1;
    var l$prime = filter(p, param.Arg0);
    var pvd = Curry._2(p, v, d);
    var r$prime = filter(p, param.Arg3);
    if (pvd) {
      return join(l$prime, v, d, r$prime);
    } else {
      return concat(l$prime, r$prime);
    }
  } else {
    return "Empty";
  }
}

function partition(p, param) {
  if (param !== "Empty") {
    var d = param.Arg2;
    var v = param.Arg1;
    var match = partition(p, param.Arg0);
    var lf = match[1];
    var lt = match[0];
    var pvd = Curry._2(p, v, d);
    var match$1 = partition(p, param.Arg3);
    var rf = match$1[1];
    var rt = match$1[0];
    if (pvd) {
      return /* tuple */[
              join(lt, v, d, rt),
              concat(lf, rf)
            ];
    } else {
      return /* tuple */[
              concat(lt, rt),
              join(lf, v, d, rf)
            ];
    }
  } else {
    return /* tuple */[
            "Empty",
            "Empty"
          ];
  }
}

function cons_enum(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (m !== "Empty") {
      _e = /* constructor */{
        tag: "More",
        Arg0: m.Arg1,
        Arg1: m.Arg2,
        Arg2: m.Arg3,
        Arg3: e
      };
      _m = m.Arg0;
      continue ;
    } else {
      return e;
    }
  };
}

function compare(cmp, m1, m2) {
  var _e1 = cons_enum(m1, "End");
  var _e2 = cons_enum(m2, "End");
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1 !== "End") {
      if (e2 !== "End") {
        var c = Caml_primitive.caml_int_compare(e1.Arg0, e2.Arg0);
        if (c !== 0) {
          return c;
        } else {
          var c$1 = Curry._2(cmp, e1.Arg1, e2.Arg1);
          if (c$1 !== 0) {
            return c$1;
          } else {
            _e2 = cons_enum(e2.Arg2, e2.Arg3);
            _e1 = cons_enum(e1.Arg2, e1.Arg3);
            continue ;
          }
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

function equal(cmp, m1, m2) {
  var _e1 = cons_enum(m1, "End");
  var _e2 = cons_enum(m2, "End");
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1 !== "End") {
      if (e2 !== "End" && e1.Arg0 === e2.Arg0 && Curry._2(cmp, e1.Arg1, e2.Arg1)) {
        _e2 = cons_enum(e2.Arg2, e2.Arg3);
        _e1 = cons_enum(e1.Arg2, e1.Arg3);
        continue ;
      } else {
        return false;
      }
    } else {
      return e2 === "End";
    }
  };
}

function cardinal(param) {
  if (param !== "Empty") {
    return (cardinal(param.Arg0) + 1 | 0) + cardinal(param.Arg3) | 0;
  } else {
    return 0;
  }
}

function bindings_aux(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param !== "Empty") {
      _param = param.Arg0;
      _accu = /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          param.Arg1,
          param.Arg2
        ],
        Arg1: bindings_aux(accu, param.Arg3)
      };
      continue ;
    } else {
      return accu;
    }
  };
}

function bindings(s) {
  return bindings_aux("[]", s);
}

var IntMap = {
  height: height,
  create: create,
  singleton: singleton,
  bal: bal,
  empty: "Empty",
  is_empty: is_empty,
  add: add,
  find: find,
  mem: mem,
  min_binding: min_binding,
  max_binding: max_binding,
  remove_min_binding: remove_min_binding,
  remove: remove,
  iter: iter,
  map: map,
  mapi: mapi,
  fold: fold,
  for_all: for_all,
  exists: exists,
  add_min_binding: add_min_binding,
  add_max_binding: add_max_binding,
  join: join,
  concat: concat,
  concat_or_join: concat_or_join,
  split: split,
  merge: merge,
  filter: filter,
  partition: partition,
  cons_enum: cons_enum,
  compare: compare,
  equal: equal,
  cardinal: cardinal,
  bindings_aux: bindings_aux,
  bindings: bindings,
  choose: min_binding
};

var m = List.fold_left((function (acc, param) {
        return add(param[0], param[1], acc);
      }), "Empty", /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        10,
        /* "a" */97
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          3,
          /* "b" */98
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            7,
            /* "c" */99
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              20,
              /* "d" */100
            ],
            Arg1: "[]"
          }
        }
      }
    });

function height$1(param) {
  if (param !== "Empty") {
    return param.Arg4;
  } else {
    return 0;
  }
}

function create$1(l, x, d, r) {
  var hl = height$1(l);
  var hr = height$1(r);
  return /* constructor */{
          tag: "Node",
          Arg0: l,
          Arg1: x,
          Arg2: d,
          Arg3: r,
          Arg4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function singleton$1(x, d) {
  return /* constructor */{
          tag: "Node",
          Arg0: "Empty",
          Arg1: x,
          Arg2: d,
          Arg3: "Empty",
          Arg4: 1
        };
}

function bal$1(l, x, d, r) {
  var hl = l !== "Empty" ? l.Arg4 : 0;
  var hr = r !== "Empty" ? r.Arg4 : 0;
  if (hl > (hr + 2 | 0)) {
    if (l !== "Empty") {
      var lr = l.Arg3;
      var ld = l.Arg2;
      var lv = l.Arg1;
      var ll = l.Arg0;
      if (height$1(ll) >= height$1(lr)) {
        return create$1(ll, lv, ld, create$1(lr, x, d, r));
      } else if (lr !== "Empty") {
        return create$1(create$1(ll, lv, ld, lr.Arg0), lr.Arg1, lr.Arg2, create$1(lr.Arg3, x, d, r));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r !== "Empty") {
      var rr = r.Arg3;
      var rd = r.Arg2;
      var rv = r.Arg1;
      var rl = r.Arg0;
      if (height$1(rr) >= height$1(rl)) {
        return create$1(create$1(l, x, d, rl), rv, rd, rr);
      } else if (rl !== "Empty") {
        return create$1(create$1(l, x, d, rl.Arg0), rl.Arg1, rl.Arg2, create$1(rl.Arg3, rv, rd, rr));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: l,
            Arg1: x,
            Arg2: d,
            Arg3: r,
            Arg4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
}

function is_empty$1(param) {
  return param === "Empty";
}

function add$1(x, data, param) {
  if (param !== "Empty") {
    var r = param.Arg3;
    var d = param.Arg2;
    var v = param.Arg1;
    var l = param.Arg0;
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return /* constructor */{
              tag: "Node",
              Arg0: l,
              Arg1: x,
              Arg2: data,
              Arg3: r,
              Arg4: param.Arg4
            };
    } else if (c < 0) {
      return bal$1(add$1(x, data, l), v, d, r);
    } else {
      return bal$1(l, v, d, add$1(x, data, r));
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: "Empty",
            Arg1: x,
            Arg2: data,
            Arg3: "Empty",
            Arg4: 1
          };
  }
}

function find$1(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var c = Caml_primitive.caml_string_compare(x, param.Arg1);
      if (c === 0) {
        return param.Arg2;
      } else {
        _param = c < 0 ? param.Arg0 : param.Arg3;
        continue ;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function mem$1(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var c = Caml_primitive.caml_string_compare(x, param.Arg1);
      if (c === 0) {
        return true;
      } else {
        _param = c < 0 ? param.Arg0 : param.Arg3;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function min_binding$1(_param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var l = param.Arg0;
      if (l !== "Empty") {
        _param = l;
        continue ;
      } else {
        return /* tuple */[
                param.Arg1,
                param.Arg2
              ];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function max_binding$1(_param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var r = param.Arg3;
      if (r !== "Empty") {
        _param = r;
        continue ;
      } else {
        return /* tuple */[
                param.Arg1,
                param.Arg2
              ];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function remove_min_binding$1(param) {
  if (param !== "Empty") {
    var l = param.Arg0;
    if (l !== "Empty") {
      return bal$1(remove_min_binding$1(l), param.Arg1, param.Arg2, param.Arg3);
    } else {
      return param.Arg3;
    }
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Map.remove_min_elt"
        ];
  }
}

function remove$1(x, param) {
  if (param !== "Empty") {
    var r = param.Arg3;
    var d = param.Arg2;
    var v = param.Arg1;
    var l = param.Arg0;
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      var t1 = l;
      var t2 = r;
      if (t1 !== "Empty") {
        if (t2 !== "Empty") {
          var match = min_binding$1(t2);
          return bal$1(t1, match[0], match[1], remove_min_binding$1(t2));
        } else {
          return t1;
        }
      } else {
        return t2;
      }
    } else if (c < 0) {
      return bal$1(remove$1(x, l), v, d, r);
    } else {
      return bal$1(l, v, d, remove$1(x, r));
    }
  } else {
    return "Empty";
  }
}

function iter$1(f, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      iter$1(f, param.Arg0);
      Curry._2(f, param.Arg1, param.Arg2);
      _param = param.Arg3;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function map$1(f, param) {
  if (param !== "Empty") {
    var l$prime = map$1(f, param.Arg0);
    var d$prime = Curry._1(f, param.Arg2);
    var r$prime = map$1(f, param.Arg3);
    return /* constructor */{
            tag: "Node",
            Arg0: l$prime,
            Arg1: param.Arg1,
            Arg2: d$prime,
            Arg3: r$prime,
            Arg4: param.Arg4
          };
  } else {
    return "Empty";
  }
}

function mapi$1(f, param) {
  if (param !== "Empty") {
    var v = param.Arg1;
    var l$prime = mapi$1(f, param.Arg0);
    var d$prime = Curry._2(f, v, param.Arg2);
    var r$prime = mapi$1(f, param.Arg3);
    return /* constructor */{
            tag: "Node",
            Arg0: l$prime,
            Arg1: v,
            Arg2: d$prime,
            Arg3: r$prime,
            Arg4: param.Arg4
          };
  } else {
    return "Empty";
  }
}

function fold$1(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (m !== "Empty") {
      _accu = Curry._3(f, m.Arg1, m.Arg2, fold$1(f, m.Arg0, accu));
      _m = m.Arg3;
      continue ;
    } else {
      return accu;
    }
  };
}

function for_all$1(p, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      if (Curry._2(p, param.Arg1, param.Arg2) && for_all$1(p, param.Arg0)) {
        _param = param.Arg3;
        continue ;
      } else {
        return false;
      }
    } else {
      return true;
    }
  };
}

function exists$1(p, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      if (Curry._2(p, param.Arg1, param.Arg2) || exists$1(p, param.Arg0)) {
        return true;
      } else {
        _param = param.Arg3;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function add_min_binding$1(k, v, param) {
  if (param !== "Empty") {
    return bal$1(add_min_binding$1(k, v, param.Arg0), param.Arg1, param.Arg2, param.Arg3);
  } else {
    return singleton$1(k, v);
  }
}

function add_max_binding$1(k, v, param) {
  if (param !== "Empty") {
    return bal$1(param.Arg0, param.Arg1, param.Arg2, add_max_binding$1(k, v, param.Arg3));
  } else {
    return singleton$1(k, v);
  }
}

function join$1(l, v, d, r) {
  if (l !== "Empty") {
    if (r !== "Empty") {
      var rh = r.Arg4;
      var lh = l.Arg4;
      if (lh > (rh + 2 | 0)) {
        return bal$1(l.Arg0, l.Arg1, l.Arg2, join$1(l.Arg3, v, d, r));
      } else if (rh > (lh + 2 | 0)) {
        return bal$1(join$1(l, v, d, r.Arg0), r.Arg1, r.Arg2, r.Arg3);
      } else {
        return create$1(l, v, d, r);
      }
    } else {
      return add_max_binding$1(v, d, l);
    }
  } else {
    return add_min_binding$1(v, d, r);
  }
}

function concat$1(t1, t2) {
  if (t1 !== "Empty") {
    if (t2 !== "Empty") {
      var match = min_binding$1(t2);
      return join$1(t1, match[0], match[1], remove_min_binding$1(t2));
    } else {
      return t1;
    }
  } else {
    return t2;
  }
}

function concat_or_join$1(t1, v, d, t2) {
  if (d !== undefined) {
    return join$1(t1, v, Caml_option.valFromOption(d), t2);
  } else {
    return concat$1(t1, t2);
  }
}

function split$1(x, param) {
  if (param !== "Empty") {
    var r = param.Arg3;
    var d = param.Arg2;
    var v = param.Arg1;
    var l = param.Arg0;
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return /* tuple */[
              l,
              Caml_option.some(d),
              r
            ];
    } else if (c < 0) {
      var match = split$1(x, l);
      return /* tuple */[
              match[0],
              match[1],
              join$1(match[2], v, d, r)
            ];
    } else {
      var match$1 = split$1(x, r);
      return /* tuple */[
              join$1(l, v, d, match$1[0]),
              match$1[1],
              match$1[2]
            ];
    }
  } else {
    return /* tuple */[
            "Empty",
            undefined,
            "Empty"
          ];
  }
}

function merge$1(f, s1, s2) {
  if (s1 !== "Empty") {
    var v1 = s1.Arg1;
    if (s1.Arg4 >= height$1(s2)) {
      var match = split$1(v1, s2);
      return concat_or_join$1(merge$1(f, s1.Arg0, match[0]), v1, Curry._3(f, v1, Caml_option.some(s1.Arg2), match[1]), merge$1(f, s1.Arg3, match[2]));
    }
    
  } else if (s2 === "Empty") {
    return "Empty";
  }
  if (s2 !== "Empty") {
    var v2 = s2.Arg1;
    var match$1 = split$1(v2, s1);
    return concat_or_join$1(merge$1(f, match$1[0], s2.Arg0), v2, Curry._3(f, v2, match$1[1], Caml_option.some(s2.Arg2)), merge$1(f, match$1[2], s2.Arg3));
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "inline_map2_test.ml",
            270,
            10
          ]
        ];
  }
}

function filter$1(p, param) {
  if (param !== "Empty") {
    var d = param.Arg2;
    var v = param.Arg1;
    var l$prime = filter$1(p, param.Arg0);
    var pvd = Curry._2(p, v, d);
    var r$prime = filter$1(p, param.Arg3);
    if (pvd) {
      return join$1(l$prime, v, d, r$prime);
    } else {
      return concat$1(l$prime, r$prime);
    }
  } else {
    return "Empty";
  }
}

function partition$1(p, param) {
  if (param !== "Empty") {
    var d = param.Arg2;
    var v = param.Arg1;
    var match = partition$1(p, param.Arg0);
    var lf = match[1];
    var lt = match[0];
    var pvd = Curry._2(p, v, d);
    var match$1 = partition$1(p, param.Arg3);
    var rf = match$1[1];
    var rt = match$1[0];
    if (pvd) {
      return /* tuple */[
              join$1(lt, v, d, rt),
              concat$1(lf, rf)
            ];
    } else {
      return /* tuple */[
              concat$1(lt, rt),
              join$1(lf, v, d, rf)
            ];
    }
  } else {
    return /* tuple */[
            "Empty",
            "Empty"
          ];
  }
}

function cons_enum$1(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (m !== "Empty") {
      _e = /* constructor */{
        tag: "More",
        Arg0: m.Arg1,
        Arg1: m.Arg2,
        Arg2: m.Arg3,
        Arg3: e
      };
      _m = m.Arg0;
      continue ;
    } else {
      return e;
    }
  };
}

function compare$1(cmp, m1, m2) {
  var _e1 = cons_enum$1(m1, "End");
  var _e2 = cons_enum$1(m2, "End");
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1 !== "End") {
      if (e2 !== "End") {
        var c = Caml_primitive.caml_string_compare(e1.Arg0, e2.Arg0);
        if (c !== 0) {
          return c;
        } else {
          var c$1 = Curry._2(cmp, e1.Arg1, e2.Arg1);
          if (c$1 !== 0) {
            return c$1;
          } else {
            _e2 = cons_enum$1(e2.Arg2, e2.Arg3);
            _e1 = cons_enum$1(e1.Arg2, e1.Arg3);
            continue ;
          }
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

function equal$1(cmp, m1, m2) {
  var _e1 = cons_enum$1(m1, "End");
  var _e2 = cons_enum$1(m2, "End");
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1 !== "End") {
      if (e2 !== "End" && Caml_primitive.caml_string_compare(e1.Arg0, e2.Arg0) === 0 && Curry._2(cmp, e1.Arg1, e2.Arg1)) {
        _e2 = cons_enum$1(e2.Arg2, e2.Arg3);
        _e1 = cons_enum$1(e1.Arg2, e1.Arg3);
        continue ;
      } else {
        return false;
      }
    } else {
      return e2 === "End";
    }
  };
}

function cardinal$1(param) {
  if (param !== "Empty") {
    return (cardinal$1(param.Arg0) + 1 | 0) + cardinal$1(param.Arg3) | 0;
  } else {
    return 0;
  }
}

function bindings_aux$1(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param !== "Empty") {
      _param = param.Arg0;
      _accu = /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          param.Arg1,
          param.Arg2
        ],
        Arg1: bindings_aux$1(accu, param.Arg3)
      };
      continue ;
    } else {
      return accu;
    }
  };
}

function bindings$1(s) {
  return bindings_aux$1("[]", s);
}

var SMap = {
  height: height$1,
  create: create$1,
  singleton: singleton$1,
  bal: bal$1,
  empty: "Empty",
  is_empty: is_empty$1,
  add: add$1,
  find: find$1,
  mem: mem$1,
  min_binding: min_binding$1,
  max_binding: max_binding$1,
  remove_min_binding: remove_min_binding$1,
  remove: remove$1,
  iter: iter$1,
  map: map$1,
  mapi: mapi$1,
  fold: fold$1,
  for_all: for_all$1,
  exists: exists$1,
  add_min_binding: add_min_binding$1,
  add_max_binding: add_max_binding$1,
  join: join$1,
  concat: concat$1,
  concat_or_join: concat_or_join$1,
  split: split$1,
  merge: merge$1,
  filter: filter$1,
  partition: partition$1,
  cons_enum: cons_enum$1,
  compare: compare$1,
  equal: equal$1,
  cardinal: cardinal$1,
  bindings_aux: bindings_aux$1,
  bindings: bindings$1,
  choose: min_binding$1
};

var s = List.fold_left((function (acc, param) {
        return add$1(param[0], param[1], acc);
      }), "Empty", /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "10",
        /* "a" */97
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "3",
          /* "b" */98
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "7",
            /* "c" */99
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "20",
              /* "d" */100
            ],
            Arg1: "[]"
          }
        }
      }
    });

Mt.from_pair_suites("Inline_map2_test", /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "assertion1",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: find(10, m),
                    Arg1: /* "a" */97
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "assertion2",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: find$1("10", s),
                      Arg1: /* "a" */97
                    };
            })
        ],
        Arg1: "[]"
      }
    });

var empty = "Empty";

exports.Make = Make;
exports.IntMap = IntMap;
exports.empty = empty;
exports.m = m;
exports.SMap = SMap;
exports.s = s;
/* m Not a pure module */
