'use strict';

var Curry = require("./curry.js");
var Caml_option = require("./caml_option.js");

function Make(funarg) {
  var height = function (param) {
    if (typeof param !== "object") {
      return 0;
    } else {
      return param.h;
    }
  };
  var create = function (l, x, d, r) {
    var hl = height(l);
    var hr = height(r);
    return {
            TAG: "Node",
            l: l,
            v: x,
            d: d,
            r: r,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  };
  var singleton = function (x, d) {
    return {
            TAG: "Node",
            l: "Empty",
            v: x,
            d: d,
            r: "Empty",
            h: 1
          };
  };
  var bal = function (l, x, d, r) {
    var hl;
    hl = typeof l !== "object" ? 0 : l.h;
    var hr;
    hr = typeof r !== "object" ? 0 : r.h;
    if (hl > (hr + 2 | 0)) {
      if (typeof l !== "object") {
        throw {
              RE_EXN_ID: "Invalid_argument",
              _1: "Map.bal",
              Error: new Error()
            };
      }
      var lr = l.r;
      var ld = l.d;
      var lv = l.v;
      var ll = l.l;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      }
      if (typeof lr === "object") {
        return create(create(ll, lv, ld, lr.l), lr.v, lr.d, create(lr.r, x, d, r));
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal",
            Error: new Error()
          };
    }
    if (hr <= (hl + 2 | 0)) {
      return {
              TAG: "Node",
              l: l,
              v: x,
              d: d,
              r: r,
              h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
            };
    }
    if (typeof r !== "object") {
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal",
            Error: new Error()
          };
    }
    var rr = r.r;
    var rd = r.d;
    var rv = r.v;
    var rl = r.l;
    if (height(rr) >= height(rl)) {
      return create(create(l, x, d, rl), rv, rd, rr);
    }
    if (typeof rl === "object") {
      return create(create(l, x, d, rl.l), rl.v, rl.d, create(rl.r, rv, rd, rr));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  };
  var is_empty = function (param) {
    if (typeof param !== "object") {
      return true;
    } else {
      return false;
    }
  };
  var add = function (x, data, param) {
    if (typeof param !== "object") {
      return {
              TAG: "Node",
              l: "Empty",
              v: x,
              d: data,
              r: "Empty",
              h: 1
            };
    }
    var r = param.r;
    var d = param.d;
    var v = param.v;
    var l = param.l;
    var c = Curry._2(funarg.compare, x, v);
    if (c === 0) {
      if (d === data) {
        return param;
      } else {
        return {
                TAG: "Node",
                l: l,
                v: x,
                d: data,
                r: r,
                h: param.h
              };
      }
    }
    if (c < 0) {
      var ll = add(x, data, l);
      if (l === ll) {
        return param;
      } else {
        return bal(ll, v, d, r);
      }
    }
    var rr = add(x, data, r);
    if (r === rr) {
      return param;
    } else {
      return bal(l, v, d, rr);
    }
  };
  var find = function (x, _param) {
    while(true) {
      var param = _param;
      if (typeof param !== "object") {
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
      }
      var c = Curry._2(funarg.compare, x, param.v);
      if (c === 0) {
        return param.d;
      }
      _param = c < 0 ? param.l : param.r;
      continue ;
    };
  };
  var find_first = function (f, _param) {
    while(true) {
      var param = _param;
      if (typeof param !== "object") {
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
      }
      var v = param.v;
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _d0 = param.d;
        var _param$1 = param.l;
        while(true) {
          var param$1 = _param$1;
          var d0 = _d0;
          var v0 = _v0;
          if (typeof param$1 !== "object") {
            return [
                    v0,
                    d0
                  ];
          }
          var v$1 = param$1.v;
          if (Curry._1(f, v$1)) {
            _param$1 = param$1.l;
            _d0 = param$1.d;
            _v0 = v$1;
            continue ;
          }
          _param$1 = param$1.r;
          continue ;
        };
      }
      _param = param.r;
      continue ;
    };
  };
  var find_first_opt = function (f, _param) {
    while(true) {
      var param = _param;
      if (typeof param !== "object") {
        return ;
      }
      var v = param.v;
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _d0 = param.d;
        var _param$1 = param.l;
        while(true) {
          var param$1 = _param$1;
          var d0 = _d0;
          var v0 = _v0;
          if (typeof param$1 !== "object") {
            return [
                    v0,
                    d0
                  ];
          }
          var v$1 = param$1.v;
          if (Curry._1(f, v$1)) {
            _param$1 = param$1.l;
            _d0 = param$1.d;
            _v0 = v$1;
            continue ;
          }
          _param$1 = param$1.r;
          continue ;
        };
      }
      _param = param.r;
      continue ;
    };
  };
  var find_last = function (f, _param) {
    while(true) {
      var param = _param;
      if (typeof param !== "object") {
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
      }
      var v = param.v;
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _d0 = param.d;
        var _param$1 = param.r;
        while(true) {
          var param$1 = _param$1;
          var d0 = _d0;
          var v0 = _v0;
          if (typeof param$1 !== "object") {
            return [
                    v0,
                    d0
                  ];
          }
          var v$1 = param$1.v;
          if (Curry._1(f, v$1)) {
            _param$1 = param$1.r;
            _d0 = param$1.d;
            _v0 = v$1;
            continue ;
          }
          _param$1 = param$1.l;
          continue ;
        };
      }
      _param = param.l;
      continue ;
    };
  };
  var find_last_opt = function (f, _param) {
    while(true) {
      var param = _param;
      if (typeof param !== "object") {
        return ;
      }
      var v = param.v;
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _d0 = param.d;
        var _param$1 = param.r;
        while(true) {
          var param$1 = _param$1;
          var d0 = _d0;
          var v0 = _v0;
          if (typeof param$1 !== "object") {
            return [
                    v0,
                    d0
                  ];
          }
          var v$1 = param$1.v;
          if (Curry._1(f, v$1)) {
            _param$1 = param$1.r;
            _d0 = param$1.d;
            _v0 = v$1;
            continue ;
          }
          _param$1 = param$1.l;
          continue ;
        };
      }
      _param = param.l;
      continue ;
    };
  };
  var find_opt = function (x, _param) {
    while(true) {
      var param = _param;
      if (typeof param !== "object") {
        return ;
      }
      var c = Curry._2(funarg.compare, x, param.v);
      if (c === 0) {
        return Caml_option.some(param.d);
      }
      _param = c < 0 ? param.l : param.r;
      continue ;
    };
  };
  var mem = function (x, _param) {
    while(true) {
      var param = _param;
      if (typeof param !== "object") {
        return false;
      }
      var c = Curry._2(funarg.compare, x, param.v);
      if (c === 0) {
        return true;
      }
      _param = c < 0 ? param.l : param.r;
      continue ;
    };
  };
  var min_binding = function (_param) {
    while(true) {
      var param = _param;
      if (typeof param !== "object") {
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
      }
      var l = param.l;
      if (typeof l !== "object") {
        return [
                param.v,
                param.d
              ];
      }
      _param = l;
      continue ;
    };
  };
  var min_binding_opt = function (_param) {
    while(true) {
      var param = _param;
      if (typeof param !== "object") {
        return ;
      }
      var l = param.l;
      if (typeof l !== "object") {
        return [
                param.v,
                param.d
              ];
      }
      _param = l;
      continue ;
    };
  };
  var max_binding = function (_param) {
    while(true) {
      var param = _param;
      if (typeof param !== "object") {
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
      }
      var r = param.r;
      if (typeof r !== "object") {
        return [
                param.v,
                param.d
              ];
      }
      _param = r;
      continue ;
    };
  };
  var max_binding_opt = function (_param) {
    while(true) {
      var param = _param;
      if (typeof param !== "object") {
        return ;
      }
      var r = param.r;
      if (typeof r !== "object") {
        return [
                param.v,
                param.d
              ];
      }
      _param = r;
      continue ;
    };
  };
  var remove_min_binding = function (param) {
    if (typeof param !== "object") {
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.remove_min_elt",
            Error: new Error()
          };
    }
    var l = param.l;
    if (typeof l !== "object") {
      return param.r;
    } else {
      return bal(remove_min_binding(l), param.v, param.d, param.r);
    }
  };
  var merge = function (t1, t2) {
    if (typeof t1 !== "object") {
      return t2;
    }
    if (typeof t2 !== "object") {
      return t1;
    }
    var match = min_binding(t2);
    return bal(t1, match[0], match[1], remove_min_binding(t2));
  };
  var remove = function (x, param) {
    if (typeof param !== "object") {
      return "Empty";
    }
    var r = param.r;
    var d = param.d;
    var v = param.v;
    var l = param.l;
    var c = Curry._2(funarg.compare, x, v);
    if (c === 0) {
      return merge(l, r);
    }
    if (c < 0) {
      var ll = remove(x, l);
      if (l === ll) {
        return param;
      } else {
        return bal(ll, v, d, r);
      }
    }
    var rr = remove(x, r);
    if (r === rr) {
      return param;
    } else {
      return bal(l, v, d, rr);
    }
  };
  var update = function (x, f, param) {
    if (typeof param !== "object") {
      var data = Curry._1(f, undefined);
      if (data !== undefined) {
        return {
                TAG: "Node",
                l: "Empty",
                v: x,
                d: Caml_option.valFromOption(data),
                r: "Empty",
                h: 1
              };
      } else {
        return "Empty";
      }
    }
    var r = param.r;
    var d = param.d;
    var v = param.v;
    var l = param.l;
    var c = Curry._2(funarg.compare, x, v);
    if (c === 0) {
      var data$1 = Curry._1(f, Caml_option.some(d));
      if (data$1 === undefined) {
        return merge(l, r);
      }
      var data$2 = Caml_option.valFromOption(data$1);
      if (d === data$2) {
        return param;
      } else {
        return {
                TAG: "Node",
                l: l,
                v: x,
                d: data$2,
                r: r,
                h: param.h
              };
      }
    }
    if (c < 0) {
      var ll = update(x, f, l);
      if (l === ll) {
        return param;
      } else {
        return bal(ll, v, d, r);
      }
    }
    var rr = update(x, f, r);
    if (r === rr) {
      return param;
    } else {
      return bal(l, v, d, rr);
    }
  };
  var iter = function (f, _param) {
    while(true) {
      var param = _param;
      if (typeof param !== "object") {
        return ;
      }
      iter(f, param.l);
      Curry._2(f, param.v, param.d);
      _param = param.r;
      continue ;
    };
  };
  var map = function (f, param) {
    if (typeof param !== "object") {
      return "Empty";
    }
    var l$p = map(f, param.l);
    var d$p = Curry._1(f, param.d);
    var r$p = map(f, param.r);
    return {
            TAG: "Node",
            l: l$p,
            v: param.v,
            d: d$p,
            r: r$p,
            h: param.h
          };
  };
  var mapi = function (f, param) {
    if (typeof param !== "object") {
      return "Empty";
    }
    var v = param.v;
    var l$p = mapi(f, param.l);
    var d$p = Curry._2(f, v, param.d);
    var r$p = mapi(f, param.r);
    return {
            TAG: "Node",
            l: l$p,
            v: v,
            d: d$p,
            r: r$p,
            h: param.h
          };
  };
  var fold = function (f, _m, _accu) {
    while(true) {
      var accu = _accu;
      var m = _m;
      if (typeof m !== "object") {
        return accu;
      }
      _accu = Curry._3(f, m.v, m.d, fold(f, m.l, accu));
      _m = m.r;
      continue ;
    };
  };
  var for_all = function (p, _param) {
    while(true) {
      var param = _param;
      if (typeof param !== "object") {
        return true;
      }
      if (!Curry._2(p, param.v, param.d)) {
        return false;
      }
      if (!for_all(p, param.l)) {
        return false;
      }
      _param = param.r;
      continue ;
    };
  };
  var exists = function (p, _param) {
    while(true) {
      var param = _param;
      if (typeof param !== "object") {
        return false;
      }
      if (Curry._2(p, param.v, param.d)) {
        return true;
      }
      if (exists(p, param.l)) {
        return true;
      }
      _param = param.r;
      continue ;
    };
  };
  var add_min_binding = function (k, x, param) {
    if (typeof param !== "object") {
      return singleton(k, x);
    } else {
      return bal(add_min_binding(k, x, param.l), param.v, param.d, param.r);
    }
  };
  var add_max_binding = function (k, x, param) {
    if (typeof param !== "object") {
      return singleton(k, x);
    } else {
      return bal(param.l, param.v, param.d, add_max_binding(k, x, param.r));
    }
  };
  var join = function (l, v, d, r) {
    if (typeof l !== "object") {
      return add_min_binding(v, d, r);
    }
    var lh = l.h;
    if (typeof r !== "object") {
      return add_max_binding(v, d, l);
    }
    var rh = r.h;
    if (lh > (rh + 2 | 0)) {
      return bal(l.l, l.v, l.d, join(l.r, v, d, r));
    } else if (rh > (lh + 2 | 0)) {
      return bal(join(l, v, d, r.l), r.v, r.d, r.r);
    } else {
      return create(l, v, d, r);
    }
  };
  var concat = function (t1, t2) {
    if (typeof t1 !== "object") {
      return t2;
    }
    if (typeof t2 !== "object") {
      return t1;
    }
    var match = min_binding(t2);
    return join(t1, match[0], match[1], remove_min_binding(t2));
  };
  var concat_or_join = function (t1, v, d, t2) {
    if (d !== undefined) {
      return join(t1, v, Caml_option.valFromOption(d), t2);
    } else {
      return concat(t1, t2);
    }
  };
  var split = function (x, param) {
    if (typeof param !== "object") {
      return [
              "Empty",
              undefined,
              "Empty"
            ];
    }
    var r = param.r;
    var d = param.d;
    var v = param.v;
    var l = param.l;
    var c = Curry._2(funarg.compare, x, v);
    if (c === 0) {
      return [
              l,
              Caml_option.some(d),
              r
            ];
    }
    if (c < 0) {
      var match = split(x, l);
      return [
              match[0],
              match[1],
              join(match[2], v, d, r)
            ];
    }
    var match$1 = split(x, r);
    return [
            join(l, v, d, match$1[0]),
            match$1[1],
            match$1[2]
          ];
  };
  var merge$1 = function (f, s1, s2) {
    if (typeof s1 !== "object") {
      if (typeof s2 !== "object") {
        return "Empty";
      }
      
    } else {
      var v1 = s1.v;
      if (s1.h >= height(s2)) {
        var match = split(v1, s2);
        return concat_or_join(merge$1(f, s1.l, match[0]), v1, Curry._3(f, v1, Caml_option.some(s1.d), match[1]), merge$1(f, s1.r, match[2]));
      }
      
    }
    if (typeof s2 !== "object") {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "map.res",
              552,
              11
            ],
            Error: new Error()
          };
    }
    var v2 = s2.v;
    var match$1 = split(v2, s1);
    return concat_or_join(merge$1(f, match$1[0], s2.l), v2, Curry._3(f, v2, match$1[1], Caml_option.some(s2.d)), merge$1(f, match$1[2], s2.r));
  };
  var union = function (f, s1, s2) {
    if (typeof s1 !== "object") {
      return s2;
    }
    var d1 = s1.d;
    var v1 = s1.v;
    if (typeof s2 !== "object") {
      return s1;
    }
    var d2 = s2.d;
    var v2 = s2.v;
    if (s1.h >= s2.h) {
      var match = split(v1, s2);
      var d2$1 = match[1];
      var l = union(f, s1.l, match[0]);
      var r = union(f, s1.r, match[2]);
      if (d2$1 !== undefined) {
        return concat_or_join(l, v1, Curry._3(f, v1, d1, Caml_option.valFromOption(d2$1)), r);
      } else {
        return join(l, v1, d1, r);
      }
    }
    var match$1 = split(v2, s1);
    var d1$1 = match$1[1];
    var l$1 = union(f, match$1[0], s2.l);
    var r$1 = union(f, match$1[2], s2.r);
    if (d1$1 !== undefined) {
      return concat_or_join(l$1, v2, Curry._3(f, v2, Caml_option.valFromOption(d1$1), d2), r$1);
    } else {
      return join(l$1, v2, d2, r$1);
    }
  };
  var filter = function (p, param) {
    if (typeof param !== "object") {
      return "Empty";
    }
    var r = param.r;
    var d = param.d;
    var v = param.v;
    var l = param.l;
    var l$p = filter(p, l);
    var pvd = Curry._2(p, v, d);
    var r$p = filter(p, r);
    if (pvd) {
      if (l === l$p && r === r$p) {
        return param;
      } else {
        return join(l$p, v, d, r$p);
      }
    } else {
      return concat(l$p, r$p);
    }
  };
  var partition = function (p, param) {
    if (typeof param !== "object") {
      return [
              "Empty",
              "Empty"
            ];
    }
    var d = param.d;
    var v = param.v;
    var match = partition(p, param.l);
    var lf = match[1];
    var lt = match[0];
    var pvd = Curry._2(p, v, d);
    var match$1 = partition(p, param.r);
    var rf = match$1[1];
    var rt = match$1[0];
    if (pvd) {
      return [
              join(lt, v, d, rt),
              concat(lf, rf)
            ];
    } else {
      return [
              concat(lt, rt),
              join(lf, v, d, rf)
            ];
    }
  };
  var cons_enum = function (_m, _e) {
    while(true) {
      var e = _e;
      var m = _m;
      if (typeof m !== "object") {
        return e;
      }
      _e = {
        TAG: "More",
        _0: m.v,
        _1: m.d,
        _2: m.r,
        _3: e
      };
      _m = m.l;
      continue ;
    };
  };
  var compare = function (cmp, m1, m2) {
    var _e1 = cons_enum(m1, "End");
    var _e2 = cons_enum(m2, "End");
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
      var c = Curry._2(funarg.compare, e1._0, e2._0);
      if (c !== 0) {
        return c;
      }
      var c$1 = Curry._2(cmp, e1._1, e2._1);
      if (c$1 !== 0) {
        return c$1;
      }
      _e2 = cons_enum(e2._2, e2._3);
      _e1 = cons_enum(e1._2, e1._3);
      continue ;
    };
  };
  var equal = function (cmp, m1, m2) {
    var _e1 = cons_enum(m1, "End");
    var _e2 = cons_enum(m2, "End");
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (typeof e1 !== "object") {
        if (typeof e2 !== "object") {
          return true;
        } else {
          return false;
        }
      }
      if (typeof e2 !== "object") {
        return false;
      }
      if (Curry._2(funarg.compare, e1._0, e2._0) !== 0) {
        return false;
      }
      if (!Curry._2(cmp, e1._1, e2._1)) {
        return false;
      }
      _e2 = cons_enum(e2._2, e2._3);
      _e1 = cons_enum(e1._2, e1._3);
      continue ;
    };
  };
  var cardinal = function (param) {
    if (typeof param !== "object") {
      return 0;
    } else {
      return (cardinal(param.l) + 1 | 0) + cardinal(param.r) | 0;
    }
  };
  var bindings_aux = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (typeof param !== "object") {
        return accu;
      }
      _param = param.l;
      _accu = {
        hd: [
          param.v,
          param.d
        ],
        tl: bindings_aux(accu, param.r)
      };
      continue ;
    };
  };
  var bindings = function (s) {
    return bindings_aux(/* [] */0, s);
  };
  return {
          empty: "Empty",
          is_empty: is_empty,
          mem: mem,
          add: add,
          update: update,
          singleton: singleton,
          remove: remove,
          merge: merge$1,
          union: union,
          compare: compare,
          equal: equal,
          iter: iter,
          fold: fold,
          for_all: for_all,
          exists: exists,
          filter: filter,
          partition: partition,
          cardinal: cardinal,
          bindings: bindings,
          min_binding: min_binding,
          min_binding_opt: min_binding_opt,
          max_binding: max_binding,
          max_binding_opt: max_binding_opt,
          choose: min_binding,
          choose_opt: min_binding_opt,
          split: split,
          find: find,
          find_opt: find_opt,
          find_first: find_first,
          find_first_opt: find_first_opt,
          find_last: find_last,
          find_last_opt: find_last_opt,
          map: map,
          mapi: mapi
        };
}

exports.Make = Make;
/* No side effect */
