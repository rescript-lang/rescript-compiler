'use strict';

var List = require("./list.js");
var Curry = require("./curry.js");
var Caml_option = require("./caml_option.js");
var HashtblLabels = require("./hashtblLabels.js");

var Hashtbl = {
  create: HashtblLabels.create,
  clear: HashtblLabels.clear,
  reset: HashtblLabels.reset,
  copy: HashtblLabels.copy,
  add: HashtblLabels.add,
  find: HashtblLabels.find,
  find_opt: HashtblLabels.find_opt,
  find_all: HashtblLabels.find_all,
  mem: HashtblLabels.mem,
  remove: HashtblLabels.remove,
  replace: HashtblLabels.replace,
  iter: HashtblLabels.iter,
  filter_map_inplace: HashtblLabels.filter_map_inplace,
  fold: HashtblLabels.fold,
  length: HashtblLabels.length,
  randomize: HashtblLabels.randomize,
  is_randomized: HashtblLabels.is_randomized,
  stats: HashtblLabels.stats,
  Make: HashtblLabels.Make,
  MakeSeeded: HashtblLabels.MakeSeeded,
  hash: HashtblLabels.hash,
  seeded_hash: HashtblLabels.seeded_hash,
  hash_param: HashtblLabels.hash_param,
  seeded_hash_param: HashtblLabels.seeded_hash_param
};

var $$Map = {
  Make: (function (funarg) {
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
      var find_first_aux = function (_v0, _d0, f, _param) {
        while(true) {
          var param = _param;
          var d0 = _d0;
          var v0 = _v0;
          if (typeof param !== "object") {
            return [
                    v0,
                    d0
                  ];
          }
          var v = param.v;
          if (Curry._1(f, v)) {
            _param = param.l;
            _d0 = param.d;
            _v0 = v;
            continue ;
          }
          _param = param.r;
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
            return find_first_aux(v, param.d, f, param.l);
          }
          _param = param.r;
          continue ;
        };
      };
      var find_first_opt_aux = function (_v0, _d0, f, _param) {
        while(true) {
          var param = _param;
          var d0 = _d0;
          var v0 = _v0;
          if (typeof param !== "object") {
            return [
                    v0,
                    d0
                  ];
          }
          var v = param.v;
          if (Curry._1(f, v)) {
            _param = param.l;
            _d0 = param.d;
            _v0 = v;
            continue ;
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
            return find_first_opt_aux(v, param.d, f, param.l);
          }
          _param = param.r;
          continue ;
        };
      };
      var find_last_aux = function (_v0, _d0, f, _param) {
        while(true) {
          var param = _param;
          var d0 = _d0;
          var v0 = _v0;
          if (typeof param !== "object") {
            return [
                    v0,
                    d0
                  ];
          }
          var v = param.v;
          if (Curry._1(f, v)) {
            _param = param.r;
            _d0 = param.d;
            _v0 = v;
            continue ;
          }
          _param = param.l;
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
            return find_last_aux(v, param.d, f, param.r);
          }
          _param = param.l;
          continue ;
        };
      };
      var find_last_opt_aux = function (_v0, _d0, f, _param) {
        while(true) {
          var param = _param;
          var d0 = _d0;
          var v0 = _v0;
          if (typeof param !== "object") {
            return [
                    v0,
                    d0
                  ];
          }
          var v = param.v;
          if (Curry._1(f, v)) {
            _param = param.r;
            _d0 = param.d;
            _v0 = v;
            continue ;
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
            return find_last_opt_aux(v, param.d, f, param.r);
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
                  "mapLabels.res",
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
    })
};

var $$Set = {
  Make: (function (funarg) {
      var height = function (param) {
        if (typeof param !== "object") {
          return 0;
        } else {
          return param.h;
        }
      };
      var create = function (l, v, r) {
        var hl;
        hl = typeof l !== "object" ? 0 : l.h;
        var hr;
        hr = typeof r !== "object" ? 0 : r.h;
        return {
                TAG: "Node",
                l: l,
                v: v,
                r: r,
                h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
              };
      };
      var bal = function (l, v, r) {
        var hl;
        hl = typeof l !== "object" ? 0 : l.h;
        var hr;
        hr = typeof r !== "object" ? 0 : r.h;
        if (hl > (hr + 2 | 0)) {
          if (typeof l !== "object") {
            throw {
                  RE_EXN_ID: "Invalid_argument",
                  _1: "Set.bal",
                  Error: new Error()
                };
          }
          var lr = l.r;
          var lv = l.v;
          var ll = l.l;
          if (height(ll) >= height(lr)) {
            return create(ll, lv, create(lr, v, r));
          }
          if (typeof lr === "object") {
            return create(create(ll, lv, lr.l), lr.v, create(lr.r, v, r));
          }
          throw {
                RE_EXN_ID: "Invalid_argument",
                _1: "Set.bal",
                Error: new Error()
              };
        }
        if (hr <= (hl + 2 | 0)) {
          return {
                  TAG: "Node",
                  l: l,
                  v: v,
                  r: r,
                  h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
                };
        }
        if (typeof r !== "object") {
          throw {
                RE_EXN_ID: "Invalid_argument",
                _1: "Set.bal",
                Error: new Error()
              };
        }
        var rr = r.r;
        var rv = r.v;
        var rl = r.l;
        if (height(rr) >= height(rl)) {
          return create(create(l, v, rl), rv, rr);
        }
        if (typeof rl === "object") {
          return create(create(l, v, rl.l), rl.v, create(rl.r, rv, rr));
        }
        throw {
              RE_EXN_ID: "Invalid_argument",
              _1: "Set.bal",
              Error: new Error()
            };
      };
      var add = function (x, param) {
        if (typeof param !== "object") {
          return {
                  TAG: "Node",
                  l: "Empty",
                  v: x,
                  r: "Empty",
                  h: 1
                };
        }
        var r = param.r;
        var v = param.v;
        var l = param.l;
        var c = Curry._2(funarg.compare, x, v);
        if (c === 0) {
          return param;
        }
        if (c < 0) {
          var ll = add(x, l);
          if (l === ll) {
            return param;
          } else {
            return bal(ll, v, r);
          }
        }
        var rr = add(x, r);
        if (r === rr) {
          return param;
        } else {
          return bal(l, v, rr);
        }
      };
      var singleton = function (x) {
        return {
                TAG: "Node",
                l: "Empty",
                v: x,
                r: "Empty",
                h: 1
              };
      };
      var add_min_element = function (x, param) {
        if (typeof param !== "object") {
          return singleton(x);
        } else {
          return bal(add_min_element(x, param.l), param.v, param.r);
        }
      };
      var add_max_element = function (x, param) {
        if (typeof param !== "object") {
          return singleton(x);
        } else {
          return bal(param.l, param.v, add_max_element(x, param.r));
        }
      };
      var join = function (l, v, r) {
        if (typeof l !== "object") {
          return add_min_element(v, r);
        }
        var lh = l.h;
        if (typeof r !== "object") {
          return add_max_element(v, l);
        }
        var rh = r.h;
        if (lh > (rh + 2 | 0)) {
          return bal(l.l, l.v, join(l.r, v, r));
        } else if (rh > (lh + 2 | 0)) {
          return bal(join(l, v, r.l), r.v, r.r);
        } else {
          return create(l, v, r);
        }
      };
      var min_elt = function (_param) {
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
            return param.v;
          }
          _param = l;
          continue ;
        };
      };
      var min_elt_opt = function (_param) {
        while(true) {
          var param = _param;
          if (typeof param !== "object") {
            return ;
          }
          var l = param.l;
          if (typeof l !== "object") {
            return Caml_option.some(param.v);
          }
          _param = l;
          continue ;
        };
      };
      var max_elt = function (_param) {
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
            return param.v;
          }
          _param = r;
          continue ;
        };
      };
      var max_elt_opt = function (_param) {
        while(true) {
          var param = _param;
          if (typeof param !== "object") {
            return ;
          }
          var r = param.r;
          if (typeof r !== "object") {
            return Caml_option.some(param.v);
          }
          _param = r;
          continue ;
        };
      };
      var remove_min_elt = function (param) {
        if (typeof param !== "object") {
          throw {
                RE_EXN_ID: "Invalid_argument",
                _1: "Set.remove_min_elt",
                Error: new Error()
              };
        }
        var l = param.l;
        if (typeof l !== "object") {
          return param.r;
        } else {
          return bal(remove_min_elt(l), param.v, param.r);
        }
      };
      var merge = function (t1, t2) {
        if (typeof t1 !== "object") {
          return t2;
        } else if (typeof t2 !== "object") {
          return t1;
        } else {
          return bal(t1, min_elt(t2), remove_min_elt(t2));
        }
      };
      var concat = function (t1, t2) {
        if (typeof t1 !== "object") {
          return t2;
        } else if (typeof t2 !== "object") {
          return t1;
        } else {
          return join(t1, min_elt(t2), remove_min_elt(t2));
        }
      };
      var split = function (x, param) {
        if (typeof param !== "object") {
          return [
                  "Empty",
                  false,
                  "Empty"
                ];
        }
        var r = param.r;
        var v = param.v;
        var l = param.l;
        var c = Curry._2(funarg.compare, x, v);
        if (c === 0) {
          return [
                  l,
                  true,
                  r
                ];
        }
        if (c < 0) {
          var match = split(x, l);
          return [
                  match[0],
                  match[1],
                  join(match[2], v, r)
                ];
        }
        var match$1 = split(x, r);
        return [
                join(l, v, match$1[0]),
                match$1[1],
                match$1[2]
              ];
      };
      var is_empty = function (param) {
        if (typeof param !== "object") {
          return true;
        } else {
          return false;
        }
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
      var remove = function (x, param) {
        if (typeof param !== "object") {
          return "Empty";
        }
        var r = param.r;
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
            return bal(ll, v, r);
          }
        }
        var rr = remove(x, r);
        if (r === rr) {
          return param;
        } else {
          return bal(l, v, rr);
        }
      };
      var union = function (s1, s2) {
        if (typeof s1 !== "object") {
          return s2;
        }
        var h1 = s1.h;
        var v1 = s1.v;
        if (typeof s2 !== "object") {
          return s1;
        }
        var h2 = s2.h;
        var v2 = s2.v;
        if (h1 >= h2) {
          if (h2 === 1) {
            return add(v2, s1);
          }
          var match = split(v1, s2);
          return join(union(s1.l, match[0]), v1, union(s1.r, match[2]));
        }
        if (h1 === 1) {
          return add(v1, s2);
        }
        var match$1 = split(v2, s1);
        return join(union(match$1[0], s2.l), v2, union(match$1[2], s2.r));
      };
      var inter = function (s1, s2) {
        if (typeof s1 !== "object") {
          return "Empty";
        }
        if (typeof s2 !== "object") {
          return "Empty";
        }
        var r1 = s1.r;
        var v1 = s1.v;
        var l1 = s1.l;
        var match = split(v1, s2);
        var l2 = match[0];
        if (match[1]) {
          return join(inter(l1, l2), v1, inter(r1, match[2]));
        } else {
          return concat(inter(l1, l2), inter(r1, match[2]));
        }
      };
      var diff = function (s1, s2) {
        if (typeof s1 !== "object") {
          return "Empty";
        }
        if (typeof s2 !== "object") {
          return s1;
        }
        var r1 = s1.r;
        var v1 = s1.v;
        var l1 = s1.l;
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
          if (typeof s !== "object") {
            return e;
          }
          _e = {
            TAG: "More",
            _0: s.v,
            _1: s.r,
            _2: e
          };
          _s = s.l;
          continue ;
        };
      };
      var compare_aux = function (_e1, _e2) {
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
          _e2 = cons_enum(e2._1, e2._2);
          _e1 = cons_enum(e1._1, e1._2);
          continue ;
        };
      };
      var compare = function (s1, s2) {
        return compare_aux(cons_enum(s1, "End"), cons_enum(s2, "End"));
      };
      var equal = function (s1, s2) {
        return compare(s1, s2) === 0;
      };
      var subset = function (_s1, _s2) {
        while(true) {
          var s2 = _s2;
          var s1 = _s1;
          if (typeof s1 !== "object") {
            return true;
          }
          var r1 = s1.r;
          var v1 = s1.v;
          var l1 = s1.l;
          if (typeof s2 !== "object") {
            return false;
          }
          var r2 = s2.r;
          var l2 = s2.l;
          var c = Curry._2(funarg.compare, v1, s2.v);
          if (c === 0) {
            if (!subset(l1, l2)) {
              return false;
            }
            _s2 = r2;
            _s1 = r1;
            continue ;
          }
          if (c < 0) {
            if (!subset({
                    TAG: "Node",
                    l: l1,
                    v: v1,
                    r: "Empty",
                    h: 0
                  }, l2)) {
              return false;
            }
            _s1 = r1;
            continue ;
          }
          if (!subset({
                  TAG: "Node",
                  l: "Empty",
                  v: v1,
                  r: r1,
                  h: 0
                }, r2)) {
            return false;
          }
          _s1 = l1;
          continue ;
        };
      };
      var iter = function (f, _param) {
        while(true) {
          var param = _param;
          if (typeof param !== "object") {
            return ;
          }
          iter(f, param.l);
          Curry._1(f, param.v);
          _param = param.r;
          continue ;
        };
      };
      var fold = function (f, _s, _accu) {
        while(true) {
          var accu = _accu;
          var s = _s;
          if (typeof s !== "object") {
            return accu;
          }
          _accu = Curry._2(f, s.v, fold(f, s.l, accu));
          _s = s.r;
          continue ;
        };
      };
      var for_all = function (p, _param) {
        while(true) {
          var param = _param;
          if (typeof param !== "object") {
            return true;
          }
          if (!Curry._1(p, param.v)) {
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
          if (Curry._1(p, param.v)) {
            return true;
          }
          if (exists(p, param.l)) {
            return true;
          }
          _param = param.r;
          continue ;
        };
      };
      var filter = function (p, param) {
        if (typeof param !== "object") {
          return "Empty";
        }
        var r = param.r;
        var v = param.v;
        var l = param.l;
        var l$p = filter(p, l);
        var pv = Curry._1(p, v);
        var r$p = filter(p, r);
        if (pv) {
          if (l === l$p && r === r$p) {
            return param;
          } else {
            return join(l$p, v, r$p);
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
        var v = param.v;
        var match = partition(p, param.l);
        var lf = match[1];
        var lt = match[0];
        var pv = Curry._1(p, v);
        var match$1 = partition(p, param.r);
        var rf = match$1[1];
        var rt = match$1[0];
        if (pv) {
          return [
                  join(lt, v, rt),
                  concat(lf, rf)
                ];
        } else {
          return [
                  concat(lt, rt),
                  join(lf, v, rf)
                ];
        }
      };
      var cardinal = function (param) {
        if (typeof param !== "object") {
          return 0;
        } else {
          return (cardinal(param.l) + 1 | 0) + cardinal(param.r) | 0;
        }
      };
      var elements_aux = function (_accu, _param) {
        while(true) {
          var param = _param;
          var accu = _accu;
          if (typeof param !== "object") {
            return accu;
          }
          _param = param.l;
          _accu = {
            hd: param.v,
            tl: elements_aux(accu, param.r)
          };
          continue ;
        };
      };
      var elements = function (s) {
        return elements_aux(/* [] */0, s);
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
          var v = param.v;
          var c = Curry._2(funarg.compare, x, v);
          if (c === 0) {
            return v;
          }
          _param = c < 0 ? param.l : param.r;
          continue ;
        };
      };
      var find_first_aux = function (_v0, f, _param) {
        while(true) {
          var param = _param;
          var v0 = _v0;
          if (typeof param !== "object") {
            return v0;
          }
          var v = param.v;
          if (Curry._1(f, v)) {
            _param = param.l;
            _v0 = v;
            continue ;
          }
          _param = param.r;
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
            return find_first_aux(v, f, param.l);
          }
          _param = param.r;
          continue ;
        };
      };
      var find_first_opt_aux = function (_v0, f, _param) {
        while(true) {
          var param = _param;
          var v0 = _v0;
          if (typeof param !== "object") {
            return Caml_option.some(v0);
          }
          var v = param.v;
          if (Curry._1(f, v)) {
            _param = param.l;
            _v0 = v;
            continue ;
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
            return find_first_opt_aux(v, f, param.l);
          }
          _param = param.r;
          continue ;
        };
      };
      var find_last_aux = function (_v0, f, _param) {
        while(true) {
          var param = _param;
          var v0 = _v0;
          if (typeof param !== "object") {
            return v0;
          }
          var v = param.v;
          if (Curry._1(f, v)) {
            _param = param.r;
            _v0 = v;
            continue ;
          }
          _param = param.l;
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
            return find_last_aux(v, f, param.r);
          }
          _param = param.l;
          continue ;
        };
      };
      var find_last_opt_aux = function (_v0, f, _param) {
        while(true) {
          var param = _param;
          var v0 = _v0;
          if (typeof param !== "object") {
            return Caml_option.some(v0);
          }
          var v = param.v;
          if (Curry._1(f, v)) {
            _param = param.r;
            _v0 = v;
            continue ;
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
            return find_last_opt_aux(v, f, param.r);
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
          var v = param.v;
          var c = Curry._2(funarg.compare, x, v);
          if (c === 0) {
            return Caml_option.some(v);
          }
          _param = c < 0 ? param.l : param.r;
          continue ;
        };
      };
      var try_join = function (l, v, r) {
        if ((l === "Empty" || Curry._2(funarg.compare, max_elt(l), v) < 0) && (r === "Empty" || Curry._2(funarg.compare, v, min_elt(r)) < 0)) {
          return join(l, v, r);
        } else {
          return union(l, add(v, r));
        }
      };
      var map = function (f, param) {
        if (typeof param !== "object") {
          return "Empty";
        }
        var r = param.r;
        var v = param.v;
        var l = param.l;
        var l$p = map(f, l);
        var v$p = Curry._1(f, v);
        var r$p = map(f, r);
        if (l === l$p && v === v$p && r === r$p) {
          return param;
        } else {
          return try_join(l$p, v$p, r$p);
        }
      };
      var of_sorted_list = function (l) {
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
                            l: "Empty",
                            v: l.hd,
                            r: "Empty",
                            h: 1
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
                              l: {
                                TAG: "Node",
                                l: "Empty",
                                v: l.hd,
                                r: "Empty",
                                h: 1
                              },
                              v: match.hd,
                              r: "Empty",
                              h: 2
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
                                l: {
                                  TAG: "Node",
                                  l: "Empty",
                                  v: l.hd,
                                  r: "Empty",
                                  h: 1
                                },
                                v: match$1.hd,
                                r: {
                                  TAG: "Node",
                                  l: "Empty",
                                  v: match$2.hd,
                                  r: "Empty",
                                  h: 1
                                },
                                h: 2
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
                  "setLabels.res",
                  691,
                  20
                ],
                Error: new Error()
              };
        };
        return sub(List.length(l), l)[0];
      };
      var of_list = function (l) {
        if (!l) {
          return "Empty";
        }
        var match = l.tl;
        var x0 = l.hd;
        if (!match) {
          return singleton(x0);
        }
        var match$1 = match.tl;
        var x1 = match.hd;
        if (!match$1) {
          return add(x1, singleton(x0));
        }
        var match$2 = match$1.tl;
        var x2 = match$1.hd;
        if (!match$2) {
          return add(x2, add(x1, singleton(x0)));
        }
        var match$3 = match$2.tl;
        var x3 = match$2.hd;
        if (match$3) {
          if (match$3.tl) {
            return of_sorted_list(List.sort_uniq(funarg.compare, l));
          } else {
            return add(match$3.hd, add(x3, add(x2, add(x1, singleton(x0)))));
          }
        } else {
          return add(x3, add(x2, add(x1, singleton(x0))));
        }
      };
      return {
              empty: "Empty",
              is_empty: is_empty,
              mem: mem,
              add: add,
              singleton: singleton,
              remove: remove,
              union: union,
              inter: inter,
              diff: diff,
              compare: compare,
              equal: equal,
              subset: subset,
              iter: iter,
              map: map,
              fold: fold,
              for_all: for_all,
              exists: exists,
              filter: filter,
              partition: partition,
              cardinal: cardinal,
              elements: elements,
              min_elt: min_elt,
              min_elt_opt: min_elt_opt,
              max_elt: max_elt,
              max_elt_opt: max_elt_opt,
              choose: min_elt,
              choose_opt: min_elt_opt,
              split: split,
              find: find,
              find_opt: find_opt,
              find_first: find_first,
              find_first_opt: find_first_opt,
              find_last: find_last,
              find_last_opt: find_last_opt,
              of_list: of_list
            };
    })
};

exports.Hashtbl = Hashtbl;
exports.$$Map = $$Map;
exports.$$Set = $$Set;
/* No side effect */
