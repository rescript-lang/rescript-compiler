// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var Obj = require("../stdlib/obj");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Pervasives = require("../stdlib/pervasives");
var Caml_oo = require("../runtime/caml_oo");
var Sys = require("../stdlib/sys");
var Caml_primitive = require("../runtime/caml_primitive");
var Caml_array = require("../runtime/caml_array");
var $$Array = require("../stdlib/array");
var Caml_string = require("../runtime/caml_string");
var List = require("../stdlib/list");

function copy(o) {
  return Caml_exceptions.caml_set_oo_id(Caml_obj_runtime.caml_obj_dup(o));
}

var params = [
  /* record */0,
  /* true */1,
  /* true */1,
  /* true */1,
  3,
  16
];

var step = Sys.word_size / 16 | 0;

var initial_object_size = 2;

var dummy_item = /* () */0;

function public_method_label(s) {
  var accu = 0;
  for(var i = 0 ,i_finish = s.length - 1; i<= i_finish; ++i){
    accu = 223 * accu + s.charCodeAt(i);
  }
  accu = accu & (1 << 31) - 1;
  if (accu > 1073741823) {
    return accu - (1 << 31);
  }
  else {
    return accu;
  }
}

function compare(x, y) {
  return Caml_string.caml_string_compare(x, y);
}

function height(param) {
  if (param) {
    return param[5];
  }
  else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return [
          /* Node */0,
          l,
          x,
          d,
          r,
          hl >= hr ? hl + 1 : hr + 1
        ];
}

function singleton(x, d) {
  return [
          /* Node */0,
          /* Empty */0,
          x,
          d,
          /* Empty */0,
          1
        ];
}

function bal(l, x, d, r) {
  var hl = l ? l[5] : 0;
  var hr = r ? r[5] : 0;
  if (hl > hr + 2) {
    if (l) {
      var lr = l[4];
      var ld = l[3];
      var lv = l[2];
      var ll = l[1];
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      }
      else {
        if (lr) {
          return create(create(ll, lv, ld, lr[1]), lr[2], lr[3], create(lr[4], x, d, r));
        }
        else {
          return Pervasives.invalid_arg("Map.bal");
        }
      }
    }
    else {
      return Pervasives.invalid_arg("Map.bal");
    }
  }
  else {
    if (hr > hl + 2) {
      if (r) {
        var rr = r[4];
        var rd = r[3];
        var rv = r[2];
        var rl = r[1];
        if (height(rr) >= height(rl)) {
          return create(create(l, x, d, rl), rv, rd, rr);
        }
        else {
          if (rl) {
            return create(create(l, x, d, rl[1]), rl[2], rl[3], create(rl[4], rv, rd, rr));
          }
          else {
            return Pervasives.invalid_arg("Map.bal");
          }
        }
      }
      else {
        return Pervasives.invalid_arg("Map.bal");
      }
    }
    else {
      return [
              /* Node */0,
              l,
              x,
              d,
              r,
              hl >= hr ? hl + 1 : hr + 1
            ];
    }
  }
}

function is_empty(param) {
  if (param) {
    return /* false */0;
  }
  else {
    return /* true */1;
  }
}

function add(x, data, param) {
  if (param) {
    var r = param[4];
    var d = param[3];
    var v = param[2];
    var l = param[1];
    var c = compare(x, v);
    if (c) {
      if (c < 0) {
        return bal(add(x, data, l), v, d, r);
      }
      else {
        return bal(l, v, d, add(x, data, r));
      }
    }
    else {
      return [
              /* Node */0,
              l,
              x,
              data,
              r,
              param[5]
            ];
    }
  }
  else {
    return [
            /* Node */0,
            /* Empty */0,
            x,
            data,
            /* Empty */0,
            1
          ];
  }
}

function find(x, _param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      var c = compare(x, param[2]);
      if (c) {
        _param = c < 0 ? param[1] : param[4];
      }
      else {
        return param[3];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function mem(x, param) {
  if (param) {
    var c = compare(x, param[2]);
    return +(c === 0 || mem(x, c < 0 ? param[1] : param[4]));
  }
  else {
    return /* false */0;
  }
}

function min_binding(_param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      var l = param[1];
      if (l) {
        _param = l;
      }
      else {
        return [
                /* tuple */0,
                param[2],
                param[3]
              ];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function max_binding(_param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      var r = param[4];
      if (r) {
        _param = r;
      }
      else {
        return [
                /* tuple */0,
                param[2],
                param[3]
              ];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function remove_min_binding(param) {
  if (param) {
    var l = param[1];
    if (l) {
      return bal(remove_min_binding(l), param[2], param[3], param[4]);
    }
    else {
      return param[4];
    }
  }
  else {
    return Pervasives.invalid_arg("Map.remove_min_elt");
  }
}

function remove(x, param) {
  if (param) {
    var r = param[4];
    var d = param[3];
    var v = param[2];
    var l = param[1];
    var c = compare(x, v);
    if (c) {
      if (c < 0) {
        return bal(remove(x, l), v, d, r);
      }
      else {
        return bal(l, v, d, remove(x, r));
      }
    }
    else {
      var t1 = l;
      var t2 = r;
      if (t1) {
        if (t2) {
          var match = min_binding(t2);
          return bal(t1, match[1], match[2], remove_min_binding(t2));
        }
        else {
          return t1;
        }
      }
      else {
        return t2;
      }
    }
  }
  else {
    return /* Empty */0;
  }
}

function iter(f, _param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      iter(f, param[1]);
      f(param[2], param[3]);
      _param = param[4];
    }
    else {
      return /* () */0;
    }
  };
}

function map(f, param) {
  if (param) {
    var l$prime = map(f, param[1]);
    var d$prime = f(param[3]);
    var r$prime = map(f, param[4]);
    return [
            /* Node */0,
            l$prime,
            param[2],
            d$prime,
            r$prime,
            param[5]
          ];
  }
  else {
    return /* Empty */0;
  }
}

function mapi(f, param) {
  if (param) {
    var v = param[2];
    var l$prime = mapi(f, param[1]);
    var d$prime = f(v, param[3]);
    var r$prime = mapi(f, param[4]);
    return [
            /* Node */0,
            l$prime,
            v,
            d$prime,
            r$prime,
            param[5]
          ];
  }
  else {
    return /* Empty */0;
  }
}

function fold(f, _m, _accu) {
  while(/* true */1) {
    var accu = _accu;
    var m = _m;
    if (m) {
      _accu = f(m[2], m[3], fold(f, m[1], accu));
      _m = m[4];
    }
    else {
      return accu;
    }
  };
}

function for_all(p, param) {
  if (param) {
    return +(p(param[2], param[3]) && for_all(p, param[1]) && for_all(p, param[4]));
  }
  else {
    return /* true */1;
  }
}

function exists(p, param) {
  if (param) {
    return +(p(param[2], param[3]) || exists(p, param[1]) || exists(p, param[4]));
  }
  else {
    return /* false */0;
  }
}

function add_min_binding(k, v, param) {
  if (param) {
    return bal(add_min_binding(k, v, param[1]), param[2], param[3], param[4]);
  }
  else {
    return singleton(k, v);
  }
}

function add_max_binding(k, v, param) {
  if (param) {
    return bal(param[1], param[2], param[3], add_max_binding(k, v, param[4]));
  }
  else {
    return singleton(k, v);
  }
}

function join(l, v, d, r) {
  if (l) {
    if (r) {
      var rh = r[5];
      var lh = l[5];
      if (lh > rh + 2) {
        return bal(l[1], l[2], l[3], join(l[4], v, d, r));
      }
      else {
        if (rh > lh + 2) {
          return bal(join(l, v, d, r[1]), r[2], r[3], r[4]);
        }
        else {
          return create(l, v, d, r);
        }
      }
    }
    else {
      return add_max_binding(v, d, l);
    }
  }
  else {
    return add_min_binding(v, d, r);
  }
}

function concat(t1, t2) {
  if (t1) {
    if (t2) {
      var match = min_binding(t2);
      return join(t1, match[1], match[2], remove_min_binding(t2));
    }
    else {
      return t1;
    }
  }
  else {
    return t2;
  }
}

function concat_or_join(t1, v, d, t2) {
  if (d) {
    return join(t1, v, d[1], t2);
  }
  else {
    return concat(t1, t2);
  }
}

function split(x, param) {
  if (param) {
    var r = param[4];
    var d = param[3];
    var v = param[2];
    var l = param[1];
    var c = compare(x, v);
    if (c) {
      if (c < 0) {
        var match = split(x, l);
        return [
                /* tuple */0,
                match[1],
                match[2],
                join(match[3], v, d, r)
              ];
      }
      else {
        var match$1 = split(x, r);
        return [
                /* tuple */0,
                join(l, v, d, match$1[1]),
                match$1[2],
                match$1[3]
              ];
      }
    }
    else {
      return [
              /* tuple */0,
              l,
              [
                /* Some */0,
                d
              ],
              r
            ];
    }
  }
  else {
    return [
            /* tuple */0,
            /* Empty */0,
            /* None */0,
            /* Empty */0
          ];
  }
}

function merge(f, s1, s2) {
  var exit = 0;
  if (s1) {
    var v1 = s1[2];
    if (s1[5] >= height(s2)) {
      var match = split(v1, s2);
      return concat_or_join(merge(f, s1[1], match[1]), v1, f(v1, [
                      /* Some */0,
                      s1[3]
                    ], match[2]), merge(f, s1[4], match[3]));
    }
    else {
      exit = 1;
    }
  }
  else {
    if (s2) {
      exit = 1;
    }
    else {
      return /* Empty */0;
    }
  }
  if (exit === 1) {
    if (s2) {
      var v2 = s2[2];
      var match$1 = split(v2, s1);
      return concat_or_join(merge(f, match$1[1], s2[1]), v2, f(v2, match$1[2], [
                      /* Some */0,
                      s2[3]
                    ]), merge(f, match$1[3], s2[4]));
    }
    else {
      throw [
            0,
            Caml_exceptions.Assert_failure,
            [
              0,
              "map.ml",
              270,
              10
            ]
          ];
    }
  }
  
}

function filter(p, param) {
  if (param) {
    var d = param[3];
    var v = param[2];
    var l$prime = filter(p, param[1]);
    var pvd = p(v, d);
    var r$prime = filter(p, param[4]);
    if (pvd) {
      return join(l$prime, v, d, r$prime);
    }
    else {
      return concat(l$prime, r$prime);
    }
  }
  else {
    return /* Empty */0;
  }
}

function partition(p, param) {
  if (param) {
    var d = param[3];
    var v = param[2];
    var match = partition(p, param[1]);
    var lf = match[2];
    var lt = match[1];
    var pvd = p(v, d);
    var match$1 = partition(p, param[4]);
    var rf = match$1[2];
    var rt = match$1[1];
    if (pvd) {
      return [
              /* tuple */0,
              join(lt, v, d, rt),
              concat(lf, rf)
            ];
    }
    else {
      return [
              /* tuple */0,
              concat(lt, rt),
              join(lf, v, d, rf)
            ];
    }
  }
  else {
    return [
            /* tuple */0,
            /* Empty */0,
            /* Empty */0
          ];
  }
}

function cons_enum(_m, _e) {
  while(/* true */1) {
    var e = _e;
    var m = _m;
    if (m) {
      _e = [
        /* More */0,
        m[2],
        m[3],
        m[4],
        e
      ];
      _m = m[1];
    }
    else {
      return e;
    }
  };
}

function compare$1(cmp, m1, m2) {
  var _e1 = cons_enum(m1, /* End */0);
  var _e2 = cons_enum(m2, /* End */0);
  while(/* true */1) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var c = compare(e1[1], e2[1]);
        if (c !== 0) {
          return c;
        }
        else {
          var c$1 = cmp(e1[2], e2[2]);
          if (c$1 !== 0) {
            return c$1;
          }
          else {
            _e2 = cons_enum(e2[3], e2[4]);
            _e1 = cons_enum(e1[3], e1[4]);
          }
        }
      }
      else {
        return 1;
      }
    }
    else {
      if (e2) {
        return -1;
      }
      else {
        return 0;
      }
    }
  };
}

function equal(cmp, m1, m2) {
  var equal_aux = function (e1, e2) {
    if (e1) {
      if (e2) {
        return +(compare(e1[1], e2[1]) === 0 && cmp(e1[2], e2[2]) && equal_aux(cons_enum(e1[3], e1[4]), cons_enum(e2[3], e2[4])));
      }
      else {
        return /* false */0;
      }
    }
    else {
      if (e2) {
        return /* false */0;
      }
      else {
        return /* true */1;
      }
    }
  };
  return equal_aux(cons_enum(m1, /* End */0), cons_enum(m2, /* End */0));
}

function cardinal(param) {
  if (param) {
    return cardinal(param[1]) + 1 + cardinal(param[4]);
  }
  else {
    return 0;
  }
}

function bindings_aux(_accu, _param) {
  while(/* true */1) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[1];
      _accu = [
        /* :: */0,
        [
          /* tuple */0,
          param[2],
          param[3]
        ],
        bindings_aux(accu, param[4])
      ];
    }
    else {
      return accu;
    }
  };
}

function bindings(s) {
  return bindings_aux(/* [] */0, s);
}

var Vars = [
  0,
  /* Empty */0,
  is_empty,
  mem,
  add,
  singleton,
  remove,
  merge,
  compare$1,
  equal,
  iter,
  fold,
  for_all,
  exists,
  filter,
  partition,
  cardinal,
  bindings,
  min_binding,
  max_binding,
  min_binding,
  split,
  find,
  map,
  mapi
];

function compare$2(x, y) {
  return Caml_string.caml_string_compare(x, y);
}

function height$1(param) {
  if (param) {
    return param[5];
  }
  else {
    return 0;
  }
}

function create$1(l, x, d, r) {
  var hl = height$1(l);
  var hr = height$1(r);
  return [
          /* Node */0,
          l,
          x,
          d,
          r,
          hl >= hr ? hl + 1 : hr + 1
        ];
}

function singleton$1(x, d) {
  return [
          /* Node */0,
          /* Empty */0,
          x,
          d,
          /* Empty */0,
          1
        ];
}

function bal$1(l, x, d, r) {
  var hl = l ? l[5] : 0;
  var hr = r ? r[5] : 0;
  if (hl > hr + 2) {
    if (l) {
      var lr = l[4];
      var ld = l[3];
      var lv = l[2];
      var ll = l[1];
      if (height$1(ll) >= height$1(lr)) {
        return create$1(ll, lv, ld, create$1(lr, x, d, r));
      }
      else {
        if (lr) {
          return create$1(create$1(ll, lv, ld, lr[1]), lr[2], lr[3], create$1(lr[4], x, d, r));
        }
        else {
          return Pervasives.invalid_arg("Map.bal");
        }
      }
    }
    else {
      return Pervasives.invalid_arg("Map.bal");
    }
  }
  else {
    if (hr > hl + 2) {
      if (r) {
        var rr = r[4];
        var rd = r[3];
        var rv = r[2];
        var rl = r[1];
        if (height$1(rr) >= height$1(rl)) {
          return create$1(create$1(l, x, d, rl), rv, rd, rr);
        }
        else {
          if (rl) {
            return create$1(create$1(l, x, d, rl[1]), rl[2], rl[3], create$1(rl[4], rv, rd, rr));
          }
          else {
            return Pervasives.invalid_arg("Map.bal");
          }
        }
      }
      else {
        return Pervasives.invalid_arg("Map.bal");
      }
    }
    else {
      return [
              /* Node */0,
              l,
              x,
              d,
              r,
              hl >= hr ? hl + 1 : hr + 1
            ];
    }
  }
}

function is_empty$1(param) {
  if (param) {
    return /* false */0;
  }
  else {
    return /* true */1;
  }
}

function add$1(x, data, param) {
  if (param) {
    var r = param[4];
    var d = param[3];
    var v = param[2];
    var l = param[1];
    var c = compare$2(x, v);
    if (c) {
      if (c < 0) {
        return bal$1(add$1(x, data, l), v, d, r);
      }
      else {
        return bal$1(l, v, d, add$1(x, data, r));
      }
    }
    else {
      return [
              /* Node */0,
              l,
              x,
              data,
              r,
              param[5]
            ];
    }
  }
  else {
    return [
            /* Node */0,
            /* Empty */0,
            x,
            data,
            /* Empty */0,
            1
          ];
  }
}

function find$1(x, _param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      var c = compare$2(x, param[2]);
      if (c) {
        _param = c < 0 ? param[1] : param[4];
      }
      else {
        return param[3];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function mem$1(x, param) {
  if (param) {
    var c = compare$2(x, param[2]);
    return +(c === 0 || mem$1(x, c < 0 ? param[1] : param[4]));
  }
  else {
    return /* false */0;
  }
}

function min_binding$1(_param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      var l = param[1];
      if (l) {
        _param = l;
      }
      else {
        return [
                /* tuple */0,
                param[2],
                param[3]
              ];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function max_binding$1(_param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      var r = param[4];
      if (r) {
        _param = r;
      }
      else {
        return [
                /* tuple */0,
                param[2],
                param[3]
              ];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function remove_min_binding$1(param) {
  if (param) {
    var l = param[1];
    if (l) {
      return bal$1(remove_min_binding$1(l), param[2], param[3], param[4]);
    }
    else {
      return param[4];
    }
  }
  else {
    return Pervasives.invalid_arg("Map.remove_min_elt");
  }
}

function remove$1(x, param) {
  if (param) {
    var r = param[4];
    var d = param[3];
    var v = param[2];
    var l = param[1];
    var c = compare$2(x, v);
    if (c) {
      if (c < 0) {
        return bal$1(remove$1(x, l), v, d, r);
      }
      else {
        return bal$1(l, v, d, remove$1(x, r));
      }
    }
    else {
      var t1 = l;
      var t2 = r;
      if (t1) {
        if (t2) {
          var match = min_binding$1(t2);
          return bal$1(t1, match[1], match[2], remove_min_binding$1(t2));
        }
        else {
          return t1;
        }
      }
      else {
        return t2;
      }
    }
  }
  else {
    return /* Empty */0;
  }
}

function iter$1(f, _param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      iter$1(f, param[1]);
      f(param[2], param[3]);
      _param = param[4];
    }
    else {
      return /* () */0;
    }
  };
}

function map$1(f, param) {
  if (param) {
    var l$prime = map$1(f, param[1]);
    var d$prime = f(param[3]);
    var r$prime = map$1(f, param[4]);
    return [
            /* Node */0,
            l$prime,
            param[2],
            d$prime,
            r$prime,
            param[5]
          ];
  }
  else {
    return /* Empty */0;
  }
}

function mapi$1(f, param) {
  if (param) {
    var v = param[2];
    var l$prime = mapi$1(f, param[1]);
    var d$prime = f(v, param[3]);
    var r$prime = mapi$1(f, param[4]);
    return [
            /* Node */0,
            l$prime,
            v,
            d$prime,
            r$prime,
            param[5]
          ];
  }
  else {
    return /* Empty */0;
  }
}

function fold$1(f, _m, _accu) {
  while(/* true */1) {
    var accu = _accu;
    var m = _m;
    if (m) {
      _accu = f(m[2], m[3], fold$1(f, m[1], accu));
      _m = m[4];
    }
    else {
      return accu;
    }
  };
}

function for_all$1(p, param) {
  if (param) {
    return +(p(param[2], param[3]) && for_all$1(p, param[1]) && for_all$1(p, param[4]));
  }
  else {
    return /* true */1;
  }
}

function exists$1(p, param) {
  if (param) {
    return +(p(param[2], param[3]) || exists$1(p, param[1]) || exists$1(p, param[4]));
  }
  else {
    return /* false */0;
  }
}

function add_min_binding$1(k, v, param) {
  if (param) {
    return bal$1(add_min_binding$1(k, v, param[1]), param[2], param[3], param[4]);
  }
  else {
    return singleton$1(k, v);
  }
}

function add_max_binding$1(k, v, param) {
  if (param) {
    return bal$1(param[1], param[2], param[3], add_max_binding$1(k, v, param[4]));
  }
  else {
    return singleton$1(k, v);
  }
}

function join$1(l, v, d, r) {
  if (l) {
    if (r) {
      var rh = r[5];
      var lh = l[5];
      if (lh > rh + 2) {
        return bal$1(l[1], l[2], l[3], join$1(l[4], v, d, r));
      }
      else {
        if (rh > lh + 2) {
          return bal$1(join$1(l, v, d, r[1]), r[2], r[3], r[4]);
        }
        else {
          return create$1(l, v, d, r);
        }
      }
    }
    else {
      return add_max_binding$1(v, d, l);
    }
  }
  else {
    return add_min_binding$1(v, d, r);
  }
}

function concat$1(t1, t2) {
  if (t1) {
    if (t2) {
      var match = min_binding$1(t2);
      return join$1(t1, match[1], match[2], remove_min_binding$1(t2));
    }
    else {
      return t1;
    }
  }
  else {
    return t2;
  }
}

function concat_or_join$1(t1, v, d, t2) {
  if (d) {
    return join$1(t1, v, d[1], t2);
  }
  else {
    return concat$1(t1, t2);
  }
}

function split$1(x, param) {
  if (param) {
    var r = param[4];
    var d = param[3];
    var v = param[2];
    var l = param[1];
    var c = compare$2(x, v);
    if (c) {
      if (c < 0) {
        var match = split$1(x, l);
        return [
                /* tuple */0,
                match[1],
                match[2],
                join$1(match[3], v, d, r)
              ];
      }
      else {
        var match$1 = split$1(x, r);
        return [
                /* tuple */0,
                join$1(l, v, d, match$1[1]),
                match$1[2],
                match$1[3]
              ];
      }
    }
    else {
      return [
              /* tuple */0,
              l,
              [
                /* Some */0,
                d
              ],
              r
            ];
    }
  }
  else {
    return [
            /* tuple */0,
            /* Empty */0,
            /* None */0,
            /* Empty */0
          ];
  }
}

function merge$1(f, s1, s2) {
  var exit = 0;
  if (s1) {
    var v1 = s1[2];
    if (s1[5] >= height$1(s2)) {
      var match = split$1(v1, s2);
      return concat_or_join$1(merge$1(f, s1[1], match[1]), v1, f(v1, [
                      /* Some */0,
                      s1[3]
                    ], match[2]), merge$1(f, s1[4], match[3]));
    }
    else {
      exit = 1;
    }
  }
  else {
    if (s2) {
      exit = 1;
    }
    else {
      return /* Empty */0;
    }
  }
  if (exit === 1) {
    if (s2) {
      var v2 = s2[2];
      var match$1 = split$1(v2, s1);
      return concat_or_join$1(merge$1(f, match$1[1], s2[1]), v2, f(v2, match$1[2], [
                      /* Some */0,
                      s2[3]
                    ]), merge$1(f, match$1[3], s2[4]));
    }
    else {
      throw [
            0,
            Caml_exceptions.Assert_failure,
            [
              0,
              "map.ml",
              270,
              10
            ]
          ];
    }
  }
  
}

function filter$1(p, param) {
  if (param) {
    var d = param[3];
    var v = param[2];
    var l$prime = filter$1(p, param[1]);
    var pvd = p(v, d);
    var r$prime = filter$1(p, param[4]);
    if (pvd) {
      return join$1(l$prime, v, d, r$prime);
    }
    else {
      return concat$1(l$prime, r$prime);
    }
  }
  else {
    return /* Empty */0;
  }
}

function partition$1(p, param) {
  if (param) {
    var d = param[3];
    var v = param[2];
    var match = partition$1(p, param[1]);
    var lf = match[2];
    var lt = match[1];
    var pvd = p(v, d);
    var match$1 = partition$1(p, param[4]);
    var rf = match$1[2];
    var rt = match$1[1];
    if (pvd) {
      return [
              /* tuple */0,
              join$1(lt, v, d, rt),
              concat$1(lf, rf)
            ];
    }
    else {
      return [
              /* tuple */0,
              concat$1(lt, rt),
              join$1(lf, v, d, rf)
            ];
    }
  }
  else {
    return [
            /* tuple */0,
            /* Empty */0,
            /* Empty */0
          ];
  }
}

function cons_enum$1(_m, _e) {
  while(/* true */1) {
    var e = _e;
    var m = _m;
    if (m) {
      _e = [
        /* More */0,
        m[2],
        m[3],
        m[4],
        e
      ];
      _m = m[1];
    }
    else {
      return e;
    }
  };
}

function compare$3(cmp, m1, m2) {
  var _e1 = cons_enum$1(m1, /* End */0);
  var _e2 = cons_enum$1(m2, /* End */0);
  while(/* true */1) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var c = compare$2(e1[1], e2[1]);
        if (c !== 0) {
          return c;
        }
        else {
          var c$1 = cmp(e1[2], e2[2]);
          if (c$1 !== 0) {
            return c$1;
          }
          else {
            _e2 = cons_enum$1(e2[3], e2[4]);
            _e1 = cons_enum$1(e1[3], e1[4]);
          }
        }
      }
      else {
        return 1;
      }
    }
    else {
      if (e2) {
        return -1;
      }
      else {
        return 0;
      }
    }
  };
}

function equal$1(cmp, m1, m2) {
  var equal_aux = function (e1, e2) {
    if (e1) {
      if (e2) {
        return +(compare$2(e1[1], e2[1]) === 0 && cmp(e1[2], e2[2]) && equal_aux(cons_enum$1(e1[3], e1[4]), cons_enum$1(e2[3], e2[4])));
      }
      else {
        return /* false */0;
      }
    }
    else {
      if (e2) {
        return /* false */0;
      }
      else {
        return /* true */1;
      }
    }
  };
  return equal_aux(cons_enum$1(m1, /* End */0), cons_enum$1(m2, /* End */0));
}

function cardinal$1(param) {
  if (param) {
    return cardinal$1(param[1]) + 1 + cardinal$1(param[4]);
  }
  else {
    return 0;
  }
}

function bindings_aux$1(_accu, _param) {
  while(/* true */1) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[1];
      _accu = [
        /* :: */0,
        [
          /* tuple */0,
          param[2],
          param[3]
        ],
        bindings_aux$1(accu, param[4])
      ];
    }
    else {
      return accu;
    }
  };
}

function bindings$1(s) {
  return bindings_aux$1(/* [] */0, s);
}

var Meths = [
  0,
  /* Empty */0,
  is_empty$1,
  mem$1,
  add$1,
  singleton$1,
  remove$1,
  merge$1,
  compare$3,
  equal$1,
  iter$1,
  fold$1,
  for_all$1,
  exists$1,
  filter$1,
  partition$1,
  cardinal$1,
  bindings$1,
  min_binding$1,
  max_binding$1,
  min_binding$1,
  split$1,
  find$1,
  map$1,
  mapi$1
];

function compare$4(x, y) {
  return Caml_primitive.caml_int_compare(x, y);
}

function height$2(param) {
  if (param) {
    return param[5];
  }
  else {
    return 0;
  }
}

function create$2(l, x, d, r) {
  var hl = height$2(l);
  var hr = height$2(r);
  return [
          /* Node */0,
          l,
          x,
          d,
          r,
          hl >= hr ? hl + 1 : hr + 1
        ];
}

function singleton$2(x, d) {
  return [
          /* Node */0,
          /* Empty */0,
          x,
          d,
          /* Empty */0,
          1
        ];
}

function bal$2(l, x, d, r) {
  var hl = l ? l[5] : 0;
  var hr = r ? r[5] : 0;
  if (hl > hr + 2) {
    if (l) {
      var lr = l[4];
      var ld = l[3];
      var lv = l[2];
      var ll = l[1];
      if (height$2(ll) >= height$2(lr)) {
        return create$2(ll, lv, ld, create$2(lr, x, d, r));
      }
      else {
        if (lr) {
          return create$2(create$2(ll, lv, ld, lr[1]), lr[2], lr[3], create$2(lr[4], x, d, r));
        }
        else {
          return Pervasives.invalid_arg("Map.bal");
        }
      }
    }
    else {
      return Pervasives.invalid_arg("Map.bal");
    }
  }
  else {
    if (hr > hl + 2) {
      if (r) {
        var rr = r[4];
        var rd = r[3];
        var rv = r[2];
        var rl = r[1];
        if (height$2(rr) >= height$2(rl)) {
          return create$2(create$2(l, x, d, rl), rv, rd, rr);
        }
        else {
          if (rl) {
            return create$2(create$2(l, x, d, rl[1]), rl[2], rl[3], create$2(rl[4], rv, rd, rr));
          }
          else {
            return Pervasives.invalid_arg("Map.bal");
          }
        }
      }
      else {
        return Pervasives.invalid_arg("Map.bal");
      }
    }
    else {
      return [
              /* Node */0,
              l,
              x,
              d,
              r,
              hl >= hr ? hl + 1 : hr + 1
            ];
    }
  }
}

function is_empty$2(param) {
  if (param) {
    return /* false */0;
  }
  else {
    return /* true */1;
  }
}

function add$2(x, data, param) {
  if (param) {
    var r = param[4];
    var d = param[3];
    var v = param[2];
    var l = param[1];
    var c = compare$4(x, v);
    if (c) {
      if (c < 0) {
        return bal$2(add$2(x, data, l), v, d, r);
      }
      else {
        return bal$2(l, v, d, add$2(x, data, r));
      }
    }
    else {
      return [
              /* Node */0,
              l,
              x,
              data,
              r,
              param[5]
            ];
    }
  }
  else {
    return [
            /* Node */0,
            /* Empty */0,
            x,
            data,
            /* Empty */0,
            1
          ];
  }
}

function find$2(x, _param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      var c = compare$4(x, param[2]);
      if (c) {
        _param = c < 0 ? param[1] : param[4];
      }
      else {
        return param[3];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function mem$2(x, param) {
  if (param) {
    var c = compare$4(x, param[2]);
    return +(c === 0 || mem$2(x, c < 0 ? param[1] : param[4]));
  }
  else {
    return /* false */0;
  }
}

function min_binding$2(_param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      var l = param[1];
      if (l) {
        _param = l;
      }
      else {
        return [
                /* tuple */0,
                param[2],
                param[3]
              ];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function max_binding$2(_param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      var r = param[4];
      if (r) {
        _param = r;
      }
      else {
        return [
                /* tuple */0,
                param[2],
                param[3]
              ];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function remove_min_binding$2(param) {
  if (param) {
    var l = param[1];
    if (l) {
      return bal$2(remove_min_binding$2(l), param[2], param[3], param[4]);
    }
    else {
      return param[4];
    }
  }
  else {
    return Pervasives.invalid_arg("Map.remove_min_elt");
  }
}

function remove$2(x, param) {
  if (param) {
    var r = param[4];
    var d = param[3];
    var v = param[2];
    var l = param[1];
    var c = compare$4(x, v);
    if (c) {
      if (c < 0) {
        return bal$2(remove$2(x, l), v, d, r);
      }
      else {
        return bal$2(l, v, d, remove$2(x, r));
      }
    }
    else {
      var t1 = l;
      var t2 = r;
      if (t1) {
        if (t2) {
          var match = min_binding$2(t2);
          return bal$2(t1, match[1], match[2], remove_min_binding$2(t2));
        }
        else {
          return t1;
        }
      }
      else {
        return t2;
      }
    }
  }
  else {
    return /* Empty */0;
  }
}

function iter$2(f, _param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      iter$2(f, param[1]);
      f(param[2], param[3]);
      _param = param[4];
    }
    else {
      return /* () */0;
    }
  };
}

function map$2(f, param) {
  if (param) {
    var l$prime = map$2(f, param[1]);
    var d$prime = f(param[3]);
    var r$prime = map$2(f, param[4]);
    return [
            /* Node */0,
            l$prime,
            param[2],
            d$prime,
            r$prime,
            param[5]
          ];
  }
  else {
    return /* Empty */0;
  }
}

function mapi$2(f, param) {
  if (param) {
    var v = param[2];
    var l$prime = mapi$2(f, param[1]);
    var d$prime = f(v, param[3]);
    var r$prime = mapi$2(f, param[4]);
    return [
            /* Node */0,
            l$prime,
            v,
            d$prime,
            r$prime,
            param[5]
          ];
  }
  else {
    return /* Empty */0;
  }
}

function fold$2(f, _m, _accu) {
  while(/* true */1) {
    var accu = _accu;
    var m = _m;
    if (m) {
      _accu = f(m[2], m[3], fold$2(f, m[1], accu));
      _m = m[4];
    }
    else {
      return accu;
    }
  };
}

function for_all$2(p, param) {
  if (param) {
    return +(p(param[2], param[3]) && for_all$2(p, param[1]) && for_all$2(p, param[4]));
  }
  else {
    return /* true */1;
  }
}

function exists$2(p, param) {
  if (param) {
    return +(p(param[2], param[3]) || exists$2(p, param[1]) || exists$2(p, param[4]));
  }
  else {
    return /* false */0;
  }
}

function add_min_binding$2(k, v, param) {
  if (param) {
    return bal$2(add_min_binding$2(k, v, param[1]), param[2], param[3], param[4]);
  }
  else {
    return singleton$2(k, v);
  }
}

function add_max_binding$2(k, v, param) {
  if (param) {
    return bal$2(param[1], param[2], param[3], add_max_binding$2(k, v, param[4]));
  }
  else {
    return singleton$2(k, v);
  }
}

function join$2(l, v, d, r) {
  if (l) {
    if (r) {
      var rh = r[5];
      var lh = l[5];
      if (lh > rh + 2) {
        return bal$2(l[1], l[2], l[3], join$2(l[4], v, d, r));
      }
      else {
        if (rh > lh + 2) {
          return bal$2(join$2(l, v, d, r[1]), r[2], r[3], r[4]);
        }
        else {
          return create$2(l, v, d, r);
        }
      }
    }
    else {
      return add_max_binding$2(v, d, l);
    }
  }
  else {
    return add_min_binding$2(v, d, r);
  }
}

function concat$2(t1, t2) {
  if (t1) {
    if (t2) {
      var match = min_binding$2(t2);
      return join$2(t1, match[1], match[2], remove_min_binding$2(t2));
    }
    else {
      return t1;
    }
  }
  else {
    return t2;
  }
}

function concat_or_join$2(t1, v, d, t2) {
  if (d) {
    return join$2(t1, v, d[1], t2);
  }
  else {
    return concat$2(t1, t2);
  }
}

function split$2(x, param) {
  if (param) {
    var r = param[4];
    var d = param[3];
    var v = param[2];
    var l = param[1];
    var c = compare$4(x, v);
    if (c) {
      if (c < 0) {
        var match = split$2(x, l);
        return [
                /* tuple */0,
                match[1],
                match[2],
                join$2(match[3], v, d, r)
              ];
      }
      else {
        var match$1 = split$2(x, r);
        return [
                /* tuple */0,
                join$2(l, v, d, match$1[1]),
                match$1[2],
                match$1[3]
              ];
      }
    }
    else {
      return [
              /* tuple */0,
              l,
              [
                /* Some */0,
                d
              ],
              r
            ];
    }
  }
  else {
    return [
            /* tuple */0,
            /* Empty */0,
            /* None */0,
            /* Empty */0
          ];
  }
}

function merge$2(f, s1, s2) {
  var exit = 0;
  if (s1) {
    var v1 = s1[2];
    if (s1[5] >= height$2(s2)) {
      var match = split$2(v1, s2);
      return concat_or_join$2(merge$2(f, s1[1], match[1]), v1, f(v1, [
                      /* Some */0,
                      s1[3]
                    ], match[2]), merge$2(f, s1[4], match[3]));
    }
    else {
      exit = 1;
    }
  }
  else {
    if (s2) {
      exit = 1;
    }
    else {
      return /* Empty */0;
    }
  }
  if (exit === 1) {
    if (s2) {
      var v2 = s2[2];
      var match$1 = split$2(v2, s1);
      return concat_or_join$2(merge$2(f, match$1[1], s2[1]), v2, f(v2, match$1[2], [
                      /* Some */0,
                      s2[3]
                    ]), merge$2(f, match$1[3], s2[4]));
    }
    else {
      throw [
            0,
            Caml_exceptions.Assert_failure,
            [
              0,
              "map.ml",
              270,
              10
            ]
          ];
    }
  }
  
}

function filter$2(p, param) {
  if (param) {
    var d = param[3];
    var v = param[2];
    var l$prime = filter$2(p, param[1]);
    var pvd = p(v, d);
    var r$prime = filter$2(p, param[4]);
    if (pvd) {
      return join$2(l$prime, v, d, r$prime);
    }
    else {
      return concat$2(l$prime, r$prime);
    }
  }
  else {
    return /* Empty */0;
  }
}

function partition$2(p, param) {
  if (param) {
    var d = param[3];
    var v = param[2];
    var match = partition$2(p, param[1]);
    var lf = match[2];
    var lt = match[1];
    var pvd = p(v, d);
    var match$1 = partition$2(p, param[4]);
    var rf = match$1[2];
    var rt = match$1[1];
    if (pvd) {
      return [
              /* tuple */0,
              join$2(lt, v, d, rt),
              concat$2(lf, rf)
            ];
    }
    else {
      return [
              /* tuple */0,
              concat$2(lt, rt),
              join$2(lf, v, d, rf)
            ];
    }
  }
  else {
    return [
            /* tuple */0,
            /* Empty */0,
            /* Empty */0
          ];
  }
}

function cons_enum$2(_m, _e) {
  while(/* true */1) {
    var e = _e;
    var m = _m;
    if (m) {
      _e = [
        /* More */0,
        m[2],
        m[3],
        m[4],
        e
      ];
      _m = m[1];
    }
    else {
      return e;
    }
  };
}

function compare$5(cmp, m1, m2) {
  var _e1 = cons_enum$2(m1, /* End */0);
  var _e2 = cons_enum$2(m2, /* End */0);
  while(/* true */1) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var c = compare$4(e1[1], e2[1]);
        if (c !== 0) {
          return c;
        }
        else {
          var c$1 = cmp(e1[2], e2[2]);
          if (c$1 !== 0) {
            return c$1;
          }
          else {
            _e2 = cons_enum$2(e2[3], e2[4]);
            _e1 = cons_enum$2(e1[3], e1[4]);
          }
        }
      }
      else {
        return 1;
      }
    }
    else {
      if (e2) {
        return -1;
      }
      else {
        return 0;
      }
    }
  };
}

function equal$2(cmp, m1, m2) {
  var equal_aux = function (e1, e2) {
    if (e1) {
      if (e2) {
        return +(compare$4(e1[1], e2[1]) === 0 && cmp(e1[2], e2[2]) && equal_aux(cons_enum$2(e1[3], e1[4]), cons_enum$2(e2[3], e2[4])));
      }
      else {
        return /* false */0;
      }
    }
    else {
      if (e2) {
        return /* false */0;
      }
      else {
        return /* true */1;
      }
    }
  };
  return equal_aux(cons_enum$2(m1, /* End */0), cons_enum$2(m2, /* End */0));
}

function cardinal$2(param) {
  if (param) {
    return cardinal$2(param[1]) + 1 + cardinal$2(param[4]);
  }
  else {
    return 0;
  }
}

function bindings_aux$2(_accu, _param) {
  while(/* true */1) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[1];
      _accu = [
        /* :: */0,
        [
          /* tuple */0,
          param[2],
          param[3]
        ],
        bindings_aux$2(accu, param[4])
      ];
    }
    else {
      return accu;
    }
  };
}

function bindings$2(s) {
  return bindings_aux$2(/* [] */0, s);
}

var Labs = [
  0,
  /* Empty */0,
  is_empty$2,
  mem$2,
  add$2,
  singleton$2,
  remove$2,
  merge$2,
  compare$5,
  equal$2,
  iter$2,
  fold$2,
  for_all$2,
  exists$2,
  filter$2,
  partition$2,
  cardinal$2,
  bindings$2,
  min_binding$2,
  max_binding$2,
  min_binding$2,
  split$2,
  find$2,
  map$2,
  mapi$2
];

var dummy_table = [
  /* record */0,
  0,
  /* array */[dummy_item],
  /* Empty */0,
  /* Empty */0,
  /* [] */0,
  /* [] */0,
  /* Empty */0,
  /* [] */0
];

var table_count = [
  0,
  0
];

var dummy_met = [0];

function fit_size(n) {
  if (n <= 2) {
    return n;
  }
  else {
    return fit_size((n + 1) / 2 | 0) * 2;
  }
}

function new_table(pub_labels) {
  ++ table_count[1];
  var len = pub_labels.length;
  var methods = Caml_array.caml_make_vect(len * 2 + 2, dummy_met);
  methods[0] = len;
  methods[1] = (fit_size(len) * Sys.word_size / 8 | 0) - 1;
  for(var i = 0 ,i_finish = len - 1; i<= i_finish; ++i){
    methods[i * 2 + 3] = pub_labels[i];
  }
  return [
          /* record */0,
          initial_object_size,
          methods,
          /* Empty */0,
          /* Empty */0,
          /* [] */0,
          /* [] */0,
          /* Empty */0,
          /* [] */0
        ];
}

function resize(array, new_size) {
  var old_size = array[2].length;
  if (new_size > old_size) {
    var new_buck = Caml_array.caml_make_vect(new_size, dummy_met);
    $$Array.blit(array[2], 0, new_buck, 0, old_size);
    array[2] = new_buck;
    return /* () */0;
  }
  else {
    return 0;
  }
}

function put(array, label, element) {
  resize(array, label + 1);
  array[2][label] = element;
  return /* () */0;
}

var method_count = [
  0,
  0
];

var inst_var_count = [
  0,
  0
];

function new_method(table) {
  var index = table[2].length;
  resize(table, index + 1);
  return index;
}

function get_method_label(table, name) {
  try {
    return find$1(name, table[3]);
  }
  catch (exn){
    if (exn === Caml_exceptions.Not_found) {
      var label = new_method(table);
      table[3] = add$1(name, label, table[3]);
      table[4] = add$2(label, /* true */1, table[4]);
      return label;
    }
    else {
      throw exn;
    }
  }
}

function get_method_labels(table, names) {
  return $$Array.map(function (param) {
              return get_method_label(table, param);
            }, names);
}

function set_method(table, label, element) {
  ++ method_count[1];
  if (find$2(label, table[4])) {
    return put(table, label, element);
  }
  else {
    table[6] = [
      /* :: */0,
      [
        /* tuple */0,
        label,
        element
      ],
      table[6]
    ];
    return /* () */0;
  }
}

function get_method(table, label) {
  try {
    return List.assoc(label, table[6]);
  }
  catch (exn){
    if (exn === Caml_exceptions.Not_found) {
      return table[2][label];
    }
    else {
      throw exn;
    }
  }
}

function to_list(arr) {
  if (arr) {
    return $$Array.to_list(arr);
  }
  else {
    return /* [] */0;
  }
}

function narrow(table, vars, virt_meths, concr_meths) {
  var vars$1 = to_list(vars);
  var virt_meths$1 = to_list(virt_meths);
  var concr_meths$1 = to_list(concr_meths);
  var virt_meth_labs = List.map(function (param) {
        return get_method_label(table, param);
      }, virt_meths$1);
  var concr_meth_labs = List.map(function (param) {
        return get_method_label(table, param);
      }, concr_meths$1);
  table[5] = [
    /* :: */0,
    [
      /* tuple */0,
      table[3],
      table[4],
      table[6],
      table[7],
      virt_meth_labs,
      vars$1
    ],
    table[5]
  ];
  table[7] = fold(function (lab, info, tvars) {
        if (List.mem(lab, vars$1)) {
          return add(lab, info, tvars);
        }
        else {
          return tvars;
        }
      }, table[7], /* Empty */0);
  var by_name = [
    0,
    /* Empty */0
  ];
  var by_label = [
    0,
    /* Empty */0
  ];
  List.iter2(function (met, label) {
        by_name[1] = add$1(met, label, by_name[1]);
        var $js;
        try {
          $js = find$2(label, table[4]);
        }
        catch (exn){
          if (exn === Caml_exceptions.Not_found) {
            $js = /* true */1;
          }
          else {
            throw exn;
          }
        }
        by_label[1] = add$2(label, $js, by_label[1]);
        return /* () */0;
      }, concr_meths$1, concr_meth_labs);
  List.iter2(function (met, label) {
        by_name[1] = add$1(met, label, by_name[1]);
        by_label[1] = add$2(label, /* false */0, by_label[1]);
        return /* () */0;
      }, virt_meths$1, virt_meth_labs);
  table[3] = by_name[1];
  table[4] = by_label[1];
  table[6] = List.fold_right(function (met, hm) {
        if (List.mem(met[1], virt_meth_labs)) {
          return hm;
        }
        else {
          return [
                  /* :: */0,
                  met,
                  hm
                ];
        }
      }, table[6], /* [] */0);
  return /* () */0;
}

function widen(table) {
  var match = List.hd(table[5]);
  var virt_meths = match[5];
  table[5] = List.tl(table[5]);
  table[7] = List.fold_left(function (s, v) {
        return add(v, find(v, table[7]), s);
      }, match[4], match[6]);
  table[3] = match[1];
  table[4] = match[2];
  table[6] = List.fold_right(function (met, hm) {
        if (List.mem(met[1], virt_meths)) {
          return hm;
        }
        else {
          return [
                  /* :: */0,
                  met,
                  hm
                ];
        }
      }, table[6], match[3]);
  return /* () */0;
}

function new_slot(table) {
  var index = table[1];
  table[1] = index + 1;
  return index;
}

function new_variable(table, name) {
  try {
    return find(name, table[7]);
  }
  catch (exn){
    if (exn === Caml_exceptions.Not_found) {
      var index = new_slot(table);
      if (name !== "") {
        table[7] = add(name, index, table[7]);
      }
      return index;
    }
    else {
      throw exn;
    }
  }
}

function to_array(arr) {
  if (Caml_primitive.caml_equal(arr, 0)) {
    return /* array */[];
  }
  else {
    return arr;
  }
}

function new_methods_variables(table, meths, vals) {
  var meths$1 = to_array(meths);
  var nmeths = meths$1.length;
  var nvals = vals.length;
  var res = Caml_array.caml_make_vect(nmeths + nvals, 0);
  for(var i = 0 ,i_finish = nmeths - 1; i<= i_finish; ++i){
    res[i] = get_method_label(table, meths$1[i]);
  }
  for(var i$1 = 0 ,i_finish$1 = nvals - 1; i$1<= i_finish$1; ++i$1){
    res[i$1 + nmeths] = new_variable(table, vals[i$1]);
  }
  return res;
}

function get_variable(table, name) {
  try {
    return find(name, table[7]);
  }
  catch (exn){
    if (exn === Caml_exceptions.Not_found) {
      throw [
            0,
            Caml_exceptions.Assert_failure,
            [
              0,
              "test_internalOO.ml",
              280,
              50
            ]
          ];
    }
    else {
      throw exn;
    }
  }
}

function get_variables(table, names) {
  return $$Array.map(function (param) {
              return get_variable(table, param);
            }, names);
}

function add_initializer(table, f) {
  table[8] = [
    /* :: */0,
    f,
    table[8]
  ];
  return /* () */0;
}

function create_table(public_methods) {
  if (public_methods) {
    var tags = $$Array.map(public_method_label, public_methods);
    var table = new_table(tags);
    $$Array.iteri(function (i, met) {
          var lab = i * 2 + 2;
          table[3] = add$1(met, lab, table[3]);
          table[4] = add$2(lab, /* true */1, table[4]);
          return /* () */0;
        }, public_methods);
    return table;
  }
  else {
    return new_table(/* array */[]);
  }
}

function init_class(table) {
  inst_var_count[1] = inst_var_count[1] + table[1] - 1;
  table[8] = List.rev(table[8]);
  return resize(table, 3 + (table[2][1] * 16 / Sys.word_size | 0));
}

function inherits(cla, vals, virt_meths, concr_meths, param, top) {
  var $$super = param[2];
  narrow(cla, vals, virt_meths, concr_meths);
  var init = top ? $$super(cla, param[4]) : $$super(cla);
  widen(cla);
  return $$Array.concat([
              /* :: */0,
              /* array */[init],
              [
                /* :: */0,
                $$Array.map(function (param) {
                      return get_variable(cla, param);
                    }, to_array(vals)),
                [
                  /* :: */0,
                  $$Array.map(function (nm) {
                        return get_method(cla, get_method_label(cla, nm));
                      }, to_array(concr_meths)),
                  /* [] */0
                ]
              ]
            ]);
}

function make_class(pub_meths, class_init) {
  var table = create_table(pub_meths);
  var env_init = class_init(table);
  init_class(table);
  return [
          /* tuple */0,
          env_init(0),
          class_init,
          env_init,
          0
        ];
}

function make_class_store(pub_meths, class_init, init_table) {
  var table = create_table(pub_meths);
  var env_init = class_init(table);
  init_class(table);
  init_table[2] = class_init;
  init_table[1] = env_init;
  return /* () */0;
}

function dummy_class(loc) {
  var undef = function () {
    throw [
          0,
          Caml_exceptions.Undefined_recursive_module,
          loc
        ];
  };
  return [
          /* tuple */0,
          undef,
          undef,
          undef,
          0
        ];
}

function create_object(table) {
  var obj = Object.defineProperty(Caml_obj_runtime.caml_obj_block(Obj.object_tag, table[1]), "##ml",{"value" : true, "writable" : false});
  obj[0] = table[2];
  return Caml_exceptions.caml_set_oo_id(obj);
}

function create_object_opt(obj_0, table) {
  if (obj_0) {
    return obj_0;
  }
  else {
    var obj = Object.defineProperty(Caml_obj_runtime.caml_obj_block(Obj.object_tag, table[1]), "##ml",{"value" : true, "writable" : false});
    obj[0] = table[2];
    return Caml_exceptions.caml_set_oo_id(obj);
  }
}

function iter_f(obj, _param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      param[1](obj);
      _param = param[2];
    }
    else {
      return /* () */0;
    }
  };
}

function run_initializers(obj, table) {
  var inits = table[8];
  if (inits !== /* [] */0) {
    return iter_f(obj, inits);
  }
  else {
    return 0;
  }
}

function run_initializers_opt(obj_0, obj, table) {
  if (obj_0) {
    return obj;
  }
  else {
    var inits = table[8];
    if (inits !== /* [] */0) {
      iter_f(obj, inits);
    }
    return obj;
  }
}

function create_object_and_run_initializers(obj_0, table) {
  if (obj_0) {
    return obj_0;
  }
  else {
    var obj = create_object(table);
    run_initializers(obj, table);
    return obj;
  }
}

function build_path(n, keys, tables) {
  var res = [
    /* record */0,
    0,
    /* Empty */0,
    /* Empty */0
  ];
  var r = res;
  for(var i = 0; i<= n; ++i){
    r = [
      /* Cons */0,
      keys[i],
      r,
      /* Empty */0
    ];
  }
  tables[2] = r;
  return res;
}

function lookup_keys(i, keys, tables) {
  if (i < 0) {
    return tables;
  }
  else {
    var key = keys[i];
    var _tables = tables;
    while(/* true */1) {
      var tables$1 = _tables;
      if (tables$1[1] === key) {
        return lookup_keys(i - 1, keys, tables$1[2]);
      }
      else {
        if (tables$1[3] !== /* Empty */0) {
          _tables = tables$1[3];
        }
        else {
          var next = [
            /* Cons */0,
            key,
            /* Empty */0,
            /* Empty */0
          ];
          tables$1[3] = next;
          return build_path(i - 1, keys, next);
        }
      }
    };
  }
}

function lookup_tables(root, keys) {
  if (root[2] !== /* Empty */0) {
    return lookup_keys(keys.length - 1, keys, root[2]);
  }
  else {
    return build_path(keys.length - 1, keys, root);
  }
}

function get_const(x) {
  return function () {
    return x;
  };
}

function get_var(n) {
  return function (obj) {
    return obj[n];
  };
}

function get_env(e, n) {
  return function (obj) {
    return obj[e][n];
  };
}

function get_meth(n) {
  return function (obj) {
    return obj[1][n](obj);
  };
}

function set_var(n) {
  return function (obj, x) {
    obj[n] = x;
    return /* () */0;
  };
}

function app_const(f, x) {
  return function () {
    return f(x);
  };
}

function app_var(f, n) {
  return function (obj) {
    return f(obj[n]);
  };
}

function app_env(f, e, n) {
  return function (obj) {
    return f(obj[e][n]);
  };
}

function app_meth(f, n) {
  return function (obj) {
    return f(obj[1][n](obj));
  };
}

function app_const_const(f, x, y) {
  return function () {
    return f(x, y);
  };
}

function app_const_var(f, x, n) {
  return function (obj) {
    return f(x, obj[n]);
  };
}

function app_const_meth(f, x, n) {
  return function (obj) {
    return f(x, obj[1][n](obj));
  };
}

function app_var_const(f, n, x) {
  return function (obj) {
    return f(obj[n], x);
  };
}

function app_meth_const(f, n, x) {
  return function (obj) {
    return f(obj[1][n](obj), x);
  };
}

function app_const_env(f, x, e, n) {
  return function (obj) {
    return f(x, obj[e][n]);
  };
}

function app_env_const(f, e, n, x) {
  return function (obj) {
    return f(obj[e][n], x);
  };
}

function meth_app_const(n, x) {
  return function (obj) {
    return obj[1][n](obj, x);
  };
}

function meth_app_var(n, m) {
  return function (obj) {
    return obj[1][n](obj, obj[m]);
  };
}

function meth_app_env(n, e, m) {
  return function (obj) {
    return obj[1][n](obj, obj[e][m]);
  };
}

function meth_app_meth(n, m) {
  return function (obj) {
    return obj[1][n](obj, obj[1][m](obj));
  };
}

function send_const(m, x, _) {
  return function () {
    return Caml_oo.caml_get_public_method(x, m, 1)(x);
  };
}

function send_var(m, n, _) {
  return function (obj) {
    return Caml_oo.caml_get_public_method(obj[n], m, 2)(obj[n]);
  };
}

function send_env(m, e, n, _) {
  return function (obj) {
    return Caml_oo.caml_get_public_method(obj[e][n], m, 3)(obj[e][n]);
  };
}

function send_meth(m, n, _) {
  return function (obj) {
    return Caml_oo.caml_get_public_method(obj[1][n](obj), m, 4)(obj[1][n](obj));
  };
}

function new_cache(table) {
  var n = new_method(table);
  var n$1 = n % 2 === 0 || n > 2 + (table[2][1] * 16 / Sys.word_size | 0) ? n : new_method(table);
  table[2][n$1] = 0;
  return n$1;
}

function method_impl(table, i, arr) {
  var next = function () {
    ++ i[1];
    return arr[i[1]];
  };
  var clo = next(/* () */0);
  if (typeof clo === "number") {
    switch (clo) {
      case 0 : 
          return get_const(next(/* () */0));
      case 1 : 
          return get_var(next(/* () */0));
      case 2 : 
          var e = next(/* () */0);
          var n = next(/* () */0);
          return get_env(e, n);
      case 3 : 
          return get_meth(next(/* () */0));
      case 4 : 
          return set_var(next(/* () */0));
      case 5 : 
          var f = next(/* () */0);
          var x = next(/* () */0);
          return app_const(f, x);
      case 6 : 
          var f$1 = next(/* () */0);
          var n$1 = next(/* () */0);
          return app_var(f$1, n$1);
      case 7 : 
          var f$2 = next(/* () */0);
          var e$1 = next(/* () */0);
          var n$2 = next(/* () */0);
          return app_env(f$2, e$1, n$2);
      case 8 : 
          var f$3 = next(/* () */0);
          var n$3 = next(/* () */0);
          return app_meth(f$3, n$3);
      case 9 : 
          var f$4 = next(/* () */0);
          var x$1 = next(/* () */0);
          var y = next(/* () */0);
          return app_const_const(f$4, x$1, y);
      case 10 : 
          var f$5 = next(/* () */0);
          var x$2 = next(/* () */0);
          var n$4 = next(/* () */0);
          return app_const_var(f$5, x$2, n$4);
      case 11 : 
          var f$6 = next(/* () */0);
          var x$3 = next(/* () */0);
          var e$2 = next(/* () */0);
          var n$5 = next(/* () */0);
          return app_const_env(f$6, x$3, e$2, n$5);
      case 12 : 
          var f$7 = next(/* () */0);
          var x$4 = next(/* () */0);
          var n$6 = next(/* () */0);
          return app_const_meth(f$7, x$4, n$6);
      case 13 : 
          var f$8 = next(/* () */0);
          var n$7 = next(/* () */0);
          var x$5 = next(/* () */0);
          return app_var_const(f$8, n$7, x$5);
      case 14 : 
          var f$9 = next(/* () */0);
          var e$3 = next(/* () */0);
          var n$8 = next(/* () */0);
          var x$6 = next(/* () */0);
          return app_env_const(f$9, e$3, n$8, x$6);
      case 15 : 
          var f$10 = next(/* () */0);
          var n$9 = next(/* () */0);
          var x$7 = next(/* () */0);
          return app_meth_const(f$10, n$9, x$7);
      case 16 : 
          var n$10 = next(/* () */0);
          var x$8 = next(/* () */0);
          return meth_app_const(n$10, x$8);
      case 17 : 
          var n$11 = next(/* () */0);
          var m = next(/* () */0);
          return meth_app_var(n$11, m);
      case 18 : 
          var n$12 = next(/* () */0);
          var e$4 = next(/* () */0);
          var m$1 = next(/* () */0);
          return meth_app_env(n$12, e$4, m$1);
      case 19 : 
          var n$13 = next(/* () */0);
          var m$2 = next(/* () */0);
          return meth_app_meth(n$13, m$2);
      case 20 : 
          var m$3 = next(/* () */0);
          var x$9 = next(/* () */0);
          return send_const(m$3, x$9, new_cache(table));
      case 21 : 
          var m$4 = next(/* () */0);
          var n$14 = next(/* () */0);
          return send_var(m$4, n$14, new_cache(table));
      case 22 : 
          var m$5 = next(/* () */0);
          var e$5 = next(/* () */0);
          var n$15 = next(/* () */0);
          return send_env(m$5, e$5, n$15, new_cache(table));
      case 23 : 
          var m$6 = next(/* () */0);
          var n$16 = next(/* () */0);
          return send_meth(m$6, n$16, new_cache(table));
      
    }
  }
  else {
    return clo;
  }
}

function set_methods(table, methods) {
  var len = methods.length;
  var i = [
    0,
    0
  ];
  while(i[1] < len) {
    var label = methods[i[1]];
    var clo = method_impl(table, i, methods);
    set_method(table, label, clo);
    ++ i[1];
  };
  return /* () */0;
}

function stats() {
  return [
          /* record */0,
          table_count[1],
          method_count[1],
          inst_var_count[1]
        ];
}

exports.copy = copy;
exports.params = params;
exports.step = step;
exports.initial_object_size = initial_object_size;
exports.dummy_item = dummy_item;
exports.public_method_label = public_method_label;
exports.Vars = Vars;
exports.Meths = Meths;
exports.Labs = Labs;
exports.dummy_table = dummy_table;
exports.table_count = table_count;
exports.dummy_met = dummy_met;
exports.fit_size = fit_size;
exports.new_table = new_table;
exports.resize = resize;
exports.put = put;
exports.method_count = method_count;
exports.inst_var_count = inst_var_count;
exports.new_method = new_method;
exports.get_method_label = get_method_label;
exports.get_method_labels = get_method_labels;
exports.set_method = set_method;
exports.get_method = get_method;
exports.to_list = to_list;
exports.narrow = narrow;
exports.widen = widen;
exports.new_slot = new_slot;
exports.new_variable = new_variable;
exports.to_array = to_array;
exports.new_methods_variables = new_methods_variables;
exports.get_variable = get_variable;
exports.get_variables = get_variables;
exports.add_initializer = add_initializer;
exports.create_table = create_table;
exports.init_class = init_class;
exports.inherits = inherits;
exports.make_class = make_class;
exports.make_class_store = make_class_store;
exports.dummy_class = dummy_class;
exports.create_object = create_object;
exports.create_object_opt = create_object_opt;
exports.iter_f = iter_f;
exports.run_initializers = run_initializers;
exports.run_initializers_opt = run_initializers_opt;
exports.create_object_and_run_initializers = create_object_and_run_initializers;
exports.build_path = build_path;
exports.lookup_keys = lookup_keys;
exports.lookup_tables = lookup_tables;
exports.get_const = get_const;
exports.get_var = get_var;
exports.get_env = get_env;
exports.get_meth = get_meth;
exports.set_var = set_var;
exports.app_const = app_const;
exports.app_var = app_var;
exports.app_env = app_env;
exports.app_meth = app_meth;
exports.app_const_const = app_const_const;
exports.app_const_var = app_const_var;
exports.app_const_meth = app_const_meth;
exports.app_var_const = app_var_const;
exports.app_meth_const = app_meth_const;
exports.app_const_env = app_const_env;
exports.app_env_const = app_env_const;
exports.meth_app_const = meth_app_const;
exports.meth_app_var = meth_app_var;
exports.meth_app_env = meth_app_env;
exports.meth_app_meth = meth_app_meth;
exports.send_const = send_const;
exports.send_var = send_var;
exports.send_env = send_env;
exports.send_meth = send_meth;
exports.new_cache = new_cache;
exports.method_impl = method_impl;
exports.set_methods = set_methods;
exports.stats = stats;
/* No side effect */
