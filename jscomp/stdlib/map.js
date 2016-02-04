// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Pervasives      = require("./pervasives");
var Caml_exceptions = require("../runtime/caml_exceptions");

function Make(funarg) {
  var height = function (param) {
    if (param) {
      return param[5];
    }
    else {
      return 0;
    }
  };
  var create = function (l, x, d, r) {
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
  };
  var singleton = function (x, d) {
    return [
            /* Node */0,
            /* Empty */0,
            x,
            d,
            /* Empty */0,
            1
          ];
  };
  var bal = function (l, x, d, r) {
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
        else if (lr) {
          return create(create(ll, lv, ld, lr[1]), lr[2], lr[3], create(lr[4], x, d, r));
        }
        else {
          return Pervasives.invalid_arg("Map.bal");
        }
      }
      else {
        return Pervasives.invalid_arg("Map.bal");
      }
    }
    else if (hr > hl + 2) {
      if (r) {
        var rr = r[4];
        var rd = r[3];
        var rv = r[2];
        var rl = r[1];
        if (height(rr) >= height(rl)) {
          return create(create(l, x, d, rl), rv, rd, rr);
        }
        else if (rl) {
          return create(create(l, x, d, rl[1]), rl[2], rl[3], create(rl[4], rv, rd, rr));
        }
        else {
          return Pervasives.invalid_arg("Map.bal");
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
  };
  var is_empty = function (param) {
    if (param) {
      return /* false */0;
    }
    else {
      return /* true */1;
    }
  };
  var add = function (x, data, param) {
    if (param) {
      var r = param[4];
      var d = param[3];
      var v = param[2];
      var l = param[1];
      var c = funarg[1](x, v);
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
  };
  var find = function (x, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var c = funarg[1](x, param[2]);
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
  };
  var mem = function (x, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var c = funarg[1](x, param[2]);
        if (c) {
          _param = c < 0 ? param[1] : param[4];
        }
        else {
          return /* true */1;
        }
      }
      else {
        return /* false */0;
      }
    };
  };
  var min_binding = function (_param) {
    while(true) {
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
  };
  var max_binding = function (_param) {
    while(true) {
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
  };
  var remove_min_binding = function (param) {
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
  };
  var remove = function (x, param) {
    if (param) {
      var r = param[4];
      var d = param[3];
      var v = param[2];
      var l = param[1];
      var c = funarg[1](x, v);
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
  };
  var iter = function (f, _param) {
    while(true) {
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
  };
  var map = function (f, param) {
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
  };
  var mapi = function (f, param) {
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
  };
  var fold = function (f, _m, _accu) {
    while(true) {
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
  };
  var for_all = function (p, _param) {
    while(true) {
      var param = _param;
      if (param) {
        if (p(param[2], param[3])) {
          if (for_all(p, param[1])) {
            _param = param[4];
          }
          else {
            return /* false */0;
          }
        }
        else {
          return /* false */0;
        }
      }
      else {
        return /* true */1;
      }
    };
  };
  var exists = function (p, _param) {
    while(true) {
      var param = _param;
      if (param) {
        if (p(param[2], param[3])) {
          return /* true */1;
        }
        else if (exists(p, param[1])) {
          return /* true */1;
        }
        else {
          _param = param[4];
        }
      }
      else {
        return /* false */0;
      }
    };
  };
  var add_min_binding = function (k, v, param) {
    if (param) {
      return bal(add_min_binding(k, v, param[1]), param[2], param[3], param[4]);
    }
    else {
      return singleton(k, v);
    }
  };
  var add_max_binding = function (k, v, param) {
    if (param) {
      return bal(param[1], param[2], param[3], add_max_binding(k, v, param[4]));
    }
    else {
      return singleton(k, v);
    }
  };
  var join = function (l, v, d, r) {
    if (l) {
      if (r) {
        var rh = r[5];
        var lh = l[5];
        if (lh > rh + 2) {
          return bal(l[1], l[2], l[3], join(l[4], v, d, r));
        }
        else if (rh > lh + 2) {
          return bal(join(l, v, d, r[1]), r[2], r[3], r[4]);
        }
        else {
          return create(l, v, d, r);
        }
      }
      else {
        return add_max_binding(v, d, l);
      }
    }
    else {
      return add_min_binding(v, d, r);
    }
  };
  var concat = function (t1, t2) {
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
  };
  var concat_or_join = function (t1, v, d, t2) {
    if (d) {
      return join(t1, v, d[1], t2);
    }
    else {
      return concat(t1, t2);
    }
  };
  var split = function (x, param) {
    if (param) {
      var r = param[4];
      var d = param[3];
      var v = param[2];
      var l = param[1];
      var c = funarg[1](x, v);
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
  };
  var merge = function (f, s1, s2) {
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
    else if (s2) {
      exit = 1;
    }
    else {
      return /* Empty */0;
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
    
  };
  var filter = function (p, param) {
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
  };
  var partition = function (p, param) {
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
  };
  var cons_enum = function (_m, _e) {
    while(true) {
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
  };
  var compare = function (cmp, m1, m2) {
    var _e1 = cons_enum(m1, /* End */0);
    var _e2 = cons_enum(m2, /* End */0);
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1) {
        if (e2) {
          var c = funarg[1](e1[1], e2[1]);
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
      else if (e2) {
        return -1;
      }
      else {
        return 0;
      }
    };
  };
  var equal = function (cmp, m1, m2) {
    var _e1 = cons_enum(m1, /* End */0);
    var _e2 = cons_enum(m2, /* End */0);
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1) {
        if (e2) {
          if (funarg[1](e1[1], e2[1])) {
            return /* false */0;
          }
          else if (cmp(e1[2], e2[2])) {
            _e2 = cons_enum(e2[3], e2[4]);
            _e1 = cons_enum(e1[3], e1[4]);
          }
          else {
            return /* false */0;
          }
        }
        else {
          return /* false */0;
        }
      }
      else if (e2) {
        return /* false */0;
      }
      else {
        return /* true */1;
      }
    };
  };
  var cardinal = function (param) {
    if (param) {
      return cardinal(param[1]) + 1 + cardinal(param[4]);
    }
    else {
      return 0;
    }
  };
  var bindings_aux = function (_accu, _param) {
    while(true) {
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
  };
  var bindings = function (s) {
    return bindings_aux(/* [] */0, s);
  };
  return [
          0,
          /* Empty */0,
          is_empty,
          mem,
          add,
          singleton,
          remove,
          merge,
          compare,
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
}

exports.Make = Make;
/* No side effect */
