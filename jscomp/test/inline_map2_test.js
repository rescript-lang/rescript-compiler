'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function Make(Ord) {
  var height = function (param) {
    if (param) {
      return param[4];
    } else {
      return 0;
    }
  };
  var create = function (l, x, d, r) {
    var hl = height(l);
    var hr = height(r);
    return /* Node */[
            l,
            x,
            d,
            r,
            hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  };
  var singleton = function (x, d) {
    return /* Node */[
            /* Empty */0,
            x,
            d,
            /* Empty */0,
            1
          ];
  };
  var bal = function (l, x, d, r) {
    var hl = l ? l[4] : 0;
    var hr = r ? r[4] : 0;
    if (hl > (hr + 2 | 0)) {
      if (l) {
        var lr = l[3];
        var ld = l[2];
        var lv = l[1];
        var ll = l[0];
        if (height(ll) >= height(lr)) {
          return create(ll, lv, ld, create(lr, x, d, r));
        } else if (lr) {
          return create(create(ll, lv, ld, lr[0]), lr[1], lr[2], create(lr[3], x, d, r));
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
      if (r) {
        var rr = r[3];
        var rd = r[2];
        var rv = r[1];
        var rl = r[0];
        if (height(rr) >= height(rl)) {
          return create(create(l, x, d, rl), rv, rd, rr);
        } else if (rl) {
          return create(create(l, x, d, rl[0]), rl[1], rl[2], create(rl[3], rv, rd, rr));
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
      return /* Node */[
              l,
              x,
              d,
              r,
              hl >= hr ? hl + 1 | 0 : hr + 1 | 0
            ];
    }
  };
  var is_empty = function (param) {
    if (param) {
      return /* false */0;
    } else {
      return /* true */1;
    }
  };
  var add = function (x, data, param) {
    if (param) {
      var r = param[3];
      var d = param[2];
      var v = param[1];
      var l = param[0];
      var c = Curry._2(Ord[/* compare */0], x, v);
      if (c === 0) {
        return /* Node */[
                l,
                x,
                data,
                r,
                param[4]
              ];
      } else if (c < 0) {
        return bal(add(x, data, l), v, d, r);
      } else {
        return bal(l, v, d, add(x, data, r));
      }
    } else {
      return /* Node */[
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
        var c = Curry._2(Ord[/* compare */0], x, param[1]);
        if (c === 0) {
          return param[2];
        } else {
          _param = c < 0 ? param[0] : param[3];
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
      if (param) {
        var c = Curry._2(Ord[/* compare */0], x, param[1]);
        if (c === 0) {
          return /* true */1;
        } else {
          _param = c < 0 ? param[0] : param[3];
          continue ;
          
        }
      } else {
        return /* false */0;
      }
    };
  };
  var min_binding = function (_param) {
    while(true) {
      var param = _param;
      if (param) {
        var l = param[0];
        if (l) {
          _param = l;
          continue ;
          
        } else {
          return /* tuple */[
                  param[1],
                  param[2]
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
      if (param) {
        var r = param[3];
        if (r) {
          _param = r;
          continue ;
          
        } else {
          return /* tuple */[
                  param[1],
                  param[2]
                ];
        }
      } else {
        throw Caml_builtin_exceptions.not_found;
      }
    };
  };
  var remove_min_binding = function (param) {
    if (param) {
      var l = param[0];
      if (l) {
        return bal(remove_min_binding(l), param[1], param[2], param[3]);
      } else {
        return param[3];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.remove_min_elt"
          ];
    }
  };
  var remove = function (x, param) {
    if (param) {
      var r = param[3];
      var d = param[2];
      var v = param[1];
      var l = param[0];
      var c = Curry._2(Ord[/* compare */0], x, v);
      if (c === 0) {
        var t1 = l;
        var t2 = r;
        if (t1) {
          if (t2) {
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
      return /* Empty */0;
    }
  };
  var iter = function (f, _param) {
    while(true) {
      var param = _param;
      if (param) {
        iter(f, param[0]);
        Curry._2(f, param[1], param[2]);
        _param = param[3];
        continue ;
        
      } else {
        return /* () */0;
      }
    };
  };
  var map = function (f, param) {
    if (param) {
      var l$prime = map(f, param[0]);
      var d$prime = Curry._1(f, param[2]);
      var r$prime = map(f, param[3]);
      return /* Node */[
              l$prime,
              param[1],
              d$prime,
              r$prime,
              param[4]
            ];
    } else {
      return /* Empty */0;
    }
  };
  var mapi = function (f, param) {
    if (param) {
      var v = param[1];
      var l$prime = mapi(f, param[0]);
      var d$prime = Curry._2(f, v, param[2]);
      var r$prime = mapi(f, param[3]);
      return /* Node */[
              l$prime,
              v,
              d$prime,
              r$prime,
              param[4]
            ];
    } else {
      return /* Empty */0;
    }
  };
  var fold = function (f, _m, _accu) {
    while(true) {
      var accu = _accu;
      var m = _m;
      if (m) {
        _accu = Curry._3(f, m[1], m[2], fold(f, m[0], accu));
        _m = m[3];
        continue ;
        
      } else {
        return accu;
      }
    };
  };
  var for_all = function (p, _param) {
    while(true) {
      var param = _param;
      if (param) {
        if (Curry._2(p, param[1], param[2]) && for_all(p, param[0])) {
          _param = param[3];
          continue ;
          
        } else {
          return /* false */0;
        }
      } else {
        return /* true */1;
      }
    };
  };
  var exists = function (p, _param) {
    while(true) {
      var param = _param;
      if (param) {
        if (Curry._2(p, param[1], param[2]) || exists(p, param[0])) {
          return /* true */1;
        } else {
          _param = param[3];
          continue ;
          
        }
      } else {
        return /* false */0;
      }
    };
  };
  var add_min_binding = function (k, v, param) {
    if (param) {
      return bal(add_min_binding(k, v, param[0]), param[1], param[2], param[3]);
    } else {
      return singleton(k, v);
    }
  };
  var add_max_binding = function (k, v, param) {
    if (param) {
      return bal(param[0], param[1], param[2], add_max_binding(k, v, param[3]));
    } else {
      return singleton(k, v);
    }
  };
  var join = function (l, v, d, r) {
    if (l) {
      if (r) {
        var rh = r[4];
        var lh = l[4];
        if (lh > (rh + 2 | 0)) {
          return bal(l[0], l[1], l[2], join(l[3], v, d, r));
        } else if (rh > (lh + 2 | 0)) {
          return bal(join(l, v, d, r[0]), r[1], r[2], r[3]);
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
    if (t1) {
      if (t2) {
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
    if (d) {
      return join(t1, v, d[0], t2);
    } else {
      return concat(t1, t2);
    }
  };
  var split = function (x, param) {
    if (param) {
      var r = param[3];
      var d = param[2];
      var v = param[1];
      var l = param[0];
      var c = Curry._2(Ord[/* compare */0], x, v);
      if (c === 0) {
        return /* tuple */[
                l,
                /* Some */[d],
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
              /* Empty */0,
              /* None */0,
              /* Empty */0
            ];
    }
  };
  var merge = function (f, s1, s2) {
    var exit = 0;
    if (s1) {
      var v1 = s1[1];
      if (s1[4] >= height(s2)) {
        var match = split(v1, s2);
        return concat_or_join(merge(f, s1[0], match[0]), v1, Curry._3(f, v1, /* Some */[s1[2]], match[1]), merge(f, s1[3], match[2]));
      } else {
        exit = 1;
      }
    } else if (s2) {
      exit = 1;
    } else {
      return /* Empty */0;
    }
    if (exit === 1) {
      if (s2) {
        var v2 = s2[1];
        var match$1 = split(v2, s1);
        return concat_or_join(merge(f, match$1[0], s2[0]), v2, Curry._3(f, v2, match$1[1], /* Some */[s2[2]]), merge(f, match$1[2], s2[3]));
      } else {
        throw [
              Caml_builtin_exceptions.assert_failure,
              [
                "inline_map2_test.ml",
                270,
                10
              ]
            ];
      }
    }
    
  };
  var filter = function (p, param) {
    if (param) {
      var d = param[2];
      var v = param[1];
      var l$prime = filter(p, param[0]);
      var pvd = Curry._2(p, v, d);
      var r$prime = filter(p, param[3]);
      if (pvd) {
        return join(l$prime, v, d, r$prime);
      } else {
        return concat(l$prime, r$prime);
      }
    } else {
      return /* Empty */0;
    }
  };
  var partition = function (p, param) {
    if (param) {
      var d = param[2];
      var v = param[1];
      var match = partition(p, param[0]);
      var lf = match[1];
      var lt = match[0];
      var pvd = Curry._2(p, v, d);
      var match$1 = partition(p, param[3]);
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
        _e = /* More */[
          m[1],
          m[2],
          m[3],
          e
        ];
        _m = m[0];
        continue ;
        
      } else {
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
          var c = Curry._2(Ord[/* compare */0], e1[0], e2[0]);
          if (c !== 0) {
            return c;
          } else {
            var c$1 = Curry._2(cmp, e1[1], e2[1]);
            if (c$1 !== 0) {
              return c$1;
            } else {
              _e2 = cons_enum(e2[2], e2[3]);
              _e1 = cons_enum(e1[2], e1[3]);
              continue ;
              
            }
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
  };
  var equal = function (cmp, m1, m2) {
    var _e1 = cons_enum(m1, /* End */0);
    var _e2 = cons_enum(m2, /* End */0);
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1) {
        if (e2 && Curry._2(Ord[/* compare */0], e1[0], e2[0]) === 0 && Curry._2(cmp, e1[1], e2[1])) {
          _e2 = cons_enum(e2[2], e2[3]);
          _e1 = cons_enum(e1[2], e1[3]);
          continue ;
          
        } else {
          return /* false */0;
        }
      } else if (e2) {
        return /* false */0;
      } else {
        return /* true */1;
      }
    };
  };
  var cardinal = function (param) {
    if (param) {
      return (cardinal(param[0]) + 1 | 0) + cardinal(param[3]) | 0;
    } else {
      return 0;
    }
  };
  var bindings_aux = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (param) {
        _param = param[0];
        _accu = /* :: */[
          /* tuple */[
            param[1],
            param[2]
          ],
          bindings_aux(accu, param[3])
        ];
        continue ;
        
      } else {
        return accu;
      }
    };
  };
  var bindings = function (s) {
    return bindings_aux(/* [] */0, s);
  };
  return /* module */[
          /* height */height,
          /* create */create,
          /* singleton */singleton,
          /* bal */bal,
          /* empty : Empty */0,
          /* is_empty */is_empty,
          /* add */add,
          /* find */find,
          /* mem */mem,
          /* min_binding */min_binding,
          /* max_binding */max_binding,
          /* remove_min_binding */remove_min_binding,
          /* remove */remove,
          /* iter */iter,
          /* map */map,
          /* mapi */mapi,
          /* fold */fold,
          /* for_all */for_all,
          /* exists */exists,
          /* add_min_binding */add_min_binding,
          /* add_max_binding */add_max_binding,
          /* join */join,
          /* concat */concat,
          /* concat_or_join */concat_or_join,
          /* split */split,
          /* merge */merge,
          /* filter */filter,
          /* partition */partition,
          /* cons_enum */cons_enum,
          /* compare */compare,
          /* equal */equal,
          /* cardinal */cardinal,
          /* bindings_aux */bindings_aux,
          /* bindings */bindings,
          /* choose */min_binding
        ];
}

function height(param) {
  if (param) {
    return param[4];
  } else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* Node */[
          l,
          x,
          d,
          r,
          hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function singleton(x, d) {
  return /* Node */[
          /* Empty */0,
          x,
          d,
          /* Empty */0,
          1
        ];
}

function bal(l, x, d, r) {
  var hl = l ? l[4] : 0;
  var hr = r ? r[4] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[3];
      var ld = l[2];
      var lv = l[1];
      var ll = l[0];
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      } else if (lr) {
        return create(create(ll, lv, ld, lr[0]), lr[1], lr[2], create(lr[3], x, d, r));
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
    if (r) {
      var rr = r[3];
      var rd = r[2];
      var rv = r[1];
      var rl = r[0];
      if (height(rr) >= height(rl)) {
        return create(create(l, x, d, rl), rv, rd, rr);
      } else if (rl) {
        return create(create(l, x, d, rl[0]), rl[1], rl[2], create(rl[3], rv, rd, rr));
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
    return /* Node */[
            l,
            x,
            d,
            r,
            hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function is_empty(param) {
  if (param) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

function add(x, data, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = Caml_primitive.caml_int_compare(x, v);
    if (c === 0) {
      return /* Node */[
              l,
              x,
              data,
              r,
              param[4]
            ];
    } else if (c < 0) {
      return bal(add(x, data, l), v, d, r);
    } else {
      return bal(l, v, d, add(x, data, r));
    }
  } else {
    return /* Node */[
            /* Empty */0,
            x,
            data,
            /* Empty */0,
            1
          ];
  }
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_primitive.caml_int_compare(x, param[1]);
      if (c === 0) {
        return param[2];
      } else {
        _param = c < 0 ? param[0] : param[3];
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
    if (param) {
      var c = Caml_primitive.caml_int_compare(x, param[1]);
      if (c === 0) {
        return /* true */1;
      } else {
        _param = c < 0 ? param[0] : param[3];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function min_binding(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param[0];
      if (l) {
        _param = l;
        continue ;
        
      } else {
        return /* tuple */[
                param[1],
                param[2]
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
    if (param) {
      var r = param[3];
      if (r) {
        _param = r;
        continue ;
        
      } else {
        return /* tuple */[
                param[1],
                param[2]
              ];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function remove_min_binding(param) {
  if (param) {
    var l = param[0];
    var exit = 0;
    if (l) {
      exit = 1;
    } else {
      return param[3];
    }
    if (exit === 1) {
      return bal(remove_min_binding(l), param[1], param[2], param[3]);
    }
    
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Map.remove_min_elt"
        ];
  }
}

function remove(x, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = Caml_primitive.caml_int_compare(x, v);
    if (c === 0) {
      var t1 = l;
      var t2 = r;
      var exit = 0;
      if (t1) {
        if (t2) {
          exit = 1;
        } else {
          return t1;
        }
      } else {
        return t2;
      }
      if (exit === 1) {
        var match = min_binding(t2);
        return bal(t1, match[0], match[1], remove_min_binding(t2));
      }
      
    } else if (c < 0) {
      return bal(remove(x, l), v, d, r);
    } else {
      return bal(l, v, d, remove(x, r));
    }
  } else {
    return /* Empty */0;
  }
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      iter(f, param[0]);
      Curry._2(f, param[1], param[2]);
      _param = param[3];
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function map(f, param) {
  if (param) {
    var l$prime = map(f, param[0]);
    var d$prime = Curry._1(f, param[2]);
    var r$prime = map(f, param[3]);
    return /* Node */[
            l$prime,
            param[1],
            d$prime,
            r$prime,
            param[4]
          ];
  } else {
    return /* Empty */0;
  }
}

function mapi(f, param) {
  if (param) {
    var v = param[1];
    var l$prime = mapi(f, param[0]);
    var d$prime = Curry._2(f, v, param[2]);
    var r$prime = mapi(f, param[3]);
    return /* Node */[
            l$prime,
            v,
            d$prime,
            r$prime,
            param[4]
          ];
  } else {
    return /* Empty */0;
  }
}

function fold(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (m) {
      _accu = Curry._3(f, m[1], m[2], fold(f, m[0], accu));
      _m = m[3];
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
      if (Curry._2(p, param[1], param[2]) && for_all(p, param[0])) {
        _param = param[3];
        continue ;
        
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
      if (Curry._2(p, param[1], param[2]) || exists(p, param[0])) {
        return /* true */1;
      } else {
        _param = param[3];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function add_min_binding(k, v, param) {
  if (param) {
    return bal(add_min_binding(k, v, param[0]), param[1], param[2], param[3]);
  } else {
    return singleton(k, v);
  }
}

function add_max_binding(k, v, param) {
  if (param) {
    return bal(param[0], param[1], param[2], add_max_binding(k, v, param[3]));
  } else {
    return singleton(k, v);
  }
}

function join(l, v, d, r) {
  if (l) {
    if (r) {
      var rh = r[4];
      var lh = l[4];
      if (lh > (rh + 2 | 0)) {
        return bal(l[0], l[1], l[2], join(l[3], v, d, r));
      } else if (rh > (lh + 2 | 0)) {
        return bal(join(l, v, d, r[0]), r[1], r[2], r[3]);
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
  var exit = 0;
  if (t1) {
    if (t2) {
      exit = 1;
    } else {
      return t1;
    }
  } else {
    return t2;
  }
  if (exit === 1) {
    var match = min_binding(t2);
    return join(t1, match[0], match[1], remove_min_binding(t2));
  }
  
}

function concat_or_join(t1, v, d, t2) {
  if (d) {
    return join(t1, v, d[0], t2);
  } else {
    return concat(t1, t2);
  }
}

function split(x, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = Caml_primitive.caml_int_compare(x, v);
    if (c === 0) {
      return /* tuple */[
              l,
              /* Some */[d],
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
            /* Empty */0,
            /* None */0,
            /* Empty */0
          ];
  }
}

function merge(f, s1, s2) {
  var exit = 0;
  if (s1) {
    var v1 = s1[1];
    if (s1[4] >= height(s2)) {
      var match = split(v1, s2);
      return concat_or_join(merge(f, s1[0], match[0]), v1, Curry._3(f, v1, /* Some */[s1[2]], match[1]), merge(f, s1[3], match[2]));
    } else {
      exit = 1;
    }
  } else if (s2) {
    exit = 1;
  } else {
    return /* Empty */0;
  }
  if (exit === 1) {
    if (s2) {
      var v2 = s2[1];
      var match$1 = split(v2, s1);
      return concat_or_join(merge(f, match$1[0], s2[0]), v2, Curry._3(f, v2, match$1[1], /* Some */[s2[2]]), merge(f, match$1[2], s2[3]));
    } else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "inline_map2_test.ml",
              270,
              10
            ]
          ];
    }
  }
  
}

function filter(p, param) {
  if (param) {
    var d = param[2];
    var v = param[1];
    var l$prime = filter(p, param[0]);
    var pvd = Curry._2(p, v, d);
    var r$prime = filter(p, param[3]);
    if (pvd) {
      return join(l$prime, v, d, r$prime);
    } else {
      return concat(l$prime, r$prime);
    }
  } else {
    return /* Empty */0;
  }
}

function partition(p, param) {
  if (param) {
    var d = param[2];
    var v = param[1];
    var match = partition(p, param[0]);
    var lf = match[1];
    var lt = match[0];
    var pvd = Curry._2(p, v, d);
    var match$1 = partition(p, param[3]);
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
            /* Empty */0,
            /* Empty */0
          ];
  }
}

function cons_enum(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (m) {
      _e = /* More */[
        m[1],
        m[2],
        m[3],
        e
      ];
      _m = m[0];
      continue ;
      
    } else {
      return e;
    }
  };
}

function compare(cmp, m1, m2) {
  var _e1 = cons_enum(m1, /* End */0);
  var _e2 = cons_enum(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var c = Caml_primitive.caml_int_compare(e1[0], e2[0]);
        if (c !== 0) {
          return c;
        } else {
          var c$1 = Curry._2(cmp, e1[1], e2[1]);
          if (c$1 !== 0) {
            return c$1;
          } else {
            _e2 = cons_enum(e2[2], e2[3]);
            _e1 = cons_enum(e1[2], e1[3]);
            continue ;
            
          }
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

function equal(cmp, m1, m2) {
  var _e1 = cons_enum(m1, /* End */0);
  var _e2 = cons_enum(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2 && e1[0] === e2[0] && Curry._2(cmp, e1[1], e2[1])) {
        _e2 = cons_enum(e2[2], e2[3]);
        _e1 = cons_enum(e1[2], e1[3]);
        continue ;
        
      } else {
        return /* false */0;
      }
    } else if (e2) {
      return /* false */0;
    } else {
      return /* true */1;
    }
  };
}

function cardinal(param) {
  if (param) {
    return (cardinal(param[0]) + 1 | 0) + cardinal(param[3]) | 0;
  } else {
    return 0;
  }
}

function bindings_aux(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[0];
      _accu = /* :: */[
        /* tuple */[
          param[1],
          param[2]
        ],
        bindings_aux(accu, param[3])
      ];
      continue ;
      
    } else {
      return accu;
    }
  };
}

function bindings(s) {
  return bindings_aux(/* [] */0, s);
}

var IntMap = /* module */[
  /* height */height,
  /* create */create,
  /* singleton */singleton,
  /* bal */bal,
  /* empty : Empty */0,
  /* is_empty */is_empty,
  /* add */add,
  /* find */find,
  /* mem */mem,
  /* min_binding */min_binding,
  /* max_binding */max_binding,
  /* remove_min_binding */remove_min_binding,
  /* remove */remove,
  /* iter */iter,
  /* map */map,
  /* mapi */mapi,
  /* fold */fold,
  /* for_all */for_all,
  /* exists */exists,
  /* add_min_binding */add_min_binding,
  /* add_max_binding */add_max_binding,
  /* join */join,
  /* concat */concat,
  /* concat_or_join */concat_or_join,
  /* split */split,
  /* merge */merge,
  /* filter */filter,
  /* partition */partition,
  /* cons_enum */cons_enum,
  /* compare */compare,
  /* equal */equal,
  /* cardinal */cardinal,
  /* bindings_aux */bindings_aux,
  /* bindings */bindings,
  /* choose */min_binding
];

var m = List.fold_left((function (acc, param) {
        return add(param[0], param[1], acc);
      }), /* Empty */0, /* :: */[
      /* tuple */[
        10,
        /* "a" */97
      ],
      /* :: */[
        /* tuple */[
          3,
          /* "b" */98
        ],
        /* :: */[
          /* tuple */[
            7,
            /* "c" */99
          ],
          /* :: */[
            /* tuple */[
              20,
              /* "d" */100
            ],
            /* [] */0
          ]
        ]
      ]
    ]);

function height$1(param) {
  if (param) {
    return param[4];
  } else {
    return 0;
  }
}

function create$1(l, x, d, r) {
  var hl = height$1(l);
  var hr = height$1(r);
  return /* Node */[
          l,
          x,
          d,
          r,
          hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function singleton$1(x, d) {
  return /* Node */[
          /* Empty */0,
          x,
          d,
          /* Empty */0,
          1
        ];
}

function bal$1(l, x, d, r) {
  var hl = l ? l[4] : 0;
  var hr = r ? r[4] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[3];
      var ld = l[2];
      var lv = l[1];
      var ll = l[0];
      if (height$1(ll) >= height$1(lr)) {
        return create$1(ll, lv, ld, create$1(lr, x, d, r));
      } else if (lr) {
        return create$1(create$1(ll, lv, ld, lr[0]), lr[1], lr[2], create$1(lr[3], x, d, r));
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
    if (r) {
      var rr = r[3];
      var rd = r[2];
      var rv = r[1];
      var rl = r[0];
      if (height$1(rr) >= height$1(rl)) {
        return create$1(create$1(l, x, d, rl), rv, rd, rr);
      } else if (rl) {
        return create$1(create$1(l, x, d, rl[0]), rl[1], rl[2], create$1(rl[3], rv, rd, rr));
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
    return /* Node */[
            l,
            x,
            d,
            r,
            hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function is_empty$1(param) {
  if (param) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

function add$1(x, data, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return /* Node */[
              l,
              x,
              data,
              r,
              param[4]
            ];
    } else if (c < 0) {
      return bal$1(add$1(x, data, l), v, d, r);
    } else {
      return bal$1(l, v, d, add$1(x, data, r));
    }
  } else {
    return /* Node */[
            /* Empty */0,
            x,
            data,
            /* Empty */0,
            1
          ];
  }
}

function find$1(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_primitive.caml_string_compare(x, param[1]);
      if (c === 0) {
        return param[2];
      } else {
        _param = c < 0 ? param[0] : param[3];
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
    if (param) {
      var c = Caml_primitive.caml_string_compare(x, param[1]);
      if (c === 0) {
        return /* true */1;
      } else {
        _param = c < 0 ? param[0] : param[3];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function min_binding$1(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param[0];
      if (l) {
        _param = l;
        continue ;
        
      } else {
        return /* tuple */[
                param[1],
                param[2]
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
    if (param) {
      var r = param[3];
      if (r) {
        _param = r;
        continue ;
        
      } else {
        return /* tuple */[
                param[1],
                param[2]
              ];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function remove_min_binding$1(param) {
  if (param) {
    var l = param[0];
    var exit = 0;
    if (l) {
      exit = 1;
    } else {
      return param[3];
    }
    if (exit === 1) {
      return bal$1(remove_min_binding$1(l), param[1], param[2], param[3]);
    }
    
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Map.remove_min_elt"
        ];
  }
}

function remove$1(x, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      var t1 = l;
      var t2 = r;
      var exit = 0;
      if (t1) {
        if (t2) {
          exit = 1;
        } else {
          return t1;
        }
      } else {
        return t2;
      }
      if (exit === 1) {
        var match = min_binding$1(t2);
        return bal$1(t1, match[0], match[1], remove_min_binding$1(t2));
      }
      
    } else if (c < 0) {
      return bal$1(remove$1(x, l), v, d, r);
    } else {
      return bal$1(l, v, d, remove$1(x, r));
    }
  } else {
    return /* Empty */0;
  }
}

function iter$1(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      iter$1(f, param[0]);
      Curry._2(f, param[1], param[2]);
      _param = param[3];
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function map$1(f, param) {
  if (param) {
    var l$prime = map$1(f, param[0]);
    var d$prime = Curry._1(f, param[2]);
    var r$prime = map$1(f, param[3]);
    return /* Node */[
            l$prime,
            param[1],
            d$prime,
            r$prime,
            param[4]
          ];
  } else {
    return /* Empty */0;
  }
}

function mapi$1(f, param) {
  if (param) {
    var v = param[1];
    var l$prime = mapi$1(f, param[0]);
    var d$prime = Curry._2(f, v, param[2]);
    var r$prime = mapi$1(f, param[3]);
    return /* Node */[
            l$prime,
            v,
            d$prime,
            r$prime,
            param[4]
          ];
  } else {
    return /* Empty */0;
  }
}

function fold$1(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (m) {
      _accu = Curry._3(f, m[1], m[2], fold$1(f, m[0], accu));
      _m = m[3];
      continue ;
      
    } else {
      return accu;
    }
  };
}

function for_all$1(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (Curry._2(p, param[1], param[2]) && for_all$1(p, param[0])) {
        _param = param[3];
        continue ;
        
      } else {
        return /* false */0;
      }
    } else {
      return /* true */1;
    }
  };
}

function exists$1(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (Curry._2(p, param[1], param[2]) || exists$1(p, param[0])) {
        return /* true */1;
      } else {
        _param = param[3];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function add_min_binding$1(k, v, param) {
  if (param) {
    return bal$1(add_min_binding$1(k, v, param[0]), param[1], param[2], param[3]);
  } else {
    return singleton$1(k, v);
  }
}

function add_max_binding$1(k, v, param) {
  if (param) {
    return bal$1(param[0], param[1], param[2], add_max_binding$1(k, v, param[3]));
  } else {
    return singleton$1(k, v);
  }
}

function join$1(l, v, d, r) {
  if (l) {
    if (r) {
      var rh = r[4];
      var lh = l[4];
      if (lh > (rh + 2 | 0)) {
        return bal$1(l[0], l[1], l[2], join$1(l[3], v, d, r));
      } else if (rh > (lh + 2 | 0)) {
        return bal$1(join$1(l, v, d, r[0]), r[1], r[2], r[3]);
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
  var exit = 0;
  if (t1) {
    if (t2) {
      exit = 1;
    } else {
      return t1;
    }
  } else {
    return t2;
  }
  if (exit === 1) {
    var match = min_binding$1(t2);
    return join$1(t1, match[0], match[1], remove_min_binding$1(t2));
  }
  
}

function concat_or_join$1(t1, v, d, t2) {
  if (d) {
    return join$1(t1, v, d[0], t2);
  } else {
    return concat$1(t1, t2);
  }
}

function split$1(x, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return /* tuple */[
              l,
              /* Some */[d],
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
            /* Empty */0,
            /* None */0,
            /* Empty */0
          ];
  }
}

function merge$1(f, s1, s2) {
  var exit = 0;
  if (s1) {
    var v1 = s1[1];
    if (s1[4] >= height$1(s2)) {
      var match = split$1(v1, s2);
      return concat_or_join$1(merge$1(f, s1[0], match[0]), v1, Curry._3(f, v1, /* Some */[s1[2]], match[1]), merge$1(f, s1[3], match[2]));
    } else {
      exit = 1;
    }
  } else if (s2) {
    exit = 1;
  } else {
    return /* Empty */0;
  }
  if (exit === 1) {
    if (s2) {
      var v2 = s2[1];
      var match$1 = split$1(v2, s1);
      return concat_or_join$1(merge$1(f, match$1[0], s2[0]), v2, Curry._3(f, v2, match$1[1], /* Some */[s2[2]]), merge$1(f, match$1[2], s2[3]));
    } else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "inline_map2_test.ml",
              270,
              10
            ]
          ];
    }
  }
  
}

function filter$1(p, param) {
  if (param) {
    var d = param[2];
    var v = param[1];
    var l$prime = filter$1(p, param[0]);
    var pvd = Curry._2(p, v, d);
    var r$prime = filter$1(p, param[3]);
    if (pvd) {
      return join$1(l$prime, v, d, r$prime);
    } else {
      return concat$1(l$prime, r$prime);
    }
  } else {
    return /* Empty */0;
  }
}

function partition$1(p, param) {
  if (param) {
    var d = param[2];
    var v = param[1];
    var match = partition$1(p, param[0]);
    var lf = match[1];
    var lt = match[0];
    var pvd = Curry._2(p, v, d);
    var match$1 = partition$1(p, param[3]);
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
            /* Empty */0,
            /* Empty */0
          ];
  }
}

function cons_enum$1(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (m) {
      _e = /* More */[
        m[1],
        m[2],
        m[3],
        e
      ];
      _m = m[0];
      continue ;
      
    } else {
      return e;
    }
  };
}

function compare$1(cmp, m1, m2) {
  var _e1 = cons_enum$1(m1, /* End */0);
  var _e2 = cons_enum$1(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var c = Caml_primitive.caml_string_compare(e1[0], e2[0]);
        if (c !== 0) {
          return c;
        } else {
          var c$1 = Curry._2(cmp, e1[1], e2[1]);
          if (c$1 !== 0) {
            return c$1;
          } else {
            _e2 = cons_enum$1(e2[2], e2[3]);
            _e1 = cons_enum$1(e1[2], e1[3]);
            continue ;
            
          }
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

function equal$1(cmp, m1, m2) {
  var _e1 = cons_enum$1(m1, /* End */0);
  var _e2 = cons_enum$1(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2 && Caml_primitive.caml_string_compare(e1[0], e2[0]) === 0 && Curry._2(cmp, e1[1], e2[1])) {
        _e2 = cons_enum$1(e2[2], e2[3]);
        _e1 = cons_enum$1(e1[2], e1[3]);
        continue ;
        
      } else {
        return /* false */0;
      }
    } else if (e2) {
      return /* false */0;
    } else {
      return /* true */1;
    }
  };
}

function cardinal$1(param) {
  if (param) {
    return (cardinal$1(param[0]) + 1 | 0) + cardinal$1(param[3]) | 0;
  } else {
    return 0;
  }
}

function bindings_aux$1(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[0];
      _accu = /* :: */[
        /* tuple */[
          param[1],
          param[2]
        ],
        bindings_aux$1(accu, param[3])
      ];
      continue ;
      
    } else {
      return accu;
    }
  };
}

function bindings$1(s) {
  return bindings_aux$1(/* [] */0, s);
}

var SMap = /* module */[
  /* height */height$1,
  /* create */create$1,
  /* singleton */singleton$1,
  /* bal */bal$1,
  /* empty : Empty */0,
  /* is_empty */is_empty$1,
  /* add */add$1,
  /* find */find$1,
  /* mem */mem$1,
  /* min_binding */min_binding$1,
  /* max_binding */max_binding$1,
  /* remove_min_binding */remove_min_binding$1,
  /* remove */remove$1,
  /* iter */iter$1,
  /* map */map$1,
  /* mapi */mapi$1,
  /* fold */fold$1,
  /* for_all */for_all$1,
  /* exists */exists$1,
  /* add_min_binding */add_min_binding$1,
  /* add_max_binding */add_max_binding$1,
  /* join */join$1,
  /* concat */concat$1,
  /* concat_or_join */concat_or_join$1,
  /* split */split$1,
  /* merge */merge$1,
  /* filter */filter$1,
  /* partition */partition$1,
  /* cons_enum */cons_enum$1,
  /* compare */compare$1,
  /* equal */equal$1,
  /* cardinal */cardinal$1,
  /* bindings_aux */bindings_aux$1,
  /* bindings */bindings$1,
  /* choose */min_binding$1
];

var s = List.fold_left((function (acc, param) {
        return add$1(param[0], param[1], acc);
      }), /* Empty */0, /* :: */[
      /* tuple */[
        "10",
        /* "a" */97
      ],
      /* :: */[
        /* tuple */[
          "3",
          /* "b" */98
        ],
        /* :: */[
          /* tuple */[
            "7",
            /* "c" */99
          ],
          /* :: */[
            /* tuple */[
              "20",
              /* "d" */100
            ],
            /* [] */0
          ]
        ]
      ]
    ]);

Mt.from_pair_suites("inline_map2_test.ml", /* :: */[
      /* tuple */[
        "assertion1",
        (function () {
            return /* Eq */Block.__(0, [
                      find(10, m),
                      /* "a" */97
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "assertion2",
          (function () {
              return /* Eq */Block.__(0, [
                        find$1("10", s),
                        /* "a" */97
                      ]);
            })
        ],
        /* [] */0
      ]
    ]);

var empty = /* Empty */0;

exports.Make = Make;
exports.IntMap = IntMap;
exports.empty = empty;
exports.m = m;
exports.SMap = SMap;
exports.s = s;
/* m Not a pure module */
