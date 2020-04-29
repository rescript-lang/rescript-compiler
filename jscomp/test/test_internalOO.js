'use strict';

var Sys = require("../../lib/js/sys.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_oo = require("../../lib/js/caml_oo.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function copy(o) {
  return Caml_exceptions.caml_set_oo_id(Caml_obj.caml_obj_dup(o));
}

var params = {
  compact_table: true,
  copy_parent: true,
  clean_when_copying: true,
  retry_count: 3,
  bucket_small_size: 16
};

var step = Sys.word_size / 16 | 0;

function public_method_label(s) {
  var accu = 0;
  for(var i = 0 ,i_finish = s.length; i < i_finish; ++i){
    accu = Caml_int32.imul(223, accu) + Caml_string.get(s, i) | 0;
  }
  accu = accu & 2147483647;
  if (accu > 1073741823) {
    return accu - -2147483648 | 0;
  } else {
    return accu;
  }
}

function height(param) {
  if (param) {
    return param[/* h */4];
  } else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* Node */[
          /* l */l,
          /* v */x,
          /* d */d,
          /* r */r,
          /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function singleton(x, d) {
  return /* Node */[
          /* l : Empty */0,
          /* v */x,
          /* d */d,
          /* r : Empty */0,
          /* h */1
        ];
}

function bal(l, x, d, r) {
  var hl = l ? l[/* h */4] : 0;
  var hr = r ? r[/* h */4] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[/* r */3];
      var ld = l[/* d */2];
      var lv = l[/* v */1];
      var ll = l[/* l */0];
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      }
      if (lr) {
        return create(create(ll, lv, ld, lr[/* l */0]), lr[/* v */1], lr[/* d */2], create(lr[/* r */3], x, d, r));
      }
      throw {
            ExceptionID: -3,
            _1: "Map.bal",
            Debug: "Invalid_argument"
          };
    }
    throw {
          ExceptionID: -3,
          _1: "Map.bal",
          Debug: "Invalid_argument"
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */[
            /* l */l,
            /* v */x,
            /* d */d,
            /* r */r,
            /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
  if (r) {
    var rr = r[/* r */3];
    var rd = r[/* d */2];
    var rv = r[/* v */1];
    var rl = r[/* l */0];
    if (height(rr) >= height(rl)) {
      return create(create(l, x, d, rl), rv, rd, rr);
    }
    if (rl) {
      return create(create(l, x, d, rl[/* l */0]), rl[/* v */1], rl[/* d */2], create(rl[/* r */3], rv, rd, rr));
    }
    throw {
          ExceptionID: -3,
          _1: "Map.bal",
          Debug: "Invalid_argument"
        };
  }
  throw {
        ExceptionID: -3,
        _1: "Map.bal",
        Debug: "Invalid_argument"
      };
}

function is_empty(param) {
  if (param) {
    return false;
  } else {
    return true;
  }
}

function add(x, data, m) {
  if (!m) {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* d */data,
            /* r : Empty */0,
            /* h */1
          ];
  }
  var r = m[/* r */3];
  var d = m[/* d */2];
  var v = m[/* v */1];
  var l = m[/* l */0];
  var c = Caml_primitive.caml_string_compare(x, v);
  if (c === 0) {
    if (d === data) {
      return m;
    } else {
      return /* Node */[
              /* l */l,
              /* v */x,
              /* d */data,
              /* r */r,
              /* h */m[/* h */4]
            ];
    }
  }
  if (c < 0) {
    var ll = add(x, data, l);
    if (l === ll) {
      return m;
    } else {
      return bal(ll, v, d, r);
    }
  }
  var rr = add(x, data, r);
  if (r === rr) {
    return m;
  } else {
    return bal(l, v, d, rr);
  }
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_primitive.caml_string_compare(x, param[/* v */1]);
      if (c === 0) {
        return param[/* d */2];
      }
      _param = c < 0 ? param[/* l */0] : param[/* r */3];
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function find_first(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[/* v */1];
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _d0 = param[/* d */2];
        var _param$1 = param[/* l */0];
        while(true) {
          var param$1 = _param$1;
          var d0 = _d0;
          var v0 = _v0;
          if (!param$1) {
            return /* tuple */[
                    v0,
                    d0
                  ];
          }
          var v$1 = param$1[/* v */1];
          if (Curry._1(f, v$1)) {
            _param$1 = param$1[/* l */0];
            _d0 = param$1[/* d */2];
            _v0 = v$1;
            continue ;
          }
          _param$1 = param$1[/* r */3];
          continue ;
        };
      }
      _param = param[/* r */3];
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function find_first_opt(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var v = param[/* v */1];
    if (Curry._1(f, v)) {
      var _v0 = v;
      var _d0 = param[/* d */2];
      var _param$1 = param[/* l */0];
      while(true) {
        var param$1 = _param$1;
        var d0 = _d0;
        var v0 = _v0;
        if (!param$1) {
          return /* tuple */[
                  v0,
                  d0
                ];
        }
        var v$1 = param$1[/* v */1];
        if (Curry._1(f, v$1)) {
          _param$1 = param$1[/* l */0];
          _d0 = param$1[/* d */2];
          _v0 = v$1;
          continue ;
        }
        _param$1 = param$1[/* r */3];
        continue ;
      };
    }
    _param = param[/* r */3];
    continue ;
  };
}

function find_last(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[/* v */1];
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _d0 = param[/* d */2];
        var _param$1 = param[/* r */3];
        while(true) {
          var param$1 = _param$1;
          var d0 = _d0;
          var v0 = _v0;
          if (!param$1) {
            return /* tuple */[
                    v0,
                    d0
                  ];
          }
          var v$1 = param$1[/* v */1];
          if (Curry._1(f, v$1)) {
            _param$1 = param$1[/* r */3];
            _d0 = param$1[/* d */2];
            _v0 = v$1;
            continue ;
          }
          _param$1 = param$1[/* l */0];
          continue ;
        };
      }
      _param = param[/* l */0];
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function find_last_opt(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var v = param[/* v */1];
    if (Curry._1(f, v)) {
      var _v0 = v;
      var _d0 = param[/* d */2];
      var _param$1 = param[/* r */3];
      while(true) {
        var param$1 = _param$1;
        var d0 = _d0;
        var v0 = _v0;
        if (!param$1) {
          return /* tuple */[
                  v0,
                  d0
                ];
        }
        var v$1 = param$1[/* v */1];
        if (Curry._1(f, v$1)) {
          _param$1 = param$1[/* r */3];
          _d0 = param$1[/* d */2];
          _v0 = v$1;
          continue ;
        }
        _param$1 = param$1[/* l */0];
        continue ;
      };
    }
    _param = param[/* l */0];
    continue ;
  };
}

function find_opt(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var c = Caml_primitive.caml_string_compare(x, param[/* v */1]);
    if (c === 0) {
      return Caml_option.some(param[/* d */2]);
    }
    _param = c < 0 ? param[/* l */0] : param[/* r */3];
    continue ;
  };
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    var c = Caml_primitive.caml_string_compare(x, param[/* v */1]);
    if (c === 0) {
      return true;
    }
    _param = c < 0 ? param[/* l */0] : param[/* r */3];
    continue ;
  };
}

function min_binding(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param[/* l */0];
      if (!l) {
        return /* tuple */[
                param[/* v */1],
                param[/* d */2]
              ];
      }
      _param = l;
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function min_binding_opt(_param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var l = param[/* l */0];
    if (!l) {
      return /* tuple */[
              param[/* v */1],
              param[/* d */2]
            ];
    }
    _param = l;
    continue ;
  };
}

function max_binding(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var r = param[/* r */3];
      if (!r) {
        return /* tuple */[
                param[/* v */1],
                param[/* d */2]
              ];
      }
      _param = r;
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function max_binding_opt(_param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var r = param[/* r */3];
    if (!r) {
      return /* tuple */[
              param[/* v */1],
              param[/* d */2]
            ];
    }
    _param = r;
    continue ;
  };
}

function remove_min_binding(param) {
  if (param) {
    var l = param[/* l */0];
    if (l) {
      return bal(remove_min_binding(l), param[/* v */1], param[/* d */2], param[/* r */3]);
    } else {
      return param[/* r */3];
    }
  }
  throw {
        ExceptionID: -3,
        _1: "Map.remove_min_elt",
        Debug: "Invalid_argument"
      };
}

function merge(t1, t2) {
  if (!t1) {
    return t2;
  }
  if (!t2) {
    return t1;
  }
  var match = min_binding(t2);
  return bal(t1, match[0], match[1], remove_min_binding(t2));
}

function remove(x, m) {
  if (!m) {
    return /* Empty */0;
  }
  var r = m[/* r */3];
  var d = m[/* d */2];
  var v = m[/* v */1];
  var l = m[/* l */0];
  var c = Caml_primitive.caml_string_compare(x, v);
  if (c === 0) {
    return merge(l, r);
  }
  if (c < 0) {
    var ll = remove(x, l);
    if (l === ll) {
      return m;
    } else {
      return bal(ll, v, d, r);
    }
  }
  var rr = remove(x, r);
  if (r === rr) {
    return m;
  } else {
    return bal(l, v, d, rr);
  }
}

function update(x, f, m) {
  if (m) {
    var r = m[/* r */3];
    var d = m[/* d */2];
    var v = m[/* v */1];
    var l = m[/* l */0];
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      var data = Curry._1(f, Caml_option.some(d));
      if (data === undefined) {
        return merge(l, r);
      }
      var data$1 = Caml_option.valFromOption(data);
      if (d === data$1) {
        return m;
      } else {
        return /* Node */[
                /* l */l,
                /* v */x,
                /* d */data$1,
                /* r */r,
                /* h */m[/* h */4]
              ];
      }
    }
    if (c < 0) {
      var ll = update(x, f, l);
      if (l === ll) {
        return m;
      } else {
        return bal(ll, v, d, r);
      }
    }
    var rr = update(x, f, r);
    if (r === rr) {
      return m;
    } else {
      return bal(l, v, d, rr);
    }
  }
  var data$2 = Curry._1(f, undefined);
  if (data$2 !== undefined) {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* d */Caml_option.valFromOption(data$2),
            /* r : Empty */0,
            /* h */1
          ];
  } else {
    return /* Empty */0;
  }
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    iter(f, param[/* l */0]);
    Curry._2(f, param[/* v */1], param[/* d */2]);
    _param = param[/* r */3];
    continue ;
  };
}

function map(f, param) {
  if (!param) {
    return /* Empty */0;
  }
  var l$prime = map(f, param[/* l */0]);
  var d$prime = Curry._1(f, param[/* d */2]);
  var r$prime = map(f, param[/* r */3]);
  return /* Node */[
          /* l */l$prime,
          /* v */param[/* v */1],
          /* d */d$prime,
          /* r */r$prime,
          /* h */param[/* h */4]
        ];
}

function mapi(f, param) {
  if (!param) {
    return /* Empty */0;
  }
  var v = param[/* v */1];
  var l$prime = mapi(f, param[/* l */0]);
  var d$prime = Curry._2(f, v, param[/* d */2]);
  var r$prime = mapi(f, param[/* r */3]);
  return /* Node */[
          /* l */l$prime,
          /* v */v,
          /* d */d$prime,
          /* r */r$prime,
          /* h */param[/* h */4]
        ];
}

function fold(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (!m) {
      return accu;
    }
    _accu = Curry._3(f, m[/* v */1], m[/* d */2], fold(f, m[/* l */0], accu));
    _m = m[/* r */3];
    continue ;
  };
}

function for_all(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return true;
    }
    if (!Curry._2(p, param[/* v */1], param[/* d */2])) {
      return false;
    }
    if (!for_all(p, param[/* l */0])) {
      return false;
    }
    _param = param[/* r */3];
    continue ;
  };
}

function exists(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    if (Curry._2(p, param[/* v */1], param[/* d */2])) {
      return true;
    }
    if (exists(p, param[/* l */0])) {
      return true;
    }
    _param = param[/* r */3];
    continue ;
  };
}

function add_min_binding(k, x, param) {
  if (param) {
    return bal(add_min_binding(k, x, param[/* l */0]), param[/* v */1], param[/* d */2], param[/* r */3]);
  } else {
    return singleton(k, x);
  }
}

function add_max_binding(k, x, param) {
  if (param) {
    return bal(param[/* l */0], param[/* v */1], param[/* d */2], add_max_binding(k, x, param[/* r */3]));
  } else {
    return singleton(k, x);
  }
}

function join(l, v, d, r) {
  if (!l) {
    return add_min_binding(v, d, r);
  }
  if (!r) {
    return add_max_binding(v, d, l);
  }
  var rh = r[/* h */4];
  var lh = l[/* h */4];
  if (lh > (rh + 2 | 0)) {
    return bal(l[/* l */0], l[/* v */1], l[/* d */2], join(l[/* r */3], v, d, r));
  } else if (rh > (lh + 2 | 0)) {
    return bal(join(l, v, d, r[/* l */0]), r[/* v */1], r[/* d */2], r[/* r */3]);
  } else {
    return create(l, v, d, r);
  }
}

function concat(t1, t2) {
  if (!t1) {
    return t2;
  }
  if (!t2) {
    return t1;
  }
  var match = min_binding(t2);
  return join(t1, match[0], match[1], remove_min_binding(t2));
}

function concat_or_join(t1, v, d, t2) {
  if (d !== undefined) {
    return join(t1, v, Caml_option.valFromOption(d), t2);
  } else {
    return concat(t1, t2);
  }
}

function split(x, param) {
  if (!param) {
    return /* tuple */[
            /* Empty */0,
            undefined,
            /* Empty */0
          ];
  }
  var r = param[/* r */3];
  var d = param[/* d */2];
  var v = param[/* v */1];
  var l = param[/* l */0];
  var c = Caml_primitive.caml_string_compare(x, v);
  if (c === 0) {
    return /* tuple */[
            l,
            Caml_option.some(d),
            r
          ];
  }
  if (c < 0) {
    var match = split(x, l);
    return /* tuple */[
            match[0],
            match[1],
            join(match[2], v, d, r)
          ];
  }
  var match$1 = split(x, r);
  return /* tuple */[
          join(l, v, d, match$1[0]),
          match$1[1],
          match$1[2]
        ];
}

function merge$1(f, s1, s2) {
  if (s1) {
    var v1 = s1[/* v */1];
    if (s1[/* h */4] >= height(s2)) {
      var match = split(v1, s2);
      return concat_or_join(merge$1(f, s1[/* l */0], match[0]), v1, Curry._3(f, v1, Caml_option.some(s1[/* d */2]), match[1]), merge$1(f, s1[/* r */3], match[2]));
    }
    
  } else if (!s2) {
    return /* Empty */0;
  }
  if (s2) {
    var v2 = s2[/* v */1];
    var match$1 = split(v2, s1);
    return concat_or_join(merge$1(f, match$1[0], s2[/* l */0]), v2, Curry._3(f, v2, match$1[1], Caml_option.some(s2[/* d */2])), merge$1(f, match$1[2], s2[/* r */3]));
  }
  throw {
        ExceptionID: -9,
        _1: /* tuple */[
          "map.ml",
          393,
          10
        ],
        Debug: "Assert_failure"
      };
}

function union(f, s1, s2) {
  if (!s1) {
    return s2;
  }
  if (!s2) {
    return s1;
  }
  var d2 = s2[/* d */2];
  var v2 = s2[/* v */1];
  var d1 = s1[/* d */2];
  var v1 = s1[/* v */1];
  if (s1[/* h */4] >= s2[/* h */4]) {
    var match = split(v1, s2);
    var d2$1 = match[1];
    var l = union(f, s1[/* l */0], match[0]);
    var r = union(f, s1[/* r */3], match[2]);
    if (d2$1 !== undefined) {
      return concat_or_join(l, v1, Curry._3(f, v1, d1, Caml_option.valFromOption(d2$1)), r);
    } else {
      return join(l, v1, d1, r);
    }
  }
  var match$1 = split(v2, s1);
  var d1$1 = match$1[1];
  var l$1 = union(f, match$1[0], s2[/* l */0]);
  var r$1 = union(f, match$1[2], s2[/* r */3]);
  if (d1$1 !== undefined) {
    return concat_or_join(l$1, v2, Curry._3(f, v2, Caml_option.valFromOption(d1$1), d2), r$1);
  } else {
    return join(l$1, v2, d2, r$1);
  }
}

function filter(p, m) {
  if (!m) {
    return /* Empty */0;
  }
  var r = m[/* r */3];
  var d = m[/* d */2];
  var v = m[/* v */1];
  var l = m[/* l */0];
  var l$prime = filter(p, l);
  var pvd = Curry._2(p, v, d);
  var r$prime = filter(p, r);
  if (pvd) {
    if (l === l$prime && r === r$prime) {
      return m;
    } else {
      return join(l$prime, v, d, r$prime);
    }
  } else {
    return concat(l$prime, r$prime);
  }
}

function partition(p, param) {
  if (!param) {
    return /* tuple */[
            /* Empty */0,
            /* Empty */0
          ];
  }
  var d = param[/* d */2];
  var v = param[/* v */1];
  var match = partition(p, param[/* l */0]);
  var lf = match[1];
  var lt = match[0];
  var pvd = Curry._2(p, v, d);
  var match$1 = partition(p, param[/* r */3]);
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
}

function cons_enum(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (!m) {
      return e;
    }
    _e = /* More */[
      m[/* v */1],
      m[/* d */2],
      m[/* r */3],
      e
    ];
    _m = m[/* l */0];
    continue ;
  };
}

function compare(cmp, m1, m2) {
  var _e1 = cons_enum(m1, /* End */0);
  var _e2 = cons_enum(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (!e1) {
      if (e2) {
        return -1;
      } else {
        return 0;
      }
    }
    if (!e2) {
      return 1;
    }
    var c = Caml_primitive.caml_string_compare(e1[0], e2[0]);
    if (c !== 0) {
      return c;
    }
    var c$1 = Curry._2(cmp, e1[1], e2[1]);
    if (c$1 !== 0) {
      return c$1;
    }
    _e2 = cons_enum(e2[2], e2[3]);
    _e1 = cons_enum(e1[2], e1[3]);
    continue ;
  };
}

function equal(cmp, m1, m2) {
  var _e1 = cons_enum(m1, /* End */0);
  var _e2 = cons_enum(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (!e1) {
      if (e2) {
        return false;
      } else {
        return true;
      }
    }
    if (!e2) {
      return false;
    }
    if (Caml_primitive.caml_string_compare(e1[0], e2[0]) !== 0) {
      return false;
    }
    if (!Curry._2(cmp, e1[1], e2[1])) {
      return false;
    }
    _e2 = cons_enum(e2[2], e2[3]);
    _e1 = cons_enum(e1[2], e1[3]);
    continue ;
  };
}

function cardinal(param) {
  if (param) {
    return (cardinal(param[/* l */0]) + 1 | 0) + cardinal(param[/* r */3]) | 0;
  } else {
    return 0;
  }
}

function bindings_aux(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (!param) {
      return accu;
    }
    _param = param[/* l */0];
    _accu = /* :: */[
      /* tuple */[
        param[/* v */1],
        param[/* d */2]
      ],
      bindings_aux(accu, param[/* r */3])
    ];
    continue ;
  };
}

function bindings(s) {
  return bindings_aux(/* [] */0, s);
}

var Vars = {
  empty: /* Empty */0,
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

function height$1(param) {
  if (param) {
    return param[/* h */4];
  } else {
    return 0;
  }
}

function create$1(l, x, d, r) {
  var hl = height$1(l);
  var hr = height$1(r);
  return /* Node */[
          /* l */l,
          /* v */x,
          /* d */d,
          /* r */r,
          /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function singleton$1(x, d) {
  return /* Node */[
          /* l : Empty */0,
          /* v */x,
          /* d */d,
          /* r : Empty */0,
          /* h */1
        ];
}

function bal$1(l, x, d, r) {
  var hl = l ? l[/* h */4] : 0;
  var hr = r ? r[/* h */4] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[/* r */3];
      var ld = l[/* d */2];
      var lv = l[/* v */1];
      var ll = l[/* l */0];
      if (height$1(ll) >= height$1(lr)) {
        return create$1(ll, lv, ld, create$1(lr, x, d, r));
      }
      if (lr) {
        return create$1(create$1(ll, lv, ld, lr[/* l */0]), lr[/* v */1], lr[/* d */2], create$1(lr[/* r */3], x, d, r));
      }
      throw {
            ExceptionID: -3,
            _1: "Map.bal",
            Debug: "Invalid_argument"
          };
    }
    throw {
          ExceptionID: -3,
          _1: "Map.bal",
          Debug: "Invalid_argument"
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */[
            /* l */l,
            /* v */x,
            /* d */d,
            /* r */r,
            /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
  if (r) {
    var rr = r[/* r */3];
    var rd = r[/* d */2];
    var rv = r[/* v */1];
    var rl = r[/* l */0];
    if (height$1(rr) >= height$1(rl)) {
      return create$1(create$1(l, x, d, rl), rv, rd, rr);
    }
    if (rl) {
      return create$1(create$1(l, x, d, rl[/* l */0]), rl[/* v */1], rl[/* d */2], create$1(rl[/* r */3], rv, rd, rr));
    }
    throw {
          ExceptionID: -3,
          _1: "Map.bal",
          Debug: "Invalid_argument"
        };
  }
  throw {
        ExceptionID: -3,
        _1: "Map.bal",
        Debug: "Invalid_argument"
      };
}

function is_empty$1(param) {
  if (param) {
    return false;
  } else {
    return true;
  }
}

function add$1(x, data, m) {
  if (!m) {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* d */data,
            /* r : Empty */0,
            /* h */1
          ];
  }
  var r = m[/* r */3];
  var d = m[/* d */2];
  var v = m[/* v */1];
  var l = m[/* l */0];
  var c = Caml_primitive.caml_string_compare(x, v);
  if (c === 0) {
    if (d === data) {
      return m;
    } else {
      return /* Node */[
              /* l */l,
              /* v */x,
              /* d */data,
              /* r */r,
              /* h */m[/* h */4]
            ];
    }
  }
  if (c < 0) {
    var ll = add$1(x, data, l);
    if (l === ll) {
      return m;
    } else {
      return bal$1(ll, v, d, r);
    }
  }
  var rr = add$1(x, data, r);
  if (r === rr) {
    return m;
  } else {
    return bal$1(l, v, d, rr);
  }
}

function find$1(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_primitive.caml_string_compare(x, param[/* v */1]);
      if (c === 0) {
        return param[/* d */2];
      }
      _param = c < 0 ? param[/* l */0] : param[/* r */3];
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function find_first$1(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[/* v */1];
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _d0 = param[/* d */2];
        var _param$1 = param[/* l */0];
        while(true) {
          var param$1 = _param$1;
          var d0 = _d0;
          var v0 = _v0;
          if (!param$1) {
            return /* tuple */[
                    v0,
                    d0
                  ];
          }
          var v$1 = param$1[/* v */1];
          if (Curry._1(f, v$1)) {
            _param$1 = param$1[/* l */0];
            _d0 = param$1[/* d */2];
            _v0 = v$1;
            continue ;
          }
          _param$1 = param$1[/* r */3];
          continue ;
        };
      }
      _param = param[/* r */3];
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function find_first_opt$1(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var v = param[/* v */1];
    if (Curry._1(f, v)) {
      var _v0 = v;
      var _d0 = param[/* d */2];
      var _param$1 = param[/* l */0];
      while(true) {
        var param$1 = _param$1;
        var d0 = _d0;
        var v0 = _v0;
        if (!param$1) {
          return /* tuple */[
                  v0,
                  d0
                ];
        }
        var v$1 = param$1[/* v */1];
        if (Curry._1(f, v$1)) {
          _param$1 = param$1[/* l */0];
          _d0 = param$1[/* d */2];
          _v0 = v$1;
          continue ;
        }
        _param$1 = param$1[/* r */3];
        continue ;
      };
    }
    _param = param[/* r */3];
    continue ;
  };
}

function find_last$1(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[/* v */1];
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _d0 = param[/* d */2];
        var _param$1 = param[/* r */3];
        while(true) {
          var param$1 = _param$1;
          var d0 = _d0;
          var v0 = _v0;
          if (!param$1) {
            return /* tuple */[
                    v0,
                    d0
                  ];
          }
          var v$1 = param$1[/* v */1];
          if (Curry._1(f, v$1)) {
            _param$1 = param$1[/* r */3];
            _d0 = param$1[/* d */2];
            _v0 = v$1;
            continue ;
          }
          _param$1 = param$1[/* l */0];
          continue ;
        };
      }
      _param = param[/* l */0];
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function find_last_opt$1(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var v = param[/* v */1];
    if (Curry._1(f, v)) {
      var _v0 = v;
      var _d0 = param[/* d */2];
      var _param$1 = param[/* r */3];
      while(true) {
        var param$1 = _param$1;
        var d0 = _d0;
        var v0 = _v0;
        if (!param$1) {
          return /* tuple */[
                  v0,
                  d0
                ];
        }
        var v$1 = param$1[/* v */1];
        if (Curry._1(f, v$1)) {
          _param$1 = param$1[/* r */3];
          _d0 = param$1[/* d */2];
          _v0 = v$1;
          continue ;
        }
        _param$1 = param$1[/* l */0];
        continue ;
      };
    }
    _param = param[/* l */0];
    continue ;
  };
}

function find_opt$1(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var c = Caml_primitive.caml_string_compare(x, param[/* v */1]);
    if (c === 0) {
      return Caml_option.some(param[/* d */2]);
    }
    _param = c < 0 ? param[/* l */0] : param[/* r */3];
    continue ;
  };
}

function mem$1(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    var c = Caml_primitive.caml_string_compare(x, param[/* v */1]);
    if (c === 0) {
      return true;
    }
    _param = c < 0 ? param[/* l */0] : param[/* r */3];
    continue ;
  };
}

function min_binding$1(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param[/* l */0];
      if (!l) {
        return /* tuple */[
                param[/* v */1],
                param[/* d */2]
              ];
      }
      _param = l;
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function min_binding_opt$1(_param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var l = param[/* l */0];
    if (!l) {
      return /* tuple */[
              param[/* v */1],
              param[/* d */2]
            ];
    }
    _param = l;
    continue ;
  };
}

function max_binding$1(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var r = param[/* r */3];
      if (!r) {
        return /* tuple */[
                param[/* v */1],
                param[/* d */2]
              ];
      }
      _param = r;
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function max_binding_opt$1(_param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var r = param[/* r */3];
    if (!r) {
      return /* tuple */[
              param[/* v */1],
              param[/* d */2]
            ];
    }
    _param = r;
    continue ;
  };
}

function remove_min_binding$1(param) {
  if (param) {
    var l = param[/* l */0];
    if (l) {
      return bal$1(remove_min_binding$1(l), param[/* v */1], param[/* d */2], param[/* r */3]);
    } else {
      return param[/* r */3];
    }
  }
  throw {
        ExceptionID: -3,
        _1: "Map.remove_min_elt",
        Debug: "Invalid_argument"
      };
}

function merge$2(t1, t2) {
  if (!t1) {
    return t2;
  }
  if (!t2) {
    return t1;
  }
  var match = min_binding$1(t2);
  return bal$1(t1, match[0], match[1], remove_min_binding$1(t2));
}

function remove$1(x, m) {
  if (!m) {
    return /* Empty */0;
  }
  var r = m[/* r */3];
  var d = m[/* d */2];
  var v = m[/* v */1];
  var l = m[/* l */0];
  var c = Caml_primitive.caml_string_compare(x, v);
  if (c === 0) {
    return merge$2(l, r);
  }
  if (c < 0) {
    var ll = remove$1(x, l);
    if (l === ll) {
      return m;
    } else {
      return bal$1(ll, v, d, r);
    }
  }
  var rr = remove$1(x, r);
  if (r === rr) {
    return m;
  } else {
    return bal$1(l, v, d, rr);
  }
}

function update$1(x, f, m) {
  if (m) {
    var r = m[/* r */3];
    var d = m[/* d */2];
    var v = m[/* v */1];
    var l = m[/* l */0];
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      var data = Curry._1(f, Caml_option.some(d));
      if (data === undefined) {
        return merge$2(l, r);
      }
      var data$1 = Caml_option.valFromOption(data);
      if (d === data$1) {
        return m;
      } else {
        return /* Node */[
                /* l */l,
                /* v */x,
                /* d */data$1,
                /* r */r,
                /* h */m[/* h */4]
              ];
      }
    }
    if (c < 0) {
      var ll = update$1(x, f, l);
      if (l === ll) {
        return m;
      } else {
        return bal$1(ll, v, d, r);
      }
    }
    var rr = update$1(x, f, r);
    if (r === rr) {
      return m;
    } else {
      return bal$1(l, v, d, rr);
    }
  }
  var data$2 = Curry._1(f, undefined);
  if (data$2 !== undefined) {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* d */Caml_option.valFromOption(data$2),
            /* r : Empty */0,
            /* h */1
          ];
  } else {
    return /* Empty */0;
  }
}

function iter$1(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    iter$1(f, param[/* l */0]);
    Curry._2(f, param[/* v */1], param[/* d */2]);
    _param = param[/* r */3];
    continue ;
  };
}

function map$1(f, param) {
  if (!param) {
    return /* Empty */0;
  }
  var l$prime = map$1(f, param[/* l */0]);
  var d$prime = Curry._1(f, param[/* d */2]);
  var r$prime = map$1(f, param[/* r */3]);
  return /* Node */[
          /* l */l$prime,
          /* v */param[/* v */1],
          /* d */d$prime,
          /* r */r$prime,
          /* h */param[/* h */4]
        ];
}

function mapi$1(f, param) {
  if (!param) {
    return /* Empty */0;
  }
  var v = param[/* v */1];
  var l$prime = mapi$1(f, param[/* l */0]);
  var d$prime = Curry._2(f, v, param[/* d */2]);
  var r$prime = mapi$1(f, param[/* r */3]);
  return /* Node */[
          /* l */l$prime,
          /* v */v,
          /* d */d$prime,
          /* r */r$prime,
          /* h */param[/* h */4]
        ];
}

function fold$1(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (!m) {
      return accu;
    }
    _accu = Curry._3(f, m[/* v */1], m[/* d */2], fold$1(f, m[/* l */0], accu));
    _m = m[/* r */3];
    continue ;
  };
}

function for_all$1(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return true;
    }
    if (!Curry._2(p, param[/* v */1], param[/* d */2])) {
      return false;
    }
    if (!for_all$1(p, param[/* l */0])) {
      return false;
    }
    _param = param[/* r */3];
    continue ;
  };
}

function exists$1(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    if (Curry._2(p, param[/* v */1], param[/* d */2])) {
      return true;
    }
    if (exists$1(p, param[/* l */0])) {
      return true;
    }
    _param = param[/* r */3];
    continue ;
  };
}

function add_min_binding$1(k, x, param) {
  if (param) {
    return bal$1(add_min_binding$1(k, x, param[/* l */0]), param[/* v */1], param[/* d */2], param[/* r */3]);
  } else {
    return singleton$1(k, x);
  }
}

function add_max_binding$1(k, x, param) {
  if (param) {
    return bal$1(param[/* l */0], param[/* v */1], param[/* d */2], add_max_binding$1(k, x, param[/* r */3]));
  } else {
    return singleton$1(k, x);
  }
}

function join$1(l, v, d, r) {
  if (!l) {
    return add_min_binding$1(v, d, r);
  }
  if (!r) {
    return add_max_binding$1(v, d, l);
  }
  var rh = r[/* h */4];
  var lh = l[/* h */4];
  if (lh > (rh + 2 | 0)) {
    return bal$1(l[/* l */0], l[/* v */1], l[/* d */2], join$1(l[/* r */3], v, d, r));
  } else if (rh > (lh + 2 | 0)) {
    return bal$1(join$1(l, v, d, r[/* l */0]), r[/* v */1], r[/* d */2], r[/* r */3]);
  } else {
    return create$1(l, v, d, r);
  }
}

function concat$1(t1, t2) {
  if (!t1) {
    return t2;
  }
  if (!t2) {
    return t1;
  }
  var match = min_binding$1(t2);
  return join$1(t1, match[0], match[1], remove_min_binding$1(t2));
}

function concat_or_join$1(t1, v, d, t2) {
  if (d !== undefined) {
    return join$1(t1, v, Caml_option.valFromOption(d), t2);
  } else {
    return concat$1(t1, t2);
  }
}

function split$1(x, param) {
  if (!param) {
    return /* tuple */[
            /* Empty */0,
            undefined,
            /* Empty */0
          ];
  }
  var r = param[/* r */3];
  var d = param[/* d */2];
  var v = param[/* v */1];
  var l = param[/* l */0];
  var c = Caml_primitive.caml_string_compare(x, v);
  if (c === 0) {
    return /* tuple */[
            l,
            Caml_option.some(d),
            r
          ];
  }
  if (c < 0) {
    var match = split$1(x, l);
    return /* tuple */[
            match[0],
            match[1],
            join$1(match[2], v, d, r)
          ];
  }
  var match$1 = split$1(x, r);
  return /* tuple */[
          join$1(l, v, d, match$1[0]),
          match$1[1],
          match$1[2]
        ];
}

function merge$3(f, s1, s2) {
  if (s1) {
    var v1 = s1[/* v */1];
    if (s1[/* h */4] >= height$1(s2)) {
      var match = split$1(v1, s2);
      return concat_or_join$1(merge$3(f, s1[/* l */0], match[0]), v1, Curry._3(f, v1, Caml_option.some(s1[/* d */2]), match[1]), merge$3(f, s1[/* r */3], match[2]));
    }
    
  } else if (!s2) {
    return /* Empty */0;
  }
  if (s2) {
    var v2 = s2[/* v */1];
    var match$1 = split$1(v2, s1);
    return concat_or_join$1(merge$3(f, match$1[0], s2[/* l */0]), v2, Curry._3(f, v2, match$1[1], Caml_option.some(s2[/* d */2])), merge$3(f, match$1[2], s2[/* r */3]));
  }
  throw {
        ExceptionID: -9,
        _1: /* tuple */[
          "map.ml",
          393,
          10
        ],
        Debug: "Assert_failure"
      };
}

function union$1(f, s1, s2) {
  if (!s1) {
    return s2;
  }
  if (!s2) {
    return s1;
  }
  var d2 = s2[/* d */2];
  var v2 = s2[/* v */1];
  var d1 = s1[/* d */2];
  var v1 = s1[/* v */1];
  if (s1[/* h */4] >= s2[/* h */4]) {
    var match = split$1(v1, s2);
    var d2$1 = match[1];
    var l = union$1(f, s1[/* l */0], match[0]);
    var r = union$1(f, s1[/* r */3], match[2]);
    if (d2$1 !== undefined) {
      return concat_or_join$1(l, v1, Curry._3(f, v1, d1, Caml_option.valFromOption(d2$1)), r);
    } else {
      return join$1(l, v1, d1, r);
    }
  }
  var match$1 = split$1(v2, s1);
  var d1$1 = match$1[1];
  var l$1 = union$1(f, match$1[0], s2[/* l */0]);
  var r$1 = union$1(f, match$1[2], s2[/* r */3]);
  if (d1$1 !== undefined) {
    return concat_or_join$1(l$1, v2, Curry._3(f, v2, Caml_option.valFromOption(d1$1), d2), r$1);
  } else {
    return join$1(l$1, v2, d2, r$1);
  }
}

function filter$1(p, m) {
  if (!m) {
    return /* Empty */0;
  }
  var r = m[/* r */3];
  var d = m[/* d */2];
  var v = m[/* v */1];
  var l = m[/* l */0];
  var l$prime = filter$1(p, l);
  var pvd = Curry._2(p, v, d);
  var r$prime = filter$1(p, r);
  if (pvd) {
    if (l === l$prime && r === r$prime) {
      return m;
    } else {
      return join$1(l$prime, v, d, r$prime);
    }
  } else {
    return concat$1(l$prime, r$prime);
  }
}

function partition$1(p, param) {
  if (!param) {
    return /* tuple */[
            /* Empty */0,
            /* Empty */0
          ];
  }
  var d = param[/* d */2];
  var v = param[/* v */1];
  var match = partition$1(p, param[/* l */0]);
  var lf = match[1];
  var lt = match[0];
  var pvd = Curry._2(p, v, d);
  var match$1 = partition$1(p, param[/* r */3]);
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
}

function cons_enum$1(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (!m) {
      return e;
    }
    _e = /* More */[
      m[/* v */1],
      m[/* d */2],
      m[/* r */3],
      e
    ];
    _m = m[/* l */0];
    continue ;
  };
}

function compare$1(cmp, m1, m2) {
  var _e1 = cons_enum$1(m1, /* End */0);
  var _e2 = cons_enum$1(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (!e1) {
      if (e2) {
        return -1;
      } else {
        return 0;
      }
    }
    if (!e2) {
      return 1;
    }
    var c = Caml_primitive.caml_string_compare(e1[0], e2[0]);
    if (c !== 0) {
      return c;
    }
    var c$1 = Curry._2(cmp, e1[1], e2[1]);
    if (c$1 !== 0) {
      return c$1;
    }
    _e2 = cons_enum$1(e2[2], e2[3]);
    _e1 = cons_enum$1(e1[2], e1[3]);
    continue ;
  };
}

function equal$1(cmp, m1, m2) {
  var _e1 = cons_enum$1(m1, /* End */0);
  var _e2 = cons_enum$1(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (!e1) {
      if (e2) {
        return false;
      } else {
        return true;
      }
    }
    if (!e2) {
      return false;
    }
    if (Caml_primitive.caml_string_compare(e1[0], e2[0]) !== 0) {
      return false;
    }
    if (!Curry._2(cmp, e1[1], e2[1])) {
      return false;
    }
    _e2 = cons_enum$1(e2[2], e2[3]);
    _e1 = cons_enum$1(e1[2], e1[3]);
    continue ;
  };
}

function cardinal$1(param) {
  if (param) {
    return (cardinal$1(param[/* l */0]) + 1 | 0) + cardinal$1(param[/* r */3]) | 0;
  } else {
    return 0;
  }
}

function bindings_aux$1(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (!param) {
      return accu;
    }
    _param = param[/* l */0];
    _accu = /* :: */[
      /* tuple */[
        param[/* v */1],
        param[/* d */2]
      ],
      bindings_aux$1(accu, param[/* r */3])
    ];
    continue ;
  };
}

function bindings$1(s) {
  return bindings_aux$1(/* [] */0, s);
}

var Meths = {
  empty: /* Empty */0,
  is_empty: is_empty$1,
  mem: mem$1,
  add: add$1,
  update: update$1,
  singleton: singleton$1,
  remove: remove$1,
  merge: merge$3,
  union: union$1,
  compare: compare$1,
  equal: equal$1,
  iter: iter$1,
  fold: fold$1,
  for_all: for_all$1,
  exists: exists$1,
  filter: filter$1,
  partition: partition$1,
  cardinal: cardinal$1,
  bindings: bindings$1,
  min_binding: min_binding$1,
  min_binding_opt: min_binding_opt$1,
  max_binding: max_binding$1,
  max_binding_opt: max_binding_opt$1,
  choose: min_binding$1,
  choose_opt: min_binding_opt$1,
  split: split$1,
  find: find$1,
  find_opt: find_opt$1,
  find_first: find_first$1,
  find_first_opt: find_first_opt$1,
  find_last: find_last$1,
  find_last_opt: find_last_opt$1,
  map: map$1,
  mapi: mapi$1
};

function height$2(param) {
  if (param) {
    return param[/* h */4];
  } else {
    return 0;
  }
}

function create$2(l, x, d, r) {
  var hl = height$2(l);
  var hr = height$2(r);
  return /* Node */[
          /* l */l,
          /* v */x,
          /* d */d,
          /* r */r,
          /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function singleton$2(x, d) {
  return /* Node */[
          /* l : Empty */0,
          /* v */x,
          /* d */d,
          /* r : Empty */0,
          /* h */1
        ];
}

function bal$2(l, x, d, r) {
  var hl = l ? l[/* h */4] : 0;
  var hr = r ? r[/* h */4] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[/* r */3];
      var ld = l[/* d */2];
      var lv = l[/* v */1];
      var ll = l[/* l */0];
      if (height$2(ll) >= height$2(lr)) {
        return create$2(ll, lv, ld, create$2(lr, x, d, r));
      }
      if (lr) {
        return create$2(create$2(ll, lv, ld, lr[/* l */0]), lr[/* v */1], lr[/* d */2], create$2(lr[/* r */3], x, d, r));
      }
      throw {
            ExceptionID: -3,
            _1: "Map.bal",
            Debug: "Invalid_argument"
          };
    }
    throw {
          ExceptionID: -3,
          _1: "Map.bal",
          Debug: "Invalid_argument"
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */[
            /* l */l,
            /* v */x,
            /* d */d,
            /* r */r,
            /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
  if (r) {
    var rr = r[/* r */3];
    var rd = r[/* d */2];
    var rv = r[/* v */1];
    var rl = r[/* l */0];
    if (height$2(rr) >= height$2(rl)) {
      return create$2(create$2(l, x, d, rl), rv, rd, rr);
    }
    if (rl) {
      return create$2(create$2(l, x, d, rl[/* l */0]), rl[/* v */1], rl[/* d */2], create$2(rl[/* r */3], rv, rd, rr));
    }
    throw {
          ExceptionID: -3,
          _1: "Map.bal",
          Debug: "Invalid_argument"
        };
  }
  throw {
        ExceptionID: -3,
        _1: "Map.bal",
        Debug: "Invalid_argument"
      };
}

function is_empty$2(param) {
  if (param) {
    return false;
  } else {
    return true;
  }
}

function add$2(x, data, m) {
  if (!m) {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* d */data,
            /* r : Empty */0,
            /* h */1
          ];
  }
  var r = m[/* r */3];
  var d = m[/* d */2];
  var v = m[/* v */1];
  var l = m[/* l */0];
  var c = Caml_primitive.caml_int_compare(x, v);
  if (c === 0) {
    if (d === data) {
      return m;
    } else {
      return /* Node */[
              /* l */l,
              /* v */x,
              /* d */data,
              /* r */r,
              /* h */m[/* h */4]
            ];
    }
  }
  if (c < 0) {
    var ll = add$2(x, data, l);
    if (l === ll) {
      return m;
    } else {
      return bal$2(ll, v, d, r);
    }
  }
  var rr = add$2(x, data, r);
  if (r === rr) {
    return m;
  } else {
    return bal$2(l, v, d, rr);
  }
}

function find$2(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_primitive.caml_int_compare(x, param[/* v */1]);
      if (c === 0) {
        return param[/* d */2];
      }
      _param = c < 0 ? param[/* l */0] : param[/* r */3];
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function find_first$2(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[/* v */1];
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _d0 = param[/* d */2];
        var _param$1 = param[/* l */0];
        while(true) {
          var param$1 = _param$1;
          var d0 = _d0;
          var v0 = _v0;
          if (!param$1) {
            return /* tuple */[
                    v0,
                    d0
                  ];
          }
          var v$1 = param$1[/* v */1];
          if (Curry._1(f, v$1)) {
            _param$1 = param$1[/* l */0];
            _d0 = param$1[/* d */2];
            _v0 = v$1;
            continue ;
          }
          _param$1 = param$1[/* r */3];
          continue ;
        };
      }
      _param = param[/* r */3];
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function find_first_opt$2(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var v = param[/* v */1];
    if (Curry._1(f, v)) {
      var _v0 = v;
      var _d0 = param[/* d */2];
      var _param$1 = param[/* l */0];
      while(true) {
        var param$1 = _param$1;
        var d0 = _d0;
        var v0 = _v0;
        if (!param$1) {
          return /* tuple */[
                  v0,
                  d0
                ];
        }
        var v$1 = param$1[/* v */1];
        if (Curry._1(f, v$1)) {
          _param$1 = param$1[/* l */0];
          _d0 = param$1[/* d */2];
          _v0 = v$1;
          continue ;
        }
        _param$1 = param$1[/* r */3];
        continue ;
      };
    }
    _param = param[/* r */3];
    continue ;
  };
}

function find_last$2(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[/* v */1];
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _d0 = param[/* d */2];
        var _param$1 = param[/* r */3];
        while(true) {
          var param$1 = _param$1;
          var d0 = _d0;
          var v0 = _v0;
          if (!param$1) {
            return /* tuple */[
                    v0,
                    d0
                  ];
          }
          var v$1 = param$1[/* v */1];
          if (Curry._1(f, v$1)) {
            _param$1 = param$1[/* r */3];
            _d0 = param$1[/* d */2];
            _v0 = v$1;
            continue ;
          }
          _param$1 = param$1[/* l */0];
          continue ;
        };
      }
      _param = param[/* l */0];
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function find_last_opt$2(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var v = param[/* v */1];
    if (Curry._1(f, v)) {
      var _v0 = v;
      var _d0 = param[/* d */2];
      var _param$1 = param[/* r */3];
      while(true) {
        var param$1 = _param$1;
        var d0 = _d0;
        var v0 = _v0;
        if (!param$1) {
          return /* tuple */[
                  v0,
                  d0
                ];
        }
        var v$1 = param$1[/* v */1];
        if (Curry._1(f, v$1)) {
          _param$1 = param$1[/* r */3];
          _d0 = param$1[/* d */2];
          _v0 = v$1;
          continue ;
        }
        _param$1 = param$1[/* l */0];
        continue ;
      };
    }
    _param = param[/* l */0];
    continue ;
  };
}

function find_opt$2(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var c = Caml_primitive.caml_int_compare(x, param[/* v */1]);
    if (c === 0) {
      return Caml_option.some(param[/* d */2]);
    }
    _param = c < 0 ? param[/* l */0] : param[/* r */3];
    continue ;
  };
}

function mem$2(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    var c = Caml_primitive.caml_int_compare(x, param[/* v */1]);
    if (c === 0) {
      return true;
    }
    _param = c < 0 ? param[/* l */0] : param[/* r */3];
    continue ;
  };
}

function min_binding$2(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param[/* l */0];
      if (!l) {
        return /* tuple */[
                param[/* v */1],
                param[/* d */2]
              ];
      }
      _param = l;
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function min_binding_opt$2(_param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var l = param[/* l */0];
    if (!l) {
      return /* tuple */[
              param[/* v */1],
              param[/* d */2]
            ];
    }
    _param = l;
    continue ;
  };
}

function max_binding$2(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var r = param[/* r */3];
      if (!r) {
        return /* tuple */[
                param[/* v */1],
                param[/* d */2]
              ];
      }
      _param = r;
      continue ;
    }
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  };
}

function max_binding_opt$2(_param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var r = param[/* r */3];
    if (!r) {
      return /* tuple */[
              param[/* v */1],
              param[/* d */2]
            ];
    }
    _param = r;
    continue ;
  };
}

function remove_min_binding$2(param) {
  if (param) {
    var l = param[/* l */0];
    if (l) {
      return bal$2(remove_min_binding$2(l), param[/* v */1], param[/* d */2], param[/* r */3]);
    } else {
      return param[/* r */3];
    }
  }
  throw {
        ExceptionID: -3,
        _1: "Map.remove_min_elt",
        Debug: "Invalid_argument"
      };
}

function merge$4(t1, t2) {
  if (!t1) {
    return t2;
  }
  if (!t2) {
    return t1;
  }
  var match = min_binding$2(t2);
  return bal$2(t1, match[0], match[1], remove_min_binding$2(t2));
}

function remove$2(x, m) {
  if (!m) {
    return /* Empty */0;
  }
  var r = m[/* r */3];
  var d = m[/* d */2];
  var v = m[/* v */1];
  var l = m[/* l */0];
  var c = Caml_primitive.caml_int_compare(x, v);
  if (c === 0) {
    return merge$4(l, r);
  }
  if (c < 0) {
    var ll = remove$2(x, l);
    if (l === ll) {
      return m;
    } else {
      return bal$2(ll, v, d, r);
    }
  }
  var rr = remove$2(x, r);
  if (r === rr) {
    return m;
  } else {
    return bal$2(l, v, d, rr);
  }
}

function update$2(x, f, m) {
  if (m) {
    var r = m[/* r */3];
    var d = m[/* d */2];
    var v = m[/* v */1];
    var l = m[/* l */0];
    var c = Caml_primitive.caml_int_compare(x, v);
    if (c === 0) {
      var data = Curry._1(f, Caml_option.some(d));
      if (data === undefined) {
        return merge$4(l, r);
      }
      var data$1 = Caml_option.valFromOption(data);
      if (d === data$1) {
        return m;
      } else {
        return /* Node */[
                /* l */l,
                /* v */x,
                /* d */data$1,
                /* r */r,
                /* h */m[/* h */4]
              ];
      }
    }
    if (c < 0) {
      var ll = update$2(x, f, l);
      if (l === ll) {
        return m;
      } else {
        return bal$2(ll, v, d, r);
      }
    }
    var rr = update$2(x, f, r);
    if (r === rr) {
      return m;
    } else {
      return bal$2(l, v, d, rr);
    }
  }
  var data$2 = Curry._1(f, undefined);
  if (data$2 !== undefined) {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* d */Caml_option.valFromOption(data$2),
            /* r : Empty */0,
            /* h */1
          ];
  } else {
    return /* Empty */0;
  }
}

function iter$2(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    iter$2(f, param[/* l */0]);
    Curry._2(f, param[/* v */1], param[/* d */2]);
    _param = param[/* r */3];
    continue ;
  };
}

function map$2(f, param) {
  if (!param) {
    return /* Empty */0;
  }
  var l$prime = map$2(f, param[/* l */0]);
  var d$prime = Curry._1(f, param[/* d */2]);
  var r$prime = map$2(f, param[/* r */3]);
  return /* Node */[
          /* l */l$prime,
          /* v */param[/* v */1],
          /* d */d$prime,
          /* r */r$prime,
          /* h */param[/* h */4]
        ];
}

function mapi$2(f, param) {
  if (!param) {
    return /* Empty */0;
  }
  var v = param[/* v */1];
  var l$prime = mapi$2(f, param[/* l */0]);
  var d$prime = Curry._2(f, v, param[/* d */2]);
  var r$prime = mapi$2(f, param[/* r */3]);
  return /* Node */[
          /* l */l$prime,
          /* v */v,
          /* d */d$prime,
          /* r */r$prime,
          /* h */param[/* h */4]
        ];
}

function fold$2(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (!m) {
      return accu;
    }
    _accu = Curry._3(f, m[/* v */1], m[/* d */2], fold$2(f, m[/* l */0], accu));
    _m = m[/* r */3];
    continue ;
  };
}

function for_all$2(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return true;
    }
    if (!Curry._2(p, param[/* v */1], param[/* d */2])) {
      return false;
    }
    if (!for_all$2(p, param[/* l */0])) {
      return false;
    }
    _param = param[/* r */3];
    continue ;
  };
}

function exists$2(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    if (Curry._2(p, param[/* v */1], param[/* d */2])) {
      return true;
    }
    if (exists$2(p, param[/* l */0])) {
      return true;
    }
    _param = param[/* r */3];
    continue ;
  };
}

function add_min_binding$2(k, x, param) {
  if (param) {
    return bal$2(add_min_binding$2(k, x, param[/* l */0]), param[/* v */1], param[/* d */2], param[/* r */3]);
  } else {
    return singleton$2(k, x);
  }
}

function add_max_binding$2(k, x, param) {
  if (param) {
    return bal$2(param[/* l */0], param[/* v */1], param[/* d */2], add_max_binding$2(k, x, param[/* r */3]));
  } else {
    return singleton$2(k, x);
  }
}

function join$2(l, v, d, r) {
  if (!l) {
    return add_min_binding$2(v, d, r);
  }
  if (!r) {
    return add_max_binding$2(v, d, l);
  }
  var rh = r[/* h */4];
  var lh = l[/* h */4];
  if (lh > (rh + 2 | 0)) {
    return bal$2(l[/* l */0], l[/* v */1], l[/* d */2], join$2(l[/* r */3], v, d, r));
  } else if (rh > (lh + 2 | 0)) {
    return bal$2(join$2(l, v, d, r[/* l */0]), r[/* v */1], r[/* d */2], r[/* r */3]);
  } else {
    return create$2(l, v, d, r);
  }
}

function concat$2(t1, t2) {
  if (!t1) {
    return t2;
  }
  if (!t2) {
    return t1;
  }
  var match = min_binding$2(t2);
  return join$2(t1, match[0], match[1], remove_min_binding$2(t2));
}

function concat_or_join$2(t1, v, d, t2) {
  if (d !== undefined) {
    return join$2(t1, v, Caml_option.valFromOption(d), t2);
  } else {
    return concat$2(t1, t2);
  }
}

function split$2(x, param) {
  if (!param) {
    return /* tuple */[
            /* Empty */0,
            undefined,
            /* Empty */0
          ];
  }
  var r = param[/* r */3];
  var d = param[/* d */2];
  var v = param[/* v */1];
  var l = param[/* l */0];
  var c = Caml_primitive.caml_int_compare(x, v);
  if (c === 0) {
    return /* tuple */[
            l,
            Caml_option.some(d),
            r
          ];
  }
  if (c < 0) {
    var match = split$2(x, l);
    return /* tuple */[
            match[0],
            match[1],
            join$2(match[2], v, d, r)
          ];
  }
  var match$1 = split$2(x, r);
  return /* tuple */[
          join$2(l, v, d, match$1[0]),
          match$1[1],
          match$1[2]
        ];
}

function merge$5(f, s1, s2) {
  if (s1) {
    var v1 = s1[/* v */1];
    if (s1[/* h */4] >= height$2(s2)) {
      var match = split$2(v1, s2);
      return concat_or_join$2(merge$5(f, s1[/* l */0], match[0]), v1, Curry._3(f, v1, Caml_option.some(s1[/* d */2]), match[1]), merge$5(f, s1[/* r */3], match[2]));
    }
    
  } else if (!s2) {
    return /* Empty */0;
  }
  if (s2) {
    var v2 = s2[/* v */1];
    var match$1 = split$2(v2, s1);
    return concat_or_join$2(merge$5(f, match$1[0], s2[/* l */0]), v2, Curry._3(f, v2, match$1[1], Caml_option.some(s2[/* d */2])), merge$5(f, match$1[2], s2[/* r */3]));
  }
  throw {
        ExceptionID: -9,
        _1: /* tuple */[
          "map.ml",
          393,
          10
        ],
        Debug: "Assert_failure"
      };
}

function union$2(f, s1, s2) {
  if (!s1) {
    return s2;
  }
  if (!s2) {
    return s1;
  }
  var d2 = s2[/* d */2];
  var v2 = s2[/* v */1];
  var d1 = s1[/* d */2];
  var v1 = s1[/* v */1];
  if (s1[/* h */4] >= s2[/* h */4]) {
    var match = split$2(v1, s2);
    var d2$1 = match[1];
    var l = union$2(f, s1[/* l */0], match[0]);
    var r = union$2(f, s1[/* r */3], match[2]);
    if (d2$1 !== undefined) {
      return concat_or_join$2(l, v1, Curry._3(f, v1, d1, Caml_option.valFromOption(d2$1)), r);
    } else {
      return join$2(l, v1, d1, r);
    }
  }
  var match$1 = split$2(v2, s1);
  var d1$1 = match$1[1];
  var l$1 = union$2(f, match$1[0], s2[/* l */0]);
  var r$1 = union$2(f, match$1[2], s2[/* r */3]);
  if (d1$1 !== undefined) {
    return concat_or_join$2(l$1, v2, Curry._3(f, v2, Caml_option.valFromOption(d1$1), d2), r$1);
  } else {
    return join$2(l$1, v2, d2, r$1);
  }
}

function filter$2(p, m) {
  if (!m) {
    return /* Empty */0;
  }
  var r = m[/* r */3];
  var d = m[/* d */2];
  var v = m[/* v */1];
  var l = m[/* l */0];
  var l$prime = filter$2(p, l);
  var pvd = Curry._2(p, v, d);
  var r$prime = filter$2(p, r);
  if (pvd) {
    if (l === l$prime && r === r$prime) {
      return m;
    } else {
      return join$2(l$prime, v, d, r$prime);
    }
  } else {
    return concat$2(l$prime, r$prime);
  }
}

function partition$2(p, param) {
  if (!param) {
    return /* tuple */[
            /* Empty */0,
            /* Empty */0
          ];
  }
  var d = param[/* d */2];
  var v = param[/* v */1];
  var match = partition$2(p, param[/* l */0]);
  var lf = match[1];
  var lt = match[0];
  var pvd = Curry._2(p, v, d);
  var match$1 = partition$2(p, param[/* r */3]);
  var rf = match$1[1];
  var rt = match$1[0];
  if (pvd) {
    return /* tuple */[
            join$2(lt, v, d, rt),
            concat$2(lf, rf)
          ];
  } else {
    return /* tuple */[
            concat$2(lt, rt),
            join$2(lf, v, d, rf)
          ];
  }
}

function cons_enum$2(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (!m) {
      return e;
    }
    _e = /* More */[
      m[/* v */1],
      m[/* d */2],
      m[/* r */3],
      e
    ];
    _m = m[/* l */0];
    continue ;
  };
}

function compare$2(cmp, m1, m2) {
  var _e1 = cons_enum$2(m1, /* End */0);
  var _e2 = cons_enum$2(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (!e1) {
      if (e2) {
        return -1;
      } else {
        return 0;
      }
    }
    if (!e2) {
      return 1;
    }
    var c = Caml_primitive.caml_int_compare(e1[0], e2[0]);
    if (c !== 0) {
      return c;
    }
    var c$1 = Curry._2(cmp, e1[1], e2[1]);
    if (c$1 !== 0) {
      return c$1;
    }
    _e2 = cons_enum$2(e2[2], e2[3]);
    _e1 = cons_enum$2(e1[2], e1[3]);
    continue ;
  };
}

function equal$2(cmp, m1, m2) {
  var _e1 = cons_enum$2(m1, /* End */0);
  var _e2 = cons_enum$2(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (!e1) {
      if (e2) {
        return false;
      } else {
        return true;
      }
    }
    if (!e2) {
      return false;
    }
    if (e1[0] !== e2[0]) {
      return false;
    }
    if (!Curry._2(cmp, e1[1], e2[1])) {
      return false;
    }
    _e2 = cons_enum$2(e2[2], e2[3]);
    _e1 = cons_enum$2(e1[2], e1[3]);
    continue ;
  };
}

function cardinal$2(param) {
  if (param) {
    return (cardinal$2(param[/* l */0]) + 1 | 0) + cardinal$2(param[/* r */3]) | 0;
  } else {
    return 0;
  }
}

function bindings_aux$2(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (!param) {
      return accu;
    }
    _param = param[/* l */0];
    _accu = /* :: */[
      /* tuple */[
        param[/* v */1],
        param[/* d */2]
      ],
      bindings_aux$2(accu, param[/* r */3])
    ];
    continue ;
  };
}

function bindings$2(s) {
  return bindings_aux$2(/* [] */0, s);
}

var Labs = {
  empty: /* Empty */0,
  is_empty: is_empty$2,
  mem: mem$2,
  add: add$2,
  update: update$2,
  singleton: singleton$2,
  remove: remove$2,
  merge: merge$5,
  union: union$2,
  compare: compare$2,
  equal: equal$2,
  iter: iter$2,
  fold: fold$2,
  for_all: for_all$2,
  exists: exists$2,
  filter: filter$2,
  partition: partition$2,
  cardinal: cardinal$2,
  bindings: bindings$2,
  min_binding: min_binding$2,
  min_binding_opt: min_binding_opt$2,
  max_binding: max_binding$2,
  max_binding_opt: max_binding_opt$2,
  choose: min_binding$2,
  choose_opt: min_binding_opt$2,
  split: split$2,
  find: find$2,
  find_opt: find_opt$2,
  find_first: find_first$2,
  find_first_opt: find_first_opt$2,
  find_last: find_last$2,
  find_last_opt: find_last_opt$2,
  map: map$2,
  mapi: mapi$2
};

var dummy_table = {
  size: 0,
  methods: [undefined],
  methods_by_name: /* Empty */0,
  methods_by_label: /* Empty */0,
  previous_states: /* [] */0,
  hidden_meths: /* [] */0,
  vars: /* Empty */0,
  initializers: /* [] */0
};

var table_count = {
  contents: 0
};

function fit_size(n) {
  if (n <= 2) {
    return n;
  } else {
    return (fit_size((n + 1 | 0) / 2 | 0) << 1);
  }
}

function new_table(pub_labels) {
  table_count.contents = table_count.contents + 1 | 0;
  var len = pub_labels.length;
  var methods = Caml_array.caml_make_vect((len << 1) + 2 | 0, /* DummyA */0);
  Caml_array.caml_array_set(methods, 0, len);
  Caml_array.caml_array_set(methods, 1, (Caml_int32.imul(fit_size(len), Sys.word_size) / 8 | 0) - 1 | 0);
  for(var i = 0; i < len; ++i){
    Caml_array.caml_array_set(methods, (i << 1) + 3 | 0, Caml_array.caml_array_get(pub_labels, i));
  }
  return {
          size: 2,
          methods: methods,
          methods_by_name: /* Empty */0,
          methods_by_label: /* Empty */0,
          previous_states: /* [] */0,
          hidden_meths: /* [] */0,
          vars: /* Empty */0,
          initializers: /* [] */0
        };
}

function resize(array, new_size) {
  var old_size = array.methods.length;
  if (new_size <= old_size) {
    return ;
  }
  var new_buck = Caml_array.caml_make_vect(new_size, /* DummyA */0);
  $$Array.blit(array.methods, 0, new_buck, 0, old_size);
  array.methods = new_buck;
  
}

function put(array, label, element) {
  resize(array, label + 1 | 0);
  return Caml_array.caml_array_set(array.methods, label, element);
}

var method_count = {
  contents: 0
};

var inst_var_count = {
  contents: 0
};

function new_method(table) {
  var index = table.methods.length;
  resize(table, index + 1 | 0);
  return index;
}

function get_method_label(table, name) {
  try {
    return find$1(name, table.methods_by_name);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.ExceptionID === /* Not_found */-6) {
      var label = new_method(table);
      table.methods_by_name = add$1(name, label, table.methods_by_name);
      table.methods_by_label = add$2(label, true, table.methods_by_label);
      return label;
    }
    throw exn;
  }
}

function get_method_labels(table, names) {
  return $$Array.map((function (param) {
                return get_method_label(table, param);
              }), names);
}

function set_method(table, label, element) {
  method_count.contents = method_count.contents + 1 | 0;
  if (find$2(label, table.methods_by_label)) {
    return put(table, label, element);
  } else {
    table.hidden_meths = /* :: */[
      /* tuple */[
        label,
        element
      ],
      table.hidden_meths
    ];
    return ;
  }
}

function get_method(table, label) {
  try {
    return List.assoc(label, table.hidden_meths);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.ExceptionID === /* Not_found */-6) {
      return Caml_array.caml_array_get(table.methods, label);
    }
    throw exn;
  }
}

function to_list(arr) {
  if (arr === 0) {
    return /* [] */0;
  } else {
    return $$Array.to_list(arr);
  }
}

function narrow(table, vars, virt_meths, concr_meths) {
  var vars$1 = to_list(vars);
  var virt_meths$1 = to_list(virt_meths);
  var concr_meths$1 = to_list(concr_meths);
  var virt_meth_labs = List.map((function (param) {
          return get_method_label(table, param);
        }), virt_meths$1);
  var concr_meth_labs = List.map((function (param) {
          return get_method_label(table, param);
        }), concr_meths$1);
  table.previous_states = /* :: */[
    /* tuple */[
      table.methods_by_name,
      table.methods_by_label,
      table.hidden_meths,
      table.vars,
      virt_meth_labs,
      vars$1
    ],
    table.previous_states
  ];
  table.vars = fold((function (lab, info, tvars) {
          if (List.mem(lab, vars$1)) {
            return add(lab, info, tvars);
          } else {
            return tvars;
          }
        }), table.vars, /* Empty */0);
  var by_name = {
    contents: /* Empty */0
  };
  var by_label = {
    contents: /* Empty */0
  };
  List.iter2((function (met, label) {
          by_name.contents = add$1(met, label, by_name.contents);
          var tmp;
          try {
            tmp = find$2(label, table.methods_by_label);
          }
          catch (raw_exn){
            var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
            if (exn.ExceptionID === /* Not_found */-6) {
              tmp = true;
            } else {
              throw exn;
            }
          }
          by_label.contents = add$2(label, tmp, by_label.contents);
          
        }), concr_meths$1, concr_meth_labs);
  List.iter2((function (met, label) {
          by_name.contents = add$1(met, label, by_name.contents);
          by_label.contents = add$2(label, false, by_label.contents);
          
        }), virt_meths$1, virt_meth_labs);
  table.methods_by_name = by_name.contents;
  table.methods_by_label = by_label.contents;
  table.hidden_meths = List.fold_right((function (met, hm) {
          if (List.mem(met[0], virt_meth_labs)) {
            return hm;
          } else {
            return /* :: */[
                    met,
                    hm
                  ];
          }
        }), table.hidden_meths, /* [] */0);
  
}

function widen(table) {
  var match = List.hd(table.previous_states);
  var virt_meths = match[4];
  table.previous_states = List.tl(table.previous_states);
  table.vars = List.fold_left((function (s, v) {
          return add(v, find(v, table.vars), s);
        }), match[3], match[5]);
  table.methods_by_name = match[0];
  table.methods_by_label = match[1];
  table.hidden_meths = List.fold_right((function (met, hm) {
          if (List.mem(met[0], virt_meths)) {
            return hm;
          } else {
            return /* :: */[
                    met,
                    hm
                  ];
          }
        }), table.hidden_meths, match[2]);
  
}

function new_slot(table) {
  var index = table.size;
  table.size = index + 1 | 0;
  return index;
}

function new_variable(table, name) {
  try {
    return find(name, table.vars);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.ExceptionID === /* Not_found */-6) {
      var index = new_slot(table);
      if (name !== "") {
        table.vars = add(name, index, table.vars);
      }
      return index;
    }
    throw exn;
  }
}

function to_array(arr) {
  if (Caml_obj.caml_equal(arr, 0)) {
    return [];
  } else {
    return arr;
  }
}

function new_methods_variables(table, meths, vals) {
  var meths$1 = to_array(meths);
  var nmeths = meths$1.length;
  var nvals = vals.length;
  var res = Caml_array.caml_make_vect(nmeths + nvals | 0, 0);
  for(var i = 0; i < nmeths; ++i){
    Caml_array.caml_array_set(res, i, get_method_label(table, Caml_array.caml_array_get(meths$1, i)));
  }
  for(var i$1 = 0; i$1 < nvals; ++i$1){
    Caml_array.caml_array_set(res, i$1 + nmeths | 0, new_variable(table, Caml_array.caml_array_get(vals, i$1)));
  }
  return res;
}

function get_variable(table, name) {
  try {
    return find(name, table.vars);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.ExceptionID === /* Not_found */-6) {
      throw {
            ExceptionID: -9,
            _1: /* tuple */[
              "test_internalOO.ml",
              280,
              50
            ],
            Debug: "Assert_failure"
          };
    }
    throw exn;
  }
}

function get_variables(table, names) {
  return $$Array.map((function (param) {
                return get_variable(table, param);
              }), names);
}

function add_initializer(table, f) {
  table.initializers = /* :: */[
    f,
    table.initializers
  ];
  
}

function create_table(public_methods) {
  if (public_methods === 0) {
    return new_table([]);
  }
  var tags = $$Array.map(public_method_label, public_methods);
  var table = new_table(tags);
  $$Array.iteri((function (i, met) {
          var lab = (i << 1) + 2 | 0;
          table.methods_by_name = add$1(met, lab, table.methods_by_name);
          table.methods_by_label = add$2(lab, true, table.methods_by_label);
          
        }), public_methods);
  return table;
}

function init_class(table) {
  inst_var_count.contents = (inst_var_count.contents + table.size | 0) - 1 | 0;
  table.initializers = List.rev(table.initializers);
  return resize(table, 3 + Caml_int32.div((Caml_array.caml_array_get(table.methods, 1) << 4), Sys.word_size) | 0);
}

function inherits(cla, vals, virt_meths, concr_meths, param, top) {
  var $$super = param[1];
  narrow(cla, vals, virt_meths, concr_meths);
  var init = top ? Curry._2($$super, cla, param[3]) : Curry._1($$super, cla);
  widen(cla);
  return Caml_array.caml_array_concat(/* :: */[
              [init],
              /* :: */[
                $$Array.map((function (param) {
                        return get_variable(cla, param);
                      }), to_array(vals)),
                /* :: */[
                  $$Array.map((function (nm) {
                          return get_method(cla, get_method_label(cla, nm));
                        }), to_array(concr_meths)),
                  /* [] */0
                ]
              ]
            ]);
}

function make_class(pub_meths, class_init) {
  var table = create_table(pub_meths);
  var env_init = Curry._1(class_init, table);
  init_class(table);
  return /* tuple */[
          Curry._1(env_init, 0),
          class_init,
          env_init,
          0
        ];
}

function make_class_store(pub_meths, class_init, init_table) {
  var table = create_table(pub_meths);
  var env_init = Curry._1(class_init, table);
  init_class(table);
  init_table.class_init = class_init;
  init_table.env_init = env_init;
  
}

function dummy_class(loc) {
  var undef = function (param) {
    throw {
          ExceptionID: -11,
          _1: loc,
          Debug: "Undefined_recursive_module"
        };
  };
  return /* tuple */[
          undef,
          undef,
          undef,
          0
        ];
}

function iter_f(obj, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    Curry._1(param[0], obj);
    _param = param[1];
    continue ;
  };
}

function run_initializers(obj, table) {
  var inits = table.initializers;
  if (inits !== /* [] */0) {
    return iter_f(obj, inits);
  }
  
}

function run_initializers_opt(obj_0, obj, table) {
  if (obj_0) {
    return obj;
  }
  var inits = table.initializers;
  if (inits !== /* [] */0) {
    iter_f(obj, inits);
  }
  return obj;
}

function build_path(n, keys, tables) {
  var res = {
    key: 0,
    data: /* Empty */0,
    next: /* Empty */0
  };
  var r = res;
  for(var i = 0; i <= n; ++i){
    r = /* Cons */[
      Caml_array.caml_array_get(keys, i),
      r,
      /* Empty */0
    ];
  }
  tables.data = r;
  return res;
}

function lookup_keys(i, keys, tables) {
  if (i < 0) {
    return tables;
  }
  var key = Caml_array.caml_array_get(keys, i);
  var _tables = tables;
  while(true) {
    var tables$1 = _tables;
    if (tables$1.key === key) {
      return lookup_keys(i - 1 | 0, keys, tables$1.data);
    }
    if (tables$1.next !== /* Empty */0) {
      _tables = tables$1.next;
      continue ;
    }
    var next = /* Cons */[
      key,
      /* Empty */0,
      /* Empty */0
    ];
    tables$1.next = next;
    return build_path(i - 1 | 0, keys, next);
  };
}

function lookup_tables(root, keys) {
  if (root.data !== /* Empty */0) {
    return lookup_keys(keys.length - 1 | 0, keys, root.data);
  } else {
    return build_path(keys.length - 1 | 0, keys, root);
  }
}

function get_const(x) {
  return (function (obj) {
      return x;
    });
}

function get_var(n) {
  return (function (obj) {
      return obj[n];
    });
}

function get_env(e, n) {
  return (function (obj) {
      return obj[e][n];
    });
}

function get_meth(n) {
  return (function (obj) {
      return Curry._1(obj[0][n], obj);
    });
}

function set_var(n) {
  return (function (obj, x) {
      obj[n] = x;
      
    });
}

function app_const(f, x) {
  return (function (obj) {
      return Curry._1(f, x);
    });
}

function app_var(f, n) {
  return (function (obj) {
      return Curry._1(f, obj[n]);
    });
}

function app_env(f, e, n) {
  return (function (obj) {
      return Curry._1(f, obj[e][n]);
    });
}

function app_meth(f, n) {
  return (function (obj) {
      return Curry._1(f, Curry._1(obj[0][n], obj));
    });
}

function app_const_const(f, x, y) {
  return (function (obj) {
      return Curry._2(f, x, y);
    });
}

function app_const_var(f, x, n) {
  return (function (obj) {
      return Curry._2(f, x, obj[n]);
    });
}

function app_const_meth(f, x, n) {
  return (function (obj) {
      return Curry._2(f, x, Curry._1(obj[0][n], obj));
    });
}

function app_var_const(f, n, x) {
  return (function (obj) {
      return Curry._2(f, obj[n], x);
    });
}

function app_meth_const(f, n, x) {
  return (function (obj) {
      return Curry._2(f, Curry._1(obj[0][n], obj), x);
    });
}

function app_const_env(f, x, e, n) {
  return (function (obj) {
      return Curry._2(f, x, obj[e][n]);
    });
}

function app_env_const(f, e, n, x) {
  return (function (obj) {
      return Curry._2(f, obj[e][n], x);
    });
}

function meth_app_const(n, x) {
  return (function (obj) {
      return Curry._2(obj[0][n], obj, x);
    });
}

function meth_app_var(n, m) {
  return (function (obj) {
      return Curry._2(obj[0][n], obj, obj[m]);
    });
}

function meth_app_env(n, e, m) {
  return (function (obj) {
      return Curry._2(obj[0][n], obj, obj[e][m]);
    });
}

function meth_app_meth(n, m) {
  return (function (obj) {
      return Curry._2(obj[0][n], obj, Curry._1(obj[0][m], obj));
    });
}

function send_const(m, x, c) {
  return (function (obj) {
      return Curry._1(Curry._3(Caml_oo.caml_get_public_method, x, m, 1), x);
    });
}

function send_var(m, n, c) {
  return (function (obj) {
      var tmp = obj[n];
      return Curry._1(Curry._3(Caml_oo.caml_get_public_method, tmp, m, 2), tmp);
    });
}

function send_env(m, e, n, c) {
  return (function (obj) {
      var tmp = obj[e][n];
      return Curry._1(Curry._3(Caml_oo.caml_get_public_method, tmp, m, 3), tmp);
    });
}

function send_meth(m, n, c) {
  return (function (obj) {
      var tmp = Curry._1(obj[0][n], obj);
      return Curry._1(Curry._3(Caml_oo.caml_get_public_method, tmp, m, 4), tmp);
    });
}

function new_cache(table) {
  var n = new_method(table);
  var n$1 = n % 2 === 0 || n > (2 + Caml_int32.div((Caml_array.caml_array_get(table.methods, 1) << 4), Sys.word_size) | 0) ? n : new_method(table);
  Caml_array.caml_array_set(table.methods, n$1, 0);
  return n$1;
}

function method_impl(table, i, arr) {
  var next = function (param) {
    i.contents = i.contents + 1 | 0;
    return Caml_array.caml_array_get(arr, i.contents);
  };
  var clo = next(undefined);
  if (typeof clo !== "number") {
    return clo;
  }
  switch (clo) {
    case /* GetConst */0 :
        var x = next(undefined);
        return (function (obj) {
            return x;
          });
    case /* GetVar */1 :
        var n = next(undefined);
        return (function (obj) {
            return obj[n];
          });
    case /* GetEnv */2 :
        var e = next(undefined);
        var n$1 = next(undefined);
        return get_env(e, n$1);
    case /* GetMeth */3 :
        return get_meth(next(undefined));
    case /* SetVar */4 :
        var n$2 = next(undefined);
        return (function (obj, x) {
            obj[n$2] = x;
            
          });
    case /* AppConst */5 :
        var f = next(undefined);
        var x$1 = next(undefined);
        return (function (obj) {
            return Curry._1(f, x$1);
          });
    case /* AppVar */6 :
        var f$1 = next(undefined);
        var n$3 = next(undefined);
        return (function (obj) {
            return Curry._1(f$1, obj[n$3]);
          });
    case /* AppEnv */7 :
        var f$2 = next(undefined);
        var e$1 = next(undefined);
        var n$4 = next(undefined);
        return app_env(f$2, e$1, n$4);
    case /* AppMeth */8 :
        var f$3 = next(undefined);
        var n$5 = next(undefined);
        return app_meth(f$3, n$5);
    case /* AppConstConst */9 :
        var f$4 = next(undefined);
        var x$2 = next(undefined);
        var y = next(undefined);
        return (function (obj) {
            return Curry._2(f$4, x$2, y);
          });
    case /* AppConstVar */10 :
        var f$5 = next(undefined);
        var x$3 = next(undefined);
        var n$6 = next(undefined);
        return app_const_var(f$5, x$3, n$6);
    case /* AppConstEnv */11 :
        var f$6 = next(undefined);
        var x$4 = next(undefined);
        var e$2 = next(undefined);
        var n$7 = next(undefined);
        return app_const_env(f$6, x$4, e$2, n$7);
    case /* AppConstMeth */12 :
        var f$7 = next(undefined);
        var x$5 = next(undefined);
        var n$8 = next(undefined);
        return app_const_meth(f$7, x$5, n$8);
    case /* AppVarConst */13 :
        var f$8 = next(undefined);
        var n$9 = next(undefined);
        var x$6 = next(undefined);
        return app_var_const(f$8, n$9, x$6);
    case /* AppEnvConst */14 :
        var f$9 = next(undefined);
        var e$3 = next(undefined);
        var n$10 = next(undefined);
        var x$7 = next(undefined);
        return app_env_const(f$9, e$3, n$10, x$7);
    case /* AppMethConst */15 :
        var f$10 = next(undefined);
        var n$11 = next(undefined);
        var x$8 = next(undefined);
        return app_meth_const(f$10, n$11, x$8);
    case /* MethAppConst */16 :
        var n$12 = next(undefined);
        var x$9 = next(undefined);
        return meth_app_const(n$12, x$9);
    case /* MethAppVar */17 :
        var n$13 = next(undefined);
        var m = next(undefined);
        return meth_app_var(n$13, m);
    case /* MethAppEnv */18 :
        var n$14 = next(undefined);
        var e$4 = next(undefined);
        var m$1 = next(undefined);
        return meth_app_env(n$14, e$4, m$1);
    case /* MethAppMeth */19 :
        var n$15 = next(undefined);
        var m$2 = next(undefined);
        return meth_app_meth(n$15, m$2);
    case /* SendConst */20 :
        var m$3 = next(undefined);
        var x$10 = next(undefined);
        return send_const(m$3, x$10, new_cache(table));
    case /* SendVar */21 :
        var m$4 = next(undefined);
        var n$16 = next(undefined);
        return send_var(m$4, n$16, new_cache(table));
    case /* SendEnv */22 :
        var m$5 = next(undefined);
        var e$5 = next(undefined);
        var n$17 = next(undefined);
        return send_env(m$5, e$5, n$17, new_cache(table));
    case /* SendMeth */23 :
        var m$6 = next(undefined);
        var n$18 = next(undefined);
        return send_meth(m$6, n$18, new_cache(table));
    
  }
}

function set_methods(table, methods) {
  var len = methods.length;
  var i = {
    contents: 0
  };
  while(i.contents < len) {
    var label = Caml_array.caml_array_get(methods, i.contents);
    var clo = method_impl(table, i, methods);
    set_method(table, label, clo);
    i.contents = i.contents + 1 | 0;
  };
  
}

function stats(param) {
  return {
          classes: table_count.contents,
          methods: method_count.contents,
          inst_vars: inst_var_count.contents
        };
}

var initial_object_size = 2;

var dummy_item;

var dummy_met = /* DummyA */0;

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
exports.iter_f = iter_f;
exports.run_initializers = run_initializers;
exports.run_initializers_opt = run_initializers_opt;
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
