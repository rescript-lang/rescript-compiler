'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");

function Make(Ord) {
  var height = function (param) {
    if (param) {
      return param._4;
    } else {
      return 0;
    }
  };
  var create = function (l, x, d, r) {
    var hl = height(l);
    var hr = height(r);
    return /* Node */{
            _0: l,
            _1: x,
            _2: d,
            _3: r,
            _4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  };
  var singleton = function (x, d) {
    return /* Node */{
            _0: /* Empty */0,
            _1: x,
            _2: d,
            _3: /* Empty */0,
            _4: 1
          };
  };
  var bal = function (l, x, d, r) {
    var hl = l ? l._4 : 0;
    var hr = r ? r._4 : 0;
    if (hl > (hr + 2 | 0)) {
      if (l) {
        var lr = l._3;
        var ld = l._2;
        var lv = l._1;
        var ll = l._0;
        if (height(ll) >= height(lr)) {
          return create(ll, lv, ld, create(lr, x, d, r));
        }
        if (lr) {
          return create(create(ll, lv, ld, lr._0), lr._1, lr._2, create(lr._3, x, d, r));
        }
        throw {
              RE_EXN_ID: "Invalid_argument",
              _1: "Map.bal",
              Error: new Error()
            };
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal",
            Error: new Error()
          };
    }
    if (hr <= (hl + 2 | 0)) {
      return /* Node */{
              _0: l,
              _1: x,
              _2: d,
              _3: r,
              _4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
            };
    }
    if (r) {
      var rr = r._3;
      var rd = r._2;
      var rv = r._1;
      var rl = r._0;
      if (height(rr) >= height(rl)) {
        return create(create(l, x, d, rl), rv, rd, rr);
      }
      if (rl) {
        return create(create(l, x, d, rl._0), rl._1, rl._2, create(rl._3, rv, rd, rr));
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal",
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  };
  var is_empty = function (param) {
    if (param) {
      return false;
    } else {
      return true;
    }
  };
  var add = function (x, data, param) {
    if (!param) {
      return /* Node */{
              _0: /* Empty */0,
              _1: x,
              _2: data,
              _3: /* Empty */0,
              _4: 1
            };
    }
    var r = param._3;
    var d = param._2;
    var v = param._1;
    var l = param._0;
    var c = Curry._2(Ord.compare, x, v);
    if (c === 0) {
      return /* Node */{
              _0: l,
              _1: x,
              _2: data,
              _3: r,
              _4: param._4
            };
    } else if (c < 0) {
      return bal(add(x, data, l), v, d, r);
    } else {
      return bal(l, v, d, add(x, data, r));
    }
  };
  var find = function (x, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var c = Curry._2(Ord.compare, x, param._1);
        if (c === 0) {
          return param._2;
        }
        _param = c < 0 ? param._0 : param._3;
        continue ;
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    };
  };
  var mem = function (x, _param) {
    while(true) {
      var param = _param;
      if (!param) {
        return false;
      }
      var c = Curry._2(Ord.compare, x, param._1);
      if (c === 0) {
        return true;
      }
      _param = c < 0 ? param._0 : param._3;
      continue ;
    };
  };
  var min_binding = function (_param) {
    while(true) {
      var param = _param;
      if (param) {
        var l = param._0;
        if (!l) {
          return [
                  param._1,
                  param._2
                ];
        }
        _param = l;
        continue ;
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    };
  };
  var max_binding = function (_param) {
    while(true) {
      var param = _param;
      if (param) {
        var r = param._3;
        if (!r) {
          return [
                  param._1,
                  param._2
                ];
        }
        _param = r;
        continue ;
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    };
  };
  var remove_min_binding = function (param) {
    if (param) {
      var l = param._0;
      if (l) {
        return bal(remove_min_binding(l), param._1, param._2, param._3);
      } else {
        return param._3;
      }
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.remove_min_elt",
          Error: new Error()
        };
  };
  var remove = function (x, param) {
    if (!param) {
      return /* Empty */0;
    }
    var r = param._3;
    var d = param._2;
    var v = param._1;
    var l = param._0;
    var c = Curry._2(Ord.compare, x, v);
    if (c === 0) {
      if (!l) {
        return r;
      }
      if (!r) {
        return l;
      }
      var match = min_binding(r);
      return bal(l, match[0], match[1], remove_min_binding(r));
    } else if (c < 0) {
      return bal(remove(x, l), v, d, r);
    } else {
      return bal(l, v, d, remove(x, r));
    }
  };
  var iter = function (f, _param) {
    while(true) {
      var param = _param;
      if (!param) {
        return ;
      }
      iter(f, param._0);
      Curry._2(f, param._1, param._2);
      _param = param._3;
      continue ;
    };
  };
  var map = function (f, param) {
    if (!param) {
      return /* Empty */0;
    }
    var l$prime = map(f, param._0);
    var d$prime = Curry._1(f, param._2);
    var r$prime = map(f, param._3);
    return /* Node */{
            _0: l$prime,
            _1: param._1,
            _2: d$prime,
            _3: r$prime,
            _4: param._4
          };
  };
  var mapi = function (f, param) {
    if (!param) {
      return /* Empty */0;
    }
    var v = param._1;
    var l$prime = mapi(f, param._0);
    var d$prime = Curry._2(f, v, param._2);
    var r$prime = mapi(f, param._3);
    return /* Node */{
            _0: l$prime,
            _1: v,
            _2: d$prime,
            _3: r$prime,
            _4: param._4
          };
  };
  var fold = function (f, _m, _accu) {
    while(true) {
      var accu = _accu;
      var m = _m;
      if (!m) {
        return accu;
      }
      _accu = Curry._3(f, m._1, m._2, fold(f, m._0, accu));
      _m = m._3;
      continue ;
    };
  };
  var for_all = function (p, _param) {
    while(true) {
      var param = _param;
      if (!param) {
        return true;
      }
      if (!Curry._2(p, param._1, param._2)) {
        return false;
      }
      if (!for_all(p, param._0)) {
        return false;
      }
      _param = param._3;
      continue ;
    };
  };
  var exists = function (p, _param) {
    while(true) {
      var param = _param;
      if (!param) {
        return false;
      }
      if (Curry._2(p, param._1, param._2)) {
        return true;
      }
      if (exists(p, param._0)) {
        return true;
      }
      _param = param._3;
      continue ;
    };
  };
  var add_min_binding = function (k, v, param) {
    if (param) {
      return bal(add_min_binding(k, v, param._0), param._1, param._2, param._3);
    } else {
      return singleton(k, v);
    }
  };
  var add_max_binding = function (k, v, param) {
    if (param) {
      return bal(param._0, param._1, param._2, add_max_binding(k, v, param._3));
    } else {
      return singleton(k, v);
    }
  };
  var join = function (l, v, d, r) {
    if (!l) {
      return add_min_binding(v, d, r);
    }
    if (!r) {
      return add_max_binding(v, d, l);
    }
    var rh = r._4;
    var lh = l._4;
    if (lh > (rh + 2 | 0)) {
      return bal(l._0, l._1, l._2, join(l._3, v, d, r));
    } else if (rh > (lh + 2 | 0)) {
      return bal(join(l, v, d, r._0), r._1, r._2, r._3);
    } else {
      return create(l, v, d, r);
    }
  };
  var concat = function (t1, t2) {
    if (!t1) {
      return t2;
    }
    if (!t2) {
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
    if (!param) {
      return [
              /* Empty */0,
              undefined,
              /* Empty */0
            ];
    }
    var r = param._3;
    var d = param._2;
    var v = param._1;
    var l = param._0;
    var c = Curry._2(Ord.compare, x, v);
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
  var merge = function (f, s1, s2) {
    if (s1) {
      var v1 = s1._1;
      if (s1._4 >= height(s2)) {
        var match = split(v1, s2);
        return concat_or_join(merge(f, s1._0, match[0]), v1, Curry._3(f, v1, Caml_option.some(s1._2), match[1]), merge(f, s1._3, match[2]));
      }
      
    } else if (!s2) {
      return /* Empty */0;
    }
    if (s2) {
      var v2 = s2._1;
      var match$1 = split(v2, s1);
      return concat_or_join(merge(f, match$1[0], s2._0), v2, Curry._3(f, v2, match$1[1], Caml_option.some(s2._2)), merge(f, match$1[2], s2._3));
    }
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "inline_map2_test.ml",
            270,
            10
          ],
          Error: new Error()
        };
  };
  var filter = function (p, param) {
    if (!param) {
      return /* Empty */0;
    }
    var d = param._2;
    var v = param._1;
    var l$prime = filter(p, param._0);
    var pvd = Curry._2(p, v, d);
    var r$prime = filter(p, param._3);
    if (pvd) {
      return join(l$prime, v, d, r$prime);
    } else {
      return concat(l$prime, r$prime);
    }
  };
  var partition = function (p, param) {
    if (!param) {
      return [
              /* Empty */0,
              /* Empty */0
            ];
    }
    var d = param._2;
    var v = param._1;
    var match = partition(p, param._0);
    var lf = match[1];
    var lt = match[0];
    var pvd = Curry._2(p, v, d);
    var match$1 = partition(p, param._3);
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
      if (!m) {
        return e;
      }
      _e = /* More */{
        _0: m._1,
        _1: m._2,
        _2: m._3,
        _3: e
      };
      _m = m._0;
      continue ;
    };
  };
  var compare = function (cmp, m1, m2) {
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
      var c = Curry._2(Ord.compare, e1._0, e2._0);
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
      if (Curry._2(Ord.compare, e1._0, e2._0) !== 0) {
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
    if (param) {
      return (cardinal(param._0) + 1 | 0) + cardinal(param._3) | 0;
    } else {
      return 0;
    }
  };
  var bindings_aux = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (!param) {
        return accu;
      }
      _param = param._0;
      _accu = {
        hd: [
          param._1,
          param._2
        ],
        tl: bindings_aux(accu, param._3)
      };
      continue ;
    };
  };
  var bindings = function (s) {
    return bindings_aux(/* [] */0, s);
  };
  return {
          height,
          create,
          singleton,
          bal,
          empty: /* Empty */0,
          is_empty,
          add,
          find,
          mem,
          min_binding,
          max_binding,
          remove_min_binding,
          remove,
          iter,
          map,
          mapi,
          fold,
          for_all,
          exists,
          add_min_binding,
          add_max_binding,
          join,
          concat,
          concat_or_join,
          split,
          merge,
          filter,
          partition,
          cons_enum,
          compare,
          equal,
          cardinal,
          bindings_aux,
          bindings,
          choose: min_binding
        };
}

function height(param) {
  if (param) {
    return param._4;
  } else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* Node */{
          _0: l,
          _1: x,
          _2: d,
          _3: r,
          _4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function singleton(x, d) {
  return /* Node */{
          _0: /* Empty */0,
          _1: x,
          _2: d,
          _3: /* Empty */0,
          _4: 1
        };
}

function bal(l, x, d, r) {
  var hl = l ? l._4 : 0;
  var hr = r ? r._4 : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l._3;
      var ld = l._2;
      var lv = l._1;
      var ll = l._0;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      }
      if (lr) {
        return create(create(ll, lv, ld, lr._0), lr._1, lr._2, create(lr._3, x, d, r));
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal",
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */{
            _0: l,
            _1: x,
            _2: d,
            _3: r,
            _4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (r) {
    var rr = r._3;
    var rd = r._2;
    var rv = r._1;
    var rl = r._0;
    if (height(rr) >= height(rl)) {
      return create(create(l, x, d, rl), rv, rd, rr);
    }
    if (rl) {
      return create(create(l, x, d, rl._0), rl._1, rl._2, create(rl._3, rv, rd, rr));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Map.bal",
        Error: new Error()
      };
}

function is_empty(param) {
  if (param) {
    return false;
  } else {
    return true;
  }
}

function add(x, data, param) {
  if (!param) {
    return /* Node */{
            _0: /* Empty */0,
            _1: x,
            _2: data,
            _3: /* Empty */0,
            _4: 1
          };
  }
  var r = param._3;
  var d = param._2;
  var v = param._1;
  var l = param._0;
  var c = Caml_primitive.caml_int_compare(x, v);
  if (c === 0) {
    return /* Node */{
            _0: l,
            _1: x,
            _2: data,
            _3: r,
            _4: param._4
          };
  } else if (c < 0) {
    return bal(add(x, data, l), v, d, r);
  } else {
    return bal(l, v, d, add(x, data, r));
  }
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_primitive.caml_int_compare(x, param._1);
      if (c === 0) {
        return param._2;
      }
      _param = c < 0 ? param._0 : param._3;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    var c = Caml_primitive.caml_int_compare(x, param._1);
    if (c === 0) {
      return true;
    }
    _param = c < 0 ? param._0 : param._3;
    continue ;
  };
}

function min_binding(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param._0;
      if (!l) {
        return [
                param._1,
                param._2
              ];
      }
      _param = l;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function max_binding(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var r = param._3;
      if (!r) {
        return [
                param._1,
                param._2
              ];
      }
      _param = r;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function remove_min_binding(param) {
  if (param) {
    var l = param._0;
    if (l) {
      return bal(remove_min_binding(l), param._1, param._2, param._3);
    } else {
      return param._3;
    }
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Map.remove_min_elt",
        Error: new Error()
      };
}

function remove(x, param) {
  if (!param) {
    return /* Empty */0;
  }
  var r = param._3;
  var d = param._2;
  var v = param._1;
  var l = param._0;
  var c = Caml_primitive.caml_int_compare(x, v);
  if (c === 0) {
    if (!l) {
      return r;
    }
    if (!r) {
      return l;
    }
    var match = min_binding(r);
    return bal(l, match[0], match[1], remove_min_binding(r));
  } else if (c < 0) {
    return bal(remove(x, l), v, d, r);
  } else {
    return bal(l, v, d, remove(x, r));
  }
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    iter(f, param._0);
    Curry._2(f, param._1, param._2);
    _param = param._3;
    continue ;
  };
}

function map(f, param) {
  if (!param) {
    return /* Empty */0;
  }
  var l$prime = map(f, param._0);
  var d$prime = Curry._1(f, param._2);
  var r$prime = map(f, param._3);
  return /* Node */{
          _0: l$prime,
          _1: param._1,
          _2: d$prime,
          _3: r$prime,
          _4: param._4
        };
}

function mapi(f, param) {
  if (!param) {
    return /* Empty */0;
  }
  var v = param._1;
  var l$prime = mapi(f, param._0);
  var d$prime = Curry._2(f, v, param._2);
  var r$prime = mapi(f, param._3);
  return /* Node */{
          _0: l$prime,
          _1: v,
          _2: d$prime,
          _3: r$prime,
          _4: param._4
        };
}

function fold(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (!m) {
      return accu;
    }
    _accu = Curry._3(f, m._1, m._2, fold(f, m._0, accu));
    _m = m._3;
    continue ;
  };
}

function for_all(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return true;
    }
    if (!Curry._2(p, param._1, param._2)) {
      return false;
    }
    if (!for_all(p, param._0)) {
      return false;
    }
    _param = param._3;
    continue ;
  };
}

function exists(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    if (Curry._2(p, param._1, param._2)) {
      return true;
    }
    if (exists(p, param._0)) {
      return true;
    }
    _param = param._3;
    continue ;
  };
}

function add_min_binding(k, v, param) {
  if (param) {
    return bal(add_min_binding(k, v, param._0), param._1, param._2, param._3);
  } else {
    return singleton(k, v);
  }
}

function add_max_binding(k, v, param) {
  if (param) {
    return bal(param._0, param._1, param._2, add_max_binding(k, v, param._3));
  } else {
    return singleton(k, v);
  }
}

function join(l, v, d, r) {
  if (!l) {
    return add_min_binding(v, d, r);
  }
  if (!r) {
    return add_max_binding(v, d, l);
  }
  var rh = r._4;
  var lh = l._4;
  if (lh > (rh + 2 | 0)) {
    return bal(l._0, l._1, l._2, join(l._3, v, d, r));
  } else if (rh > (lh + 2 | 0)) {
    return bal(join(l, v, d, r._0), r._1, r._2, r._3);
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
    return [
            /* Empty */0,
            undefined,
            /* Empty */0
          ];
  }
  var r = param._3;
  var d = param._2;
  var v = param._1;
  var l = param._0;
  var c = Caml_primitive.caml_int_compare(x, v);
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
}

function merge(f, s1, s2) {
  if (s1) {
    var v1 = s1._1;
    if (s1._4 >= height(s2)) {
      var match = split(v1, s2);
      return concat_or_join(merge(f, s1._0, match[0]), v1, Curry._3(f, v1, Caml_option.some(s1._2), match[1]), merge(f, s1._3, match[2]));
    }
    
  } else if (!s2) {
    return /* Empty */0;
  }
  if (s2) {
    var v2 = s2._1;
    var match$1 = split(v2, s1);
    return concat_or_join(merge(f, match$1[0], s2._0), v2, Curry._3(f, v2, match$1[1], Caml_option.some(s2._2)), merge(f, match$1[2], s2._3));
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "inline_map2_test.ml",
          270,
          10
        ],
        Error: new Error()
      };
}

function filter(p, param) {
  if (!param) {
    return /* Empty */0;
  }
  var d = param._2;
  var v = param._1;
  var l$prime = filter(p, param._0);
  var pvd = Curry._2(p, v, d);
  var r$prime = filter(p, param._3);
  if (pvd) {
    return join(l$prime, v, d, r$prime);
  } else {
    return concat(l$prime, r$prime);
  }
}

function partition(p, param) {
  if (!param) {
    return [
            /* Empty */0,
            /* Empty */0
          ];
  }
  var d = param._2;
  var v = param._1;
  var match = partition(p, param._0);
  var lf = match[1];
  var lt = match[0];
  var pvd = Curry._2(p, v, d);
  var match$1 = partition(p, param._3);
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
}

function cons_enum(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (!m) {
      return e;
    }
    _e = /* More */{
      _0: m._1,
      _1: m._2,
      _2: m._3,
      _3: e
    };
    _m = m._0;
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
    var c = Caml_primitive.caml_int_compare(e1._0, e2._0);
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
    if (e1._0 !== e2._0) {
      return false;
    }
    if (!Curry._2(cmp, e1._1, e2._1)) {
      return false;
    }
    _e2 = cons_enum(e2._2, e2._3);
    _e1 = cons_enum(e1._2, e1._3);
    continue ;
  };
}

function cardinal(param) {
  if (param) {
    return (cardinal(param._0) + 1 | 0) + cardinal(param._3) | 0;
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
    _param = param._0;
    _accu = {
      hd: [
        param._1,
        param._2
      ],
      tl: bindings_aux(accu, param._3)
    };
    continue ;
  };
}

function bindings(s) {
  return bindings_aux(/* [] */0, s);
}

var IntMap = {
  height,
  create,
  singleton,
  bal,
  empty: /* Empty */0,
  is_empty,
  add,
  find,
  mem,
  min_binding,
  max_binding,
  remove_min_binding,
  remove,
  iter,
  map,
  mapi,
  fold,
  for_all,
  exists,
  add_min_binding,
  add_max_binding,
  join,
  concat,
  concat_or_join,
  split,
  merge,
  filter,
  partition,
  cons_enum,
  compare,
  equal,
  cardinal,
  bindings_aux,
  bindings,
  choose: min_binding
};

var m = List.fold_left((function (acc, param) {
        return add(param[0], param[1], acc);
      }), /* Empty */0, {
      hd: [
        10,
        /* "a" */97
      ],
      tl: {
        hd: [
          3,
          /* "b" */98
        ],
        tl: {
          hd: [
            7,
            /* "c" */99
          ],
          tl: {
            hd: [
              20,
              /* "d" */100
            ],
            tl: /* [] */0
          }
        }
      }
    });

function height$1(param) {
  if (param) {
    return param._4;
  } else {
    return 0;
  }
}

function create$1(l, x, d, r) {
  var hl = height$1(l);
  var hr = height$1(r);
  return /* Node */{
          _0: l,
          _1: x,
          _2: d,
          _3: r,
          _4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function singleton$1(x, d) {
  return /* Node */{
          _0: /* Empty */0,
          _1: x,
          _2: d,
          _3: /* Empty */0,
          _4: 1
        };
}

function bal$1(l, x, d, r) {
  var hl = l ? l._4 : 0;
  var hr = r ? r._4 : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l._3;
      var ld = l._2;
      var lv = l._1;
      var ll = l._0;
      if (height$1(ll) >= height$1(lr)) {
        return create$1(ll, lv, ld, create$1(lr, x, d, r));
      }
      if (lr) {
        return create$1(create$1(ll, lv, ld, lr._0), lr._1, lr._2, create$1(lr._3, x, d, r));
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal",
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */{
            _0: l,
            _1: x,
            _2: d,
            _3: r,
            _4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (r) {
    var rr = r._3;
    var rd = r._2;
    var rv = r._1;
    var rl = r._0;
    if (height$1(rr) >= height$1(rl)) {
      return create$1(create$1(l, x, d, rl), rv, rd, rr);
    }
    if (rl) {
      return create$1(create$1(l, x, d, rl._0), rl._1, rl._2, create$1(rl._3, rv, rd, rr));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Map.bal",
        Error: new Error()
      };
}

function is_empty$1(param) {
  if (param) {
    return false;
  } else {
    return true;
  }
}

function add$1(x, data, param) {
  if (!param) {
    return /* Node */{
            _0: /* Empty */0,
            _1: x,
            _2: data,
            _3: /* Empty */0,
            _4: 1
          };
  }
  var r = param._3;
  var d = param._2;
  var v = param._1;
  var l = param._0;
  var c = Caml_primitive.caml_string_compare(x, v);
  if (c === 0) {
    return /* Node */{
            _0: l,
            _1: x,
            _2: data,
            _3: r,
            _4: param._4
          };
  } else if (c < 0) {
    return bal$1(add$1(x, data, l), v, d, r);
  } else {
    return bal$1(l, v, d, add$1(x, data, r));
  }
}

function find$1(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_primitive.caml_string_compare(x, param._1);
      if (c === 0) {
        return param._2;
      }
      _param = c < 0 ? param._0 : param._3;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function mem$1(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    var c = Caml_primitive.caml_string_compare(x, param._1);
    if (c === 0) {
      return true;
    }
    _param = c < 0 ? param._0 : param._3;
    continue ;
  };
}

function min_binding$1(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param._0;
      if (!l) {
        return [
                param._1,
                param._2
              ];
      }
      _param = l;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function max_binding$1(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var r = param._3;
      if (!r) {
        return [
                param._1,
                param._2
              ];
      }
      _param = r;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function remove_min_binding$1(param) {
  if (param) {
    var l = param._0;
    if (l) {
      return bal$1(remove_min_binding$1(l), param._1, param._2, param._3);
    } else {
      return param._3;
    }
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Map.remove_min_elt",
        Error: new Error()
      };
}

function remove$1(x, param) {
  if (!param) {
    return /* Empty */0;
  }
  var r = param._3;
  var d = param._2;
  var v = param._1;
  var l = param._0;
  var c = Caml_primitive.caml_string_compare(x, v);
  if (c === 0) {
    if (!l) {
      return r;
    }
    if (!r) {
      return l;
    }
    var match = min_binding$1(r);
    return bal$1(l, match[0], match[1], remove_min_binding$1(r));
  } else if (c < 0) {
    return bal$1(remove$1(x, l), v, d, r);
  } else {
    return bal$1(l, v, d, remove$1(x, r));
  }
}

function iter$1(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    iter$1(f, param._0);
    Curry._2(f, param._1, param._2);
    _param = param._3;
    continue ;
  };
}

function map$1(f, param) {
  if (!param) {
    return /* Empty */0;
  }
  var l$prime = map$1(f, param._0);
  var d$prime = Curry._1(f, param._2);
  var r$prime = map$1(f, param._3);
  return /* Node */{
          _0: l$prime,
          _1: param._1,
          _2: d$prime,
          _3: r$prime,
          _4: param._4
        };
}

function mapi$1(f, param) {
  if (!param) {
    return /* Empty */0;
  }
  var v = param._1;
  var l$prime = mapi$1(f, param._0);
  var d$prime = Curry._2(f, v, param._2);
  var r$prime = mapi$1(f, param._3);
  return /* Node */{
          _0: l$prime,
          _1: v,
          _2: d$prime,
          _3: r$prime,
          _4: param._4
        };
}

function fold$1(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (!m) {
      return accu;
    }
    _accu = Curry._3(f, m._1, m._2, fold$1(f, m._0, accu));
    _m = m._3;
    continue ;
  };
}

function for_all$1(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return true;
    }
    if (!Curry._2(p, param._1, param._2)) {
      return false;
    }
    if (!for_all$1(p, param._0)) {
      return false;
    }
    _param = param._3;
    continue ;
  };
}

function exists$1(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    if (Curry._2(p, param._1, param._2)) {
      return true;
    }
    if (exists$1(p, param._0)) {
      return true;
    }
    _param = param._3;
    continue ;
  };
}

function add_min_binding$1(k, v, param) {
  if (param) {
    return bal$1(add_min_binding$1(k, v, param._0), param._1, param._2, param._3);
  } else {
    return singleton$1(k, v);
  }
}

function add_max_binding$1(k, v, param) {
  if (param) {
    return bal$1(param._0, param._1, param._2, add_max_binding$1(k, v, param._3));
  } else {
    return singleton$1(k, v);
  }
}

function join$1(l, v, d, r) {
  if (!l) {
    return add_min_binding$1(v, d, r);
  }
  if (!r) {
    return add_max_binding$1(v, d, l);
  }
  var rh = r._4;
  var lh = l._4;
  if (lh > (rh + 2 | 0)) {
    return bal$1(l._0, l._1, l._2, join$1(l._3, v, d, r));
  } else if (rh > (lh + 2 | 0)) {
    return bal$1(join$1(l, v, d, r._0), r._1, r._2, r._3);
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
    return [
            /* Empty */0,
            undefined,
            /* Empty */0
          ];
  }
  var r = param._3;
  var d = param._2;
  var v = param._1;
  var l = param._0;
  var c = Caml_primitive.caml_string_compare(x, v);
  if (c === 0) {
    return [
            l,
            Caml_option.some(d),
            r
          ];
  }
  if (c < 0) {
    var match = split$1(x, l);
    return [
            match[0],
            match[1],
            join$1(match[2], v, d, r)
          ];
  }
  var match$1 = split$1(x, r);
  return [
          join$1(l, v, d, match$1[0]),
          match$1[1],
          match$1[2]
        ];
}

function merge$1(f, s1, s2) {
  if (s1) {
    var v1 = s1._1;
    if (s1._4 >= height$1(s2)) {
      var match = split$1(v1, s2);
      return concat_or_join$1(merge$1(f, s1._0, match[0]), v1, Curry._3(f, v1, Caml_option.some(s1._2), match[1]), merge$1(f, s1._3, match[2]));
    }
    
  } else if (!s2) {
    return /* Empty */0;
  }
  if (s2) {
    var v2 = s2._1;
    var match$1 = split$1(v2, s1);
    return concat_or_join$1(merge$1(f, match$1[0], s2._0), v2, Curry._3(f, v2, match$1[1], Caml_option.some(s2._2)), merge$1(f, match$1[2], s2._3));
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "inline_map2_test.ml",
          270,
          10
        ],
        Error: new Error()
      };
}

function filter$1(p, param) {
  if (!param) {
    return /* Empty */0;
  }
  var d = param._2;
  var v = param._1;
  var l$prime = filter$1(p, param._0);
  var pvd = Curry._2(p, v, d);
  var r$prime = filter$1(p, param._3);
  if (pvd) {
    return join$1(l$prime, v, d, r$prime);
  } else {
    return concat$1(l$prime, r$prime);
  }
}

function partition$1(p, param) {
  if (!param) {
    return [
            /* Empty */0,
            /* Empty */0
          ];
  }
  var d = param._2;
  var v = param._1;
  var match = partition$1(p, param._0);
  var lf = match[1];
  var lt = match[0];
  var pvd = Curry._2(p, v, d);
  var match$1 = partition$1(p, param._3);
  var rf = match$1[1];
  var rt = match$1[0];
  if (pvd) {
    return [
            join$1(lt, v, d, rt),
            concat$1(lf, rf)
          ];
  } else {
    return [
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
    _e = /* More */{
      _0: m._1,
      _1: m._2,
      _2: m._3,
      _3: e
    };
    _m = m._0;
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
    var c = Caml_primitive.caml_string_compare(e1._0, e2._0);
    if (c !== 0) {
      return c;
    }
    var c$1 = Curry._2(cmp, e1._1, e2._1);
    if (c$1 !== 0) {
      return c$1;
    }
    _e2 = cons_enum$1(e2._2, e2._3);
    _e1 = cons_enum$1(e1._2, e1._3);
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
    if (Caml_primitive.caml_string_compare(e1._0, e2._0) !== 0) {
      return false;
    }
    if (!Curry._2(cmp, e1._1, e2._1)) {
      return false;
    }
    _e2 = cons_enum$1(e2._2, e2._3);
    _e1 = cons_enum$1(e1._2, e1._3);
    continue ;
  };
}

function cardinal$1(param) {
  if (param) {
    return (cardinal$1(param._0) + 1 | 0) + cardinal$1(param._3) | 0;
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
    _param = param._0;
    _accu = {
      hd: [
        param._1,
        param._2
      ],
      tl: bindings_aux$1(accu, param._3)
    };
    continue ;
  };
}

function bindings$1(s) {
  return bindings_aux$1(/* [] */0, s);
}

var SMap = {
  height: height$1,
  create: create$1,
  singleton: singleton$1,
  bal: bal$1,
  empty: /* Empty */0,
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
      }), /* Empty */0, {
      hd: [
        "10",
        /* "a" */97
      ],
      tl: {
        hd: [
          "3",
          /* "b" */98
        ],
        tl: {
          hd: [
            "7",
            /* "c" */99
          ],
          tl: {
            hd: [
              "20",
              /* "d" */100
            ],
            tl: /* [] */0
          }
        }
      }
    });

Mt.from_pair_suites("Inline_map2_test", {
      hd: [
        "assertion1",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: find(10, m),
                    _1: /* "a" */97
                  };
          })
      ],
      tl: {
        hd: [
          "assertion2",
          (function (param) {
              return {
                      TAG: /* Eq */0,
                      _0: find$1("10", s),
                      _1: /* "a" */97
                    };
            })
        ],
        tl: /* [] */0
      }
    });

var empty = /* Empty */0;

exports.Make = Make;
exports.IntMap = IntMap;
exports.empty = empty;
exports.m = m;
exports.SMap = SMap;
exports.s = s;
/* m Not a pure module */
