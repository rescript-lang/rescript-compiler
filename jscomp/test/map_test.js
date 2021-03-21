'use strict';

var Mt = require("./mt.js");
var Caml = require("../../lib/js/caml.js");
var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");

function height(param) {
  if (param) {
    return param.h;
  } else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* Node */{
          l: l,
          v: x,
          d: d,
          r: r,
          h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal(l, x, d, r) {
  var hl = l ? l.h : 0;
  var hr = r ? r.h : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l.r;
      var ld = l.d;
      var lv = l.v;
      var ll = l.l;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      }
      if (lr) {
        return create(create(ll, lv, ld, lr.l), lr.v, lr.d, create(lr.r, x, d, r));
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
            l: l,
            v: x,
            d: d,
            r: r,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (r) {
    var rr = r.r;
    var rd = r.d;
    var rv = r.v;
    var rl = r.l;
    if (height(rr) >= height(rl)) {
      return create(create(l, x, d, rl), rv, rd, rr);
    }
    if (rl) {
      return create(create(l, x, d, rl.l), rl.v, rl.d, create(rl.r, rv, rd, rr));
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

function add(x, data, m) {
  if (!m) {
    return /* Node */{
            l: /* Empty */0,
            v: x,
            d: data,
            r: /* Empty */0,
            h: 1
          };
  }
  var r = m.r;
  var d = m.d;
  var v = m.v;
  var l = m.l;
  var c = Caml.caml_int_compare(x, v);
  if (c === 0) {
    if (d === data) {
      return m;
    } else {
      return /* Node */{
              l: l,
              v: x,
              d: data,
              r: r,
              h: m.h
            };
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

function cons_enum(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (!m) {
      return e;
    }
    _e = /* More */{
      _0: m.v,
      _1: m.d,
      _2: m.r,
      _3: e
    };
    _m = m.l;
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
    var c = Caml.caml_int_compare(e1._0, e2._0);
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
    return (cardinal(param.l) + 1 | 0) + cardinal(param.r) | 0;
  } else {
    return 0;
  }
}

function height$1(param) {
  if (param) {
    return param.h;
  } else {
    return 0;
  }
}

function create$1(l, x, d, r) {
  var hl = height$1(l);
  var hr = height$1(r);
  return /* Node */{
          l: l,
          v: x,
          d: d,
          r: r,
          h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal$1(l, x, d, r) {
  var hl = l ? l.h : 0;
  var hr = r ? r.h : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l.r;
      var ld = l.d;
      var lv = l.v;
      var ll = l.l;
      if (height$1(ll) >= height$1(lr)) {
        return create$1(ll, lv, ld, create$1(lr, x, d, r));
      }
      if (lr) {
        return create$1(create$1(ll, lv, ld, lr.l), lr.v, lr.d, create$1(lr.r, x, d, r));
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
            l: l,
            v: x,
            d: d,
            r: r,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (r) {
    var rr = r.r;
    var rd = r.d;
    var rv = r.v;
    var rl = r.l;
    if (height$1(rr) >= height$1(rl)) {
      return create$1(create$1(l, x, d, rl), rv, rd, rr);
    }
    if (rl) {
      return create$1(create$1(l, x, d, rl.l), rl.v, rl.d, create$1(rl.r, rv, rd, rr));
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

function add$1(x, data, m) {
  if (!m) {
    return /* Node */{
            l: /* Empty */0,
            v: x,
            d: data,
            r: /* Empty */0,
            h: 1
          };
  }
  var r = m.r;
  var d = m.d;
  var v = m.v;
  var l = m.l;
  var c = Caml.caml_string_compare(x, v);
  if (c === 0) {
    if (d === data) {
      return m;
    } else {
      return /* Node */{
              l: l,
              v: x,
              d: data,
              r: r,
              h: m.h
            };
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

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml.caml_string_compare(x, param.v);
      if (c === 0) {
        return param.d;
      }
      _param = c < 0 ? param.l : param.r;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function of_list(kvs) {
  return List.fold_left((function (acc, param) {
                return add(param[0], param[1], acc);
              }), /* Empty */0, kvs);
}

var int_map_suites_0 = [
  "add",
  (function (param) {
      var v = of_list({
            hd: [
              1,
              /* '1' */49
            ],
            tl: {
              hd: [
                2,
                /* '3' */51
              ],
              tl: {
                hd: [
                  3,
                  /* '4' */52
                ],
                tl: /* [] */0
              }
            }
          });
      return {
              TAG: /* Eq */0,
              _0: cardinal(v),
              _1: 3
            };
    })
];

var int_map_suites_1 = {
  hd: [
    "equal",
    (function (param) {
        var v = of_list({
              hd: [
                1,
                /* '1' */49
              ],
              tl: {
                hd: [
                  2,
                  /* '3' */51
                ],
                tl: {
                  hd: [
                    3,
                    /* '4' */52
                  ],
                  tl: /* [] */0
                }
              }
            });
        var u = of_list({
              hd: [
                2,
                /* '3' */51
              ],
              tl: {
                hd: [
                  3,
                  /* '4' */52
                ],
                tl: {
                  hd: [
                    1,
                    /* '1' */49
                  ],
                  tl: /* [] */0
                }
              }
            });
        return {
                TAG: /* Eq */0,
                _0: compare(Caml.caml_int_compare, u, v),
                _1: 0
              };
      })
  ],
  tl: {
    hd: [
      "equal2",
      (function (param) {
          var v = of_list({
                hd: [
                  1,
                  /* '1' */49
                ],
                tl: {
                  hd: [
                    2,
                    /* '3' */51
                  ],
                  tl: {
                    hd: [
                      3,
                      /* '4' */52
                    ],
                    tl: /* [] */0
                  }
                }
              });
          var u = of_list({
                hd: [
                  2,
                  /* '3' */51
                ],
                tl: {
                  hd: [
                    3,
                    /* '4' */52
                  ],
                  tl: {
                    hd: [
                      1,
                      /* '1' */49
                    ],
                    tl: /* [] */0
                  }
                }
              });
          return {
                  TAG: /* Eq */0,
                  _0: true,
                  _1: equal((function (x, y) {
                          return x === y;
                        }), u, v)
                };
        })
    ],
    tl: {
      hd: [
        "iteration",
        (function (param) {
            var m = /* Empty */0;
            for(var i = 0; i <= 10000; ++i){
              m = add$1(String(i), String(i), m);
            }
            var v = -1;
            for(var i$1 = 0; i$1 <= 10000; ++i$1){
              if (find(String(i$1), m) !== String(i$1)) {
                v = i$1;
              }
              
            }
            return {
                    TAG: /* Eq */0,
                    _0: v,
                    _1: -1
                  };
          })
      ],
      tl: /* [] */0
    }
  }
};

var int_map_suites = {
  hd: int_map_suites_0,
  tl: int_map_suites_1
};

Mt.from_pair_suites("Map_test", int_map_suites);

/*  Not a pure module */
