// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Caml = require("../../lib/js/caml.js");
let List = require("../../lib/js/list.js");

function height(param) {
  if (typeof param !== "object") {
    return 0;
  } else {
    return param.h;
  }
}

function create(l, x, d, r) {
  let hl = height(l);
  let hr = height(r);
  return {
    TAG: "Node",
    l: l,
    v: x,
    d: d,
    r: r,
    h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
  };
}

function bal(l, x, d, r) {
  let hl;
  hl = typeof l !== "object" ? 0 : l.h;
  let hr;
  hr = typeof r !== "object" ? 0 : r.h;
  if (hl > (hr + 2 | 0)) {
    if (typeof l !== "object") {
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "Map.bal"
            }
          });
    }
    let lr = l.r;
    let ld = l.d;
    let lv = l.v;
    let ll = l.l;
    if (height(ll) >= height(lr)) {
      return create(ll, lv, ld, create(lr, x, d, r));
    }
    if (typeof lr === "object") {
      return create(create(ll, lv, ld, lr.l), lr.v, lr.d, create(lr.r, x, d, r));
    }
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal"
          }
        });
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
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal"
          }
        });
  }
  let rr = r.r;
  let rd = r.d;
  let rv = r.v;
  let rl = r.l;
  if (height(rr) >= height(rl)) {
    return create(create(l, x, d, rl), rv, rd, rr);
  }
  if (typeof rl === "object") {
    return create(create(l, x, d, rl.l), rl.v, rl.d, create(rl.r, rv, rd, rr));
  }
  throw new Error("Invalid_argument", {
        cause: {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal"
        }
      });
}

function add(x, data, param) {
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
  let r = param.r;
  let d = param.d;
  let v = param.v;
  let l = param.l;
  let c = Caml.int_compare(x, v);
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
    let ll = add(x, data, l);
    if (l === ll) {
      return param;
    } else {
      return bal(ll, v, d, r);
    }
  }
  let rr = add(x, data, r);
  if (r === rr) {
    return param;
  } else {
    return bal(l, v, d, rr);
  }
}

function cons_enum(_m, _e) {
  while(true) {
    let e = _e;
    let m = _m;
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
    continue;
  };
}

function compare(cmp, m1, m2) {
  let _e1 = cons_enum(m1, "End");
  let _e2 = cons_enum(m2, "End");
  while(true) {
    let e2 = _e2;
    let e1 = _e1;
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
    let c = Caml.int_compare(e1._0, e2._0);
    if (c !== 0) {
      return c;
    }
    let c$1 = cmp(e1._1, e2._1);
    if (c$1 !== 0) {
      return c$1;
    }
    _e2 = cons_enum(e2._2, e2._3);
    _e1 = cons_enum(e1._2, e1._3);
    continue;
  };
}

function equal(cmp, m1, m2) {
  let _e1 = cons_enum(m1, "End");
  let _e2 = cons_enum(m2, "End");
  while(true) {
    let e2 = _e2;
    let e1 = _e1;
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
    if (e1._0 !== e2._0) {
      return false;
    }
    if (!cmp(e1._1, e2._1)) {
      return false;
    }
    _e2 = cons_enum(e2._2, e2._3);
    _e1 = cons_enum(e1._2, e1._3);
    continue;
  };
}

function cardinal(param) {
  if (typeof param !== "object") {
    return 0;
  } else {
    return (cardinal(param.l) + 1 | 0) + cardinal(param.r) | 0;
  }
}

function height$1(param) {
  if (typeof param !== "object") {
    return 0;
  } else {
    return param.h;
  }
}

function create$1(l, x, d, r) {
  let hl = height$1(l);
  let hr = height$1(r);
  return {
    TAG: "Node",
    l: l,
    v: x,
    d: d,
    r: r,
    h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
  };
}

function bal$1(l, x, d, r) {
  let hl;
  hl = typeof l !== "object" ? 0 : l.h;
  let hr;
  hr = typeof r !== "object" ? 0 : r.h;
  if (hl > (hr + 2 | 0)) {
    if (typeof l !== "object") {
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "Map.bal"
            }
          });
    }
    let lr = l.r;
    let ld = l.d;
    let lv = l.v;
    let ll = l.l;
    if (height$1(ll) >= height$1(lr)) {
      return create$1(ll, lv, ld, create$1(lr, x, d, r));
    }
    if (typeof lr === "object") {
      return create$1(create$1(ll, lv, ld, lr.l), lr.v, lr.d, create$1(lr.r, x, d, r));
    }
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal"
          }
        });
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
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal"
          }
        });
  }
  let rr = r.r;
  let rd = r.d;
  let rv = r.v;
  let rl = r.l;
  if (height$1(rr) >= height$1(rl)) {
    return create$1(create$1(l, x, d, rl), rv, rd, rr);
  }
  if (typeof rl === "object") {
    return create$1(create$1(l, x, d, rl.l), rl.v, rl.d, create$1(rl.r, rv, rd, rr));
  }
  throw new Error("Invalid_argument", {
        cause: {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal"
        }
      });
}

function add$1(x, data, param) {
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
  let r = param.r;
  let d = param.d;
  let v = param.v;
  let l = param.l;
  let c = Caml.string_compare(x, v);
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
    let ll = add$1(x, data, l);
    if (l === ll) {
      return param;
    } else {
      return bal$1(ll, v, d, r);
    }
  }
  let rr = add$1(x, data, r);
  if (r === rr) {
    return param;
  } else {
    return bal$1(l, v, d, rr);
  }
}

function find(x, _param) {
  while(true) {
    let param = _param;
    if (typeof param !== "object") {
      throw new Error("Not_found", {
            cause: {
              RE_EXN_ID: "Not_found"
            }
          });
    }
    let c = Caml.string_compare(x, param.v);
    if (c === 0) {
      return param.d;
    }
    _param = c < 0 ? param.l : param.r;
    continue;
  };
}

function of_list(kvs) {
  return List.fold_left((function (acc, param) {
    return add(param[0], param[1], acc);
  }), "Empty", kvs);
}

let int_map_suites_0 = [
  "add",
  (function (param) {
    let v = of_list({
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
      TAG: "Eq",
      _0: cardinal(v),
      _1: 3
    };
  })
];

let int_map_suites_1 = {
  hd: [
    "equal",
    (function (param) {
      let v = of_list({
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
      let u = of_list({
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
        TAG: "Eq",
        _0: compare(Caml.int_compare, u, v),
        _1: 0
      };
    })
  ],
  tl: {
    hd: [
      "equal2",
      (function (param) {
        let v = of_list({
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
        let u = of_list({
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
          TAG: "Eq",
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
          let m = "Empty";
          for(let i = 0; i <= 10000; ++i){
            m = add$1(String(i), String(i), m);
          }
          let v = -1;
          for(let i$1 = 0; i$1 <= 10000; ++i$1){
            if (find(String(i$1), m) !== String(i$1)) {
              v = i$1;
            }
            
          }
          return {
            TAG: "Eq",
            _0: v,
            _1: -1
          };
        })
      ],
      tl: /* [] */0
    }
  }
};

let int_map_suites = {
  hd: int_map_suites_0,
  tl: int_map_suites_1
};

Mt.from_pair_suites("Map_test", int_map_suites);

/*  Not a pure module */
