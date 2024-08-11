// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Caml = require("../../lib/js/caml.js");
let List = require("../../lib/js/list.js");

function height(x) {
  if (typeof x !== "object") {
    return 0;
  } else {
    return x._4;
  }
}

function create(l, x, d, r) {
  let hl = height(l);
  let hr = height(r);
  return {
    TAG: "Node",
    _0: l,
    _1: x,
    _2: d,
    _3: r,
    _4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
  };
}

function bal(l, x, d, r) {
  let hl;
  hl = typeof l !== "object" ? 0 : l._4;
  let hr;
  hr = typeof r !== "object" ? 0 : r._4;
  if (hl > (hr + 2 | 0)) {
    if (typeof l !== "object") {
      throw new Error("Assert_failure", {
        cause: {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "inline_map_demo.res",
            41,
            15
          ]
        }
      });
    }
    let lr = l._3;
    let ld = l._2;
    let lv = l._1;
    let ll = l._0;
    if (height(ll) >= height(lr)) {
      return create(ll, lv, ld, create(lr, x, d, r));
    }
    if (typeof lr === "object") {
      return create(create(ll, lv, ld, lr._0), lr._1, lr._2, create(lr._3, x, d, r));
    }
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "inline_map_demo.res",
          47,
          19
        ]
      }
    });
  }
  if (hr <= (hl + 2 | 0)) {
    return {
      TAG: "Node",
      _0: l,
      _1: x,
      _2: d,
      _3: r,
      _4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
    };
  }
  if (typeof r !== "object") {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "inline_map_demo.res",
          55,
          15
        ]
      }
    });
  }
  let rr = r._3;
  let rd = r._2;
  let rv = r._1;
  let rl = r._0;
  if (height(rr) >= height(rl)) {
    return create(create(l, x, d, rl), rv, rd, rr);
  }
  if (typeof rl === "object") {
    return create(create(l, x, d, rl._0), rl._1, rl._2, create(rl._3, rv, rd, rr));
  }
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "inline_map_demo.res",
        61,
        19
      ]
    }
  });
}

function add(x, data, tree) {
  if (typeof tree !== "object") {
    return {
      TAG: "Node",
      _0: "Empty",
      _1: x,
      _2: data,
      _3: "Empty",
      _4: 1
    };
  }
  let r = tree._3;
  let d = tree._2;
  let v = tree._1;
  let l = tree._0;
  let c = Caml.int_compare(x, v);
  if (c === 0) {
    return {
      TAG: "Node",
      _0: l,
      _1: x,
      _2: data,
      _3: r,
      _4: tree._4
    };
  } else if (c < 0) {
    return bal(add(x, data, l), v, d, r);
  } else {
    return bal(l, v, d, add(x, data, r));
  }
}

let m = List.fold_left(((acc, param) => {
  return add(param[0], param[1], acc);
}), "Empty", {
  hd: [
    10,
    /* 'a' */97
  ],
  tl: {
    hd: [
      3,
      /* 'b' */98
    ],
    tl: {
      hd: [
        7,
        /* 'c' */99
      ],
      tl: {
        hd: [
          20,
          /* 'd' */100
        ],
        tl: /* [] */0
      }
    }
  }
});

function find(px, _x) {
  while (true) {
    let x = _x;
    if (typeof x !== "object") {
      throw new Error("Not_found", {
        cause: {
          RE_EXN_ID: "Not_found"
        }
      });
    }
    let c = Caml.int_compare(px, x._1);
    if (c === 0) {
      return x._2;
    }
    _x = c < 0 ? x._0 : x._3;
    continue;
  };
}

Mt.from_pair_suites("Inline_map_demo", {
  hd: [
    "find",
    (() => {
      return {
        TAG: "Eq",
        _0: find(10, m),
        _1: /* 'a' */97
      };
    })
  ],
  tl: /* [] */0
});

/* m Not a pure module */
