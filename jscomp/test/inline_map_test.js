'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");

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

var m = List.fold_left((function (acc, param) {
        return add(param[0], param[1], acc);
      }), /* Empty */0, /* :: */{
      _0: /* tuple */[
        10,
        /* "a" */97
      ],
      _1: /* :: */{
        _0: /* tuple */[
          3,
          /* "b" */98
        ],
        _1: /* :: */{
          _0: /* tuple */[
            7,
            /* "c" */99
          ],
          _1: /* :: */{
            _0: /* tuple */[
              20,
              /* "d" */100
            ],
            _1: /* [] */0
          }
        }
      }
    });

Mt.from_pair_suites("Inline_map_test", /* :: */{
      _0: /* tuple */[
        "find",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: find(10, m),
                    _1: /* "a" */97
                  };
          })
      ],
      _1: /* [] */0
    });

/* m Not a pure module */
