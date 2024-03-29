// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Caml = require("../../lib/js/caml.js");

function getcompare(x) {
  return x;
}

function Make(M) {
  return M;
}

var Comparable = {
  getcompare: getcompare,
  Make: Make
};

function height(x) {
  if (typeof x !== "object") {
    return 0;
  } else {
    return x._4;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
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
  var hl;
  hl = typeof l !== "object" ? 0 : l._4;
  var hr;
  hr = typeof r !== "object" ? 0 : r._4;
  if (hl > (hr + 2 | 0)) {
    if (typeof l !== "object") {
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal",
            Error: new Error()
          };
    }
    var lr = l._3;
    var ld = l._2;
    var lv = l._1;
    var ll = l._0;
    if (height(ll) >= height(lr)) {
      return create(ll, lv, ld, create(lr, x, d, r));
    }
    if (typeof lr === "object") {
      return create(create(ll, lv, ld, lr._0), lr._1, lr._2, create(lr._3, x, d, r));
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
            _0: l,
            _1: x,
            _2: d,
            _3: r,
            _4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (typeof r !== "object") {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  }
  var rr = r._3;
  var rd = r._2;
  var rv = r._1;
  var rl = r._0;
  if (height(rr) >= height(rl)) {
    return create(create(l, x, d, rl), rv, rd, rr);
  }
  if (typeof rl === "object") {
    return create(create(l, x, d, rl._0), rl._1, rl._2, create(rl._3, rv, rd, rr));
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Map.bal",
        Error: new Error()
      };
}

function add(x, data, compare, x_) {
  if (typeof x_ !== "object") {
    return {
            TAG: "Node",
            _0: "Empty",
            _1: x,
            _2: data,
            _3: "Empty",
            _4: 1
          };
  }
  var r = x_._3;
  var d = x_._2;
  var v = x_._1;
  var l = x_._0;
  var c = compare(x, v);
  if (c === 0) {
    return {
            TAG: "Node",
            _0: l,
            _1: x,
            _2: data,
            _3: r,
            _4: x_._4
          };
  } else if (c < 0) {
    return bal(add(x, data, compare, l), v, d, r);
  } else {
    return bal(l, v, d, add(x, data, compare, r));
  }
}

function add$1(x, data, v) {
  var X = v.compare;
  return {
          compare: v.compare,
          data: add(x, data, X.compare, v.data)
        };
}

function empty(v) {
  return {
          compare: v,
          data: "Empty"
        };
}

var compare = Caml.int_compare;

var V0 = {
  compare: compare
};

var compare$1 = Caml.int_compare;

var V1 = {
  compare: compare$1
};

var v0 = {
  compare: V0,
  data: "Empty"
};

var v1 = {
  compare: V1,
  data: "Empty"
};

var v3 = add$1(3, "a", v0);

console.log(v3);

exports.Comparable = Comparable;
exports.height = height;
exports.create = create;
exports.bal = bal;
exports.add = add$1;
exports.empty = empty;
exports.V0 = V0;
exports.V1 = V1;
exports.v0 = v0;
exports.v1 = v1;
exports.v3 = v3;
/* v3 Not a pure module */
