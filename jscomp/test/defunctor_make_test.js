'use strict';

var Caml_primitive = require("../../lib/js/caml_primitive.js");

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
      }
      if (lr) {
        return create(create(ll, lv, ld, lr[0]), lr[1], lr[2], create(lr[3], x, d, r));
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal"
          };
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal"
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */[
            l,
            x,
            d,
            r,
            hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
  if (r) {
    var rr = r[3];
    var rd = r[2];
    var rv = r[1];
    var rl = r[0];
    if (height(rr) >= height(rl)) {
      return create(create(l, x, d, rl), rv, rd, rr);
    }
    if (rl) {
      return create(create(l, x, d, rl[0]), rl[1], rl[2], create(rl[3], rv, rd, rr));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal"
        };
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Map.bal"
      };
}

function add(x, data, compare, param) {
  if (!param) {
    return /* Node */[
            /* Empty */0,
            x,
            data,
            /* Empty */0,
            1
          ];
  }
  var r = param[3];
  var d = param[2];
  var v = param[1];
  var l = param[0];
  var c = compare(x, v);
  if (c === 0) {
    return /* Node */[
            l,
            x,
            data,
            r,
            param[4]
          ];
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
          data: /* Empty */0
        };
}

var compare = Caml_primitive.caml_int_compare;

var V0 = {
  compare: compare
};

var compare$1 = Caml_primitive.caml_int_compare;

var V1 = {
  compare: compare$1
};

var v0 = {
  compare: V0,
  data: /* Empty */0
};

var v1 = {
  compare: V1,
  data: /* Empty */0
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
