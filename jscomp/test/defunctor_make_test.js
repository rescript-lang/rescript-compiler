'use strict';

var Caml_obj                = require("../../lib/js/caml_obj.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function getComapre(x) {
  return x;
}

function Make(M) {
  var compare = M[/* compare */0];
  return /* module */[/* compare */compare];
}

var Comparable = /* module */[
  /* getComapre */getComapre,
  /* Make */Make
];

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

function add(x, data, compare, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = compare(x, v);
    if (c) {
      if (c < 0) {
        return bal(add(x, data, compare, l), v, d, r);
      } else {
        return bal(l, v, d, add(x, data, compare, r));
      }
    } else {
      return /* Node */[
              l,
              x,
              data,
              r,
              param[4]
            ];
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

function add$1(x, data, v) {
  var X = v[/* compare */0];
  return /* record */[
          /* compare */v[/* compare */0],
          /* data */add(x, data, X[/* compare */0], v[/* data */1])
        ];
}

function empty(v) {
  return /* record */[
          /* compare */v,
          /* data : Empty */0
        ];
}

var compare = Caml_obj.caml_int_compare;

var V0 = /* module */[/* compare */compare];

var compare$1 = Caml_obj.caml_int_compare;

var V1 = /* module */[/* compare */compare$1];

var v0 = /* record */[
  /* compare */V0,
  /* data : Empty */0
];

var v1 = /* record */[
  /* compare */V1,
  /* data : Empty */0
];

var v3 = add$1(3, "a", v0);

console.log(v3);

exports.Comparable = Comparable;
exports.height     = height;
exports.create     = create;
exports.bal        = bal;
exports.add        = add$1;
exports.empty      = empty;
exports.V0         = V0;
exports.V1         = V1;
exports.v0         = v0;
exports.v1         = v1;
exports.v3         = v3;
/* v3 Not a pure module */
