'use strict';

var Caml = require("../../lib/js/caml.js");
var Curry = require("../../lib/js/curry.js");

function height(param) {
  if (typeof param !== "object") {
    return 0;
  } else {
    return param.h;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
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
  var hl;
  hl = typeof l !== "object" ? 0 : l.h;
  var hr;
  hr = typeof r !== "object" ? 0 : r.h;
  if (hl > (hr + 2 | 0)) {
    if (typeof l !== "object") {
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal",
            Error: new Error()
          };
    }
    var lr = l.r;
    var ld = l.d;
    var lv = l.v;
    var ll = l.l;
    if (height(ll) >= height(lr)) {
      return create(ll, lv, ld, create(lr, x, d, r));
    }
    if (typeof lr === "object") {
      return create(create(ll, lv, ld, lr.l), lr.v, lr.d, create(lr.r, x, d, r));
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
            l: l,
            v: x,
            d: d,
            r: r,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (typeof r !== "object") {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  }
  var rr = r.r;
  var rd = r.d;
  var rv = r.v;
  var rl = r.l;
  if (height(rr) >= height(rl)) {
    return create(create(l, x, d, rl), rv, rd, rr);
  }
  if (typeof rl === "object") {
    return create(create(l, x, d, rl.l), rl.v, rl.d, create(rl.r, rv, rd, rr));
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Map.bal",
        Error: new Error()
      };
}

function add(x, data, m) {
  if (typeof m !== "object") {
    return {
            TAG: "Node",
            l: "Empty",
            v: x,
            d: data,
            r: "Empty",
            h: 1
          };
  }
  var r = m.r;
  var d = m.d;
  var v = m.v;
  var l = m.l;
  var c = Caml.string_compare(x, v);
  if (c === 0) {
    if (d === data) {
      return m;
    } else {
      return {
              TAG: "Node",
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

function find(x, _param) {
  while(true) {
    var param = _param;
    if (typeof param !== "object") {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    var c = Caml.string_compare(x, param.v);
    if (c === 0) {
      return param.d;
    }
    _param = c < 0 ? param.l : param.r;
    continue ;
  };
}

function timing(label, f) {
  console.time(label);
  Curry._1(f, undefined);
  console.timeEnd(label);
}

function assertion_test(param) {
  var m = {
    contents: "Empty"
  };
  timing("building", (function (param) {
          for(var i = 0; i <= 1000000; ++i){
            m.contents = add(String(i), String(i), m.contents);
          }
        }));
  timing("querying", (function (param) {
          for(var i = 0; i <= 1000000; ++i){
            find(String(i), m.contents);
          }
        }));
}

exports.assertion_test = assertion_test;
/* No side effect */
