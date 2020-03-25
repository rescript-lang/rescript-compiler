'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

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

function bal(l, x, d, r) {
  var hl = l ? l[/* h */4] : 0;
  var hr = r ? r[/* h */4] : 0;
  if (hl > (hr + 2 | 0)) {
    if (!l) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
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
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Map.bal"
        ];
  } else {
    if (hr <= (hl + 2 | 0)) {
      return /* Node */[
              /* l */l,
              /* v */x,
              /* d */d,
              /* r */r,
              /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
            ];
    }
    if (!r) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
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
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Map.bal"
        ];
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
  var c = x - v | 0;
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
  } else {
    var rr = add(x, data, r);
    if (r === rr) {
      return m;
    } else {
      return bal(l, v, d, rr);
    }
  }
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      throw Caml_builtin_exceptions.not_found;
    }
    var c = x - param[/* v */1] | 0;
    if (c === 0) {
      return param[/* d */2];
    }
    _param = c < 0 ? param[/* l */0] : param[/* r */3];
    continue ;
  };
}

function test(param) {
  var m = /* Empty */0;
  for(var i = 0; i <= 1000000; ++i){
    m = add(i, i, m);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    find(i$1, m);
  }
  
}

test(undefined);

/*  Not a pure module */
