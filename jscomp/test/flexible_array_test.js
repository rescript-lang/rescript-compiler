'use strict';

var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Format = require("../../lib/js/format.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_array = require("../../lib/js/caml_array.js");

function sub(_tr, _k) {
  while(true) {
    var k = _k;
    var tr = _tr;
    if (tr) {
      if (k === 1) {
        return tr._0;
      }
      if (k % 2 === 0) {
        _k = k / 2 | 0;
        _tr = tr._1;
        continue ;
      }
      _k = k / 2 | 0;
      _tr = tr._2;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function update(tr, k, w) {
  if (tr) {
    var r = tr._2;
    var l = tr._1;
    if (k === 1) {
      return /* Br */{
              _0: w,
              _1: l,
              _2: r
            };
    }
    var v = tr._0;
    if (k % 2 === 0) {
      return /* Br */{
              _0: v,
              _1: update(l, k / 2 | 0, w),
              _2: r
            };
    } else {
      return /* Br */{
              _0: v,
              _1: l,
              _2: update(r, k / 2 | 0, w)
            };
    }
  }
  if (k === 1) {
    return /* Br */{
            _0: w,
            _1: /* Lf */0,
            _2: /* Lf */0
          };
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

function $$delete(tr, n) {
  if (tr) {
    if (n === 1) {
      return /* Lf */0;
    }
    var r = tr._2;
    var l = tr._1;
    var v = tr._0;
    if (n % 2 === 0) {
      return /* Br */{
              _0: v,
              _1: $$delete(l, n / 2 | 0),
              _2: r
            };
    } else {
      return /* Br */{
              _0: v,
              _1: l,
              _2: $$delete(r, n / 2 | 0)
            };
    }
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

function loext(tr, w) {
  if (tr) {
    return /* Br */{
            _0: w,
            _1: loext(tr._2, tr._0),
            _2: tr._1
          };
  } else {
    return /* Br */{
            _0: w,
            _1: /* Lf */0,
            _2: /* Lf */0
          };
  }
}

function lorem(tr) {
  if (tr) {
    var l = tr._1;
    if (l) {
      return /* Br */{
              _0: l._0,
              _1: tr._2,
              _2: lorem(l)
            };
    }
    if (!tr._2) {
      return /* Lf */0;
    }
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "flexible_array_test.ml",
            66,
            9
          ],
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

var empty = [
  /* Lf */0,
  0
];

function length(param) {
  return param[1];
}

function get(param, i) {
  if (i >= 0 && i < param[1]) {
    return sub(param[0], i + 1 | 0);
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Array.get",
        Error: new Error()
      };
}

function set(param, i, v) {
  var k = param[1];
  if (i >= 0 && i < k) {
    return [
            update(param[0], i + 1 | 0, v),
            k
          ];
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Array.set",
        Error: new Error()
      };
}

function push_front(param, v) {
  return [
          loext(param[0], v),
          param[1] + 1 | 0
        ];
}

function pop_front(param) {
  var k = param[1];
  if (k > 0) {
    return [
            lorem(param[0]),
            k - 1 | 0
          ];
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Array.pop_front",
        Error: new Error()
      };
}

function push_back(param, v) {
  var k = param[1];
  return [
          update(param[0], k + 1 | 0, v),
          k + 1 | 0
        ];
}

function pop_back(param) {
  var k = param[1];
  if (k > 0) {
    return [
            $$delete(param[0], k),
            k - 1 | 0
          ];
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Array.pop_back",
        Error: new Error()
      };
}

function pp(fmt, s) {
  var v = "[ ";
  for(var i = 0 ,i_finish = length(s); i < i_finish; ++i){
    v = v + (", " + String(get(s, i)));
  }
  v = v + "]";
  return Curry._1(Format.fprintf(fmt, /* Format */{
                  _0: {
                    tag: /* String */2,
                    _0: /* No_padding */0,
                    _1: /* End_of_format */0
                  },
                  _1: "%s"
                }), v);
}

function filter_from(i, p, s) {
  var u = empty;
  for(var i$1 = i ,i_finish = length(s); i$1 < i_finish; ++i$1){
    var ele = get(s, i$1);
    if (Curry._1(p, ele)) {
      u = push_back(u, ele);
    }
    
  }
  return u;
}

function append(a, b) {
  var empty$1 = empty;
  for(var i = 0 ,i_finish = length(a); i < i_finish; ++i){
    empty$1 = push_back(empty$1, get(a, i));
  }
  for(var i$1 = 0 ,i_finish$1 = length(b); i$1 < i_finish$1; ++i$1){
    empty$1 = push_back(empty$1, get(b, i$1));
  }
  return empty$1;
}

function sort(s) {
  var size = length(s);
  if (size <= 1) {
    return s;
  }
  var head = get(s, 0);
  var larger = sort(filter_from(1, (function (x) {
              return Caml_obj.caml_greaterthan(x, head);
            }), s));
  var smaller = sort(filter_from(1, (function (x) {
              return Caml_obj.caml_lessequal(x, head);
            }), s));
  return append(smaller, push_front(larger, head));
}

function of_array(arr) {
  var v = empty;
  for(var i = 0 ,i_finish = arr.length; i < i_finish; ++i){
    v = push_back(v, Caml_array.caml_array_get(arr, i));
  }
  return v;
}

var equal = Caml_obj.caml_equal;

var Int_array = {
  empty: empty,
  get: get,
  set: set,
  push_front: push_front,
  pop_front: pop_front,
  push_back: push_back,
  pop_back: pop_back,
  pp: pp,
  append: append,
  sort: sort,
  of_array: of_array,
  equal: equal
};

function $eq$tilde(x, y) {
  return Caml_obj.caml_equal(x, of_array(y));
}

var u = of_array([
      1,
      2,
      2,
      5,
      3,
      6
    ]);

var x = sort(u);

if (!Caml_obj.caml_equal(x, of_array([
            1,
            2,
            2,
            3,
            5,
            6
          ]))) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "flexible_array_test.ml",
          166,
          4
        ],
        Error: new Error()
      };
}

var v = $$Array.init(500, (function (i) {
        return 500 - i | 0;
      }));

var y = $$Array.init(500, (function (i) {
        return i + 1 | 0;
      }));

var x$1 = sort(of_array(v));

Caml_obj.caml_equal(x$1, of_array(y));

exports.sub = sub;
exports.update = update;
exports.$$delete = $$delete;
exports.loext = loext;
exports.lorem = lorem;
exports.Int_array = Int_array;
exports.$eq$tilde = $eq$tilde;
/* u Not a pure module */
