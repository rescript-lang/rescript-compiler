'use strict';

var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Format = require("../../lib/js/format.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function sub(_tr, _k) {
  while(true) {
    var k = _k;
    var tr = _tr;
    if (tr !== "Lf") {
      if (k === 1) {
        return tr.Arg0;
      } else {
        _k = k / 2 | 0;
        if (k % 2 === 0) {
          _tr = tr.Arg1;
          continue ;
        } else {
          _tr = tr.Arg2;
          continue ;
        }
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function update(tr, k, w) {
  if (tr !== "Lf") {
    var r = tr.Arg2;
    var l = tr.Arg1;
    if (k === 1) {
      return /* constructor */{
              tag: "Br",
              Arg0: w,
              Arg1: l,
              Arg2: r
            };
    } else {
      var v = tr.Arg0;
      if (k % 2 === 0) {
        return /* constructor */{
                tag: "Br",
                Arg0: v,
                Arg1: update(l, k / 2 | 0, w),
                Arg2: r
              };
      } else {
        return /* constructor */{
                tag: "Br",
                Arg0: v,
                Arg1: l,
                Arg2: update(r, k / 2 | 0, w)
              };
      }
    }
  } else if (k === 1) {
    return /* constructor */{
            tag: "Br",
            Arg0: w,
            Arg1: "Lf",
            Arg2: "Lf"
          };
  } else {
    throw Caml_builtin_exceptions.not_found;
  }
}

function $$delete(tr, n) {
  if (tr !== "Lf") {
    if (n === 1) {
      return "Lf";
    } else {
      var r = tr.Arg2;
      var l = tr.Arg1;
      var v = tr.Arg0;
      if (n % 2 === 0) {
        return /* constructor */{
                tag: "Br",
                Arg0: v,
                Arg1: $$delete(l, n / 2 | 0),
                Arg2: r
              };
      } else {
        return /* constructor */{
                tag: "Br",
                Arg0: v,
                Arg1: l,
                Arg2: $$delete(r, n / 2 | 0)
              };
      }
    }
  } else {
    throw Caml_builtin_exceptions.not_found;
  }
}

function loext(tr, w) {
  if (tr !== "Lf") {
    return /* constructor */{
            tag: "Br",
            Arg0: w,
            Arg1: loext(tr.Arg2, tr.Arg0),
            Arg2: tr.Arg1
          };
  } else {
    return /* constructor */{
            tag: "Br",
            Arg0: w,
            Arg1: "Lf",
            Arg2: "Lf"
          };
  }
}

function lorem(tr) {
  if (tr !== "Lf") {
    var l = tr.Arg1;
    if (l !== "Lf") {
      return /* constructor */{
              tag: "Br",
              Arg0: l.Arg0,
              Arg1: tr.Arg2,
              Arg2: lorem(l)
            };
    } else if (tr.Arg2 !== "Lf") {
      throw [
            Caml_builtin_exceptions.assert_failure,
            /* tuple */[
              "flexible_array_test.ml",
              66,
              9
            ]
          ];
    } else {
      return "Lf";
    }
  } else {
    throw Caml_builtin_exceptions.not_found;
  }
}

var empty = /* tuple */[
  "Lf",
  0
];

function length(param) {
  return param[1];
}

function get(param, i) {
  if (i >= 0 && i < param[1]) {
    return sub(param[0], i + 1 | 0);
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Array.get"
        ];
  }
}

function set(param, i, v) {
  var k = param[1];
  if (i >= 0 && i < k) {
    return /* tuple */[
            update(param[0], i + 1 | 0, v),
            k
          ];
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Array.set"
        ];
  }
}

function push_front(param, v) {
  return /* tuple */[
          loext(param[0], v),
          param[1] + 1 | 0
        ];
}

function pop_front(param) {
  var k = param[1];
  if (k > 0) {
    return /* tuple */[
            lorem(param[0]),
            k - 1 | 0
          ];
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Array.pop_front"
        ];
  }
}

function push_back(param, v) {
  var k = param[1];
  return /* tuple */[
          update(param[0], k + 1 | 0, v),
          k + 1 | 0
        ];
}

function pop_back(param) {
  var k = param[1];
  if (k > 0) {
    return /* tuple */[
            $$delete(param[0], k),
            k - 1 | 0
          ];
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Array.pop_back"
        ];
  }
}

function pp(fmt, s) {
  var v = "[ ";
  for(var i = 0 ,i_finish = length(s) - 1 | 0; i <= i_finish; ++i){
    v = v + (", " + String(get(s, i)));
  }
  v = v + "]";
  return Curry._1(Format.fprintf(fmt, /* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String",
                    Arg0: "No_padding",
                    Arg1: "End_of_format"
                  },
                  Arg1: "%s"
                }), v);
}

function filter_from(i, p, s) {
  var u = empty;
  for(var i$1 = i ,i_finish = length(s) - 1 | 0; i$1 <= i_finish; ++i$1){
    var ele = get(s, i$1);
    if (Curry._1(p, ele)) {
      u = push_back(u, ele);
    }
    
  }
  return u;
}

function append(a, b) {
  var empty$1 = empty;
  for(var i = 0 ,i_finish = length(a) - 1 | 0; i <= i_finish; ++i){
    empty$1 = push_back(empty$1, get(a, i));
  }
  for(var i$1 = 0 ,i_finish$1 = length(b) - 1 | 0; i$1 <= i_finish$1; ++i$1){
    empty$1 = push_back(empty$1, get(b, i$1));
  }
  return empty$1;
}

function sort(s) {
  var size = length(s);
  if (size <= 1) {
    return s;
  } else {
    var head = get(s, 0);
    var larger = sort(filter_from(1, (function (x) {
                return Caml_obj.caml_greaterthan(x, head);
              }), s));
    var smaller = sort(filter_from(1, (function (x) {
                return Caml_obj.caml_lessequal(x, head);
              }), s));
    return append(smaller, push_front(larger, head));
  }
}

function of_array(arr) {
  var v = empty;
  for(var i = 0 ,i_finish = arr.length - 1 | 0; i <= i_finish; ++i){
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

var u = of_array(/* array */[
      1,
      2,
      2,
      5,
      3,
      6
    ]);

if (!$eq$tilde(sort(u), /* array */[
        1,
        2,
        2,
        3,
        5,
        6
      ])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "flexible_array_test.ml",
          166,
          4
        ]
      ];
}

var v = $$Array.init(500, (function (i) {
        return 500 - i | 0;
      }));

$eq$tilde(sort(of_array(v)), $$Array.init(500, (function (i) {
            return i + 1 | 0;
          })));

exports.sub = sub;
exports.update = update;
exports.$$delete = $$delete;
exports.loext = loext;
exports.lorem = lorem;
exports.Int_array = Int_array;
exports.$eq$tilde = $eq$tilde;
/* u Not a pure module */
