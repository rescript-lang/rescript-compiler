'use strict';

var Caml_obj = require("../../lib/js/caml_obj");
var List     = require("../../lib/js/list");

function _zs() {
  return /* tuple */[
          List.hd(ys),
          List.hd(xs[0])
        ];
}

var ys = [];

var xs = [];

Caml_obj.caml_update_dummy(ys, /* :: */[
      1,
      ys
    ]);

Caml_obj.caml_update_dummy(xs, /* tuple */[
      /* :: */[
        2,
        /* :: */[
          List.hd(ys),
          /* [] */0
        ]
      ],
      _zs
    ]);

function even(_n) {
  while(true) {
    var n = _n;
    if (n) {
      _n = n - 1 | 0;
      continue ;
      
    }
    else {
      return /* true */1;
    }
  };
}

function even2(n) {
  if (n) {
    var n$1 = n - 1 | 0;
    if (n$1 === 1) {
      return /* true */1;
    }
    else {
      return even2(n$1 - 1 | 0);
    }
  }
  else {
    return /* true */1;
  }
}

var v = [0];

var obj = [];

Caml_obj.caml_update_dummy(obj, /* record */[
      /* get */function () {
        return v[0];
      },
      /* set */function (i) {
        v[0] = i;
        return /* () */0;
      }
    ]);

exports.xs    = xs;
exports.even  = even;
exports.even2 = even2;
exports.v     = v;
exports.obj   = obj;
/* xs Not a pure module */
