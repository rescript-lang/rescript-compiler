'use strict';

var Caml_obj = require("../../lib/js/caml_obj");
var List     = require("../../lib/js/list");

var ys = [];

var xs = [];

function _zs() {
  return /* tuple */[
          List.hd(ys),
          List.hd(xs[0])
        ];
}

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

var odd = even;

function even(n) {
  if (n) {
    return odd(n - 1 | 0);
  }
  else {
    return /* true */1;
  }
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

exports.xs    = xs;
exports.even  = even;
exports.even2 = even2;
/* xs Not a pure module */
