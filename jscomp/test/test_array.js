// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Caml_array = require("../runtime/caml_array");
var $$Array    = require("../stdlib/array");

var v = Caml_array.caml_make_vect(6, 5);

new Array(30);

var h = $$Array.sub(v, 0, 2);

var hhh = $$Array.append(/* array */[
      1,
      2,
      3,
      4
    ], /* array */[
      1,
      2,
      3,
      5
    ]);

var u = $$Array.concat([
      /* :: */0,
      /* array */[
        1,
        2
      ],
      [
        /* :: */0,
        /* array */[
          2,
          3
        ],
        [
          /* :: */0,
          /* array */[
            3,
            4
          ],
          /* [] */0
        ]
      ]
    ]);

var hh = $$Array.blit;

exports.v   = v;
exports.h   = h;
exports.hh  = hh;
exports.hhh = hhh;
exports.u   = u;
/*  Not a pure module */
