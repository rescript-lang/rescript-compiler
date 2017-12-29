'use strict';

var $$Array = require("../../lib/js/array.js");
var Caml_array = require("../../lib/js/caml_array.js");

var v = Caml_array.caml_make_vect(6, 5);

new Array(30);

var h = $$Array.sub(v, 0, 2);

var hhh = $$Array.append(/* int array */[
      1,
      2,
      3,
      4
    ], /* int array */[
      1,
      2,
      3,
      5
    ]);

var u = Caml_array.caml_array_concat(/* :: */[
      /* int array */[
        1,
        2
      ],
      /* :: */[
        /* int array */[
          2,
          3
        ],
        /* :: */[
          /* int array */[
            3,
            4
          ],
          /* [] */0
        ]
      ]
    ]);

var hh = $$Array.blit;

exports.v = v;
exports.h = h;
exports.hh = hh;
exports.hhh = hhh;
exports.u = u;
/*  Not a pure module */
