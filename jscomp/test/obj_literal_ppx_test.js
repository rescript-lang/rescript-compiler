'use strict';


var a = {
  x: 3,
  y: /* Nested :: */[
    1,[
      2,[
        3,/* [] */0
      ]
    ]
  ]
};

exports.a = a;
/* a Not a pure module */
