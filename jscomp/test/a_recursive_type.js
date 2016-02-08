// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_curry = require("../runtime/caml_curry");

function g(x) {
  return Caml_curry.app1(x[1], x);
}

var loop = g([
      /* A */0,
      g
    ]);

var x = [
  /* A */0,
  g
];

var non_terminate = g(x);

exports.loop          = loop;
exports.non_terminate = non_terminate;
/* loop Not a pure module */
