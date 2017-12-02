'use strict';

var Js_dict = require("../../lib/js/js_dict.js");

function f(x, y) {
  return Js_dict.fromArray(/* array */[
              /* tuple */[
                "x",
                x
              ],
              /* tuple */[
                "y",
                y
              ]
            ]);
}

function f2(x, y) {
  return Js_dict.fromArray(/* array */[
              /* tuple */[
                "x0",
                x
              ],
              /* tuple */[
                "x1",
                y
              ],
              /* tuple */[
                "x2",
                x
              ],
              /* tuple */[
                "x3",
                x
              ],
              /* tuple */[
                "x4",
                x
              ],
              /* tuple */[
                "x5",
                x
              ],
              /* tuple */[
                "x6",
                x
              ],
              /* tuple */[
                "x7",
                x
              ],
              /* tuple */[
                "x8",
                x
              ]
            ]);
}

exports.f  = f;
exports.f2 = f2;
/* Js_dict Not a pure module */
