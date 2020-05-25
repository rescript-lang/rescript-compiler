'use strict';

var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");

console.error(Curry._2(Printf.sprintf(/* Format */{
              _0: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: /* End_of_format */0
                }
              },
              _1: "%d%s"
            }), 32, "ss"));

/*  Not a pure module */
