'use strict';

var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");

console.error(Curry._2(Printf.sprintf(/* constructor */{
              tag: "Format",
              Arg0: /* constructor */{
                tag: "Int",
                Arg0: "Int_d",
                Arg1: "No_padding",
                Arg2: "No_precision",
                Arg3: /* constructor */{
                  tag: "String",
                  Arg0: "No_padding",
                  Arg1: "End_of_format"
                }
              },
              Arg1: "%d%s"
            }), 32, "ss"));

/*  Not a pure module */
