'use strict';

var Curry = require("../../lib/js/curry.js");
var Format = require("../../lib/js/format.js");

Curry._1(Format.fprintf(Format.std_formatter, /* constructor */{
          tag: "Format",
          Arg0: /* constructor */{
            tag: "Int",
            Arg0: "Int_d",
            Arg1: "No_padding",
            Arg2: "No_precision",
            Arg3: "End_of_format"
          },
          Arg1: "%d"
        }), 3);

/*  Not a pure module */
