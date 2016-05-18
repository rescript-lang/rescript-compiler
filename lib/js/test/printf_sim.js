// GENERATED CODE BY BUCKLESCRIPT VERSION 0.4.1 , PLEASE EDIT WITH CARE
'use strict';

var Block  = require("../block");
var Curry  = require("../curry");
var Printf = require("../printf");

Curry._1(Printf.printf(/* Format */[
          /* Int64 */Block.__(7, [
              /* Int_d */0,
              /* No_padding */0,
              /* No_precision */0,
              /* Char_literal */Block.__(12, [
                  /* "\n" */10,
                  /* End_of_format */0
                ])
            ]),
          "%Ld\n"
        ]), /* int64 */[
      /* hi */0,
      /* lo */32
    ]);

Printf.printf(/* Format */[
      /* String_literal */Block.__(11, [
          "heloo!\nhelloxx\n",
          /* End_of_format */0
        ]),
      "heloo!\nhelloxx\n"
    ]);

Printf.printf(/* Format */[
      /* String_literal */Block.__(11, [
          "hello\nhi\n",
          /* End_of_format */0
        ]),
      "hello\nhi\n"
    ]);

Curry._2(Printf.printf(/* Format */[
          /* Int */Block.__(4, [
              /* Int_d */0,
              /* Arg_padding */Block.__(1, [/* Right */1]),
              /* No_precision */0,
              /* String_literal */Block.__(11, [
                  "\n\n",
                  /* End_of_format */0
                ])
            ]),
          "%*d\n\n"
        ]), 32, 3);

Curry._1(Printf.printf(/* Format */[
          /* String */Block.__(2, [
              /* No_padding */0,
              /* End_of_format */0
            ]),
          "%s"
        ]), Curry._2(Printf.sprintf(/* Format */[
              /* Int */Block.__(4, [
                  /* Int_d */0,
                  /* Arg_padding */Block.__(1, [/* Right */1]),
                  /* No_precision */0,
                  /* Char_literal */Block.__(12, [
                      /* "\n" */10,
                      /* End_of_format */0
                    ])
                ]),
              "%*d\n"
            ]), 32, 3));

/*  Not a pure module */
