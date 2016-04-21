// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Curry  = require("../runtime/curry");
var Printf = require("../stdlib/printf");

Curry._1(Printf.printf(/* Format */[
          /* Int64 */{
            0: /* Int_d */0,
            1: /* No_padding */0,
            2: /* No_precision */0,
            3: /* Char_literal */{
              0: /* "\n" */10,
              1: /* End_of_format */0,
              length: 2,
              tag: 12
            },
            length: 4,
            tag: 7
          },
          "%Ld\n"
        ]), /* int64 */[
      0,
      32
    ]);

Printf.printf(/* Format */[
      /* String_literal */{
        0: "heloo!\nhelloxx\n",
        1: /* End_of_format */0,
        length: 2,
        tag: 11
      },
      "heloo!\nhelloxx\n"
    ]);

Printf.printf(/* Format */[
      /* String_literal */{
        0: "hello\nhi\n",
        1: /* End_of_format */0,
        length: 2,
        tag: 11
      },
      "hello\nhi\n"
    ]);

Curry._2(Printf.printf(/* Format */[
          /* Int */{
            0: /* Int_d */0,
            1: /* Arg_padding */{
              0: /* Right */1,
              length: 1,
              tag: 1
            },
            2: /* No_precision */0,
            3: /* String_literal */{
              0: "\n\n",
              1: /* End_of_format */0,
              length: 2,
              tag: 11
            },
            length: 4,
            tag: 4
          },
          "%*d\n\n"
        ]), 32, 3);

Curry._1(Printf.printf(/* Format */[
          /* String */{
            0: /* No_padding */0,
            1: /* End_of_format */0,
            length: 2,
            tag: 2
          },
          "%s"
        ]), Curry._2(Printf.sprintf(/* Format */[
              /* Int */{
                0: /* Int_d */0,
                1: /* Arg_padding */{
                  0: /* Right */1,
                  length: 1,
                  tag: 1
                },
                2: /* No_precision */0,
                3: /* Char_literal */{
                  0: /* "\n" */10,
                  1: /* End_of_format */0,
                  length: 2,
                  tag: 12
                },
                length: 4,
                tag: 4
              },
              "%*d\n"
            ]), 32, 3));

/*  Not a pure module */
