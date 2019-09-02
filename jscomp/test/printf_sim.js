'use strict';

var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");

Curry._1(Printf.printf(/* constructor */{
          tag: "Format",
          Arg0: /* constructor */{
            tag: "Int64",
            Arg0: "Int_d",
            Arg1: "No_padding",
            Arg2: "No_precision",
            Arg3: /* constructor */{
              tag: "Char_literal",
              Arg0: /* "\n" */10,
              Arg1: "End_of_format"
            }
          },
          Arg1: "%Ld\n"
        }), /* int64 */[
      /* hi */0,
      /* lo */32
    ]);

Printf.printf(/* constructor */{
      tag: "Format",
      Arg0: /* constructor */{
        tag: "String_literal",
        Arg0: "heloo!\nhelloxx\n",
        Arg1: "End_of_format"
      },
      Arg1: "heloo!\nhelloxx\n"
    });

Printf.printf(/* constructor */{
      tag: "Format",
      Arg0: /* constructor */{
        tag: "String_literal",
        Arg0: "hello\nhi\n",
        Arg1: "End_of_format"
      },
      Arg1: "hello\nhi\n"
    });

Curry._2(Printf.printf(/* constructor */{
          tag: "Format",
          Arg0: /* constructor */{
            tag: "Int",
            Arg0: "Int_d",
            Arg1: /* constructor */{
              tag: "Arg_padding",
              Arg0: "Right"
            },
            Arg2: "No_precision",
            Arg3: /* constructor */{
              tag: "String_literal",
              Arg0: "\n\n",
              Arg1: "End_of_format"
            }
          },
          Arg1: "%*d\n\n"
        }), 32, 3);

Curry._1(Printf.printf(/* constructor */{
          tag: "Format",
          Arg0: /* constructor */{
            tag: "String",
            Arg0: "No_padding",
            Arg1: "End_of_format"
          },
          Arg1: "%s"
        }), Curry._2(Printf.sprintf(/* constructor */{
              tag: "Format",
              Arg0: /* constructor */{
                tag: "Int",
                Arg0: "Int_d",
                Arg1: /* constructor */{
                  tag: "Arg_padding",
                  Arg0: "Right"
                },
                Arg2: "No_precision",
                Arg3: /* constructor */{
                  tag: "Char_literal",
                  Arg0: /* "\n" */10,
                  Arg1: "End_of_format"
                }
              },
              Arg1: "%*d\n"
            }), 32, 3));

/*  Not a pure module */
