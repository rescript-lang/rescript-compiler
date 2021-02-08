'use strict';

var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");

Curry._1(Printf.printf(/* Format */{
          _0: {
            TAG: /* Int64 */7,
            _0: /* Int_d */0,
            _1: /* No_padding */0,
            _2: /* No_precision */0,
            _3: {
              TAG: /* Char_literal */12,
              _0: /* '\n' */10,
              _1: /* End_of_format */0
            }
          },
          _1: "%Ld\n"
        }), [
      0,
      32
    ]);

Printf.printf(/* Format */{
      _0: {
        TAG: /* String_literal */11,
        _0: "heloo!\nhelloxx\n",
        _1: /* End_of_format */0
      },
      _1: "heloo!\nhelloxx\n"
    });

Printf.printf(/* Format */{
      _0: {
        TAG: /* String_literal */11,
        _0: "hello\nhi\n",
        _1: /* End_of_format */0
      },
      _1: "hello\nhi\n"
    });

Curry._2(Printf.printf(/* Format */{
          _0: {
            TAG: /* Int */4,
            _0: /* Int_d */0,
            _1: {
              TAG: /* Arg_padding */1,
              _0: /* Right */1
            },
            _2: /* No_precision */0,
            _3: {
              TAG: /* String_literal */11,
              _0: "\n\n",
              _1: /* End_of_format */0
            }
          },
          _1: "%*d\n\n"
        }), 32, 3);

Curry._1(Printf.printf(/* Format */{
          _0: {
            TAG: /* String */2,
            _0: /* No_padding */0,
            _1: /* End_of_format */0
          },
          _1: "%s"
        }), Curry._2(Printf.sprintf(/* Format */{
              _0: {
                TAG: /* Int */4,
                _0: /* Int_d */0,
                _1: {
                  TAG: /* Arg_padding */1,
                  _0: /* Right */1
                },
                _2: /* No_precision */0,
                _3: {
                  TAG: /* Char_literal */12,
                  _0: /* '\n' */10,
                  _1: /* End_of_format */0
                }
              },
              _1: "%*d\n"
            }), 32, 3));

/*  Not a pure module */
