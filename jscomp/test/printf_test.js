// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt         = require("./mt");
var Printf     = require("../stdlib/printf");
var Caml_curry = require("../runtime/caml_curry");
var Format     = require("../stdlib/format");

function print_pair(fmt, param) {
  return Caml_curry.app2(Format.fprintf(fmt, /* Format */[
                  /* Char_literal */{
                    0: /* "(" */40,
                    1: /* Int */{
                      0: /* Int_d */0,
                      1: /* No_padding */0,
                      2: /* No_precision */0,
                      3: /* Char_literal */{
                        0: /* "," */44,
                        1: /* Int */{
                          0: /* Int_d */0,
                          1: /* No_padding */0,
                          2: /* No_precision */0,
                          3: /* Char_literal */{
                            0: /* ")" */41,
                            1: /* End_of_format */0,
                            length: 2,
                            tag: 12
                          },
                          length: 4,
                          tag: 4
                        },
                        length: 2,
                        tag: 12
                      },
                      length: 4,
                      tag: 4
                    },
                    length: 2,
                    tag: 12
                  },
                  "(%d,%d)"
                ]), param[0], param[1]);
}

var suites_000 = /* tuple */[
  "sprintf_simple",
  function () {
    return /* Eq */{
            0: "3232",
            1: Caml_curry.app2(Printf.sprintf(/* Format */[
                      /* String */{
                        0: /* No_padding */0,
                        1: /* Int */{
                          0: /* Int_d */0,
                          1: /* No_padding */0,
                          2: /* No_precision */0,
                          3: /* End_of_format */0,
                          length: 4,
                          tag: 4
                        },
                        length: 2,
                        tag: 2
                      },
                      "%s%d"
                    ]), "32", 32),
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "print_asprintf",
    function () {
      return /* Eq */{
              0: "xx",
              1: Format.asprintf(/* Format */[
                    /* String_literal */{
                      0: "xx",
                      1: /* End_of_format */0,
                      length: 2,
                      tag: 11
                    },
                    "xx"
                  ]),
              length: 2,
              tag: 0
            };
    }
  ],
  /* :: */[
    /* tuple */[
      "print_pair",
      function () {
        return /* Eq */{
                0: "(1,2)",
                1: Caml_curry.app2(Format.asprintf(/* Format */[
                          /* Alpha */{
                            0: /* End_of_format */0,
                            length: 1,
                            tag: 15
                          },
                          "%a"
                        ]), print_pair, /* tuple */[
                      1,
                      2
                    ]),
                length: 2,
                tag: 0
              };
      }
    ],
    /* [] */0
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

var v = Format.asprintf(/* Format */[
      /* String_literal */{
        0: "xx",
        1: /* End_of_format */0,
        length: 2,
        tag: 11
      },
      "xx"
    ]);

Mt.from_pair_suites("printf_test.ml", suites);

exports.print_pair = print_pair;
exports.suites     = suites;
exports.v          = v;
/* v Not a pure module */
