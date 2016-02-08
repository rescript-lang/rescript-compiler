// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt         = require("./mt");
var Printf     = require("../stdlib/printf");
var Caml_curry = require("../runtime/caml_curry");
var Format     = require("../stdlib/format");

function print_pair(fmt, param) {
  return Caml_curry.app2(Format.fprintf(fmt, [
                  /* Format */0,
                  [
                    /* Char_literal */12,
                    /* "(" */40,
                    [
                      /* Int */4,
                      /* Int_d */0,
                      /* No_padding */0,
                      /* No_precision */0,
                      [
                        /* Char_literal */12,
                        /* "," */44,
                        [
                          /* Int */4,
                          /* Int_d */0,
                          /* No_padding */0,
                          /* No_precision */0,
                          [
                            /* Char_literal */12,
                            /* ")" */41,
                            /* End_of_format */0
                          ]
                        ]
                      ]
                    ]
                  ],
                  "(%d,%d)"
                ]), param[1], param[2]);
}

var suites_001 = [
  /* tuple */0,
  "sprintf_simple",
  function () {
    return [
            /* Eq */0,
            "3232",
            Caml_curry.app2(Printf.sprintf([
                      /* Format */0,
                      [
                        /* String */2,
                        /* No_padding */0,
                        [
                          /* Int */4,
                          /* Int_d */0,
                          /* No_padding */0,
                          /* No_precision */0,
                          /* End_of_format */0
                        ]
                      ],
                      "%s%d"
                    ]), "32", 32)
          ];
  }
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "print_asprintf",
    function () {
      return [
              /* Eq */0,
              "xx",
              Format.asprintf([
                    /* Format */0,
                    [
                      /* String_literal */11,
                      "xx",
                      /* End_of_format */0
                    ],
                    "xx"
                  ])
            ];
    }
  ],
  [
    /* :: */0,
    [
      /* tuple */0,
      "print_pair",
      function () {
        return [
                /* Eq */0,
                "(1,2)",
                Caml_curry.app2(Format.asprintf([
                          /* Format */0,
                          [
                            /* Alpha */15,
                            /* End_of_format */0
                          ],
                          "%a"
                        ]), print_pair, [
                      /* tuple */0,
                      1,
                      2
                    ])
              ];
      }
    ],
    /* [] */0
  ]
];

var suites = [
  /* :: */0,
  suites_001,
  suites_002
];

var v = Format.asprintf([
      /* Format */0,
      [
        /* String_literal */11,
        "xx",
        /* End_of_format */0
      ],
      "xx"
    ]);

Mt.from_pair_suites("printf_test.ml", suites);

exports.print_pair = print_pair;
exports.suites     = suites;
exports.v          = v;
/* v Not a pure module */
