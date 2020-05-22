'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Sexpm = require("./sexpm.js");
var Format = require("../../lib/js/format.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  
}

function print_or_error(fmt, x) {
  if (x.HASH >= 106380200) {
    return Curry._1(Format.fprintf(fmt, /* Format */[
                    /* Formatting_gen */Block.__(18, [
                        /* Open_box */Block.__(1, [/* Format */[
                              /* End_of_format */0,
                              ""
                            ]]),
                        /* String_literal */Block.__(11, [
                            "Error:",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* Formatting_lit */Block.__(17, [
                                    /* Close_box */0,
                                    /* Formatting_lit */Block.__(17, [
                                        /* Flush_newline */4,
                                        /* End_of_format */0
                                      ])
                                  ])
                              ])
                          ])
                      ]),
                    "@[Error:%s@]@."
                  ]), x.value);
  } else {
    return Curry._2(Format.fprintf(fmt, /* Format */[
                    /* Formatting_gen */Block.__(18, [
                        /* Open_box */Block.__(1, [/* Format */[
                              /* End_of_format */0,
                              ""
                            ]]),
                        /* String_literal */Block.__(11, [
                            "Ok:",
                            /* Alpha */Block.__(15, [/* Formatting_lit */Block.__(17, [
                                    /* Close_box */0,
                                    /* Formatting_lit */Block.__(17, [
                                        /* Flush_newline */4,
                                        /* End_of_format */0
                                      ])
                                  ])])
                          ])
                      ]),
                    "@[Ok:%a@]@."
                  ]), Sexpm.print, x.value);
  }
}

var a = Sexpm.parse_string("(x x gh 3 3)");

eq("File \"sexpm_test.ml\", line 17, characters 7-14", /* tuple */[
      {
        HASH: /* Ok */17724,
        value: {
          HASH: /* List */848054398,
          value: /* :: */[
            {
              HASH: /* Atom */726615281,
              value: "x"
            },
            /* :: */[
              {
                HASH: /* Atom */726615281,
                value: "x"
              },
              /* :: */[
                {
                  HASH: /* Atom */726615281,
                  value: "gh"
                },
                /* :: */[
                  {
                    HASH: /* Atom */726615281,
                    value: "3"
                  },
                  /* :: */[
                    {
                      HASH: /* Atom */726615281,
                      value: "3"
                    },
                    /* [] */0
                  ]
                ]
              ]
            ]
          ]
        }
      },
      a
    ]);

eq("File \"sexpm_test.ml\", line 21, characters 7-14", /* tuple */[
      Curry._2(Format.asprintf(/* Format */[
                  /* Alpha */Block.__(15, [/* End_of_format */0]),
                  "%a"
                ]), print_or_error, a).trim(),
      "Ok:(x x gh 3 3)\n".trim()
    ]);

Mt.from_pair_suites("Sexpm_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.print_or_error = print_or_error;
/* a Not a pure module */
