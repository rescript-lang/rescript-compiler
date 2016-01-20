// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_exceptions = require("../runtime/caml_exceptions");
var Arith_syntax = require("./arith_syntax");
var Mt = require("./mt");
var Lexing = require("../stdlib/lexing");
var Number_lexer = require("./number_lexer");
var Arith_lexer = require("./arith_lexer");
var List = require("../stdlib/list");
var Arith_parser = require("./arith_parser");

function get_tokens(lex, str) {
  var buf = Lexing.from_string(str);
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var v = lex(buf);
    if (v === /* EOF */7) {
      return List.rev(acc);
    }
    else {
      _acc = [
        /* :: */0,
        v,
        acc
      ];
    }
  };
}

function f(param) {
  return get_tokens(Arith_lexer.lexeme, param);
}

function from_tokens(lst) {
  var l = [
    0,
    lst
  ];
  var aux = function () {
    var match = l[1];
    if (match) {
      l[1] = match[2];
      return match[1];
    }
    else {
      throw Caml_exceptions.End_of_file;
    }
  };
  return aux;
}

var lexer_suites_001 = [
  /* tuple */0,
  "arith_token",
  function () {
    return [
            /* Eq */0,
            f("x + 3 + 4 + y"),
            [
              /* :: */0,
              [
                /* IDENT */1,
                "x"
              ],
              [
                /* :: */0,
                /* PLUS */0,
                [
                  /* :: */0,
                  [
                    /* NUMERAL */0,
                    3
                  ],
                  [
                    /* :: */0,
                    /* PLUS */0,
                    [
                      /* :: */0,
                      [
                        /* NUMERAL */0,
                        4
                      ],
                      [
                        /* :: */0,
                        /* PLUS */0,
                        [
                          /* :: */0,
                          [
                            /* IDENT */1,
                            "y"
                          ],
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ];
  }
];

var lexer_suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "simple token",
    function () {
      return [
              /* Eq */0,
              Arith_lexer.lexeme(Lexing.from_string("10")),
              [
                /* NUMERAL */0,
                10
              ]
            ];
    }
  ],
  [
    /* :: */0,
    [
      /* tuple */0,
      "number_lexer",
      function () {
        var v = [
          0,
          /* [] */0
        ];
        var add = function (t) {
          v[1] = [
            /* :: */0,
            t,
            v[1]
          ];
          return /* () */0;
        };
        Number_lexer.token(add, Lexing.from_string("32 + 32 ( ) * / "));
        return [
                /* Eq */0,
                List.rev(v[1]),
                [
                  /* :: */0,
                  "number",
                  [
                    /* :: */0,
                    "32",
                    [
                      /* :: */0,
                      "new line",
                      [
                        /* :: */0,
                        "+",
                        [
                          /* :: */0,
                          "new line",
                          [
                            /* :: */0,
                            "number",
                            [
                              /* :: */0,
                              "32",
                              [
                                /* :: */0,
                                "new line",
                                [
                                  /* :: */0,
                                  "(",
                                  [
                                    /* :: */0,
                                    "new line",
                                    [
                                      /* :: */0,
                                      ")",
                                      [
                                        /* :: */0,
                                        "new line",
                                        [
                                          /* :: */0,
                                          "*",
                                          [
                                            /* :: */0,
                                            "new line",
                                            [
                                              /* :: */0,
                                              "/",
                                              [
                                                /* :: */0,
                                                "new line",
                                                [
                                                  /* :: */0,
                                                  "eof",
                                                  /* [] */0
                                                ]
                                              ]
                                            ]
                                          ]
                                        ]
                                      ]
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ];
      }
    ],
    [
      /* :: */0,
      [
        /* tuple */0,
        "simple number",
        function () {
          return [
                  /* Eq */0,
                  Arith_syntax.str(Arith_parser.toplevel(Arith_lexer.lexeme, Lexing.from_string("10"))),
                  "10."
                ];
        }
      ],
      [
        /* :: */0,
        [
          /* tuple */0,
          "arith",
          function () {
            return [
                    /* Eq */0,
                    Arith_syntax.str(Arith_parser.toplevel(Arith_lexer.lexeme, Lexing.from_string("x + 3 + 4 + y"))),
                    "x+3.+4.+y"
                  ];
          }
        ],
        /* [] */0
      ]
    ]
  ]
];

var lexer_suites = [
  /* :: */0,
  lexer_suites_001,
  lexer_suites_002
];

Mt.from_pair_suites("lexer_test.ml", lexer_suites);

exports.get_tokens = get_tokens;
exports.f = f;
exports.from_tokens = from_tokens;
exports.lexer_suites = lexer_suites;
/*  Not a pure module */
