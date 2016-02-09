// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Stream = require("../stdlib/stream");
var Mt     = require("./mt");
var Genlex = require("../stdlib/genlex");
var List   = require("../stdlib/list");

var lexer = Genlex.make_lexer([
      /* :: */0,
      "+",
      [
        /* :: */0,
        "-",
        [
          /* :: */0,
          "*",
          [
            /* :: */0,
            "/",
            [
              /* :: */0,
              "let",
              [
                /* :: */0,
                "=",
                [
                  /* :: */0,
                  "(",
                  [
                    /* :: */0,
                    ")",
                    /* [] */0
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]);

function to_list(s) {
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var exit = 0;
    var v;
    try {
      v = Stream.next(s);
      exit = 1;
    }
    catch (exn){
      if (exn === Stream.Failure) {
        return List.rev(acc);
      }
      else {
        throw exn;
      }
    }
    if (exit === 1) {
      _acc = [
        /* :: */0,
        v,
        acc
      ];
      continue ;
      
    }
    
  };
}

var suites_001 = [
  /* tuple */0,
  "lexer_stream_genlex",
  function () {
    return [
            /* Eq */0,
            [
              /* :: */0,
              [
                /* Int */2,
                3
              ],
              [
                /* :: */0,
                [
                  /* Kwd */0,
                  "("
                ],
                [
                  /* :: */0,
                  [
                    /* Int */2,
                    3
                  ],
                  [
                    /* :: */0,
                    [
                      /* Kwd */0,
                      "+"
                    ],
                    [
                      /* :: */0,
                      [
                        /* Int */2,
                        2
                      ],
                      [
                        /* :: */0,
                        [
                          /* Int */2,
                          -1
                        ],
                        [
                          /* :: */0,
                          [
                            /* Kwd */0,
                            ")"
                          ],
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ],
            to_list(lexer(Stream.of_string("3(3 + 2 -1)")))
          ];
  }
];

var suites = [
  /* :: */0,
  suites_001,
  /* [] */0
];

Mt.from_pair_suites("genlex_test.ml", suites);

exports.lexer   = lexer;
exports.to_list = to_list;
exports.suites  = suites;
/* lexer Not a pure module */
