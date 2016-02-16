// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Stream = require("../stdlib/stream");
var Mt     = require("./mt");
var Genlex = require("../stdlib/genlex");
var List   = require("../stdlib/list");

var lexer = Genlex.make_lexer(/* :: */[
      "+",
      /* :: */[
        "-",
        /* :: */[
          "*",
          /* :: */[
            "/",
            /* :: */[
              "let",
              /* :: */[
                "=",
                /* :: */[
                  "(",
                  /* :: */[
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
      _acc = /* :: */[
        v,
        acc
      ];
      continue ;
      
    }
    
  };
}

var suites_000 = /* tuple */[
  "lexer_stream_genlex",
  function () {
    return /* Eq */{
            0: /* :: */[
              /* Int */{
                0: 3,
                length: 1,
                tag: 2
              },
              /* :: */[
                /* Kwd */{
                  0: "(",
                  length: 1,
                  tag: 0
                },
                /* :: */[
                  /* Int */{
                    0: 3,
                    length: 1,
                    tag: 2
                  },
                  /* :: */[
                    /* Kwd */{
                      0: "+",
                      length: 1,
                      tag: 0
                    },
                    /* :: */[
                      /* Int */{
                        0: 2,
                        length: 1,
                        tag: 2
                      },
                      /* :: */[
                        /* Int */{
                          0: -1,
                          length: 1,
                          tag: 2
                        },
                        /* :: */[
                          /* Kwd */{
                            0: ")",
                            length: 1,
                            tag: 0
                          },
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ],
            1: to_list(lexer(Stream.of_string("3(3 + 2 -1)"))),
            length: 2,
            tag: 0
          };
  }
];

var suites = /* :: */[
  suites_000,
  /* [] */0
];

Mt.from_pair_suites("genlex_test.ml", suites);

exports.lexer   = lexer;
exports.to_list = to_list;
exports.suites  = suites;
/* lexer Not a pure module */
