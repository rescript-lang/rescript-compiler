// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_string = require("../runtime/caml_string");
var Mt          = require("./mt");
var $$String    = require("../stdlib/string");
var List        = require("../stdlib/list");
var Caml_string = require("../runtime/caml_string");

var suites_000 = /* tuple */[
  "string_of_char_array",
  function () {
    return /* Eq */{
            0: Caml_string.caml_string_of_char_array(/* int array */[
                  /* "a" */97,
                  /* "b" */98,
                  /* "c" */99
                ]),
            1: "abc",
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "caml_is_printable",
    function () {
      return /* Eq */{
              0: Caml_string.caml_is_printable(/* "a" */97),
              1: /* true */1,
              length: 2,
              tag: 0
            };
    }
  ],
  /* :: */[
    /* tuple */[
      "caml_string_of_bytes",
      function () {
        var match = List.split(List.map(function (x) {
                  var len = x;
                  var b = new Array(1000);
                  Caml_string.caml_fill_string(b, 0, len, /* "c" */99);
                  return /* tuple */[
                          Caml_string.bytes_to_string(b),
                          $$String.init(len, function () {
                                return /* "c" */99;
                              })
                        ];
                }, /* :: */[
                  1000,
                  /* :: */[
                    1024,
                    /* :: */[
                      1025,
                      /* :: */[
                        4095,
                        /* :: */[
                          4096,
                          /* :: */[
                            5000,
                            /* :: */[
                              10000,
                              /* [] */0
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]));
        return /* Eq */{
                0: match[0],
                1: match[1],
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

Mt.from_pair_suites("string_runtime_test.ml", suites);

var S = 0;

exports.S      = S;
exports.suites = suites;
/*  Not a pure module */
