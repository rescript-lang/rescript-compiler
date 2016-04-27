// GENERATED CODE BY BUCKLESCRIPT VERSION 0.3 , PLEASE EDIT WITH CARE
'use strict';

var Caml_string = require("../runtime/caml_string");
var Bytes       = require("../stdlib/bytes");
var Mt          = require("./mt");
var Block       = require("../runtime/block");
var List        = require("../stdlib/list");
var Caml_string = require("../runtime/caml_string");

var suites_000 = /* tuple */[
  "string_of_char_array",
  function () {
    return /* Eq */Block.__(0, [
              Caml_string.caml_string_of_char_array(/* int array */[
                    /* "a" */97,
                    /* "b" */98,
                    /* "c" */99
                  ]),
              "abc"
            ]);
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "caml_is_printable",
    function () {
      return /* Eq */Block.__(0, [
                Caml_string.caml_is_printable(/* "a" */97),
                /* true */1
              ]);
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
                          Caml_string.bytes_to_string(Bytes.init(len, function () {
                                    return /* "c" */99;
                                  }))
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
        return /* Eq */Block.__(0, [
                  match[0],
                  match[1]
                ]);
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
