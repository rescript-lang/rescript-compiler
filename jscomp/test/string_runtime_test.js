// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_string = require("../runtime/caml_string");
var Mt = require("./mt");
var $$String = require("../stdlib/string");
var Caml_string = require("../runtime/caml_string");
var List = require("../stdlib/list");

var suites_001 = [
  /* tuple */0,
  "string_of_char_array",
  function () {
    return [
            /* Eq */0,
            Caml_string.caml_string_of_char_array(/* array */[
                  /* "a" */97,
                  /* "b" */98,
                  /* "c" */99
                ]),
            "abc"
          ];
  }
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "caml_is_printable",
    function () {
      return [
              /* Eq */0,
              Caml_string.caml_is_printable(/* "a" */97),
              /* true */1
            ];
    }
  ],
  [
    /* :: */0,
    [
      /* tuple */0,
      "caml_string_of_bytes",
      function () {
        var f = function (len) {
          var b = new Array(1000);
          Caml_string.caml_fill_string(b, 0, len, /* "c" */99);
          return [
                  /* tuple */0,
                  Caml_string.bytes_to_string(b),
                  $$String.init(len, function () {
                        return /* "c" */99;
                      })
                ];
        };
        var match = List.split(List.map(function (x) {
                  return f(x);
                }, [
                  /* :: */0,
                  1000,
                  [
                    /* :: */0,
                    1024,
                    [
                      /* :: */0,
                      1025,
                      [
                        /* :: */0,
                        4095,
                        [
                          /* :: */0,
                          4096,
                          [
                            /* :: */0,
                            5000,
                            [
                              /* :: */0,
                              10000,
                              /* [] */0
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]));
        return [
                /* Eq */0,
                match[1],
                match[2]
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

Mt.from_pair_suites("string_runtime_test.ml", suites);

var S = 0;

exports.S = S;
exports.suites = suites;
/*  Not a pure module */
