'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Bytes = require("../../lib/js/bytes.js");
var Caml_string = require("../../lib/js/caml_string.js");

var suites_000 = /* tuple */[
  "string_of_char_array",
  (function () {
      return /* Eq */Block.__(0, [
                Caml_string.caml_string_of_char_array(/* array */[
                      /* "a" */97,
                      /* "b" */98,
                      /* "c" */99
                    ]),
                "abc"
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "caml_is_printable",
    (function () {
        return /* Eq */Block.__(0, [
                  Caml_string.caml_is_printable(/* "a" */97),
                  true
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "caml_string_of_bytes",
      (function () {
          var match = List.split(List.map((function (x) {
                      var len = x;
                      var b = Caml_string.caml_create_string(1000);
                      Caml_string.caml_fill_string(b, 0, len, /* "c" */99);
                      return /* tuple */[
                              Caml_string.bytes_to_string(b),
                              Caml_string.bytes_to_string(Bytes.init(len, (function () {
                                          return /* "c" */99;
                                        })))
                            ];
                    }), /* :: */[
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
        })
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

exports.S = S;
exports.suites = suites;
/*  Not a pure module */
