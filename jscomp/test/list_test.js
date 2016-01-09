// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_exceptions = require("../runtime/caml_exceptions");
var Mt = require("./mt");
var Caml_primitive = require("../runtime/caml_primitive");
var $$Array = require("../stdlib/array");
var List = require("../stdlib/list");

var list_suites_001 = [
  /* tuple */0,
  "length",
  function () {
    if (1 === List.length([
            /* :: */0,
            [
              /* tuple */0,
              0,
              1,
              2,
              3,
              4
            ],
            /* [] */0
          ])) {
      return 0;
    }
    else {
      throw [
            0,
            Caml_exceptions.Assert_failure,
            [
              0,
              "list_test.ml",
              5,
              3
            ]
          ];
    }
  }
];

var list_suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "length2",
    function () {
      if (5 === List.length([
              /* :: */0,
              0,
              [
                /* :: */0,
                1,
                [
                  /* :: */0,
                  2,
                  [
                    /* :: */0,
                    3,
                    [
                      /* :: */0,
                      4,
                      /* [] */0
                    ]
                  ]
                ]
              ]
            ])) {
        return 0;
      }
      else {
        throw [
              0,
              Caml_exceptions.Assert_failure,
              [
                0,
                "list_test.ml",
                8,
                3
              ]
            ];
      }
    }
  ],
  [
    /* :: */0,
    [
      /* tuple */0,
      "long_length",
      function () {
        var v = 100000;
        return Mt.assert_equal(v, List.length($$Array.to_list($$Array.init(v, function () {
                                return 0;
                              }))));
      }
    ],
    [
      /* :: */0,
      [
        /* tuple */0,
        "sort",
        function () {
          return Mt.assert_equal(List.sort(function (x, y) {
                          return Caml_primitive.caml_int_compare(x, y);
                        }, [
                          /* :: */0,
                          4,
                          [
                            /* :: */0,
                            1,
                            [
                              /* :: */0,
                              2,
                              [
                                /* :: */0,
                                3,
                                /* [] */0
                              ]
                            ]
                          ]
                        ]), [
                      /* :: */0,
                      1,
                      [
                        /* :: */0,
                        2,
                        [
                          /* :: */0,
                          3,
                          [
                            /* :: */0,
                            4,
                            /* [] */0
                          ]
                        ]
                      ]
                    ]);
        }
      ],
      /* [] */0
    ]
  ]
];

var list_suites = [
  /* :: */0,
  list_suites_001,
  list_suites_002
];

Mt.from_suites("List_suites", list_suites);

exports.list_suites = list_suites;
/*  Not a pure module */
