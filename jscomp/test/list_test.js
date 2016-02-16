// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_obj                = require("../runtime/caml_obj");
var Mt                      = require("./mt");
var $$Array                 = require("../stdlib/array");
var Assert                  = require("assert");
var List                    = require("../stdlib/list");

var list_suites_000 = /* tuple */[
  "length",
  function () {
    if (1 === List.length(/* :: */[
            /* tuple */[
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
            Caml_builtin_exceptions.Assert_failure,
            [
              "list_test.ml",
              5,
              3
            ]
          ];
    }
  }
];

var list_suites_001 = /* :: */[
  /* tuple */[
    "length2",
    function () {
      if (5 === List.length(/* :: */[
              0,
              /* :: */[
                1,
                /* :: */[
                  2,
                  /* :: */[
                    3,
                    /* :: */[
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
              Caml_builtin_exceptions.Assert_failure,
              [
                "list_test.ml",
                8,
                3
              ]
            ];
      }
    }
  ],
  /* :: */[
    /* tuple */[
      "long_length",
      function () {
        var v = 100000;
        var prim = List.length($$Array.to_list($$Array.init(v, function () {
                      return 0;
                    })));
        return Assert.deepEqual(v, prim);
      }
    ],
    /* :: */[
      /* tuple */[
        "sort",
        function () {
          var prim = List.sort(function (x, y) {
                return Caml_obj.caml_int_compare(x, y);
              }, /* :: */[
                4,
                /* :: */[
                  1,
                  /* :: */[
                    2,
                    /* :: */[
                      3,
                      /* [] */0
                    ]
                  ]
                ]
              ]);
          return Assert.deepEqual(prim, /* :: */[
                      1,
                      /* :: */[
                        2,
                        /* :: */[
                          3,
                          /* :: */[
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

var list_suites = /* :: */[
  list_suites_000,
  list_suites_001
];

Mt.from_suites("List_suites", list_suites);

exports.list_suites = list_suites;
/*  Not a pure module */
