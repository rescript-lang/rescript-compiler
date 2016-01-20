// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Mt = require("./mt");
var Caml_primitive = require("../runtime/caml_primitive");
var Caml_array = require("../runtime/caml_array");
var $$Array = require("../stdlib/array");
var List = require("../stdlib/list");

function is_sorted(x) {
  var len = x.length;
  var _i = 0;
  while(/* true */1) {
    var i = _i;
    if (i >= len - 1) {
      return /* true */1;
    }
    else {
      if (Caml_primitive.caml_lessthan(x[i], x[i + 1])) {
        _i = i + 1;
      }
      else {
        return /* false */0;
      }
    }
  };
}

var array_suites_001 = [
  /* tuple */0,
  "init",
  function () {
    return [
            /* Eq */0,
            $$Array.init(5, function (x) {
                  return x;
                }),
            /* array */[
              0,
              1,
              2,
              3,
              4
            ]
          ];
  }
];

var array_suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "toList",
    function () {
      var aux = function (xs) {
        return List.fold_left(function (acc, param) {
                    return [
                            /* :: */0,
                            [
                              /* tuple */0,
                              $$Array.to_list(param[1]),
                              param[2]
                            ],
                            acc
                          ];
                  }, /* [] */0, xs);
      };
      var match = List.split(aux([
                /* :: */0,
                [
                  /* tuple */0,
                  /* array */[],
                  /* [] */0
                ],
                /* [] */0
              ]));
      return [
              /* Eq */0,
              match[1],
              match[2]
            ];
    }
  ],
  [
    /* :: */0,
    [
      /* tuple */0,
      "concat",
      function () {
        return [
                /* Eq */0,
                /* array */[
                  0,
                  1,
                  2,
                  3,
                  4,
                  5
                ],
                $$Array.concat([
                      /* :: */0,
                      /* array */[
                        0,
                        1,
                        2
                      ],
                      [
                        /* :: */0,
                        /* array */[
                          3,
                          4
                        ],
                        [
                          /* :: */0,
                          /* array */[],
                          [
                            /* :: */0,
                            /* array */[5],
                            /* [] */0
                          ]
                        ]
                      ]
                    ])
              ];
      }
    ],
    [
      /* :: */0,
      [
        /* tuple */0,
        "make",
        function () {
          return [
                  /* Eq */0,
                  Caml_array.caml_make_vect(100, /* "a" */97),
                  $$Array.init(100, function () {
                        return /* "a" */97;
                      })
                ];
        }
      ],
      [
        /* :: */0,
        [
          /* tuple */0,
          "sub",
          function () {
            return [
                    /* Eq */0,
                    $$Array.sub(/* array */[
                          0,
                          1,
                          2,
                          3,
                          4
                        ], 2, 2),
                    /* array */[
                      2,
                      3
                    ]
                  ];
          }
        ],
        [
          /* :: */0,
          [
            /* tuple */0,
            "blit",
            function () {
              var u = /* array */[
                100,
                0,
                0
              ];
              var v = $$Array.init(3, function (x) {
                    return x * 2;
                  });
              $$Array.blit(v, 1, u, 1, 2);
              return [
                      /* Eq */0,
                      [
                        /* tuple */0,
                        /* array */[
                          0,
                          2,
                          4
                        ],
                        /* array */[
                          100,
                          2,
                          4
                        ]
                      ],
                      [
                        /* tuple */0,
                        v,
                        u
                      ]
                    ];
            }
          ],
          [
            /* :: */0,
            [
              /* tuple */0,
              "make",
              function () {
                return [
                        /* Eq */0,
                        Caml_array.caml_make_vect(2, 1),
                        /* array */[
                          1,
                          1
                        ]
                      ];
              }
            ],
            [
              /* :: */0,
              [
                /* tuple */0,
                "sort",
                function () {
                  var u = /* array */[
                    3,
                    0,
                    1
                  ];
                  $$Array.sort(function (x, y) {
                        return Caml_primitive.caml_int_compare(x, y);
                      }, u);
                  return [
                          /* Eq */0,
                          Caml_primitive.caml_equal(/* array */[
                                0,
                                1,
                                3
                              ], u),
                          /* true */1
                        ];
                }
              ],
              [
                /* :: */0,
                [
                  /* tuple */0,
                  "sort_large",
                  function () {
                    var v = $$Array.init(4, function (i) {
                          return i % 17;
                        });
                    $$Array.sort(function (x, y) {
                          return Caml_primitive.caml_int_compare(x, y);
                        }, v);
                    return [
                            /* Eq */0,
                            /* true */1,
                            is_sorted(v)
                          ];
                  }
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

var array_suites = [
  /* :: */0,
  array_suites_001,
  array_suites_002
];

Mt.from_pair_suites("array_test.ml", array_suites);

/*  Not a pure module */
