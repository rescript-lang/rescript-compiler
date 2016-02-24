// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj   = require("../runtime/caml_obj");
var Mt         = require("./mt");
var Caml_array = require("../runtime/caml_array");
var $$Array    = require("../stdlib/array");
var List       = require("../stdlib/list");

function is_sorted(x) {
  var len = x.length;
  var _i = 0;
  while(true) {
    var i = _i;
    if (i >= len - 1) {
      return /* true */1;
    }
    else if (Caml_obj.caml_lessthan(x[i], x[i + 1])) {
      _i = i + 1;
      continue ;
      
    }
    else {
      return /* false */0;
    }
  };
}

var array_suites_000 = /* tuple */[
  "init",
  function () {
    return /* Eq */{
            0: $$Array.init(5, function (x) {
                  return x;
                }),
            1: /* array */[
              0,
              1,
              2,
              3,
              4
            ],
            length: 2,
            tag: 0
          };
  }
];

var array_suites_001 = /* :: */[
  /* tuple */[
    "toList",
    function () {
      var aux = function (xs) {
        return List.fold_left(function (acc, param) {
                    return /* :: */[
                            /* tuple */[
                              $$Array.to_list(param[0]),
                              param[1]
                            ],
                            acc
                          ];
                  }, /* [] */0, xs);
      };
      var match = List.split(aux(/* :: */[
                /* tuple */[
                  /* int array */[],
                  /* [] */0
                ],
                /* [] */0
              ]));
      return /* Eq */{
              0: match[0],
              1: match[1],
              length: 2,
              tag: 0
            };
    }
  ],
  /* :: */[
    /* tuple */[
      "concat",
      function () {
        return /* Eq */{
                0: /* array */[
                  0,
                  1,
                  2,
                  3,
                  4,
                  5
                ],
                1: Caml_array.caml_array_concat(/* :: */[
                      /* int array */[
                        0,
                        1,
                        2
                      ],
                      /* :: */[
                        /* int array */[
                          3,
                          4
                        ],
                        /* :: */[
                          /* int array */[],
                          /* :: */[
                            /* int array */[5],
                            /* [] */0
                          ]
                        ]
                      ]
                    ]),
                length: 2,
                tag: 0
              };
      }
    ],
    /* :: */[
      /* tuple */[
        "make",
        function () {
          return /* Eq */{
                  0: Caml_array.caml_make_vect(100, /* "a" */97),
                  1: $$Array.init(100, function () {
                        return /* "a" */97;
                      }),
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* :: */[
        /* tuple */[
          "sub",
          function () {
            return /* Eq */{
                    0: $$Array.sub(/* array */[
                          0,
                          1,
                          2,
                          3,
                          4
                        ], 2, 2),
                    1: /* int array */[
                      2,
                      3
                    ],
                    length: 2,
                    tag: 0
                  };
          }
        ],
        /* :: */[
          /* tuple */[
            "blit",
            function () {
              var u = /* int array */[
                100,
                0,
                0
              ];
              var v = $$Array.init(3, function (x) {
                    return x * 2;
                  });
              $$Array.blit(v, 1, u, 1, 2);
              return /* Eq */{
                      0: /* tuple */[
                        /* int array */[
                          0,
                          2,
                          4
                        ],
                        /* int array */[
                          100,
                          2,
                          4
                        ]
                      ],
                      1: /* tuple */[
                        v,
                        u
                      ],
                      length: 2,
                      tag: 0
                    };
            }
          ],
          /* :: */[
            /* tuple */[
              "make",
              function () {
                return /* Eq */{
                        0: Caml_array.caml_make_vect(2, 1),
                        1: /* int array */[
                          1,
                          1
                        ],
                        length: 2,
                        tag: 0
                      };
              }
            ],
            /* :: */[
              /* tuple */[
                "sort",
                function () {
                  var u = /* int array */[
                    3,
                    0,
                    1
                  ];
                  $$Array.sort(function (x, y) {
                        return Caml_obj.caml_int_compare(x, y);
                      }, u);
                  return /* Eq */{
                          0: Caml_obj.caml_equal(/* int array */[
                                0,
                                1,
                                3
                              ], u),
                          1: /* true */1,
                          length: 2,
                          tag: 0
                        };
                }
              ],
              /* :: */[
                /* tuple */[
                  "sort_large",
                  function () {
                    var v = $$Array.init(4, function (i) {
                          return i % 17;
                        });
                    $$Array.sort(function (x, y) {
                          return Caml_obj.caml_int_compare(x, y);
                        }, v);
                    return /* Eq */{
                            0: /* true */1,
                            1: is_sorted(v),
                            length: 2,
                            tag: 0
                          };
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

var array_suites = /* :: */[
  array_suites_000,
  array_suites_001
];

Mt.from_pair_suites("array_test.ml", array_suites);

/*  Not a pure module */
