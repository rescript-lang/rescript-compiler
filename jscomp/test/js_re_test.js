'use strict';

var Mt    = require("./mt");
var Block = require("../../lib/js/block");

var suites_000 = /* tuple */[
  "exec_literal",
  function () {
    var res = (/[^.]+/).exec("http://xxx.domain.com");
    return /* Eq */Block.__(0, [
              "xxx",
              res[0].substring(7)
            ]);
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "test_str",
    function () {
      var res = new RegExp("foo").test("#foo#");
      return /* Eq */Block.__(0, [
                true,
                res
              ]);
    }
  ],
  /* :: */[
    /* tuple */[
      "fromStringWithFlags",
      function () {
        var res = new RegExp("foo", "g");
        return /* Eq */Block.__(0, [
                  true,
                  res.global
                ]);
      }
    ],
    /* :: */[
      /* tuple */[
        "result_index",
        function () {
          var res = new RegExp("zbar").exec("foobarbazbar");
          return /* Eq */Block.__(0, [
                    8,
                    res.index
                  ]);
        }
      ],
      /* :: */[
        /* tuple */[
          "result_input",
          function () {
            return /* Eq */Block.__(0, [
                      "foobar",
                      (/foo/g).exec("foobar").input
                    ]);
          }
        ],
        /* :: */[
          /* tuple */[
            "t_global",
            function () {
              return /* Eq */Block.__(0, [
                        true,
                        (/./ig).global
                      ]);
            }
          ],
          /* :: */[
            /* tuple */[
              "t_ignoreCase",
              function () {
                return /* Eq */Block.__(0, [
                          true,
                          (/./ig).ignoreCase
                        ]);
              }
            ],
            /* :: */[
              /* tuple */[
                "t_lastIndex",
                function () {
                  var re = (/na/g);
                  re.exec("banana");
                  return /* Eq */Block.__(0, [
                            4,
                            re.lastIndex
                          ]);
                }
              ],
              /* :: */[
                /* tuple */[
                  "t_multiline",
                  function () {
                    return /* Eq */Block.__(0, [
                              false,
                              (/./ig).multiline
                            ]);
                  }
                ],
                /* :: */[
                  /* tuple */[
                    "t_source",
                    function () {
                      return /* Eq */Block.__(0, [
                                "f.+o",
                                (/f.+o/ig).source
                              ]);
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
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_re_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
