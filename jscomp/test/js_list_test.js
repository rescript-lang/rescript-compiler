'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_list = require("../../lib/js/js_list.js");
var Js_vector = require("../../lib/js/js_vector.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

eq("File \"js_list_test.ml\", line 11, characters 7-14", Js_list.flatten(/* :: */[
          /* :: */[
            1,
            /* :: */[
              2,
              /* [] */0
            ]
          ],
          /* :: */[
            /* :: */[
              3,
              /* [] */0
            ],
            /* :: */[
              /* [] */0,
              /* :: */[
                /* :: */[
                  1,
                  /* :: */[
                    2,
                    /* :: */[
                      3,
                      /* [] */0
                    ]
                  ]
                ],
                /* [] */0
              ]
            ]
          ]
        ]), /* :: */[
      1,
      /* :: */[
        2,
        /* :: */[
          3,
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
        ]
      ]
    ]);

eq("File \"js_list_test.ml\", line 14, characters 7-14", Js_list.filterMap((function (x) {
            if (x % 2) {
              return /* None */0;
            } else {
              return /* Some */[x];
            }
          }), /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                4,
                /* :: */[
                  5,
                  /* :: */[
                    6,
                    /* :: */[
                      7,
                      /* [] */0
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]), /* :: */[
      2,
      /* :: */[
        4,
        /* :: */[
          6,
          /* [] */0
        ]
      ]
    ]);

eq("File \"js_list_test.ml\", line 17, characters 7-14", Js_list.filterMap((function (x) {
            if (x % 2) {
              return /* None */0;
            } else {
              return /* Some */[x];
            }
          }), /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                4,
                /* :: */[
                  5,
                  /* :: */[
                    6,
                    /* [] */0
                  ]
                ]
              ]
            ]
          ]
        ]), /* :: */[
      2,
      /* :: */[
        4,
        /* :: */[
          6,
          /* [] */0
        ]
      ]
    ]);

eq("File \"js_list_test.ml\", line 20, characters 7-14", Js_list.countBy((function (x) {
            return +(x % 2 === 0);
          }), /* :: */[
          1,
          /* :: */[
            2,
            /* :: */[
              3,
              /* :: */[
                4,
                /* :: */[
                  5,
                  /* :: */[
                    6,
                    /* [] */0
                  ]
                ]
              ]
            ]
          ]
        ]), 3);

function f(i) {
  return i;
}

var v = Js_vector.toList(Js_vector.init(100000, f));

eq("File \"js_list_test.ml\", line 23, characters 7-14", Js_list.countBy((function (x) {
            return +(x % 2 === 0);
          }), v), 50000);

var vv = Js_list.foldRight((function (x, y) {
        return /* :: */[
                x,
                y
              ];
      }), v, /* [] */0);

eq("File \"js_list_test.ml\", line 27, characters 7-14", /* true */1, Js_list.equal((function (x, y) {
            return +(x === y);
          }), v, vv));

var vvv = Js_list.filter((function (x) {
        return +(x % 10 === 0);
      }), vv);

eq("File \"js_list_test.ml\", line 31, characters 7-14", Js_list.length(vvv), 10000);

function f$1(x) {
  return Caml_int32.imul(x, 10);
}

eq("File \"js_list_test.ml\", line 32, characters 7-14", /* true */1, Js_list.equal((function (x, y) {
            return +(x === y);
          }), vvv, Js_vector.toList(Js_vector.init(10000, f$1))));

Mt.from_pair_suites("js_list_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
