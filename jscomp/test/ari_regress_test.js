// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt");

var g = 7;

function gg(x, y) {
  var u = x + y;
  return function (z) {
    return u + z;
  };
}

function g1(x, y) {
  var u = x + y;
  return function (xx, yy) {
    return xx + yy + u;
  };
}

var x = gg(3, 5)(6);

function v(param) {
  return g1(3, 4)(6, param);
}

var suites_001 = [
  /* tuple */0,
  "curry",
  function () {
    return [
            /* Eq */0,
            g,
            7
          ];
  }
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "curry2",
    function () {
      return [
              /* Eq */0,
              14,
              v(1)
            ];
    }
  ],
  [
    /* :: */0,
    [
      /* tuple */0,
      "curry3",
      function () {
        return [
                /* Eq */0,
                x,
                14
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

Mt.from_pair_suites("ari_regress_test.ml", suites);

/* x Not a pure module */
