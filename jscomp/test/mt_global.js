// Generated CODE, PLEASE EDIT WITH CARE
'use strict';


function collect_eq(test_id, suites, loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      function () {
        return /* Eq */{
                0: x,
                1: y,
                length: 2,
                tag: 0
              };
      }
    ],
    suites[0]
  ];
  return /* () */0;
}

function collect_neq(test_id, suites, loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      function () {
        return /* Neq */{
                0: x,
                1: y,
                length: 2,
                tag: 1
              };
      }
    ],
    suites[0]
  ];
  return /* () */0;
}

function collect_approx(test_id, suites, loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      function () {
        return /* Approx */{
                0: x,
                1: y,
                length: 2,
                tag: 2
              };
      }
    ],
    suites[0]
  ];
  return /* () */0;
}

exports.collect_eq     = collect_eq;
exports.collect_neq    = collect_neq;
exports.collect_approx = collect_approx;
/* No side effect */
