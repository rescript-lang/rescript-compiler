// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt");

function f(b, _, _n) {
  while(true) {
    var n = _n;
    if (n > 100000) {
      return /* false */0;
    }
    else if (b) {
      _n = n + 1;
      continue ;
      
    }
    else {
      return /* false */0;
    }
  };
}

function or_f(b, _, _n) {
  while(true) {
    var n = _n;
    if (n > 100000) {
      return /* false */0;
    }
    else if (b) {
      return /* true */1;
    }
    else {
      _n = n + 1;
      continue ;
      
    }
  };
}

var suites_001 = [
  /* tuple */0,
  "and_tail",
  function () {
    return [
            /* Eq */0,
            /* false */0,
            f(/* true */1, 1, 0)
          ];
  }
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "or_tail",
    function () {
      return [
              /* Eq */0,
              /* false */0,
              or_f(/* false */0, 1, 0)
            ];
    }
  ],
  /* [] */0
];

var suites = [
  /* :: */0,
  suites_001,
  suites_002
];

Mt.from_pair_suites("and_or_tailcall_test.ml", suites);

exports.f      = f;
exports.or_f   = or_f;
exports.suites = suites;
/*  Not a pure module */
