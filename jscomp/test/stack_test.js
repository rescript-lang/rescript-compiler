// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Mt    = require("./mt");
var Stack = require("../stdlib/stack");
var List  = require("../stdlib/list");

function to_list(v) {
  var acc = /* [] */0;
  while(!Stack.is_empty(v)) {
    acc = [
      /* :: */0,
      Stack.pop(v),
      acc
    ];
  };
  return List.rev(acc);
}

function v() {
  var v$1 = Stack.create(/* () */0);
  Stack.push(3, v$1);
  Stack.push(4, v$1);
  Stack.push(1, v$1);
  return to_list(v$1);
}

var suites_001 = [
  /* tuple */0,
  "push_test",
  function () {
    return [
            /* Eq */0,
            [
              /* :: */0,
              1,
              [
                /* :: */0,
                4,
                [
                  /* :: */0,
                  3,
                  /* [] */0
                ]
              ]
            ],
            v(/* () */0)
          ];
  }
];

var suites = [
  /* :: */0,
  suites_001,
  /* [] */0
];

Mt.from_pair_suites("stack_test.ml", suites);

exports.to_list = to_list;
exports.v       = v;
exports.suites  = suites;
/*  Not a pure module */
