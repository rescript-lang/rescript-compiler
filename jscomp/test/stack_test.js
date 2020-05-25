'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Stack = require("../../lib/js/stack.js");

function to_list(v) {
  var acc = /* [] */0;
  while(v.c !== /* [] */0) {
    acc = /* :: */{
      _0: Stack.pop(v),
      _1: acc
    };
  };
  return List.rev(acc);
}

function v(param) {
  var v$1 = {
    c: /* [] */0,
    len: 0
  };
  Stack.push(3, v$1);
  Stack.push(4, v$1);
  Stack.push(1, v$1);
  return to_list(v$1);
}

var suites_0 = /* tuple */[
  "push_test",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: /* :: */{
                _0: 1,
                _1: /* :: */{
                  _0: 4,
                  _1: /* :: */{
                    _0: 3,
                    _1: /* [] */0
                  }
                }
              },
              _1: v(undefined)
            };
    })
];

var suites = /* :: */{
  _0: suites_0,
  _1: /* [] */0
};

Mt.from_pair_suites("Stack_test", suites);

exports.to_list = to_list;
exports.v = v;
exports.suites = suites;
/*  Not a pure module */
