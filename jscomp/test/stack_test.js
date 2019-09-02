'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Stack = require("../../lib/js/stack.js");

function to_list(v) {
  var acc = "[]";
  while(v[/* c */0] !== "[]") {
    acc = /* constructor */{
      tag: "::",
      Arg0: Stack.pop(v),
      Arg1: acc
    };
  };
  return List.rev(acc);
}

function v(param) {
  var v$1 = /* record */[/* c */"[]"];
  Stack.push(3, v$1);
  Stack.push(4, v$1);
  Stack.push(1, v$1);
  return to_list(v$1);
}

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "push_test",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: /* constructor */{
                  tag: "::",
                  Arg0: 1,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 4,
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: 3,
                      Arg1: "[]"
                    }
                  }
                },
                Arg1: v(/* () */0)
              };
      })
  ],
  Arg1: "[]"
};

Mt.from_pair_suites("Stack_test", suites);

exports.to_list = to_list;
exports.v = v;
exports.suites = suites;
/*  Not a pure module */
