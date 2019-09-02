'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");

function length(param) {
  return 3;
}

Mt.from_pair_suites("Es6_module_test", /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "list_length",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: List.length(/* constructor */{
                          tag: "::",
                          Arg0: 1,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: 2,
                            Arg1: "[]"
                          }
                        }),
                    Arg1: 2
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "length",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: 3,
                      Arg1: 3
                    };
            })
        ],
        Arg1: "[]"
      }
    });

exports.length = length;
/*  Not a pure module */
