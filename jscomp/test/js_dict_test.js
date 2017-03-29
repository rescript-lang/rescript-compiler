'use strict';

var Mt           = require("./mt.js");
var Block        = require("../../lib/js/block.js");
var Js_primitive = require("../../lib/js/js_primitive.js");

function obj() {
  return {
          foo: 43,
          bar: "baz"
        };
}

var suites_000 = /* tuple */[
  "get",
  function () {
    return /* Eq */Block.__(0, [
              /* Some */[43],
              Js_primitive.undefined_to_opt({
                      foo: 43,
                      bar: "baz"
                    }["foo"])
            ]);
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "get - property not in object",
    function () {
      return /* Eq */Block.__(0, [
                /* None */0,
                Js_primitive.undefined_to_opt({
                        foo: 43,
                        bar: "baz"
                      }["baz"])
              ]);
    }
  ],
  /* :: */[
    /* tuple */[
      "unsafe_get",
      function () {
        return /* Eq */Block.__(0, [
                  43,
                  {
                      foo: 43,
                      bar: "baz"
                    }["foo"]
                ]);
      }
    ],
    /* :: */[
      /* tuple */[
        "set",
        function () {
          var o = {
            foo: 43,
            bar: "baz"
          };
          o["foo"] = 36;
          return /* Eq */Block.__(0, [
                    /* Some */[36],
                    Js_primitive.undefined_to_opt(o["foo"])
                  ]);
        }
      ],
      /* :: */[
        /* tuple */[
          "keys",
          function () {
            return /* Eq */Block.__(0, [
                      /* array */[
                        "foo",
                        "bar"
                      ],
                      Object.keys({
                            foo: 43,
                            bar: "baz"
                          })
                    ]);
          }
        ],
        /* :: */[
          /* tuple */[
            "empty",
            function () {
              return /* Eq */Block.__(0, [
                        /* array */[],
                        Object.keys({ })
                      ]);
            }
          ],
          /* [] */0
        ]
      ]
    ]
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_dict_test.ml", suites);

exports.obj    = obj;
exports.suites = suites;
/*  Not a pure module */
