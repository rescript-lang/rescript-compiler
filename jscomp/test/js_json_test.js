'use strict';

var Mt                      = require("./mt");
var Block                   = require("../../lib/js/block");
var Js_json                 = require("../../lib/js/js_json");
var Js_primitive            = require("../../lib/js/js_primitive");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      function () {
        return /* Eq */Block.__(0, [
                  x,
                  y
                ]);
      }
    ],
    suites[0]
  ];
  return /* () */0;
}

var v = JSON.parse(' { "x" : [1, 2, 3 ] } ');

var match = Js_json.reify_type(v);

if (match[0] !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "js_json_test.ml",
          33,
          9
        ]
      ];
}
else {
  var match$1 = match[1]["x"];
  if (match$1 !== undefined) {
    var match$2 = Js_json.reify_type(match$1);
    if (match$2[0] !== 3) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "js_json_test.ml",
              28,
              13
            ]
          ];
    }
    else {
      match$2[1].forEach(function (x) {
            var match = Js_json.reify_type(x);
            if (match[0] !== 1) {
              throw [
                    Caml_builtin_exceptions.assert_failure,
                    [
                      "js_json_test.ml",
                      26,
                      19
                    ]
                  ];
            }
            else {
              console.log(match[1] + 0);
              return /* () */0;
            }
          });
    }
  }
  else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "js_json_test.ml",
            31,
            6
          ]
        ];
  }
}

eq('File "js_json_test.ml", line 37, characters 5-12', Js_json.test(v, /* Object */2), /* true */1);

eq('File "js_json_test.ml", line 40, characters 12-19', Js_primitive.undefined_to_opt(JSON.stringify(/* int array */[
              1,
              2,
              3
            ])), /* Some */["[1,2,3]"]);

eq('File "js_json_test.ml", line 44, characters 2-9', Js_primitive.undefined_to_opt(JSON.stringify({
              foo: 1,
              bar: "hello",
              baz: {
                baaz: 10
              }
            })), /* Some */['{"foo":1,"bar":"hello","baz":{"baaz":10}}']);

eq('File "js_json_test.ml", line 48, characters 12-19', Js_primitive.undefined_to_opt(JSON.stringify(null)), /* Some */["null"]);

eq('File "js_json_test.ml", line 50, characters 12-19', Js_primitive.undefined_to_opt(JSON.stringify(undefined)), /* None */0);

Mt.from_pair_suites("js_json_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
exports.v       = v;
/* v Not a pure module */
