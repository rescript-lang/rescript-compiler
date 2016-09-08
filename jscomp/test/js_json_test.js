'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");
var Js_json                 = require("../../lib/js/js_json");

var v = JSON.parse(' { "x" : [1, 2, 3 ] } ');

var match = Js_json.reify_type(v);

if (match[0] !== 2) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "js_json_test.ml",
          26,
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
              21,
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
                      19,
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
            24,
            6
          ]
        ];
  }
}

console.log(Js_json.test(v, /* Object */2));

exports.v = v;
/* v Not a pure module */
