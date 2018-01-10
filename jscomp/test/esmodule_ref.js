'use strict';

var Escape_esmodule = require("./escape_esmodule.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

if (!!Escape_esmodule.$$__esModule) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "esmodule_ref.ml",
          4,
          3
        ]
      ];
}

console.log(Escape_esmodule.$$__esModule);

/*  Not a pure module */
