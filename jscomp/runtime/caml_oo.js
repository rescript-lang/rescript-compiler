// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_exceptions = require("./caml_exceptions");
var Caml_array      = require("./caml_array");

var caml_methods_cache = Caml_array.caml_make_vect(1000, 0);

function caml_get_public_method(obj, tag, cacheid) {
  var meths = obj[1];
  var offs = caml_methods_cache[cacheid];
  if (meths[offs] === tag) {
    return meths[offs - 1];
  }
  else {
    var aux = function (_i) {
      while(true) {
        var i = _i;
        if (i < 3) {
          throw [
                0,
                Caml_exceptions.Assert_failure,
                [
                  0,
                  "caml_oo.ml",
                  43,
                  20
                ]
              ];
        }
        else if (meths[i] === tag) {
          caml_methods_cache[cacheid] = i;
          return i;
        }
        else {
          _i = i - 2;
        }
      };
    };
    return meths[aux(meths[0] * 2 + 1) - 1];
  }
}

exports.caml_get_public_method = caml_get_public_method;
/* No side effect */
