

import * as Caml_array from "./caml_array.mjs";
import * as Caml_exceptions from "./caml_exceptions.mjs";

var caml_methods_cache = Caml_array.caml_make_vect(1000, 0);

function caml_set_oo_id(b) {
  b[1] = Caml_exceptions.id.contents;
  Caml_exceptions.id.contents = Caml_exceptions.id.contents + 1 | 0;
  return b;
}

function caml_get_public_method(obj, tag, cacheid) {
  var meths = obj[0];
  var offs = caml_methods_cache[cacheid];
  if (meths[offs] === tag) {
    return meths[offs - 1 | 0];
  }
  var aux = function (_i) {
    while(true) {
      var i = _i;
      if (i < 3) {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "caml_oo.ml",
                80,
                20
              ],
              Error: new Error()
            };
      }
      if (meths[i] === tag) {
        caml_methods_cache[cacheid] = i;
        return i;
      }
      _i = i - 2 | 0;
      continue ;
    };
  };
  return meths[aux((meths[0] << 1) + 1 | 0) - 1 | 0];
}

export {
  caml_get_public_method ,
  caml_set_oo_id ,
  
}
/* No side effect */
