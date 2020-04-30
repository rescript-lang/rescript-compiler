'use strict';


function f(param) {
  switch (param) {
    case "abcd" :
        return 0;
    case "bcde" :
        return 1;
    default:
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: /* tuple */[
              "test_string_case.ml",
              4,
              9
            ]
          };
  }
}

exports.f = f;
/* No side effect */
