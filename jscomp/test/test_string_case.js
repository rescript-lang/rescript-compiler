'use strict';


function f(x) {
  switch (x) {
    case "abcd" :
        return 0;
    case "bcde" :
        return 1;
    default:
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "test_string_case.res",
              5,
              9
            ],
            Error: new Error()
          };
  }
}

exports.f = f;
/* No side effect */
