'use strict';


function f(param) {
  switch (param) {
    case "abcd" :
        return 0;
    case "bcde" :
        return 1;
    default:
      throw {
            ExceptionID: -9,
            _1: /* tuple */[
              "test_string_case.ml",
              4,
              9
            ],
            Debug: "Assert_failure"
          };
  }
}

exports.f = f;
/* No side effect */
