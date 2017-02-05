'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");

function f(children) {
  if (children) {
    var children$1 = children[1];
    var a0 = children[0];
    if (children$1) {
      var children$2 = children$1[1];
      var a1 = children$1[0];
      if (children$2) {
        var children$3 = children$2[1];
        var a2 = children$2[0];
        if (children$3) {
          var children$4 = children$3[1];
          var a3 = children$3[0];
          if (children$4) {
            if (children$4[1]) {
              throw [
                    Caml_builtin_exceptions.assert_failure,
                    [
                      "gpr_1150.ml",
                      20,
                      18
                    ]
                  ];
            }
            else {
              return /* array */[
                      a0,
                      a1,
                      a2,
                      a3,
                      children$4[0]
                    ];
            }
          }
          else {
            return /* array */[
                    a0,
                    a1,
                    a2,
                    a3
                  ];
          }
        }
        else {
          return /* array */[
                  a0,
                  a1,
                  a2
                ];
        }
      }
      else {
        return /* array */[
                a0,
                a1
              ];
      }
    }
    else {
      return /* array */[a0];
    }
  }
  else {
    return /* array */[];
  }
}

exports.f = f;
/* No side effect */
