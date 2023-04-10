'use strict';

var Mt = require("./mt.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");

function u(param) {
  var n;
  try {
    n = "123".length;
  }
  catch (exn){
    return 42;
  }
  return Caml_int32.div(3, 0);
}

Mt.from_pair_suites("Jsoo_400_test", {
      hd: [
        "File \"jsoo_400_test.res\", line 7, characters 38-45",
        (function (param) {
            return {
                    TAG: "ThrowAny",
                    _0: (function (param) {
                        u(undefined);
                      })
                  };
          })
      ],
      tl: /* [] */0
    });

exports.u = u;
/*  Not a pure module */
