'use strict';

var Mt = require("./mt.js");

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "setTimeout/clearTimeout sanity check",
    (function (param) {
        var handle = setTimeout((function (param) {
                return /* () */0;
              }), 0);
        clearTimeout(handle);
        return /* constructor */{
                tag: "Ok",
                Arg0: true
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "setInerval/clearInterval sanity check",
      (function (param) {
          var handle = setInterval((function (param) {
                  return /* () */0;
                }), 0);
          clearInterval(handle);
          return /* constructor */{
                  tag: "Ok",
                  Arg0: true
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "encodeURI",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: encodeURI("[-=-]"),
                    Arg1: "%5B-=-%5D"
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "decodeURI",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: decodeURI("%5B-=-%5D"),
                      Arg1: "[-=-]"
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "encodeURIComponent",
            (function (param) {
                return /* constructor */{
                        tag: "Eq",
                        Arg0: encodeURIComponent("[-=-]"),
                        Arg1: "%5B-%3D-%5D"
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "decodeURIComponent",
              (function (param) {
                  return /* constructor */{
                          tag: "Eq",
                          Arg0: decodeURIComponent("%5B-%3D-%5D"),
                          Arg1: "[-=-]"
                        };
                })
            ],
            Arg1: "[]"
          }
        }
      }
    }
  }
};

Mt.from_pair_suites("Js_global_test", suites);

exports.suites = suites;
/*  Not a pure module */
