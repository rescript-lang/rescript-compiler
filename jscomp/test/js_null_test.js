'use strict';

var Mt = require("./mt.js");
var Js_null = require("../../lib/js/js_null.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites_0 = [
  "toOption - empty",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: undefined,
              _1: undefined
            };
    })
];

var suites_1 = {
  hd: [
    "toOption - 'a",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: Caml_option.some(undefined),
                _1: Caml_option.some(undefined)
              };
      })
  ],
  tl: {
    hd: [
      "return",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: "something",
                  _1: Caml_option.null_to_opt("something")
                };
        })
    ],
    tl: {
      hd: [
        "test - empty",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: true,
                    _1: true
                  };
          })
      ],
      tl: {
        hd: [
          "test - 'a",
          (function (param) {
              return {
                      TAG: /* Eq */0,
                      _0: false,
                      _1: false
                    };
            })
        ],
        tl: {
          hd: [
            "bind - empty",
            (function (param) {
                return {
                        TAG: /* StrictEq */2,
                        _0: null,
                        _1: Js_null.bind(null, (function (v) {
                                return v;
                              }))
                      };
              })
          ],
          tl: {
            hd: [
              "bind - 'a",
              (function (param) {
                  return {
                          TAG: /* StrictEq */2,
                          _0: 4,
                          _1: Js_null.bind(2, (function (n) {
                                  return (n << 1);
                                }))
                        };
                })
            ],
            tl: {
              hd: [
                "iter - empty",
                (function (param) {
                    var hit = {
                      contents: false
                    };
                    Js_null.iter(null, (function (param) {
                            hit.contents = true;
                            
                          }));
                    return {
                            TAG: /* Eq */0,
                            _0: false,
                            _1: hit.contents
                          };
                  })
              ],
              tl: {
                hd: [
                  "iter - 'a",
                  (function (param) {
                      var hit = {
                        contents: 0
                      };
                      Js_null.iter(2, (function (v) {
                              hit.contents = v;
                              
                            }));
                      return {
                              TAG: /* Eq */0,
                              _0: 2,
                              _1: hit.contents
                            };
                    })
                ],
                tl: {
                  hd: [
                    "fromOption - None",
                    (function (param) {
                        return {
                                TAG: /* Eq */0,
                                _0: null,
                                _1: Js_null.fromOption(undefined)
                              };
                      })
                  ],
                  tl: {
                    hd: [
                      "fromOption - Some",
                      (function (param) {
                          return {
                                  TAG: /* Eq */0,
                                  _0: 2,
                                  _1: Js_null.fromOption(2)
                                };
                        })
                    ],
                    tl: /* [] */0
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};

var suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Js_null_test", suites);

exports.suites = suites;
/*  Not a pure module */
