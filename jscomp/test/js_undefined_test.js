'use strict';

var Mt = require("./mt.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Js_undefined = require("../../lib/js/js_undefined.js");

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
    "File \"js_undefined_test.ml\", line 5, characters 2-9",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: undefined,
                _1: undefined
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
                  _1: Caml_option.undefined_to_opt("something")
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
          "File \"js_undefined_test.ml\", line 8, characters 2-9",
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
            "bind - empty",
            (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: undefined,
                        _1: Js_undefined.bind(undefined, (function (v) {
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
                          TAG: /* Eq */0,
                          _0: 4,
                          _1: Js_undefined.bind(2, (function (n) {
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
                    Js_undefined.iter(undefined, (function (param) {
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
                      Js_undefined.iter(2, (function (v) {
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
                                _0: undefined,
                                _1: Js_undefined.fromOption(undefined)
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
                                  _1: Js_undefined.fromOption(2)
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

Mt.from_pair_suites("Js_undefined_test", suites);

exports.suites = suites;
/*  Not a pure module */
