'use strict';

var Mt = require("./mt.js");
var Js_null = require("../../lib/js/js_null.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites_0 = [
  "toOption - empty",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: undefined,
              _1: undefined
            };
    })
];

var suites_1 = /* :: */{
  _0: [
    "toOption - 'a",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: Caml_option.some(undefined),
                _1: Caml_option.some(undefined)
              };
      })
  ],
  _1: /* :: */{
    _0: [
      "return",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: "something",
                  _1: Caml_option.null_to_opt("something")
                };
        })
    ],
    _1: /* :: */{
      _0: [
        "test - empty",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: true,
                    _1: true
                  };
          })
      ],
      _1: /* :: */{
        _0: [
          "test - 'a",
          (function (param) {
              return {
                      tag: /* Eq */0,
                      _0: false,
                      _1: false
                    };
            })
        ],
        _1: /* :: */{
          _0: [
            "bind - empty",
            (function (param) {
                return {
                        tag: /* StrictEq */2,
                        _0: null,
                        _1: Js_null.bind(null, (function (v) {
                                return v;
                              }))
                      };
              })
          ],
          _1: /* :: */{
            _0: [
              "bind - 'a",
              (function (param) {
                  return {
                          tag: /* StrictEq */2,
                          _0: 4,
                          _1: Js_null.bind(2, (function (n) {
                                  return (n << 1);
                                }))
                        };
                })
            ],
            _1: /* :: */{
              _0: [
                "iter - empty",
                (function (param) {
                    var hit = {
                      contents: false
                    };
                    Js_null.iter(null, (function (param) {
                            hit.contents = true;
                            
                          }));
                    return {
                            tag: /* Eq */0,
                            _0: false,
                            _1: hit.contents
                          };
                  })
              ],
              _1: /* :: */{
                _0: [
                  "iter - 'a",
                  (function (param) {
                      var hit = {
                        contents: 0
                      };
                      Js_null.iter(2, (function (v) {
                              hit.contents = v;
                              
                            }));
                      return {
                              tag: /* Eq */0,
                              _0: 2,
                              _1: hit.contents
                            };
                    })
                ],
                _1: /* :: */{
                  _0: [
                    "fromOption - None",
                    (function (param) {
                        return {
                                tag: /* Eq */0,
                                _0: null,
                                _1: Js_null.fromOption(undefined)
                              };
                      })
                  ],
                  _1: /* :: */{
                    _0: [
                      "fromOption - Some",
                      (function (param) {
                          return {
                                  tag: /* Eq */0,
                                  _0: 2,
                                  _1: Js_null.fromOption(2)
                                };
                        })
                    ],
                    _1: /* [] */0
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

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("Js_null_test", suites);

exports.suites = suites;
/*  Not a pure module */
