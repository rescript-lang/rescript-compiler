'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Js_undefined = require("../../lib/js/js_undefined.js");

var suites_000 = /* tuple */[
  "toOption - empty",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: undefined,
              _1: undefined
            };
    })
];

var suites_001 = /* :: */{
  _0: /* tuple */[
    "File \"js_undefined_test.ml\", line 5, characters 2-9",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: undefined,
                _1: undefined
              };
      })
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "return",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: "something",
                  _1: Caml_option.undefined_to_opt("something")
                };
        })
    ],
    _1: /* :: */{
      _0: /* tuple */[
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
        _0: /* tuple */[
          "File \"js_undefined_test.ml\", line 8, characters 2-9",
          (function (param) {
              return {
                      tag: /* Eq */0,
                      _0: true,
                      _1: true
                    };
            })
        ],
        _1: /* :: */{
          _0: /* tuple */[
            "bind - empty",
            (function (param) {
                return {
                        tag: /* Eq */0,
                        _0: undefined,
                        _1: Js_undefined.bind(undefined, (function (v) {
                                return v;
                              }))
                      };
              })
          ],
          _1: /* :: */{
            _0: /* tuple */[
              "bind - 'a",
              (function (param) {
                  return {
                          tag: /* Eq */0,
                          _0: 4,
                          _1: Js_undefined.bind(2, (function (n) {
                                  return (n << 1);
                                }))
                        };
                })
            ],
            _1: /* :: */{
              _0: /* tuple */[
                "iter - empty",
                (function (param) {
                    var hit = {
                      contents: false
                    };
                    Js_undefined.iter(undefined, (function (param) {
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
                _0: /* tuple */[
                  "iter - 'a",
                  (function (param) {
                      var hit = {
                        contents: 0
                      };
                      Js_undefined.iter(2, (function (v) {
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
                  _0: /* tuple */[
                    "fromOption - None",
                    (function (param) {
                        return {
                                tag: /* Eq */0,
                                _0: undefined,
                                _1: Js_undefined.fromOption(undefined)
                              };
                      })
                  ],
                  _1: /* :: */{
                    _0: /* tuple */[
                      "fromOption - Some",
                      (function (param) {
                          return {
                                  tag: /* Eq */0,
                                  _0: 2,
                                  _1: Js_undefined.fromOption(2)
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
  _0: suites_000,
  _1: suites_001
};

Mt.from_pair_suites("Js_undefined_test", suites);

exports.suites = suites;
/*  Not a pure module */
