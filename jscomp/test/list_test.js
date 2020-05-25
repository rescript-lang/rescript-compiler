'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");

var list_suites_0 = [
  "length",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: 1,
              _1: List.length(/* :: */{
                    _0: [
                      0,
                      1,
                      2,
                      3,
                      4
                    ],
                    _1: /* [] */0
                  })
            };
    })
];

var list_suites_1 = /* :: */{
  _0: [
    "length2",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: 5,
                _1: List.length(/* :: */{
                      _0: 0,
                      _1: /* :: */{
                        _0: 1,
                        _1: /* :: */{
                          _0: 2,
                          _1: /* :: */{
                            _0: 3,
                            _1: /* :: */{
                              _0: 4,
                              _1: /* [] */0
                            }
                          }
                        }
                      }
                    })
              };
      })
  ],
  _1: /* :: */{
    _0: [
      "long_length",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: 30000,
                  _1: List.length($$Array.to_list($$Array.init(30000, (function (param) {
                                  return 0;
                                }))))
                };
        })
    ],
    _1: /* :: */{
      _0: [
        "sort",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: List.sort(Caml_primitive.caml_int_compare, /* :: */{
                          _0: 4,
                          _1: /* :: */{
                            _0: 1,
                            _1: /* :: */{
                              _0: 2,
                              _1: /* :: */{
                                _0: 3,
                                _1: /* [] */0
                              }
                            }
                          }
                        }),
                    _1: /* :: */{
                      _0: 1,
                      _1: /* :: */{
                        _0: 2,
                        _1: /* :: */{
                          _0: 3,
                          _1: /* :: */{
                            _0: 4,
                            _1: /* [] */0
                          }
                        }
                      }
                    }
                  };
          })
      ],
      _1: /* [] */0
    }
  }
};

var list_suites = /* :: */{
  _0: list_suites_0,
  _1: list_suites_1
};

Mt.from_pair_suites("List_test", list_suites);

exports.list_suites = list_suites;
/*  Not a pure module */
