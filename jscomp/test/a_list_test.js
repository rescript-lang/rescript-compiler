'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Ext_list_test = require("./ext_list_test.js");

var suites_000 = /* tuple */[
  "drop",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: Ext_list_test.drop(3, /* :: */{
                    _0: 0,
                    _1: /* :: */{
                      _0: 1,
                      _1: /* :: */{
                        _0: 2,
                        _1: /* [] */0
                      }
                    }
                  }),
              _1: /* [] */0
            };
    })
];

var suites_001 = /* :: */{
  _0: /* tuple */[
    "drop1",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: Ext_list_test.drop(2, /* :: */{
                      _0: 0,
                      _1: /* :: */{
                        _0: 1,
                        _1: /* :: */{
                          _0: 2,
                          _1: /* [] */0
                        }
                      }
                    }),
                _1: /* :: */{
                  _0: 2,
                  _1: /* [] */0
                }
              };
      })
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "flat_map",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: /* :: */{
                    _0: 0,
                    _1: /* :: */{
                      _0: 0,
                      _1: /* :: */{
                        _0: 1,
                        _1: /* :: */{
                          _0: 1,
                          _1: /* :: */{
                            _0: 0,
                            _1: /* [] */0
                          }
                        }
                      }
                    }
                  },
                  _1: Ext_list_test.flat_map((function (x) {
                          if (x % 2 === 0) {
                            return /* :: */{
                                    _0: 0,
                                    _1: /* [] */0
                                  };
                          } else {
                            return /* :: */{
                                    _0: 1,
                                    _1: /* :: */{
                                      _0: 1,
                                      _1: /* [] */0
                                    }
                                  };
                          }
                        }), /* :: */{
                        _0: 0,
                        _1: /* :: */{
                          _0: 0,
                          _1: /* :: */{
                            _0: 3,
                            _1: /* :: */{
                              _0: 0,
                              _1: /* [] */0
                            }
                          }
                        }
                      })
                };
        })
    ],
    _1: /* [] */0
  }
};

var suites = /* :: */{
  _0: suites_000,
  _1: suites_001
};

Mt.from_pair_suites("A_list_test", suites);

exports.suites = suites;
/*  Not a pure module */
