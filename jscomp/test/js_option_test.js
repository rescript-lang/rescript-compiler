'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_option = require("../../lib/js/js_option.js");

function simpleEq(a, b) {
  return a === b;
}

var option_suites_0 = /* tuple */[
  "option_isSome_Some",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: true,
              _1: true
            };
    })
];

var option_suites_1 = /* :: */{
  _0: /* tuple */[
    "option_isSome_None",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: false,
                _1: false
              };
      })
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "option_isNone_Some",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: false,
                  _1: false
                };
        })
    ],
    _1: /* :: */{
      _0: /* tuple */[
        "option_isNone_None",
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
          "option_isSomeValue_Eq",
          (function (param) {
              return {
                      tag: /* Eq */0,
                      _0: true,
                      _1: Js_option.isSomeValue(simpleEq, 2, 2)
                    };
            })
        ],
        _1: /* :: */{
          _0: /* tuple */[
            "option_isSomeValue_Diff",
            (function (param) {
                return {
                        tag: /* Eq */0,
                        _0: false,
                        _1: Js_option.isSomeValue(simpleEq, 1, 2)
                      };
              })
          ],
          _1: /* :: */{
            _0: /* tuple */[
              "option_isSomeValue_DiffNone",
              (function (param) {
                  return {
                          tag: /* Eq */0,
                          _0: false,
                          _1: Js_option.isSomeValue(simpleEq, 1, undefined)
                        };
                })
            ],
            _1: /* :: */{
              _0: /* tuple */[
                "option_getExn_Some",
                (function (param) {
                    return {
                            tag: /* Eq */0,
                            _0: 2,
                            _1: Js_option.getExn(2)
                          };
                  })
              ],
              _1: /* :: */{
                _0: /* tuple */[
                  "option_equal_Eq",
                  (function (param) {
                      return {
                              tag: /* Eq */0,
                              _0: true,
                              _1: Js_option.equal(simpleEq, 2, 2)
                            };
                    })
                ],
                _1: /* :: */{
                  _0: /* tuple */[
                    "option_equal_Diff",
                    (function (param) {
                        return {
                                tag: /* Eq */0,
                                _0: false,
                                _1: Js_option.equal(simpleEq, 1, 2)
                              };
                      })
                  ],
                  _1: /* :: */{
                    _0: /* tuple */[
                      "option_equal_DiffNone",
                      (function (param) {
                          return {
                                  tag: /* Eq */0,
                                  _0: false,
                                  _1: Js_option.equal(simpleEq, 1, undefined)
                                };
                        })
                    ],
                    _1: /* :: */{
                      _0: /* tuple */[
                        "option_andThen_SomeSome",
                        (function (param) {
                            return {
                                    tag: /* Eq */0,
                                    _0: true,
                                    _1: Js_option.isSomeValue(simpleEq, 3, Js_option.andThen((function (a) {
                                                return a + 1 | 0;
                                              }), 2))
                                  };
                          })
                      ],
                      _1: /* :: */{
                        _0: /* tuple */[
                          "option_andThen_SomeNone",
                          (function (param) {
                              return {
                                      tag: /* Eq */0,
                                      _0: false,
                                      _1: Js_option.isSomeValue(simpleEq, 3, Js_option.andThen((function (param) {
                                                  
                                                }), 2))
                                    };
                            })
                        ],
                        _1: /* :: */{
                          _0: /* tuple */[
                            "option_map_Some",
                            (function (param) {
                                return {
                                        tag: /* Eq */0,
                                        _0: true,
                                        _1: Js_option.isSomeValue(simpleEq, 3, Js_option.map((function (a) {
                                                    return a + 1 | 0;
                                                  }), 2))
                                      };
                              })
                          ],
                          _1: /* :: */{
                            _0: /* tuple */[
                              "option_map_None",
                              (function (param) {
                                  return {
                                          tag: /* Eq */0,
                                          _0: undefined,
                                          _1: Js_option.map((function (a) {
                                                  return a + 1 | 0;
                                                }), undefined)
                                        };
                                })
                            ],
                            _1: /* :: */{
                              _0: /* tuple */[
                                "option_default_Some",
                                (function (param) {
                                    return {
                                            tag: /* Eq */0,
                                            _0: 2,
                                            _1: Js_option.getWithDefault(3, 2)
                                          };
                                  })
                              ],
                              _1: /* :: */{
                                _0: /* tuple */[
                                  "option_default_None",
                                  (function (param) {
                                      return {
                                              tag: /* Eq */0,
                                              _0: 3,
                                              _1: Js_option.getWithDefault(3, undefined)
                                            };
                                    })
                                ],
                                _1: /* :: */{
                                  _0: /* tuple */[
                                    "option_filter_Pass",
                                    (function (param) {
                                        return {
                                                tag: /* Eq */0,
                                                _0: true,
                                                _1: Js_option.isSomeValue(simpleEq, 2, Js_option.filter((function (a) {
                                                            return a % 2 === 0;
                                                          }), 2))
                                              };
                                      })
                                  ],
                                  _1: /* :: */{
                                    _0: /* tuple */[
                                      "option_filter_Reject",
                                      (function (param) {
                                          return {
                                                  tag: /* Eq */0,
                                                  _0: undefined,
                                                  _1: Js_option.filter((function (a) {
                                                          return a % 3 === 0;
                                                        }), 2)
                                                };
                                        })
                                    ],
                                    _1: /* :: */{
                                      _0: /* tuple */[
                                        "option_filter_None",
                                        (function (param) {
                                            return {
                                                    tag: /* Eq */0,
                                                    _0: undefined,
                                                    _1: Js_option.filter((function (a) {
                                                            return a % 3 === 0;
                                                          }), undefined)
                                                  };
                                          })
                                      ],
                                      _1: /* :: */{
                                        _0: /* tuple */[
                                          "option_firstSome_First",
                                          (function (param) {
                                              return {
                                                      tag: /* Eq */0,
                                                      _0: true,
                                                      _1: Js_option.isSomeValue(simpleEq, 3, Js_option.firstSome(3, 2))
                                                    };
                                            })
                                        ],
                                        _1: /* :: */{
                                          _0: /* tuple */[
                                            "option_firstSome_First",
                                            (function (param) {
                                                return {
                                                        tag: /* Eq */0,
                                                        _0: true,
                                                        _1: Js_option.isSomeValue(simpleEq, 2, Js_option.firstSome(undefined, 2))
                                                      };
                                              })
                                          ],
                                          _1: /* :: */{
                                            _0: /* tuple */[
                                              "option_firstSome_None",
                                              (function (param) {
                                                  return {
                                                          tag: /* Eq */0,
                                                          _0: undefined,
                                                          _1: Js_option.firstSome(undefined, undefined)
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
                        }
                      }
                    }
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

var option_suites = /* :: */{
  _0: option_suites_0,
  _1: option_suites_1
};

Mt.from_pair_suites("Js_option_test", option_suites);

exports.simpleEq = simpleEq;
exports.option_suites = option_suites;
/*  Not a pure module */
