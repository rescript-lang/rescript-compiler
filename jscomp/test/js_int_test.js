'use strict';

var Mt = require("./mt.js");

var suites_0 = [
  "toExponential",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: "1.23456e+5",
              _1: (123456).toExponential()
            };
    })
];

var suites_1 = /* :: */{
  _0: [
    "toExponentialWithPrecision - digits:2",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: "1.23e+5",
                _1: (123456).toExponential(2)
              };
      })
  ],
  _1: /* :: */{
    _0: [
      "toExponentialWithPrecision - digits:4",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: "1.2346e+5",
                  _1: (123456).toExponential(4)
                };
        })
    ],
    _1: /* :: */{
      _0: [
        "toExponentialWithPrecision - digits:20",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: "0.00000000000000000000e+0",
                    _1: (0).toExponential(20)
                  };
          })
      ],
      _1: /* :: */{
        _0: [
          "File \"js_int_test.ml\", line 12, characters 3-10",
          (function (param) {
              return {
                      tag: /* ThrowAny */7,
                      _0: (function (param) {
                          (0).toExponential(101);
                          
                        })
                    };
            })
        ],
        _1: /* :: */{
          _0: [
            "toExponentialWithPrecision - digits:-1",
            (function (param) {
                return {
                        tag: /* ThrowAny */7,
                        _0: (function (param) {
                            (0).toExponential(-1);
                            
                          })
                      };
              })
          ],
          _1: /* :: */{
            _0: [
              "toPrecision",
              (function (param) {
                  return {
                          tag: /* Eq */0,
                          _0: "123456",
                          _1: (123456).toPrecision()
                        };
                })
            ],
            _1: /* :: */{
              _0: [
                "toPrecisionWithPrecision - digits:2",
                (function (param) {
                    return {
                            tag: /* Eq */0,
                            _0: "1.2e+5",
                            _1: (123456).toPrecision(2)
                          };
                  })
              ],
              _1: /* :: */{
                _0: [
                  "toPrecisionWithPrecision - digits:4",
                  (function (param) {
                      return {
                              tag: /* Eq */0,
                              _0: "1.235e+5",
                              _1: (123456).toPrecision(4)
                            };
                    })
                ],
                _1: /* :: */{
                  _0: [
                    "toPrecisionWithPrecision - digits:20",
                    (function (param) {
                        return {
                                tag: /* Eq */0,
                                _0: "0.0000000000000000000",
                                _1: (0).toPrecision(20)
                              };
                      })
                  ],
                  _1: /* :: */{
                    _0: [
                      "File \"js_int_test.ml\", line 25, characters 3-10",
                      (function (param) {
                          return {
                                  tag: /* ThrowAny */7,
                                  _0: (function (param) {
                                      (0).toPrecision(101);
                                      
                                    })
                                };
                        })
                    ],
                    _1: /* :: */{
                      _0: [
                        "toPrecisionWithPrecision - digits:-1",
                        (function (param) {
                            return {
                                    tag: /* ThrowAny */7,
                                    _0: (function (param) {
                                        (0).toPrecision(-1);
                                        
                                      })
                                  };
                          })
                      ],
                      _1: /* :: */{
                        _0: [
                          "toString",
                          (function (param) {
                              return {
                                      tag: /* Eq */0,
                                      _0: "123",
                                      _1: (123).toString()
                                    };
                            })
                        ],
                        _1: /* :: */{
                          _0: [
                            "toStringWithRadix - radix:2",
                            (function (param) {
                                return {
                                        tag: /* Eq */0,
                                        _0: "11110001001000000",
                                        _1: (123456).toString(2)
                                      };
                              })
                          ],
                          _1: /* :: */{
                            _0: [
                              "toStringWithRadix - radix:16",
                              (function (param) {
                                  return {
                                          tag: /* Eq */0,
                                          _0: "1e240",
                                          _1: (123456).toString(16)
                                        };
                                })
                            ],
                            _1: /* :: */{
                              _0: [
                                "toStringWithRadix - radix:36",
                                (function (param) {
                                    return {
                                            tag: /* Eq */0,
                                            _0: "2n9c",
                                            _1: (123456).toString(36)
                                          };
                                  })
                              ],
                              _1: /* :: */{
                                _0: [
                                  "toStringWithRadix - radix:37",
                                  (function (param) {
                                      return {
                                              tag: /* ThrowAny */7,
                                              _0: (function (param) {
                                                  (0).toString(37);
                                                  
                                                })
                                            };
                                    })
                                ],
                                _1: /* :: */{
                                  _0: [
                                    "toStringWithRadix - radix:1",
                                    (function (param) {
                                        return {
                                                tag: /* ThrowAny */7,
                                                _0: (function (param) {
                                                    (0).toString(1);
                                                    
                                                  })
                                              };
                                      })
                                  ],
                                  _1: /* :: */{
                                    _0: [
                                      "toStringWithRadix - radix:-1",
                                      (function (param) {
                                          return {
                                                  tag: /* ThrowAny */7,
                                                  _0: (function (param) {
                                                      (0).toString(-1);
                                                      
                                                    })
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
};

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("Js_int_test", suites);

exports.suites = suites;
/*  Not a pure module */
