'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Js_null_undefined = require("../../lib/js/js_null_undefined.js");

var suites_0 = /* tuple */[
  "toOption - null",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: undefined,
              _1: undefined
            };
    })
];

var suites_1 = /* :: */{
  _0: /* tuple */[
    "toOption - undefined",
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
      "toOption - empty",
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
        "File \"js_null_undefined_test.ml\", line 7, characters 2-9",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: "foo",
                    _1: Caml_option.nullable_to_opt("foo")
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
                      _1: Caml_option.nullable_to_opt("something")
                    };
            })
        ],
        _1: /* :: */{
          _0: /* tuple */[
            "test - null",
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
              "test - undefined",
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
                  "File \"js_null_undefined_test.ml\", line 12, characters 2-9",
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
                    "bind - null",
                    (function (param) {
                        return {
                                tag: /* StrictEq */2,
                                _0: null,
                                _1: Js_null_undefined.bind(null, (function (v) {
                                        return v;
                                      }))
                              };
                      })
                  ],
                  _1: /* :: */{
                    _0: /* tuple */[
                      "bind - undefined",
                      (function (param) {
                          return {
                                  tag: /* StrictEq */2,
                                  _0: undefined,
                                  _1: Js_null_undefined.bind(undefined, (function (v) {
                                          return v;
                                        }))
                                };
                        })
                    ],
                    _1: /* :: */{
                      _0: /* tuple */[
                        "bind - empty",
                        (function (param) {
                            return {
                                    tag: /* StrictEq */2,
                                    _0: undefined,
                                    _1: Js_null_undefined.bind(undefined, (function (v) {
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
                                      _1: Js_null_undefined.bind(2, (function (n) {
                                              return (n << 1);
                                            }))
                                    };
                            })
                        ],
                        _1: /* :: */{
                          _0: /* tuple */[
                            "iter - null",
                            (function (param) {
                                var hit = {
                                  contents: false
                                };
                                Js_null_undefined.iter(null, (function (param) {
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
                              "iter - undefined",
                              (function (param) {
                                  var hit = {
                                    contents: false
                                  };
                                  Js_null_undefined.iter(undefined, (function (param) {
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
                                "iter - empty",
                                (function (param) {
                                    var hit = {
                                      contents: false
                                    };
                                    Js_null_undefined.iter(undefined, (function (param) {
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
                                      Js_null_undefined.iter(2, (function (v) {
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
                                                _1: Js_null_undefined.fromOption(undefined)
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
                                                  _1: Js_null_undefined.fromOption(2)
                                                };
                                        })
                                    ],
                                    _1: /* :: */{
                                      _0: /* tuple */[
                                        "null <> undefined",
                                        (function (param) {
                                            return {
                                                    tag: /* Ok */4,
                                                    _0: true
                                                  };
                                          })
                                      ],
                                      _1: /* :: */{
                                        _0: /* tuple */[
                                          "null <> empty",
                                          (function (param) {
                                              return {
                                                      tag: /* Ok */4,
                                                      _0: true
                                                    };
                                            })
                                        ],
                                        _1: /* :: */{
                                          _0: /* tuple */[
                                            "undefined = empty",
                                            (function (param) {
                                                return {
                                                        tag: /* Ok */4,
                                                        _0: true
                                                      };
                                              })
                                          ],
                                          _1: /* :: */{
                                            _0: /* tuple */[
                                              "File \"js_null_undefined_test.ml\", line 42, characters 2-9",
                                              (function (param) {
                                                  return {
                                                          tag: /* Ok */4,
                                                          _0: true
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

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("Js_null_undefined_test", suites);

exports.suites = suites;
/*  Not a pure module */
