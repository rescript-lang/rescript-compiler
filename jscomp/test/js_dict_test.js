'use strict';

var Mt = require("./mt.js");
var Js_dict = require("../../lib/js/js_dict.js");

function obj(param) {
  return {
          foo: 43,
          bar: 86
        };
}

var suites_0 = [
  "empty",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: [],
              _1: Object.keys({})
            };
    })
];

var suites_1 = /* :: */{
  _0: [
    "get",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: 43,
                _1: Js_dict.get({
                      foo: 43,
                      bar: 86
                    }, "foo")
              };
      })
  ],
  _1: /* :: */{
    _0: [
      "get - property not in object",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: undefined,
                  _1: Js_dict.get({
                        foo: 43,
                        bar: 86
                      }, "baz")
                };
        })
    ],
    _1: /* :: */{
      _0: [
        "unsafe_get",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: 43,
                    _1: ({
                          foo: 43,
                          bar: 86
                        })["foo"]
                  };
          })
      ],
      _1: /* :: */{
        _0: [
          "set",
          (function (param) {
              var o = {
                foo: 43,
                bar: 86
              };
              o["foo"] = 36;
              return {
                      TAG: /* Eq */0,
                      _0: 36,
                      _1: Js_dict.get(o, "foo")
                    };
            })
        ],
        _1: /* :: */{
          _0: [
            "keys",
            (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: [
                          "foo",
                          "bar"
                        ],
                        _1: Object.keys({
                              foo: 43,
                              bar: 86
                            })
                      };
              })
          ],
          _1: /* :: */{
            _0: [
              "entries",
              (function (param) {
                  return {
                          TAG: /* Eq */0,
                          _0: [
                            [
                              "foo",
                              43
                            ],
                            [
                              "bar",
                              86
                            ]
                          ],
                          _1: Js_dict.entries({
                                foo: 43,
                                bar: 86
                              })
                        };
                })
            ],
            _1: /* :: */{
              _0: [
                "values",
                (function (param) {
                    return {
                            TAG: /* Eq */0,
                            _0: [
                              43,
                              86
                            ],
                            _1: Js_dict.values({
                                  foo: 43,
                                  bar: 86
                                })
                          };
                  })
              ],
              _1: /* :: */{
                _0: [
                  "fromList - []",
                  (function (param) {
                      return {
                              TAG: /* Eq */0,
                              _0: {},
                              _1: Js_dict.fromList(/* [] */0)
                            };
                    })
                ],
                _1: /* :: */{
                  _0: [
                    "fromList",
                    (function (param) {
                        return {
                                TAG: /* Eq */0,
                                _0: [
                                  [
                                    "x",
                                    23
                                  ],
                                  [
                                    "y",
                                    46
                                  ]
                                ],
                                _1: Js_dict.entries(Js_dict.fromList(/* :: */{
                                          _0: [
                                            "x",
                                            23
                                          ],
                                          _1: /* :: */{
                                            _0: [
                                              "y",
                                              46
                                            ],
                                            _1: /* [] */0
                                          }
                                        }))
                              };
                      })
                  ],
                  _1: /* :: */{
                    _0: [
                      "fromArray - []",
                      (function (param) {
                          return {
                                  TAG: /* Eq */0,
                                  _0: {},
                                  _1: Js_dict.fromArray([])
                                };
                        })
                    ],
                    _1: /* :: */{
                      _0: [
                        "fromArray",
                        (function (param) {
                            return {
                                    TAG: /* Eq */0,
                                    _0: [
                                      [
                                        "x",
                                        23
                                      ],
                                      [
                                        "y",
                                        46
                                      ]
                                    ],
                                    _1: Js_dict.entries(Js_dict.fromArray([
                                              [
                                                "x",
                                                23
                                              ],
                                              [
                                                "y",
                                                46
                                              ]
                                            ]))
                                  };
                          })
                      ],
                      _1: /* :: */{
                        _0: [
                          "map",
                          (function (param) {
                              return {
                                      TAG: /* Eq */0,
                                      _0: {
                                        foo: "43",
                                        bar: "86"
                                      },
                                      _1: Js_dict.map((function (i) {
                                              return String(i);
                                            }), {
                                            foo: 43,
                                            bar: 86
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
};

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("Js_dict_test", suites);

exports.obj = obj;
exports.suites = suites;
/*  Not a pure module */
