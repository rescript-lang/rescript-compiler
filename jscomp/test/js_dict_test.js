'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_dict = require("../../lib/js/js_dict.js");

function obj(param) {
  return {
          foo: 43,
          bar: 86
        };
}

var suites_000 = /* tuple */[
  "empty",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: [],
              _1: Object.keys({ })
            };
    })
];

var suites_001 = /* :: */{
  _0: /* tuple */[
    "get",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: 43,
                _1: Js_dict.get({
                      foo: 43,
                      bar: 86
                    }, "foo")
              };
      })
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "get - property not in object",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: undefined,
                  _1: Js_dict.get({
                        foo: 43,
                        bar: 86
                      }, "baz")
                };
        })
    ],
    _1: /* :: */{
      _0: /* tuple */[
        "unsafe_get",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: 43,
                    _1: ({
                          foo: 43,
                          bar: 86
                        })["foo"]
                  };
          })
      ],
      _1: /* :: */{
        _0: /* tuple */[
          "set",
          (function (param) {
              var o = {
                foo: 43,
                bar: 86
              };
              o["foo"] = 36;
              return {
                      tag: /* Eq */0,
                      _0: 36,
                      _1: Js_dict.get(o, "foo")
                    };
            })
        ],
        _1: /* :: */{
          _0: /* tuple */[
            "keys",
            (function (param) {
                return {
                        tag: /* Eq */0,
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
            _0: /* tuple */[
              "entries",
              (function (param) {
                  return {
                          tag: /* Eq */0,
                          _0: [
                            /* tuple */[
                              "foo",
                              43
                            ],
                            /* tuple */[
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
              _0: /* tuple */[
                "values",
                (function (param) {
                    return {
                            tag: /* Eq */0,
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
                _0: /* tuple */[
                  "fromList - []",
                  (function (param) {
                      return {
                              tag: /* Eq */0,
                              _0: { },
                              _1: Js_dict.fromList(/* [] */0)
                            };
                    })
                ],
                _1: /* :: */{
                  _0: /* tuple */[
                    "fromList",
                    (function (param) {
                        return {
                                tag: /* Eq */0,
                                _0: [
                                  /* tuple */[
                                    "x",
                                    23
                                  ],
                                  /* tuple */[
                                    "y",
                                    46
                                  ]
                                ],
                                _1: Js_dict.entries(Js_dict.fromList(/* :: */{
                                          _0: /* tuple */[
                                            "x",
                                            23
                                          ],
                                          _1: /* :: */{
                                            _0: /* tuple */[
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
                    _0: /* tuple */[
                      "fromArray - []",
                      (function (param) {
                          return {
                                  tag: /* Eq */0,
                                  _0: { },
                                  _1: Js_dict.fromArray([])
                                };
                        })
                    ],
                    _1: /* :: */{
                      _0: /* tuple */[
                        "fromArray",
                        (function (param) {
                            return {
                                    tag: /* Eq */0,
                                    _0: [
                                      /* tuple */[
                                        "x",
                                        23
                                      ],
                                      /* tuple */[
                                        "y",
                                        46
                                      ]
                                    ],
                                    _1: Js_dict.entries(Js_dict.fromArray([
                                              /* tuple */[
                                                "x",
                                                23
                                              ],
                                              /* tuple */[
                                                "y",
                                                46
                                              ]
                                            ]))
                                  };
                          })
                      ],
                      _1: /* :: */{
                        _0: /* tuple */[
                          "map",
                          (function (param) {
                              return {
                                      tag: /* Eq */0,
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
  _0: suites_000,
  _1: suites_001
};

Mt.from_pair_suites("Js_dict_test", suites);

exports.obj = obj;
exports.suites = suites;
/*  Not a pure module */
