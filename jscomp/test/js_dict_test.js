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

var suites_1 = {
  hd: [
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
  tl: {
    hd: [
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
    tl: {
      hd: [
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
      tl: {
        hd: [
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
        tl: {
          hd: [
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
          tl: {
            hd: [
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
            tl: {
              hd: [
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
              tl: {
                hd: [
                  "fromList - []",
                  (function (param) {
                      return {
                              TAG: /* Eq */0,
                              _0: {},
                              _1: Js_dict.fromList(/* [] */0)
                            };
                    })
                ],
                tl: {
                  hd: [
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
                                _1: Js_dict.entries(Js_dict.fromList({
                                          hd: [
                                            "x",
                                            23
                                          ],
                                          tl: {
                                            hd: [
                                              "y",
                                              46
                                            ],
                                            tl: /* [] */0
                                          }
                                        }))
                              };
                      })
                  ],
                  tl: {
                    hd: [
                      "fromArray - []",
                      (function (param) {
                          return {
                                  TAG: /* Eq */0,
                                  _0: {},
                                  _1: Js_dict.fromArray([])
                                };
                        })
                    ],
                    tl: {
                      hd: [
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
                      tl: {
                        hd: [
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
    }
  }
};

var suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Js_dict_test", suites);

exports.obj = obj;
exports.suites = suites;
/*  Not a pure module */
