// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Mt from "./mt.mjs";
import * as Primitive_option from "rescript/lib/es6/Primitive_option.js";
import * as Js_null_undefined from "rescript/lib/es6/Js_null_undefined.js";

let suites_0 = [
  "toOption - null",
  param => ({
    TAG: "Eq",
    _0: undefined,
    _1: undefined
  })
];

let suites_1 = {
  hd: [
    "toOption - undefined",
    param => ({
      TAG: "Eq",
      _0: undefined,
      _1: undefined
    })
  ],
  tl: {
    hd: [
      "toOption - empty",
      param => ({
        TAG: "Eq",
        _0: undefined,
        _1: undefined
      })
    ],
    tl: {
      hd: [
        "File \"js_null_undefined_test.res\", line 9, characters 5-12",
        param => ({
          TAG: "Eq",
          _0: "foo",
          _1: Primitive_option.fromNullable("foo")
        })
      ],
      tl: {
        hd: [
          "return",
          param => ({
            TAG: "Eq",
            _0: "something",
            _1: Primitive_option.fromNullable("something")
          })
        ],
        tl: {
          hd: [
            "test - null",
            param => ({
              TAG: "Eq",
              _0: true,
              _1: true
            })
          ],
          tl: {
            hd: [
              "test - undefined",
              param => ({
                TAG: "Eq",
                _0: true,
                _1: true
              })
            ],
            tl: {
              hd: [
                "test - empty",
                param => ({
                  TAG: "Eq",
                  _0: true,
                  _1: true
                })
              ],
              tl: {
                hd: [
                  "File \"js_null_undefined_test.res\", line 14, characters 5-12",
                  param => ({
                    TAG: "Eq",
                    _0: true,
                    _1: true
                  })
                ],
                tl: {
                  hd: [
                    "bind - null",
                    param => ({
                      TAG: "StrictEq",
                      _0: null,
                      _1: Js_null_undefined.bind(null, v => v)
                    })
                  ],
                  tl: {
                    hd: [
                      "bind - undefined",
                      param => ({
                        TAG: "StrictEq",
                        _0: undefined,
                        _1: Js_null_undefined.bind(undefined, v => v)
                      })
                    ],
                    tl: {
                      hd: [
                        "bind - empty",
                        param => ({
                          TAG: "StrictEq",
                          _0: undefined,
                          _1: Js_null_undefined.bind(undefined, v => v)
                        })
                      ],
                      tl: {
                        hd: [
                          "bind - 'a",
                          param => ({
                            TAG: "Eq",
                            _0: 4,
                            _1: Js_null_undefined.bind(2, n => (n << 1))
                          })
                        ],
                        tl: {
                          hd: [
                            "iter - null",
                            param => {
                              let hit = {
                                contents: false
                              };
                              Js_null_undefined.iter(null, param => {
                                hit.contents = true;
                              });
                              return {
                                TAG: "Eq",
                                _0: false,
                                _1: hit.contents
                              };
                            }
                          ],
                          tl: {
                            hd: [
                              "iter - undefined",
                              param => {
                                let hit = {
                                  contents: false
                                };
                                Js_null_undefined.iter(undefined, param => {
                                  hit.contents = true;
                                });
                                return {
                                  TAG: "Eq",
                                  _0: false,
                                  _1: hit.contents
                                };
                              }
                            ],
                            tl: {
                              hd: [
                                "iter - empty",
                                param => {
                                  let hit = {
                                    contents: false
                                  };
                                  Js_null_undefined.iter(undefined, param => {
                                    hit.contents = true;
                                  });
                                  return {
                                    TAG: "Eq",
                                    _0: false,
                                    _1: hit.contents
                                  };
                                }
                              ],
                              tl: {
                                hd: [
                                  "iter - 'a",
                                  param => {
                                    let hit = {
                                      contents: 0
                                    };
                                    Js_null_undefined.iter(2, v => {
                                      hit.contents = v;
                                    });
                                    return {
                                      TAG: "Eq",
                                      _0: 2,
                                      _1: hit.contents
                                    };
                                  }
                                ],
                                tl: {
                                  hd: [
                                    "fromOption - None",
                                    param => ({
                                      TAG: "Eq",
                                      _0: undefined,
                                      _1: Js_null_undefined.fromOption(undefined)
                                    })
                                  ],
                                  tl: {
                                    hd: [
                                      "fromOption - Some",
                                      param => ({
                                        TAG: "Eq",
                                        _0: 2,
                                        _1: Js_null_undefined.fromOption(2)
                                      })
                                    ],
                                    tl: {
                                      hd: [
                                        "null <> undefined",
                                        param => ({
                                          TAG: "Ok",
                                          _0: true
                                        })
                                      ],
                                      tl: {
                                        hd: [
                                          "null <> empty",
                                          param => ({
                                            TAG: "Ok",
                                            _0: true
                                          })
                                        ],
                                        tl: {
                                          hd: [
                                            "undefined = empty",
                                            param => ({
                                              TAG: "Ok",
                                              _0: true
                                            })
                                          ],
                                          tl: {
                                            hd: [
                                              "File \"js_null_undefined_test.res\", line 57, characters 6-13",
                                              param => ({
                                                TAG: "Ok",
                                                _0: true
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

let suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Js_null_undefined_test", suites);

export {
  suites,
}
/*  Not a pure module */