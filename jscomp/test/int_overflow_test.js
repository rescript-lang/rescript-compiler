// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Primitive_string = require("../../lib/js/primitive_string.js");

function hash_variant(s) {
  let accu = 0;
  for (let i = 0, i_finish = s.length; i < i_finish; ++i) {
    accu = Math.imul(223, accu) + Primitive_string.get(s, i) & 2147483647;
  }
  if (accu > 1073741823) {
    return accu - -2147483648 | 0;
  } else {
    return accu;
  }
}

function hash_variant2(s) {
  let accu = 0;
  for (let i = 0, i_finish = s.length; i < i_finish; ++i) {
    accu = Math.imul(223, accu) + Primitive_string.get(s, i) | 0;
  }
  accu = accu & 2147483647;
  if (accu > 1073741823) {
    return accu - -2147483648 | 0;
  } else {
    return accu;
  }
}

function fib(x) {
  if (x === 0 || x === 1) {
    return 1;
  } else {
    return fib(x - 1 | 0) + fib(x - 2 | 0) | 0;
  }
}

Mt.from_pair_suites("Int_overflow_test", {
  hd: [
    "plus_overflow",
    () => ({
      TAG: "Eq",
      _0: true,
      _1: true
    })
  ],
  tl: {
    hd: [
      "minus_overflow",
      () => ({
        TAG: "Eq",
        _0: true,
        _1: true
      })
    ],
    tl: {
      hd: [
        "flow_again",
        () => ({
          TAG: "Eq",
          _0: 2147483646,
          _1: 2147483646
        })
      ],
      tl: {
        hd: [
          "flow_again",
          () => ({
            TAG: "Eq",
            _0: -2,
            _1: -2
          })
        ],
        tl: {
          hd: [
            "hash_test",
            () => ({
              TAG: "Eq",
              _0: hash_variant("xxyyzzuuxxzzyy00112233"),
              _1: 544087776
            })
          ],
          tl: {
            hd: [
              "hash_test2",
              () => ({
                TAG: "Eq",
                _0: hash_variant("xxyyzxzzyy"),
                _1: -449896130
              })
            ],
            tl: {
              hd: [
                "File \"int_overflow_test.res\", line 72, characters 5-12",
                () => ({
                  TAG: "Eq",
                  _0: hash_variant2("xxyyzzuuxxzzyy00112233"),
                  _1: 544087776
                })
              ],
              tl: {
                hd: [
                  "File \"int_overflow_test.res\", line 73, characters 5-12",
                  () => ({
                    TAG: "Eq",
                    _0: hash_variant2("xxyyzxzzyy"),
                    _1: -449896130
                  })
                ],
                tl: {
                  hd: [
                    "int_literal_flow",
                    () => ({
                      TAG: "Eq",
                      _0: -1,
                      _1: -1
                    })
                  ],
                  tl: {
                    hd: [
                      "int_literal_flow2",
                      () => ({
                        TAG: "Eq",
                        _0: -1,
                        _1: -1
                      })
                    ],
                    tl: {
                      hd: [
                        "File \"int_overflow_test.res\", line 76, characters 5-12",
                        () => ({
                          TAG: "Eq",
                          _0: Number("3") | 0,
                          _1: 3
                        })
                      ],
                      tl: {
                        hd: [
                          "File \"int_overflow_test.res\", line 78, characters 5-12",
                          () => ({
                            TAG: "Eq",
                            _0: Number("3.2") | 0,
                            _1: 3
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
});

let max_int = 2147483647;

let min_int = -2147483648;

exports.max_int = max_int;
exports.min_int = min_int;
exports.hash_variant = hash_variant;
exports.hash_variant2 = hash_variant2;
exports.fib = fib;
/*  Not a pure module */
