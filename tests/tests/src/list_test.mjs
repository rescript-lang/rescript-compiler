// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Mt from "./mt.mjs";
import * as Belt_List from "rescript/lib/es6/Belt_List.js";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Primitive_int from "rescript/lib/es6/Primitive_int.js";
import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";

let eq = Primitive_object.equal;

let list_suites_0 = [
  "length",
  param => ({
    TAG: "Eq",
    _0: 1,
    _1: Belt_List.length({
      hd: [
        0,
        1,
        2,
        3,
        4
      ],
      tl: /* [] */0
    })
  })
];

let list_suites_1 = {
  hd: [
    "length2",
    param => ({
      TAG: "Eq",
      _0: 5,
      _1: Belt_List.length({
        hd: 0,
        tl: {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: /* [] */0
              }
            }
          }
        }
      })
    })
  ],
  tl: {
    hd: [
      "long_length",
      param => ({
        TAG: "Eq",
        _0: 30000,
        _1: Belt_List.length(Belt_List.fromArray(Belt_Array.init(30000, param => 0)))
      })
    ],
    tl: {
      hd: [
        "sort",
        param => ({
          TAG: "Eq",
          _0: Belt_List.sort({
            hd: 4,
            tl: {
              hd: 1,
              tl: {
                hd: 2,
                tl: {
                  hd: 3,
                  tl: /* [] */0
                }
              }
            }
          }, Primitive_int.compare),
          _1: {
            hd: 1,
            tl: {
              hd: 2,
              tl: {
                hd: 3,
                tl: {
                  hd: 4,
                  tl: /* [] */0
                }
              }
            }
          }
        })
      ],
      tl: {
        hd: [
          "File \"list_test.res\", line 23, characters 5-12",
          param => ({
            TAG: "Eq",
            _0: true,
            _1: Belt_List.has({
              hd: 1,
              tl: {
                hd: 2,
                tl: {
                  hd: 3,
                  tl: /* [] */0
                }
              }
            }, 3, eq)
          })
        ],
        tl: {
          hd: [
            "File \"list_test.res\", line 24, characters 5-12",
            param => ({
              TAG: "Eq",
              _0: false,
              _1: Belt_List.has({
                hd: 1,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 3,
                    tl: /* [] */0
                  }
                }
              }, 4, eq)
            })
          ],
          tl: {
            hd: [
              "File \"list_test.res\", line 25, characters 5-12",
              param => ({
                TAG: "Eq",
                _0: 9,
                _1: Belt_List.getAssoc({
                  hd: [
                    1,
                    2
                  ],
                  tl: {
                    hd: [
                      4,
                      9
                    ],
                    tl: /* [] */0
                  }
                }, 4, eq)
              })
            ],
            tl: /* [] */0
          }
        }
      }
    }
  }
};

let list_suites = {
  hd: list_suites_0,
  tl: list_suites_1
};

Mt.from_pair_suites("List_test", list_suites);

export {
  list_suites,
}
/*  Not a pure module */
