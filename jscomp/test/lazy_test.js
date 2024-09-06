// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Lazy = require("../../lib/js/lazy.js");
let CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");

let u = {
  contents: 3
};

let v = CamlinternalLazy.from_fun(() => {
  u.contents = 32;
});

function lazy_test() {
  let h = u.contents;
  CamlinternalLazy.force(v);
  let g = u.contents;
  return [
    h,
    g
  ];
}

let u_v = {
  contents: 0
};

let u$1 = CamlinternalLazy.from_fun(() => {
  u_v.contents = 2;
});

CamlinternalLazy.force(u$1);

let exotic = CamlinternalLazy.force;

let l_from_fun = CamlinternalLazy.from_fun(() => 3);

function f() {
  let u = 3;
  u = u + 1 | 0;
  return u;
}

let forward_test = CamlinternalLazy.from_fun(() => f());

let f005 = CamlinternalLazy.from_fun(() => 6);

let f006 = CamlinternalLazy.from_fun(() => (() => 3));

let f007 = CamlinternalLazy.from_fun(() => {
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
});

function f$1() {
  console.log("hi");
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
}

let f008 = CamlinternalLazy.from_fun(() => f$1());

let a2 = CamlinternalLazy.from_val;

let a3 = CamlinternalLazy.from_val(3);

let a4 = CamlinternalLazy.from_val(3);

let a5 = CamlinternalLazy.from_val(undefined);

let a6 = CamlinternalLazy.from_val();

let a7 = CamlinternalLazy.force(a5);

let a8 = CamlinternalLazy.force(a6);

Mt.from_pair_suites("Lazy_test", {
  hd: [
    "simple",
    () => ({
      TAG: "Eq",
      _0: lazy_test(),
      _1: [
        3,
        32
      ]
    })
  ],
  tl: {
    hd: [
      "lazy_force",
      () => ({
        TAG: "Eq",
        _0: u_v.contents,
        _1: 2
      })
    ],
    tl: {
      hd: [
        "lazy_from_fun",
        () => ({
          TAG: "Eq",
          _0: CamlinternalLazy.force(l_from_fun),
          _1: 3
        })
      ],
      tl: {
        hd: [
          "lazy_from_val",
          () => ({
            TAG: "Eq",
            _0: CamlinternalLazy.force(CamlinternalLazy.from_val(3)),
            _1: 3
          })
        ],
        tl: {
          hd: [
            "lazy_from_val2",
            () => ({
              TAG: "Eq",
              _0: CamlinternalLazy.force(CamlinternalLazy.force(CamlinternalLazy.from_val(CamlinternalLazy.from_fun(() => 3)))),
              _1: 3
            })
          ],
          tl: {
            hd: [
              "lazy_from_val3",
              () => {
                debugger;
                return {
                  TAG: "Eq",
                  _0: CamlinternalLazy.force(CamlinternalLazy.force(CamlinternalLazy.from_val(forward_test))),
                  _1: 4
                };
              }
            ],
            tl: {
              hd: [
                "lazy_test.res",
                () => ({
                  TAG: "Eq",
                  _0: a3,
                  _1: a4
                })
              ],
              tl: {
                hd: [
                  "lazy_test.res",
                  () => ({
                    TAG: "Eq",
                    _0: a7,
                    _1: undefined
                  })
                ],
                tl: {
                  hd: [
                    "lazy_test.res",
                    () => ({
                      TAG: "Eq",
                      _0: a8,
                      _1: undefined
                    })
                  ],
                  tl: {
                    hd: [
                      "File \"lazy_test.res\", line 98, characters 7-14",
                      () => ({
                        TAG: "Ok",
                        _0: Lazy.is_val(CamlinternalLazy.from_val(3))
                      })
                    ],
                    tl: {
                      hd: [
                        "File \"lazy_test.res\", line 99, characters 7-14",
                        () => ({
                          TAG: "Ok",
                          _0: !Lazy.is_val(CamlinternalLazy.from_fun(() => {
                            throw {
                              RE_EXN_ID: "Not_found",
                              Error: new Error()
                            };
                          }))
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
});

exports.v = v;
exports.lazy_test = lazy_test;
exports.u_v = u_v;
exports.u = u$1;
exports.exotic = exotic;
exports.l_from_fun = l_from_fun;
exports.forward_test = forward_test;
exports.f005 = f005;
exports.f006 = f006;
exports.f007 = f007;
exports.f008 = f008;
exports.a2 = a2;
exports.a3 = a3;
exports.a4 = a4;
exports.a5 = a5;
exports.a6 = a6;
exports.a7 = a7;
exports.a8 = a8;
/*  Not a pure module */
