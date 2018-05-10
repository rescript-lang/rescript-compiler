'use strict';

var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");

function for_3(x) {
  var v = [0];
  var arr = $$Array.map((function (_, _$1) {
          return /* () */0;
        }), x);
  for(var i = 0 ,i_finish = x.length - 1 | 0; i <= i_finish; ++i){
    var j = (i << 1);
    Caml_array.caml_array_set(arr, i, (function(j){
        return function () {
          v[0] = v[0] + j | 0;
          return /* () */0;
        }
        }(j)));
  }
  $$Array.iter((function (x) {
          return Curry._1(x, /* () */0);
        }), arr);
  return v[0];
}

function for_4(x) {
  var v = [0];
  var arr = $$Array.map((function (_, _$1) {
          return /* () */0;
        }), x);
  for(var i = 0 ,i_finish = x.length - 1 | 0; i <= i_finish; ++i){
    var j = (i << 1);
    var k = (j << 1);
    Caml_array.caml_array_set(arr, i, (function(k){
        return function () {
          v[0] = v[0] + k | 0;
          return /* () */0;
        }
        }(k)));
  }
  $$Array.iter((function (x) {
          return Curry._1(x, /* () */0);
        }), arr);
  return v[0];
}

function for_5(x, u) {
  var v = [0];
  var arr = $$Array.map((function (_, _$1) {
          return /* () */0;
        }), x);
  for(var i = 0 ,i_finish = x.length - 1 | 0; i <= i_finish; ++i){
    var k = Caml_int32.imul((u << 1), u);
    Caml_array.caml_array_set(arr, i, (function(k){
        return function () {
          v[0] = v[0] + k | 0;
          return /* () */0;
        }
        }(k)));
  }
  $$Array.iter((function (x) {
          return Curry._1(x, /* () */0);
        }), arr);
  return v[0];
}

function for_6(x, u) {
  var v = [0];
  var arr = $$Array.map((function (_, _$1) {
          return /* () */0;
        }), x);
  var v4 = [0];
  var v5 = [0];
  var inspect_3 = -1;
  v4[0] = v4[0] + 1 | 0;
  for(var j = 0; j <= 1; ++j){
    v5[0] = v5[0] + 1 | 0;
    var v2 = [0];
    (function(v2){
    for(var i = 0 ,i_finish = x.length - 1 | 0; i <= i_finish; ++i){
      var k = Caml_int32.imul((u << 1), u);
      var h = (v5[0] << 1);
      v2[0] = v2[0] + 1 | 0;
      Caml_array.caml_array_set(arr, i, (function(k,h){
          return function () {
            v[0] = (((((v[0] + k | 0) + v2[0] | 0) + v4[0] | 0) + v5[0] | 0) + h | 0) + u | 0;
            return /* () */0;
          }
          }(k,h)));
    }
    }(v2));
    inspect_3 = v2[0];
  }
  $$Array.iter((function (x) {
          return Curry._1(x, /* () */0);
        }), arr);
  return /* array */[
          v[0],
          v4[0],
          v5[0],
          inspect_3
        ];
}

function for_7() {
  var v = [0];
  var arr = Caml_array.caml_make_vect(21, (function () {
          return /* () */0;
        }));
  for(var i = 0; i <= 6; ++i){
    (function(i){
    for(var j = 0; j <= 2; ++j){
      Caml_array.caml_array_set(arr, Caml_int32.imul(i, 3) + j | 0, (function(j){
          return function () {
            v[0] = (v[0] + i | 0) + j | 0;
            return /* () */0;
          }
          }(j)));
    }
    }(i));
  }
  $$Array.iter((function (f) {
          return Curry._1(f, /* () */0);
        }), arr);
  return v[0];
}

function for_8() {
  var v = [0];
  var arr = Caml_array.caml_make_vect(21, (function () {
          return /* () */0;
        }));
  for(var i = 0; i <= 6; ++i){
    var k = (i << 1);
    (function(i,k){
    for(var j = 0; j <= 2; ++j){
      var h = i + j | 0;
      Caml_array.caml_array_set(arr, Caml_int32.imul(i, 3) + j | 0, (function(j,h){
          return function () {
            v[0] = (((v[0] + i | 0) + j | 0) + h | 0) + k | 0;
            return /* () */0;
          }
          }(j,h)));
    }
    }(i,k));
  }
  $$Array.iter((function (f) {
          return Curry._1(f, /* () */0);
        }), arr);
  return v[0];
}

function for_9() {
  var v = [/* [] */0];
  var collect = function (x) {
    v[0] = /* :: */[
      x,
      v[0]
    ];
    return /* () */0;
  };
  var vv = [0];
  var vv2 = [0];
  var arr = Caml_array.caml_make_vect(4, (function () {
          return /* () */0;
        }));
  var arr2 = Caml_array.caml_make_vect(2, (function () {
          return /* () */0;
        }));
  for(var i = 0; i <= 1; ++i){
    var v$1 = [0];
    v$1[0] = v$1[0] + i | 0;
    (function(v$1){
    for(var j = 0; j <= 1; ++j){
      v$1[0] = v$1[0] + 1 | 0;
      collect(v$1[0]);
      Caml_array.caml_array_set(arr, (i << 1) + j | 0, (function () {
              vv[0] = vv[0] + v$1[0] | 0;
              return /* () */0;
            }));
    }
    }(v$1));
    Caml_array.caml_array_set(arr2, i, (function(v$1){
        return function () {
          vv2[0] = vv2[0] + v$1[0] | 0;
          return /* () */0;
        }
        }(v$1)));
  }
  $$Array.iter((function (f) {
          return Curry._1(f, /* () */0);
        }), arr);
  $$Array.iter((function (f) {
          return Curry._1(f, /* () */0);
        }), arr2);
  return /* array */[/* tuple */[
            vv[0],
            $$Array.of_list(List.rev(v[0])),
            vv2[0]
          ]];
}

var suites_000 = /* tuple */[
  "for_loop_test_3",
  (function () {
      return /* Eq */Block.__(0, [
                90,
                for_3(Caml_array.caml_make_vect(10, 2))
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "for_loop_test_4",
    (function () {
        return /* Eq */Block.__(0, [
                  180,
                  for_4(Caml_array.caml_make_vect(10, 2))
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "for_loop_test_5",
      (function () {
          return /* Eq */Block.__(0, [
                    2420,
                    for_5(Caml_array.caml_make_vect(10, 2), 11)
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "for_loop_test_6",
        (function () {
            return /* Eq */Block.__(0, [
                      /* array */[
                        30,
                        1,
                        2,
                        3
                      ],
                      for_6(Caml_array.caml_make_vect(3, 0), 0)
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "for_loop_test_7",
          (function () {
              return /* Eq */Block.__(0, [
                        84,
                        for_7(/* () */0)
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "for_loop_test_8",
            (function () {
                return /* Eq */Block.__(0, [
                          294,
                          for_8(/* () */0)
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "for_loop_test_9",
              (function () {
                  return /* Eq */Block.__(0, [
                            /* array */[/* tuple */[
                                10,
                                /* array */[
                                  1,
                                  2,
                                  2,
                                  3
                                ],
                                5
                              ]],
                            for_9(/* () */0)
                          ]);
                })
            ],
            /* [] */0
          ]
        ]
      ]
    ]
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

exports.for_3 = for_3;
exports.for_4 = for_4;
exports.for_5 = for_5;
exports.for_6 = for_6;
exports.for_7 = for_7;
exports.for_8 = for_8;
exports.for_9 = for_9;
exports.suites = suites;
/* No side effect */
