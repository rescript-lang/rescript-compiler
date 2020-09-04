'use strict';

var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");

function for_3(x) {
  var v = {
    contents: 0
  };
  var arr = $$Array.map((function (param, param$1) {
          
        }), x);
  for(var i = 0 ,i_finish = x.length; i < i_finish; ++i){
    var j = (i << 1);
    Caml_array.set(arr, i, (function(j){
        return function (param) {
          v.contents = v.contents + j | 0;
          
        }
        }(j)));
  }
  $$Array.iter((function (x) {
          return Curry._1(x, undefined);
        }), arr);
  return v.contents;
}

function for_4(x) {
  var v = {
    contents: 0
  };
  var arr = $$Array.map((function (param, param$1) {
          
        }), x);
  for(var i = 0 ,i_finish = x.length; i < i_finish; ++i){
    var j = (i << 1);
    var k = (j << 1);
    Caml_array.set(arr, i, (function(k){
        return function (param) {
          v.contents = v.contents + k | 0;
          
        }
        }(k)));
  }
  $$Array.iter((function (x) {
          return Curry._1(x, undefined);
        }), arr);
  return v.contents;
}

function for_5(x, u) {
  var v = {
    contents: 0
  };
  var arr = $$Array.map((function (param, param$1) {
          
        }), x);
  for(var i = 0 ,i_finish = x.length; i < i_finish; ++i){
    var k = Math.imul((u << 1), u);
    Caml_array.set(arr, i, (function(k){
        return function (param) {
          v.contents = v.contents + k | 0;
          
        }
        }(k)));
  }
  $$Array.iter((function (x) {
          return Curry._1(x, undefined);
        }), arr);
  return v.contents;
}

function for_6(x, u) {
  var v = {
    contents: 0
  };
  var arr = $$Array.map((function (param, param$1) {
          
        }), x);
  var v4 = {
    contents: 0
  };
  var v5 = {
    contents: 0
  };
  var inspect_3 = -1;
  v4.contents = v4.contents + 1 | 0;
  for(var j = 0; j <= 1; ++j){
    v5.contents = v5.contents + 1 | 0;
    var v2 = {
      contents: 0
    };
    (function(v2){
    for(var i = 0 ,i_finish = x.length; i < i_finish; ++i){
      var k = Math.imul((u << 1), u);
      var h = (v5.contents << 1);
      v2.contents = v2.contents + 1 | 0;
      Caml_array.set(arr, i, (function(k,h){
          return function (param) {
            v.contents = (((((v.contents + k | 0) + v2.contents | 0) + v4.contents | 0) + v5.contents | 0) + h | 0) + u | 0;
            
          }
          }(k,h)));
    }
    }(v2));
    inspect_3 = v2.contents;
  }
  $$Array.iter((function (x) {
          return Curry._1(x, undefined);
        }), arr);
  return [
          v.contents,
          v4.contents,
          v5.contents,
          inspect_3
        ];
}

function for_7(param) {
  var v = {
    contents: 0
  };
  var arr = Caml_array.caml_make_vect(21, (function (param) {
          
        }));
  for(var i = 0; i <= 6; ++i){
    (function(i){
    for(var j = 0; j <= 2; ++j){
      Caml_array.set(arr, Math.imul(i, 3) + j | 0, (function(j){
          return function (param) {
            v.contents = (v.contents + i | 0) + j | 0;
            
          }
          }(j)));
    }
    }(i));
  }
  $$Array.iter((function (f) {
          return Curry._1(f, undefined);
        }), arr);
  return v.contents;
}

function for_8(param) {
  var v = {
    contents: 0
  };
  var arr = Caml_array.caml_make_vect(21, (function (param) {
          
        }));
  for(var i = 0; i <= 6; ++i){
    var k = (i << 1);
    (function(i,k){
    for(var j = 0; j <= 2; ++j){
      var h = i + j | 0;
      Caml_array.set(arr, Math.imul(i, 3) + j | 0, (function(j,h){
          return function (param) {
            v.contents = (((v.contents + i | 0) + j | 0) + h | 0) + k | 0;
            
          }
          }(j,h)));
    }
    }(i,k));
  }
  $$Array.iter((function (f) {
          return Curry._1(f, undefined);
        }), arr);
  return v.contents;
}

function for_9(param) {
  var v = {
    contents: /* [] */0
  };
  var collect = function (x) {
    v.contents = {
      hd: x,
      tl: v.contents
    };
    
  };
  var vv = {
    contents: 0
  };
  var vv2 = {
    contents: 0
  };
  var arr = Caml_array.caml_make_vect(4, (function (param) {
          
        }));
  var arr2 = Caml_array.caml_make_vect(2, (function (param) {
          
        }));
  for(var i = 0; i <= 1; ++i){
    var v$1 = {
      contents: 0
    };
    v$1.contents = v$1.contents + i | 0;
    (function(v$1){
    for(var j = 0; j <= 1; ++j){
      v$1.contents = v$1.contents + 1 | 0;
      collect(v$1.contents);
      Caml_array.set(arr, (i << 1) + j | 0, (function (param) {
              vv.contents = vv.contents + v$1.contents | 0;
              
            }));
    }
    }(v$1));
    Caml_array.set(arr2, i, (function(v$1){
        return function (param) {
          vv2.contents = vv2.contents + v$1.contents | 0;
          
        }
        }(v$1)));
  }
  $$Array.iter((function (f) {
          return Curry._1(f, undefined);
        }), arr);
  $$Array.iter((function (f) {
          return Curry._1(f, undefined);
        }), arr2);
  return [[
            vv.contents,
            $$Array.of_list(List.rev(v.contents)),
            vv2.contents
          ]];
}

var suites_0 = [
  "for_loop_test_3",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: 90,
              _1: for_3(Caml_array.caml_make_vect(10, 2))
            };
    })
];

var suites_1 = {
  hd: [
    "for_loop_test_4",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: 180,
                _1: for_4(Caml_array.caml_make_vect(10, 2))
              };
      })
  ],
  tl: {
    hd: [
      "for_loop_test_5",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: 2420,
                  _1: for_5(Caml_array.caml_make_vect(10, 2), 11)
                };
        })
    ],
    tl: {
      hd: [
        "for_loop_test_6",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: [
                      30,
                      1,
                      2,
                      3
                    ],
                    _1: for_6(Caml_array.caml_make_vect(3, 0), 0)
                  };
          })
      ],
      tl: {
        hd: [
          "for_loop_test_7",
          (function (param) {
              return {
                      TAG: /* Eq */0,
                      _0: 84,
                      _1: for_7(undefined)
                    };
            })
        ],
        tl: {
          hd: [
            "for_loop_test_8",
            (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: 294,
                        _1: for_8(undefined)
                      };
              })
          ],
          tl: {
            hd: [
              "for_loop_test_9",
              (function (param) {
                  return {
                          TAG: /* Eq */0,
                          _0: [[
                              10,
                              [
                                1,
                                2,
                                2,
                                3
                              ],
                              5
                            ]],
                          _1: for_9(undefined)
                        };
                })
            ],
            tl: /* [] */0
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

exports.for_3 = for_3;
exports.for_4 = for_4;
exports.for_5 = for_5;
exports.for_6 = for_6;
exports.for_7 = for_7;
exports.for_8 = for_8;
exports.for_9 = for_9;
exports.suites = suites;
/* No side effect */
