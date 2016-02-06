// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_array = require("../runtime/caml_array");
var $$Array    = require("../stdlib/array");
var Caml_curry = require("../runtime/caml_curry");
var List       = require("../stdlib/list");

function for_3(x) {
  var v = [
    0,
    0
  ];
  var arr = $$Array.map(function (_, _$1) {
        return /* () */0;
      }, x);
  for(var i = 0 ,i_finish = x.length - 1; i<= i_finish; ++i){
    var j = i * 2;
    arr[i] = (function(j){
    return function () {
      v[1] += j;
      return /* () */0;
    }
    }(j));
  }
  $$Array.iter(function (x) {
        return Caml_curry.app1(x, /* () */0);
      }, arr);
  return v[1];
}

function for_4(x) {
  var v = [
    0,
    0
  ];
  var arr = $$Array.map(function (_, _$1) {
        return /* () */0;
      }, x);
  for(var i = 0 ,i_finish = x.length - 1; i<= i_finish; ++i){
    var j = i * 2;
    var k = 2 * j;
    arr[i] = (function(k){
    return function () {
      v[1] += k;
      return /* () */0;
    }
    }(k));
  }
  $$Array.iter(function (x) {
        return Caml_curry.app1(x, /* () */0);
      }, arr);
  return v[1];
}

function for_5(x, u) {
  var v = [
    0,
    0
  ];
  var arr = $$Array.map(function (_, _$1) {
        return /* () */0;
      }, x);
  for(var i = 0 ,i_finish = x.length - 1; i<= i_finish; ++i){
    var k = 2 * u * u;
    arr[i] = (function(k){
    return function () {
      v[1] += k;
      return /* () */0;
    }
    }(k));
  }
  $$Array.iter(function (x) {
        return Caml_curry.app1(x, /* () */0);
      }, arr);
  return v[1];
}

function for_6(x, u) {
  var v = [
    0,
    0
  ];
  var arr = $$Array.map(function (_, _$1) {
        return /* () */0;
      }, x);
  var v4 = [
    0,
    0
  ];
  var v5 = [
    0,
    0
  ];
  var inspect_3 = -1;
  ++ v4[1];
  for(var j = 0; j<= 1; ++j){
    ++ v5[1];
    var v2 = [
      0,
      0
    ];
    (function(v2){
    for(var i = 0 ,i_finish = x.length - 1; i<= i_finish; ++i){
      var k = 2 * u * u;
      var h = 2 * v5[1];
      ++ v2[1];
      arr[i] = (function(k,h){
      return function () {
        v[1] = v[1] + k + v2[1] + v4[1] + v5[1] + h + u;
        return /* () */0;
      }
      }(k,h));
    }
    }(v2));
    inspect_3 = v2[1];
  }
  $$Array.iter(function (x) {
        return Caml_curry.app1(x, /* () */0);
      }, arr);
  return /* array */[
          v[1],
          v4[1],
          v5[1],
          inspect_3
        ];
}

function for_7() {
  var i_len = 7;
  var j_len = 3;
  var v = [
    0,
    0
  ];
  var arr = Caml_array.caml_make_vect(i_len * j_len, function () {
        return /* () */0;
      });
  for(var i = 0 ,i_finish = i_len - 1; i<= i_finish; ++i){
    (function(i){
    for(var j = 0 ,j_finish = j_len - 1; j<= j_finish; ++j){
      arr[i * j_len + j] = (function(j){
      return function () {
        v[1] = v[1] + i + j;
        return /* () */0;
      }
      }(j));
    }
    }(i));
  }
  $$Array.iter(function (f) {
        return Caml_curry.app1(f, /* () */0);
      }, arr);
  return v[1];
}

function for_8() {
  var i_len = 7;
  var j_len = 3;
  var v = [
    0,
    0
  ];
  var arr = Caml_array.caml_make_vect(i_len * j_len, function () {
        return /* () */0;
      });
  for(var i = 0 ,i_finish = i_len - 1; i<= i_finish; ++i){
    var k = 2 * i;
    (function(i,k){
    for(var j = 0 ,j_finish = j_len - 1; j<= j_finish; ++j){
      var h = i + j;
      arr[i * j_len + j] = (function(j,h){
      return function () {
        v[1] = v[1] + i + j + h + k;
        return /* () */0;
      }
      }(j,h));
    }
    }(i,k));
  }
  $$Array.iter(function (f) {
        return Caml_curry.app1(f, /* () */0);
      }, arr);
  return v[1];
}

function for_9() {
  var v = [
    0,
    /* [] */0
  ];
  var match_001 = function (x) {
    v[1] = [
      /* :: */0,
      x,
      v[1]
    ];
    return /* () */0;
  };
  var match_002 = function () {
    return $$Array.of_list(List.rev(v[1]));
  };
  var collect = match_001;
  var i_len = 2;
  var j_len = 2;
  var vv = [
    0,
    0
  ];
  var vv2 = [
    0,
    0
  ];
  var arr = Caml_array.caml_make_vect(i_len * j_len, function () {
        return /* () */0;
      });
  var arr2 = Caml_array.caml_make_vect(i_len, function () {
        return /* () */0;
      });
  for(var i = 0 ,i_finish = i_len - 1; i<= i_finish; ++i){
    var v$1 = [
      0,
      0
    ];
    v$1[1] += i;
    (function(v$1){
    for(var j = 0 ,j_finish = j_len - 1; j<= j_finish; ++j){
      ++ v$1[1];
      Caml_curry.app1(collect, v$1[1]);
      arr[i * j_len + j] = function () {
        vv[1] += v$1[1];
        return /* () */0;
      };
    }
    }(v$1));
    arr2[i] = (function(v$1){
    return function () {
      vv2[1] += v$1[1];
      return /* () */0;
    }
    }(v$1));
  }
  $$Array.iter(function (f) {
        return Caml_curry.app1(f, /* () */0);
      }, arr);
  $$Array.iter(function (f) {
        return Caml_curry.app1(f, /* () */0);
      }, arr2);
  return /* array */[[
            /* tuple */0,
            vv[1],
            Caml_curry.app1(match_002, /* () */0),
            vv2[1]
          ]];
}

var suites_001 = [
  /* tuple */0,
  "for_loop_test_3",
  function () {
    return [
            /* Eq */0,
            90,
            for_3(Caml_array.caml_make_vect(10, 2))
          ];
  }
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "for_loop_test_4",
    function () {
      return [
              /* Eq */0,
              180,
              for_4(Caml_array.caml_make_vect(10, 2))
            ];
    }
  ],
  [
    /* :: */0,
    [
      /* tuple */0,
      "for_loop_test_5",
      function () {
        return [
                /* Eq */0,
                2420,
                for_5(Caml_array.caml_make_vect(10, 2), 11)
              ];
      }
    ],
    [
      /* :: */0,
      [
        /* tuple */0,
        "for_loop_test_6",
        function () {
          return [
                  /* Eq */0,
                  /* array */[
                    30,
                    1,
                    2,
                    3
                  ],
                  for_6(Caml_array.caml_make_vect(3, 0), 0)
                ];
        }
      ],
      [
        /* :: */0,
        [
          /* tuple */0,
          "for_loop_test_7",
          function () {
            return [
                    /* Eq */0,
                    84,
                    for_7(/* () */0)
                  ];
          }
        ],
        [
          /* :: */0,
          [
            /* tuple */0,
            "for_loop_test_8",
            function () {
              return [
                      /* Eq */0,
                      294,
                      for_8(/* () */0)
                    ];
            }
          ],
          [
            /* :: */0,
            [
              /* tuple */0,
              "for_loop_test_9",
              function () {
                return [
                        /* Eq */0,
                        /* array */[[
                            /* tuple */0,
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
                      ];
              }
            ],
            /* [] */0
          ]
        ]
      ]
    ]
  ]
];

var suites = [
  /* :: */0,
  suites_001,
  suites_002
];

exports.for_3  = for_3;
exports.for_4  = for_4;
exports.for_5  = for_5;
exports.for_6  = for_6;
exports.for_7  = for_7;
exports.for_8  = for_8;
exports.for_9  = for_9;
exports.suites = suites;
/* No side effect */
