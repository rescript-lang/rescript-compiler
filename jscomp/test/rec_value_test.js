// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var CamlinternalLazy = require("../stdlib/camlinternalLazy");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Mt = require("./mt");
var Caml_primitive = require("../runtime/caml_primitive");
var List = require("../stdlib/list");

var x = [];

Caml_primitive.caml_update_dummy(x, [
      /* :: */0,
      1,
      x
    ]);

var a = [];

var b = [];

var c = [];

Caml_primitive.caml_update_dummy(a, [
      /* :: */0,
      2,
      b
    ]);

Caml_primitive.caml_update_dummy(b, [
      /* :: */0,
      3,
      c
    ]);

Caml_primitive.caml_update_dummy(c, [
      /* :: */0,
      3,
      a
    ]);

var xx = [];

Caml_primitive.caml_update_dummy(xx, [
      /* :: */0,
      1,
      xx
    ]);

function naive(n) {
  return 1 < (n >>> 0) ? n + naive(n - 1) + naive(n - 2) : 1;
}

var one = 1;

var four = [
  0,
  2
];

var three = [
  0,
  3
];

var v = [
  0,
  function () {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "rec_value_test.ml",
            23,
            24
          ]
        ];
  }
];

var h = [];

Caml_primitive.caml_update_dummy(h, [
      250,
      fib
    ]);

function fib(n) {
  if (3 < (n >>> 0)) {
    return fib(n - 1) + fib(n - 2);
  }
  else {
    switch (n) {
      case 0 : 
          return four[1];
      case 1 : 
          return one;
      case 2 : 
          return three[1];
      case 3 : 
          var tag = Caml_obj_runtime.caml_obj_tag(h);
          v[1] = tag === 250 ? fib : (
              tag === 246 ? CamlinternalLazy.force_lazy_block(h) : h
            );
          return one;
      
    }
  }
}

var ys = [];

var xs = [];

Caml_primitive.caml_update_dummy(ys, [
      /* :: */0,
      1,
      ys
    ]);

function _zs() {
  return [
          /* tuple */0,
          List.hd(ys),
          List.hd(xs[1])
        ];
}

Caml_primitive.caml_update_dummy(xs, [
      /* tuple */0,
      [
        /* :: */0,
        2,
        [
          /* :: */0,
          List.hd(ys),
          /* [] */0
        ]
      ],
      _zs
    ]);

function zs() {
  return List.hd([
              /* :: */0,
              2,
              /* [] */0
            ]);
}

var xs$1 = [];

Caml_primitive.caml_update_dummy(xs$1, [
      /* tuple */0,
      [
        /* :: */0,
        2,
        /* [] */0
      ],
      zs
    ]);

var two = 2;

function fib2(n) {
  return 1 < (n >>> 0) ? fib2(n - 1) + fib2(n - 2) : 1;
}

function fib3(n) {
  return 1 < (n >>> 0) ? fib3(n - 1) + fib3(n - 2) : 1;
}

function odd(n) {
  return n === 1 ? /* true */1 : even(n - 1);
}

function even(n) {
  return n ? odd(n - 1) : /* true */1;
}

function even2(_n) {
  while(/* true */1) {
    var n = _n;
    if (n) {
      _n = n - 1;
    }
    else {
      return /* true */1;
    }
  };
}

function lazy_v() {
  var tag = Caml_obj_runtime.caml_obj_tag(lazy_v);
  return tag === 250 ? lazy_v[1] : (
            tag === 246 ? CamlinternalLazy.force_lazy_block(lazy_v) : lazy_v
          );
}

function sum(_acc, _n) {
  while(/* true */1) {
    var n = _n;
    var acc = _acc;
    if (n > 0) {
      _n = n - 1;
      _acc = acc + n;
    }
    else {
      return acc;
    }
  };
}

var fake_v = [
  /* :: */0,
  1,
  [
    /* :: */0,
    2,
    /* [] */0
  ]
];

var fake_y = [
  /* :: */0,
  2,
  [
    /* :: */0,
    3,
    /* [] */0
  ]
];

var fake_z = [];

Caml_primitive.caml_update_dummy(fake_z, [
      /* :: */0,
      1,
      fake_y
    ]);

var fake_y2 = [
  /* :: */0,
  2,
  [
    /* :: */0,
    3,
    /* [] */0
  ]
];

var fake_z2 = [];

Caml_primitive.caml_update_dummy(fake_z2, [
      /* :: */0,
      1,
      [
        /* :: */0,
        sum(0, 10),
        fake_y2
      ]
    ]);

var v$1 = 3;

var suites_001 = [
  /* tuple */0,
  "hd",
  function () {
    return [
            /* Eq */0,
            1,
            List.hd(List.tl(x))
          ];
  }
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "mutual",
    function () {
      var $js;
      var exit = 0;
      if (a) {
        if (b) {
          var match = b[2];
          if (match) {
            var match$1 = match[2];
            if (match$1) {
              var match$2 = match$1[2];
              match$2 ? ($js = match$2[1]) : (exit = 1);
            }
            else {
              exit = 1;
            }
          }
          else {
            exit = 1;
          }
        }
        else {
          exit = 1;
        }
      }
      else {
        exit = 1;
      }
      if (exit === 1) {
        throw [
              0,
              Caml_exceptions.Assert_failure,
              [
                0,
                "rec_value_test.ml",
                97,
                2
              ]
            ];
      }
      return [
              /* Eq */0,
              3,
              $js
            ];
    }
  ],
  [
    /* :: */0,
    [
      /* tuple */0,
      "rec_sum",
      function () {
        return [
                /* Eq */0,
                55,
                sum(0, 10)
              ];
      }
    ],
    [
      /* :: */0,
      [
        /* tuple */0,
        "fake_rec",
        function () {
          return [
                  /* Eq */0,
                  [
                    /* tuple */0,
                    [
                      /* :: */0,
                      1,
                      [
                        /* :: */0,
                        2,
                        /* [] */0
                      ]
                    ],
                    [
                      /* :: */0,
                      2,
                      [
                        /* :: */0,
                        3,
                        /* [] */0
                      ]
                    ],
                    [
                      /* :: */0,
                      1,
                      [
                        /* :: */0,
                        2,
                        [
                          /* :: */0,
                          3,
                          /* [] */0
                        ]
                      ]
                    ],
                    [
                      /* :: */0,
                      1,
                      [
                        /* :: */0,
                        55,
                        [
                          /* :: */0,
                          2,
                          [
                            /* :: */0,
                            3,
                            /* [] */0
                          ]
                        ]
                      ]
                    ],
                    [
                      /* :: */0,
                      2,
                      [
                        /* :: */0,
                        3,
                        /* [] */0
                      ]
                    ],
                    3
                  ],
                  [
                    /* tuple */0,
                    fake_v,
                    fake_y,
                    fake_z,
                    fake_z2,
                    fake_y2,
                    v$1
                  ]
                ];
        }
      ],
      /* [] */0
    ]
  ]
];

var suites = [
  /* :: */0,
  suites_001,
  suites_002
];

Mt.from_pair_suites("rec_value_test.ml", suites);

exports.x = x;
exports.a = a;
exports.b = b;
exports.c = c;
exports.xx = xx;
exports.naive = naive;
exports.fib = fib;
exports.xs = xs$1;
exports.fib2 = fib2;
exports.two = two;
exports.fib3 = fib3;
exports.even = even;
exports.even2 = even2;
exports.lazy_v = lazy_v;
exports.sum = sum;
exports.fake_v = fake_v;
exports.fake_y = fake_y;
exports.fake_z = fake_z;
exports.fake_z2 = fake_z2;
exports.fake_y2 = fake_y2;
exports.v = v$1;
exports.suites = suites;
/* xs Not a pure module */
