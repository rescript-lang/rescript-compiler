'use strict';

var Caml_obj                = require("../caml_obj");
var Caml_builtin_exceptions = require("../caml_builtin_exceptions");
var CamlinternalLazy        = require("../camlinternalLazy");
var Block                   = require("../block");
var List                    = require("../list");

var x = {
  
};

Caml_obj.caml_update_dummy(x, /* :: */[
      1,
      x
    ]);

var a = {
  
};

var b = {
  
};

var c = {
  
};

Caml_obj.caml_update_dummy(a, /* :: */[
      2,
      b
    ]);

Caml_obj.caml_update_dummy(b, /* :: */[
      3,
      c
    ]);

Caml_obj.caml_update_dummy(c, /* :: */[
      3,
      a
    ]);

var xx = {
  
};

Caml_obj.caml_update_dummy(xx, /* :: */[
      1,
      xx
    ]);

function naive(n) {
  if (n === 0 || n === 1) {
    return 1;
  }
  else {
    return (n + naive(n - 1 | 0) | 0) + naive(n - 2 | 0) | 0;
  }
}

var four = [2];

var three = [3];

var v = [function () {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "rec_value_test.ml",
            23,
            24
          ]
        ];
  }];

var h = {
  
};

Caml_obj.caml_update_dummy(h, Block.__(250, [fib]));

function fib(n) {
  if (n > 3 || n < 0) {
    return fib(n - 1 | 0) + fib(n - 2 | 0) | 0;
  }
  else {
    switch (n) {
      case 0 : 
          return four[0];
      case 1 : 
          return 1;
      case 2 : 
          return three[0];
      case 3 : 
          var tag = h.tag | 0;
          v[0] = tag === 250 ? fib : (
              tag === 246 ? CamlinternalLazy.force_lazy_block(h) : h
            );
          return 1;
      
    }
  }
}

var ys = {
  
};

var xs = {
  
};

Caml_obj.caml_update_dummy(ys, /* :: */[
      1,
      ys
    ]);

function _zs() {
  return /* tuple */[
          List.hd(ys),
          List.hd(xs[0])
        ];
}

Caml_obj.caml_update_dummy(xs, /* tuple */[
      /* :: */[
        2,
        /* :: */[
          List.hd(ys),
          /* [] */0
        ]
      ],
      _zs
    ]);

function zs() {
  return List.hd(/* :: */[
              2,
              /* [] */0
            ]);
}

var xs$1 = {
  
};

Caml_obj.caml_update_dummy(xs$1, /* tuple */[
      /* :: */[
        2,
        /* [] */0
      ],
      zs
    ]);

var two = 2;

function fib2(n) {
  if (n === 0 || n === 1) {
    return 1;
  }
  else {
    return fib2(n - 1 | 0) + fib2(n - 2 | 0) | 0;
  }
}

function fib3(n) {
  if (n === 0 || n === 1) {
    return 1;
  }
  else {
    return fib3(n - 1 | 0) + fib3(n - 2 | 0) | 0;
  }
}

function even(n) {
  if (n) {
    var n$1 = n - 1 | 0;
    if (n$1 === 1) {
      return /* true */1;
    }
    else {
      return even(n$1 - 1 | 0);
    }
  }
  else {
    return /* true */1;
  }
}

function even2(_n) {
  while(true) {
    var n = _n;
    if (n) {
      _n = n - 1 | 0;
      continue ;
      
    }
    else {
      return /* true */1;
    }
  };
}

function lazy_v() {
  var tag = lazy_v.tag | 0;
  tag === 250 ? lazy_v[0] : (
      tag === 246 ? CamlinternalLazy.force_lazy_block(lazy_v) : lazy_v
    );
  return /* () */0;
}

function sum(_acc, _n) {
  while(true) {
    var n = _n;
    var acc = _acc;
    if (n > 0) {
      _n = n - 1 | 0;
      _acc = acc + n | 0;
      continue ;
      
    }
    else {
      return acc;
    }
  };
}

var fake_v = /* :: */[
  1,
  /* :: */[
    2,
    /* [] */0
  ]
];

var fake_y = /* :: */[
  2,
  /* :: */[
    3,
    /* [] */0
  ]
];

var fake_z = {
  
};

Caml_obj.caml_update_dummy(fake_z, /* :: */[
      1,
      fake_y
    ]);

var fake_y2 = /* :: */[
  2,
  /* :: */[
    3,
    /* [] */0
  ]
];

var fake_z2 = {
  
};

Caml_obj.caml_update_dummy(fake_z2, /* :: */[
      1,
      /* :: */[
        sum(0, 10),
        fake_y2
      ]
    ]);

var v$1 = 3;

var suites_000 = /* tuple */[
  "hd",
  function () {
    return /* Eq */Block.__(0, [
              1,
              List.hd(List.tl(x))
            ]);
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "mutual",
    function () {
      var $js;
      if (a) {
        var match = a[1];
        if (match) {
          $js = match[0];
        }
        else {
          throw [
                Caml_builtin_exceptions.assert_failure,
                [
                  "rec_value_test.ml",
                  97,
                  2
                ]
              ];
        }
      }
      else {
        throw [
              Caml_builtin_exceptions.assert_failure,
              [
                "rec_value_test.ml",
                97,
                2
              ]
            ];
      }
      return /* Eq */Block.__(0, [
                3,
                $js
              ]);
    }
  ],
  /* :: */[
    /* tuple */[
      "rec_sum",
      function () {
        return /* Eq */Block.__(0, [
                  55,
                  sum(0, 10)
                ]);
      }
    ],
    /* :: */[
      /* tuple */[
        "fake_rec",
        function () {
          return /* Eq */Block.__(0, [
                    /* tuple */[
                      /* :: */[
                        1,
                        /* :: */[
                          2,
                          /* [] */0
                        ]
                      ],
                      /* :: */[
                        2,
                        /* :: */[
                          3,
                          /* [] */0
                        ]
                      ],
                      /* :: */[
                        1,
                        /* :: */[
                          2,
                          /* :: */[
                            3,
                            /* [] */0
                          ]
                        ]
                      ],
                      /* :: */[
                        1,
                        /* :: */[
                          55,
                          /* :: */[
                            2,
                            /* :: */[
                              3,
                              /* [] */0
                            ]
                          ]
                        ]
                      ],
                      /* :: */[
                        2,
                        /* :: */[
                          3,
                          /* [] */0
                        ]
                      ],
                      3
                    ],
                    /* tuple */[
                      fake_v,
                      fake_y,
                      fake_z,
                      fake_z2,
                      fake_y2,
                      v$1
                    ]
                  ]);
        }
      ],
      /* [] */0
    ]
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

exports.x       = x;
exports.a       = a;
exports.b       = b;
exports.c       = c;
exports.xx      = xx;
exports.naive   = naive;
exports.fib     = fib;
exports.xs      = xs$1;
exports.fib2    = fib2;
exports.two     = two;
exports.fib3    = fib3;
exports.even    = even;
exports.even2   = even2;
exports.lazy_v  = lazy_v;
exports.sum     = sum;
exports.fake_v  = fake_v;
exports.fake_y  = fake_y;
exports.fake_z  = fake_z;
exports.fake_z2 = fake_z2;
exports.fake_y2 = fake_y2;
exports.v       = v$1;
exports.suites  = suites;
/* xs Not a pure module */
