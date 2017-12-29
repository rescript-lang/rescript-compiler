'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function height(param) {
  if (param) {
    return param[4];
  } else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* Node */[
          l,
          x,
          d,
          r,
          hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function bal(l, x, d, r) {
  var hl = l ? l[4] : 0;
  var hr = r ? r[4] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[3];
      var ld = l[2];
      var lv = l[1];
      var ll = l[0];
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      } else if (lr) {
        return create(create(ll, lv, ld, lr[0]), lr[1], lr[2], create(lr[3], x, d, r));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r) {
      var rr = r[3];
      var rd = r[2];
      var rv = r[1];
      var rl = r[0];
      if (height(rr) >= height(rl)) {
        return create(create(l, x, d, rl), rv, rd, rr);
      } else if (rl) {
        return create(create(l, x, d, rl[0]), rl[1], rl[2], create(rl[3], rv, rd, rr));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  } else {
    return /* Node */[
            l,
            x,
            d,
            r,
            hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function add(x, data, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = Caml_primitive.caml_int_compare(x, v);
    if (c) {
      if (c < 0) {
        return bal(add(x, data, l), v, d, r);
      } else {
        return bal(l, v, d, add(x, data, r));
      }
    } else {
      return /* Node */[
              l,
              x,
              data,
              r,
              param[4]
            ];
    }
  } else {
    return /* Node */[
            /* Empty */0,
            x,
            data,
            /* Empty */0,
            1
          ];
  }
}

function cons_enum(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (m) {
      _e = /* More */[
        m[1],
        m[2],
        m[3],
        e
      ];
      _m = m[0];
      continue ;
      
    } else {
      return e;
    }
  };
}

function compare(cmp, m1, m2) {
  var _e1 = cons_enum(m1, /* End */0);
  var _e2 = cons_enum(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var c = Caml_primitive.caml_int_compare(e1[0], e2[0]);
        if (c !== 0) {
          return c;
        } else {
          var c$1 = Curry._2(cmp, e1[1], e2[1]);
          if (c$1 !== 0) {
            return c$1;
          } else {
            _e2 = cons_enum(e2[2], e2[3]);
            _e1 = cons_enum(e1[2], e1[3]);
            continue ;
            
          }
        }
      } else {
        return 1;
      }
    } else if (e2) {
      return -1;
    } else {
      return 0;
    }
  };
}

function equal(cmp, m1, m2) {
  var _e1 = cons_enum(m1, /* End */0);
  var _e2 = cons_enum(m2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        if (e1[0] === e2[0]) {
          if (Curry._2(cmp, e1[1], e2[1])) {
            _e2 = cons_enum(e2[2], e2[3]);
            _e1 = cons_enum(e1[2], e1[3]);
            continue ;
            
          } else {
            return /* false */0;
          }
        } else {
          return /* false */0;
        }
      } else {
        return /* false */0;
      }
    } else if (e2) {
      return /* false */0;
    } else {
      return /* true */1;
    }
  };
}

function cardinal(param) {
  if (param) {
    return (cardinal(param[0]) + 1 | 0) + cardinal(param[3]) | 0;
  } else {
    return 0;
  }
}

function height$1(param) {
  if (param) {
    return param[4];
  } else {
    return 0;
  }
}

function create$1(l, x, d, r) {
  var hl = height$1(l);
  var hr = height$1(r);
  return /* Node */[
          l,
          x,
          d,
          r,
          hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function bal$1(l, x, d, r) {
  var hl = l ? l[4] : 0;
  var hr = r ? r[4] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[3];
      var ld = l[2];
      var lv = l[1];
      var ll = l[0];
      if (height$1(ll) >= height$1(lr)) {
        return create$1(ll, lv, ld, create$1(lr, x, d, r));
      } else if (lr) {
        return create$1(create$1(ll, lv, ld, lr[0]), lr[1], lr[2], create$1(lr[3], x, d, r));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r) {
      var rr = r[3];
      var rd = r[2];
      var rv = r[1];
      var rl = r[0];
      if (height$1(rr) >= height$1(rl)) {
        return create$1(create$1(l, x, d, rl), rv, rd, rr);
      } else if (rl) {
        return create$1(create$1(l, x, d, rl[0]), rl[1], rl[2], create$1(rl[3], rv, rd, rr));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  } else {
    return /* Node */[
            l,
            x,
            d,
            r,
            hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function add$1(x, data, param) {
  if (param) {
    var r = param[3];
    var d = param[2];
    var v = param[1];
    var l = param[0];
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c) {
      if (c < 0) {
        return bal$1(add$1(x, data, l), v, d, r);
      } else {
        return bal$1(l, v, d, add$1(x, data, r));
      }
    } else {
      return /* Node */[
              l,
              x,
              data,
              r,
              param[4]
            ];
    }
  } else {
    return /* Node */[
            /* Empty */0,
            x,
            data,
            /* Empty */0,
            1
          ];
  }
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_primitive.caml_string_compare(x, param[1]);
      if (c) {
        _param = c < 0 ? param[0] : param[3];
        continue ;
        
      } else {
        return param[2];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function of_list(kvs) {
  return List.fold_left((function (acc, param) {
                return add(param[0], param[1], acc);
              }), /* Empty */0, kvs);
}

var int_map_suites_000 = /* tuple */[
  "add",
  (function () {
      var v = of_list(/* :: */[
            /* tuple */[
              1,
              /* "1" */49
            ],
            /* :: */[
              /* tuple */[
                2,
                /* "3" */51
              ],
              /* :: */[
                /* tuple */[
                  3,
                  /* "4" */52
                ],
                /* [] */0
              ]
            ]
          ]);
      return /* Eq */Block.__(0, [
                cardinal(v),
                3
              ]);
    })
];

var int_map_suites_001 = /* :: */[
  /* tuple */[
    "equal",
    (function () {
        var v = of_list(/* :: */[
              /* tuple */[
                1,
                /* "1" */49
              ],
              /* :: */[
                /* tuple */[
                  2,
                  /* "3" */51
                ],
                /* :: */[
                  /* tuple */[
                    3,
                    /* "4" */52
                  ],
                  /* [] */0
                ]
              ]
            ]);
        var u = of_list(/* :: */[
              /* tuple */[
                2,
                /* "3" */51
              ],
              /* :: */[
                /* tuple */[
                  3,
                  /* "4" */52
                ],
                /* :: */[
                  /* tuple */[
                    1,
                    /* "1" */49
                  ],
                  /* [] */0
                ]
              ]
            ]);
        return /* Eq */Block.__(0, [
                  compare(Caml_obj.caml_compare, u, v),
                  0
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "equal2",
      (function () {
          var v = of_list(/* :: */[
                /* tuple */[
                  1,
                  /* "1" */49
                ],
                /* :: */[
                  /* tuple */[
                    2,
                    /* "3" */51
                  ],
                  /* :: */[
                    /* tuple */[
                      3,
                      /* "4" */52
                    ],
                    /* [] */0
                  ]
                ]
              ]);
          var u = of_list(/* :: */[
                /* tuple */[
                  2,
                  /* "3" */51
                ],
                /* :: */[
                  /* tuple */[
                    3,
                    /* "4" */52
                  ],
                  /* :: */[
                    /* tuple */[
                      1,
                      /* "1" */49
                    ],
                    /* [] */0
                  ]
                ]
              ]);
          return /* Eq */Block.__(0, [
                    /* true */1,
                    equal((function (x, y) {
                            return +(x === y);
                          }), u, v)
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "iteration",
        (function () {
            var m = /* Empty */0;
            for(var i = 0; i <= 10000; ++i){
              m = add$1("" + i, "" + i, m);
            }
            var v = -1;
            for(var i$1 = 0; i$1 <= 10000; ++i$1){
              if (find("" + i$1, m) !== "" + i$1) {
                v = i$1;
              }
              
            }
            return /* Eq */Block.__(0, [
                      v,
                      -1
                    ]);
          })
      ],
      /* [] */0
    ]
  ]
];

var int_map_suites = /* :: */[
  int_map_suites_000,
  int_map_suites_001
];

Mt.from_pair_suites("map_test.ml", int_map_suites);

/*  Not a pure module */
