'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function height(param) {
  if (param) {
    return param[/* h */4];
  } else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* Node */[
          /* l */l,
          /* v */x,
          /* d */d,
          /* r */r,
          /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function bal(l, x, d, r) {
  var hl = l ? l[/* h */4] : 0;
  var hr = r ? r[/* h */4] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[/* r */3];
      var ld = l[/* d */2];
      var lv = l[/* v */1];
      var ll = l[/* l */0];
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      } else if (lr) {
        return create(create(ll, lv, ld, lr[/* l */0]), lr[/* v */1], lr[/* d */2], create(lr[/* r */3], x, d, r));
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
      var rr = r[/* r */3];
      var rd = r[/* d */2];
      var rv = r[/* v */1];
      var rl = r[/* l */0];
      if (height(rr) >= height(rl)) {
        return create(create(l, x, d, rl), rv, rd, rr);
      } else if (rl) {
        return create(create(l, x, d, rl[/* l */0]), rl[/* v */1], rl[/* d */2], create(rl[/* r */3], rv, rd, rr));
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
            /* l */l,
            /* v */x,
            /* d */d,
            /* r */r,
            /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function add(x, data, m) {
  if (m) {
    var r = m[/* r */3];
    var d = m[/* d */2];
    var v = m[/* v */1];
    var l = m[/* l */0];
    var c = Caml_primitive.caml_int_compare(x, v);
    if (c === 0) {
      if (d === data) {
        return m;
      } else {
        return /* Node */[
                /* l */l,
                /* v */x,
                /* d */data,
                /* r */r,
                /* h */m[/* h */4]
              ];
      }
    } else if (c < 0) {
      var ll = add(x, data, l);
      if (l === ll) {
        return m;
      } else {
        return bal(ll, v, d, r);
      }
    } else {
      var rr = add(x, data, r);
      if (r === rr) {
        return m;
      } else {
        return bal(l, v, d, rr);
      }
    }
  } else {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* d */data,
            /* r : Empty */0,
            /* h */1
          ];
  }
}

function cons_enum(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (m) {
      _e = /* More */[
        m[/* v */1],
        m[/* d */2],
        m[/* r */3],
        e
      ];
      _m = m[/* l */0];
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
      if (e2 && e1[0] === e2[0] && Curry._2(cmp, e1[1], e2[1])) {
        _e2 = cons_enum(e2[2], e2[3]);
        _e1 = cons_enum(e1[2], e1[3]);
        continue ;
      } else {
        return false;
      }
    } else if (e2) {
      return false;
    } else {
      return true;
    }
  };
}

function cardinal(param) {
  if (param) {
    return (cardinal(param[/* l */0]) + 1 | 0) + cardinal(param[/* r */3]) | 0;
  } else {
    return 0;
  }
}

function height$1(param) {
  if (param) {
    return param[/* h */4];
  } else {
    return 0;
  }
}

function create$1(l, x, d, r) {
  var hl = height$1(l);
  var hr = height$1(r);
  return /* Node */[
          /* l */l,
          /* v */x,
          /* d */d,
          /* r */r,
          /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function bal$1(l, x, d, r) {
  var hl = l ? l[/* h */4] : 0;
  var hr = r ? r[/* h */4] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[/* r */3];
      var ld = l[/* d */2];
      var lv = l[/* v */1];
      var ll = l[/* l */0];
      if (height$1(ll) >= height$1(lr)) {
        return create$1(ll, lv, ld, create$1(lr, x, d, r));
      } else if (lr) {
        return create$1(create$1(ll, lv, ld, lr[/* l */0]), lr[/* v */1], lr[/* d */2], create$1(lr[/* r */3], x, d, r));
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
      var rr = r[/* r */3];
      var rd = r[/* d */2];
      var rv = r[/* v */1];
      var rl = r[/* l */0];
      if (height$1(rr) >= height$1(rl)) {
        return create$1(create$1(l, x, d, rl), rv, rd, rr);
      } else if (rl) {
        return create$1(create$1(l, x, d, rl[/* l */0]), rl[/* v */1], rl[/* d */2], create$1(rl[/* r */3], rv, rd, rr));
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
            /* l */l,
            /* v */x,
            /* d */d,
            /* r */r,
            /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function add$1(x, data, m) {
  if (m) {
    var r = m[/* r */3];
    var d = m[/* d */2];
    var v = m[/* v */1];
    var l = m[/* l */0];
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      if (d === data) {
        return m;
      } else {
        return /* Node */[
                /* l */l,
                /* v */x,
                /* d */data,
                /* r */r,
                /* h */m[/* h */4]
              ];
      }
    } else if (c < 0) {
      var ll = add$1(x, data, l);
      if (l === ll) {
        return m;
      } else {
        return bal$1(ll, v, d, r);
      }
    } else {
      var rr = add$1(x, data, r);
      if (r === rr) {
        return m;
      } else {
        return bal$1(l, v, d, rr);
      }
    }
  } else {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* d */data,
            /* r : Empty */0,
            /* h */1
          ];
  }
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_primitive.caml_string_compare(x, param[/* v */1]);
      if (c === 0) {
        return param[/* d */2];
      } else {
        _param = c < 0 ? param[/* l */0] : param[/* r */3];
        continue ;
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
  (function (param) {
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
    (function (param) {
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
                  compare(Caml_primitive.caml_int_compare, u, v),
                  0
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "equal2",
      (function (param) {
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
                    true,
                    equal((function (x, y) {
                            return x === y;
                          }), u, v)
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "iteration",
        (function (param) {
            var m = /* Empty */0;
            for(var i = 0; i <= 10000; ++i){
              m = add$1(String(i), String(i), m);
            }
            var v = -1;
            for(var i$1 = 0; i$1 <= 10000; ++i$1){
              if (find(String(i$1), m) !== String(i$1)) {
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

Mt.from_pair_suites("Map_test", int_map_suites);

/*  Not a pure module */
