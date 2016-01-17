// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var CamlinternalLazy = require("./camlinternalLazy");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Pervasives = require("./pervasives");
var List = require("./list");

var Failure = [
  248,
  "Stream.Failure",
  ++ Caml_exceptions.caml_oo_last_id
];

var $$Error = [
  248,
  "Stream.Error",
  ++ Caml_exceptions.caml_oo_last_id
];

function set_data(s, d) {
  s[1] = d;
  return /* () */0;
}

function fill_buff(b) {
  b[3] = Pervasives.input(b[1], b[2], 0, b[2].length);
  b[4] = 0;
  return /* () */0;
}

function get_data(count, _d) {
  while(/* true */1) {
    var d = _d;
    if (typeof d === "number") {
      return d;
    }
    else {
      switch (d[0]) {
        case 0 : 
            return d;
        case 1 : 
            var d2 = d[2];
            var match = get_data(count, d[1]);
            if (typeof match === "number") {
              if (match) {
                throw [
                      0,
                      Caml_exceptions.Assert_failure,
                      [
                        0,
                        "stream.ml",
                        53,
                        12
                      ]
                    ];
              }
              else {
                _d = d2;
              }
            }
            else {
              if (match[0]) {
                throw [
                      0,
                      Caml_exceptions.Assert_failure,
                      [
                        0,
                        "stream.ml",
                        53,
                        12
                      ]
                    ];
              }
              else {
                return [
                        /* Scons */0,
                        match[1],
                        [
                          /* Sapp */1,
                          match[2],
                          d2
                        ]
                      ];
              }
            }
            break;
        case 2 : 
            var f = d[1];
            var tag = Caml_obj_runtime.caml_obj_tag(f);
            _d = tag === 250 ? f[1] : (
                tag === 246 ? CamlinternalLazy.force_lazy_block(f) : f
              );
            break;
        case 3 : 
            var g = d[1];
            var match$1 = g[1];
            if (match$1) {
              var match$2 = match$1[1];
              return match$2 ? (g[1] = /* None */0, [
                          /* Scons */0,
                          match$2[1],
                          d
                        ]) : /* Sempty */0;
            }
            else {
              var match$3 = g[2](count);
              return match$3 ? [
                        /* Scons */0,
                        match$3[1],
                        d
                      ] : (g[1] = [
                          /* Some */0,
                          /* None */0
                        ], /* Sempty */0);
            }
            break;
        case 4 : 
            var b = d[1];
            if (b[4] >= b[3]) {
              fill_buff(b);
            }
            if (b[3]) {
              var r = b[2][b[4]];
              ++ b[4];
              return [
                      /* Scons */0,
                      r,
                      d
                    ];
            }
            else {
              return /* Sempty */0;
            }
            break;
        
      }
    }
  };
}

function peek(s) {
  while(/* true */1) {
    var match = s[2];
    if (typeof match === "number") {
      return /* None */0;
    }
    else {
      switch (match[0]) {
        case 0 : 
            return [
                    /* Some */0,
                    match[1]
                  ];
        case 1 : 
            var d = get_data(s[1], s[2]);
            if (typeof d === "number") {
              if (d) {
                throw [
                      0,
                      Caml_exceptions.Assert_failure,
                      [
                        0,
                        "stream.ml",
                        82,
                        12
                      ]
                    ];
              }
              else {
                return /* None */0;
              }
            }
            else {
              if (d[0]) {
                throw [
                      0,
                      Caml_exceptions.Assert_failure,
                      [
                        0,
                        "stream.ml",
                        82,
                        12
                      ]
                    ];
              }
              else {
                set_data(s, d);
                return [
                        /* Some */0,
                        d[1]
                      ];
              }
            }
            break;
        case 2 : 
            var f = match[1];
            var tag = Caml_obj_runtime.caml_obj_tag(f);
            set_data(s, tag === 250 ? f[1] : (
                    tag === 246 ? CamlinternalLazy.force_lazy_block(f) : f
                  ));
            break;
        case 3 : 
            var g = match[1];
            var match$1 = g[1];
            if (match$1) {
              return match$1[1];
            }
            else {
              var x = g[2](s[1]);
              g[1] = [
                /* Some */0,
                x
              ];
              return x;
            }
            break;
        case 4 : 
            var b = match[1];
            if (b[4] >= b[3]) {
              fill_buff(b);
            }
            return b[3] ? [
                      /* Some */0,
                      b[2][b[4]]
                    ] : (set_data(s, /* Sempty */0), /* None */0);
        
      }
    }
  };
}

function junk(s) {
  while(/* true */1) {
    var match = s[2];
    var exit = 0;
    if (typeof match === "number") {
      exit = 1;
    }
    else {
      switch (match[0]) {
        case 0 : 
            ++ s[1];
            return set_data(s, match[2]);
        case 3 : 
            var g = match[1];
            var match$1 = g[1];
            if (match$1) {
              ++ s[1];
              g[1] = /* None */0;
              return /* () */0;
            }
            else {
              exit = 1;
            }
            break;
        case 4 : 
            var b = match[1];
            ++ s[1];
            ++ b[4];
            return /* () */0;
        default:
          exit = 1;
      }
    }
    if (exit === 1) {
      var match$2 = peek(s);
      if (!match$2) {
        return /* () */0;
      }
      
    }
    
  };
}

function nget(n, s) {
  if (n <= 0) {
    return [
            /* tuple */0,
            /* [] */0,
            s[2],
            0
          ];
  }
  else {
    var match = peek(s);
    if (match) {
      var a = match[1];
      junk(s);
      var match$1 = nget(-1 + n, s);
      return [
              /* tuple */0,
              [
                /* :: */0,
                a,
                match$1[1]
              ],
              [
                /* Scons */0,
                a,
                match$1[2]
              ],
              1 + match$1[3]
            ];
    }
    else {
      return [
              /* tuple */0,
              /* [] */0,
              s[2],
              0
            ];
    }
  }
}

function npeek(n, s) {
  var match = nget(n, s);
  s[1] -= match[3];
  set_data(s, match[2]);
  return match[1];
}

function next(s) {
  var match = peek(s);
  if (match) {
    junk(s);
    return match[1];
  }
  else {
    throw Failure;
  }
}

function empty(s) {
  var match = peek(s);
  if (match) {
    throw Failure;
  }
  else {
    return /* () */0;
  }
}

function iter(f, strm) {
  var do_rec = function (_param) {
    while(/* true */1) {
      var match = peek(strm);
      if (match) {
        junk(strm);
        f(match[1]);
        _param = /* () */0;
      }
      else {
        return /* () */0;
      }
    };
  };
  return do_rec(/* () */0);
}

function from(f) {
  return [
          /* record */0,
          0,
          [
            /* Sgen */3,
            [
              /* record */0,
              /* None */0,
              f
            ]
          ]
        ];
}

function of_list(l) {
  return [
          /* record */0,
          0,
          List.fold_right(function (x, l) {
                return [
                        /* Scons */0,
                        x,
                        l
                      ];
              }, l, /* Sempty */0)
        ];
}

function of_string(s) {
  var count = [
    0,
    0
  ];
  return from(function () {
              var c = count[1];
              return c < s.length ? (++ count[1], [
                          /* Some */0,
                          s.charCodeAt(c)
                        ]) : /* None */0;
            });
}

function of_bytes(s) {
  var count = [
    0,
    0
  ];
  return from(function () {
              var c = count[1];
              return c < s.length ? (++ count[1], [
                          /* Some */0,
                          s[c]
                        ]) : /* None */0;
            });
}

function of_channel(ic) {
  return [
          /* record */0,
          0,
          [
            /* Sbuffio */4,
            [
              /* record */0,
              ic,
              new Array(4096),
              0,
              0
            ]
          ]
        ];
}

function iapp(i, s) {
  return [
          /* record */0,
          0,
          [
            /* Sapp */1,
            i[2],
            s[2]
          ]
        ];
}

function icons(i, s) {
  return [
          /* record */0,
          0,
          [
            /* Scons */0,
            i,
            s[2]
          ]
        ];
}

function ising(i) {
  return [
          /* record */0,
          0,
          [
            /* Scons */0,
            i,
            /* Sempty */0
          ]
        ];
}

function lapp(f, s) {
  return [
          /* record */0,
          0,
          [
            /* Slazy */2,
            [
              246,
              function () {
                return [
                        /* Sapp */1,
                        f(/* () */0)[2],
                        s[2]
                      ];
              }
            ]
          ]
        ];
}

function lcons(f, s) {
  return [
          /* record */0,
          0,
          [
            /* Slazy */2,
            [
              246,
              function () {
                return [
                        /* Scons */0,
                        f(/* () */0),
                        s[2]
                      ];
              }
            ]
          ]
        ];
}

function lsing(f) {
  return [
          /* record */0,
          0,
          [
            /* Slazy */2,
            [
              246,
              function () {
                return [
                        /* Scons */0,
                        f(/* () */0),
                        /* Sempty */0
                      ];
              }
            ]
          ]
        ];
}

function slazy(f) {
  return [
          /* record */0,
          0,
          [
            /* Slazy */2,
            [
              246,
              function () {
                return f(/* () */0)[2];
              }
            ]
          ]
        ];
}

function dump(f, s) {
  Pervasives.print_string("{count = ");
  Pervasives.print_int(s[1]);
  Pervasives.print_string("; data = ");
  dump_data(f, s[2]);
  Pervasives.print_string("}");
  return Pervasives.print_newline(/* () */0);
}

function dump_data(f, param) {
  if (typeof param === "number") {
    return Pervasives.print_string("Sempty");
  }
  else {
    switch (param[0]) {
      case 0 : 
          Pervasives.print_string("Scons (");
          f(param[1]);
          Pervasives.print_string(", ");
          dump_data(f, param[2]);
          return Pervasives.print_string(")");
      case 1 : 
          Pervasives.print_string("Sapp (");
          dump_data(f, param[1]);
          Pervasives.print_string(", ");
          dump_data(f, param[2]);
          return Pervasives.print_string(")");
      case 2 : 
          return Pervasives.print_string("Slazy");
      case 3 : 
          return Pervasives.print_string("Sgen");
      case 4 : 
          return Pervasives.print_string("Sbuffio");
      
    }
  }
}

function count(prim) {
  return prim[1];
}

var sempty = [
  /* record */0,
  0,
  /* Sempty */0
];

exports.Failure = Failure;
exports.$$Error = $$Error;
exports.from = from;
exports.of_list = of_list;
exports.of_string = of_string;
exports.of_bytes = of_bytes;
exports.of_channel = of_channel;
exports.iter = iter;
exports.next = next;
exports.empty = empty;
exports.peek = peek;
exports.junk = junk;
exports.count = count;
exports.npeek = npeek;
exports.iapp = iapp;
exports.icons = icons;
exports.ising = ising;
exports.lapp = lapp;
exports.lcons = lcons;
exports.lsing = lsing;
exports.sempty = sempty;
exports.slazy = slazy;
exports.dump = dump;
/* No side effect */
