// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var CamlinternalLazy        = require("./camlinternalLazy");
var Pervasives              = require("./pervasives");
var Caml_curry              = require("../runtime/caml_curry");
var List                    = require("./list");

var Failure = {
  0: "Stream.Failure",
  1: ++ Caml_builtin_exceptions.caml_oo_last_id,
  length: 2,
  tag: 248
};

var $$Error = {
  0: "Stream.Error",
  1: ++ Caml_builtin_exceptions.caml_oo_last_id,
  length: 2,
  tag: 248
};

function fill_buff(b) {
  b[2] = Pervasives.input(b[0], b[1], 0, b[1].length);
  b[3] = 0;
  return /* () */0;
}

function get_data(count, _d) {
  while(true) {
    var d = _d;
    if (typeof d === "number") {
      return d;
    }
    else {
      switch (d.tag | 0) {
        case 0 : 
            return d;
        case 1 : 
            var d2 = d[1];
            var match = get_data(count, d[0]);
            if (typeof match === "number") {
              if (match) {
                throw [
                      Caml_builtin_exceptions.Assert_failure,
                      [
                        "stream.ml",
                        53,
                        12
                      ]
                    ];
              }
              else {
                _d = d2;
                continue ;
                
              }
            }
            else if (match.tag) {
              throw [
                    Caml_builtin_exceptions.Assert_failure,
                    [
                      "stream.ml",
                      53,
                      12
                    ]
                  ];
            }
            else {
              return /* Scons */{
                      0: match[0],
                      1: /* Sapp */{
                        0: match[1],
                        1: d2,
                        length: 2,
                        tag: 1
                      },
                      length: 2,
                      tag: 0
                    };
            }
            break;
        case 2 : 
            var f = d[0];
            var tag = f.tag | 0;
            _d = tag === 250 ? f[0] : (
                tag === 246 ? CamlinternalLazy.force_lazy_block(f) : f
              );
            continue ;
            case 3 : 
            var g = d[0];
            var match$1 = g[0];
            if (match$1) {
              var match$2 = match$1[0];
              if (match$2) {
                g[0] = /* None */0;
                return /* Scons */{
                        0: match$2[0],
                        1: d,
                        length: 2,
                        tag: 0
                      };
              }
              else {
                return /* Sempty */0;
              }
            }
            else {
              var match$3 = Caml_curry.app1(g[1], count);
              if (match$3) {
                return /* Scons */{
                        0: match$3[0],
                        1: d,
                        length: 2,
                        tag: 0
                      };
              }
              else {
                g[0] = /* Some */[/* None */0];
                return /* Sempty */0;
              }
            }
            break;
        case 4 : 
            var b = d[0];
            if (b[3] >= b[2]) {
              fill_buff(b);
            }
            if (b[2]) {
              var r = b[1][b[3]];
              ++ b[3];
              return /* Scons */{
                      0: r,
                      1: d,
                      length: 2,
                      tag: 0
                    };
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
  while(true) {
    var match = s[1];
    if (typeof match === "number") {
      return /* None */0;
    }
    else {
      switch (match.tag | 0) {
        case 0 : 
            return /* Some */[match[0]];
        case 1 : 
            var d = get_data(s[0], s[1]);
            if (typeof d === "number") {
              if (d) {
                throw [
                      Caml_builtin_exceptions.Assert_failure,
                      [
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
            else if (d.tag) {
              throw [
                    Caml_builtin_exceptions.Assert_failure,
                    [
                      "stream.ml",
                      82,
                      12
                    ]
                  ];
            }
            else {
              s[1] = d;
              return /* Some */[d[0]];
            }
            break;
        case 2 : 
            var f = match[0];
            var tag = f.tag | 0;
            var d$1 = tag === 250 ? f[0] : (
                tag === 246 ? CamlinternalLazy.force_lazy_block(f) : f
              );
            s[1] = d$1;
            continue ;
            case 3 : 
            var g = match[0];
            var match$1 = g[0];
            if (match$1) {
              return match$1[0];
            }
            else {
              var x = Caml_curry.app1(g[1], s[0]);
              g[0] = /* Some */[x];
              return x;
            }
            break;
        case 4 : 
            var b = match[0];
            if (b[3] >= b[2]) {
              fill_buff(b);
            }
            if (b[2]) {
              return /* Some */[b[1][b[3]]];
            }
            else {
              s[1] = /* Sempty */0;
              return /* None */0;
            }
        
      }
    }
  };
}

function junk(s) {
  while(true) {
    var match = s[1];
    var exit = 0;
    if (typeof match === "number") {
      exit = 1;
    }
    else {
      switch (match.tag | 0) {
        case 0 : 
            ++ s[0];
            s[1] = match[1];
            return /* () */0;
        case 3 : 
            var g = match[0];
            var match$1 = g[0];
            if (match$1) {
              ++ s[0];
              g[0] = /* None */0;
              return /* () */0;
            }
            else {
              exit = 1;
            }
            break;
        case 4 : 
            var b = match[0];
            ++ s[0];
            ++ b[3];
            return /* () */0;
        default:
          exit = 1;
      }
    }
    if (exit === 1) {
      var match$2 = peek(s);
      if (match$2) {
        continue ;
        
      }
      else {
        return /* () */0;
      }
    }
    
  };
}

function nget(n, s) {
  if (n <= 0) {
    return /* tuple */[
            /* [] */0,
            s[1],
            0
          ];
  }
  else {
    var match = peek(s);
    if (match) {
      var a = match[0];
      junk(s);
      var match$1 = nget(n - 1, s);
      return /* tuple */[
              /* :: */[
                a,
                match$1[0]
              ],
              /* Scons */{
                0: a,
                1: match$1[1],
                length: 2,
                tag: 0
              },
              match$1[2] + 1
            ];
    }
    else {
      return /* tuple */[
              /* [] */0,
              s[1],
              0
            ];
    }
  }
}

function npeek(n, s) {
  var match = nget(n, s);
  s[0] -= match[2];
  s[1] = match[1];
  return match[0];
}

function next(s) {
  var match = peek(s);
  if (match) {
    junk(s);
    return match[0];
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
  var _param = /* () */0;
  while(true) {
    var match = peek(strm);
    if (match) {
      junk(strm);
      Caml_curry.app1(f, match[0]);
      _param = /* () */0;
      continue ;
      
    }
    else {
      return /* () */0;
    }
  };
}

function from(f) {
  return /* record */[
          0,
          /* Sgen */{
            0: /* record */[
              /* None */0,
              f
            ],
            length: 1,
            tag: 3
          }
        ];
}

function of_list(l) {
  return /* record */[
          0,
          List.fold_right(function (x, l) {
                return /* Scons */{
                        0: x,
                        1: l,
                        length: 2,
                        tag: 0
                      };
              }, l, /* Sempty */0)
        ];
}

function of_string(s) {
  var count = [0];
  return from(function () {
              var c = count[0];
              if (c < s.length) {
                ++ count[0];
                return /* Some */[s.charCodeAt(c)];
              }
              else {
                return /* None */0;
              }
            });
}

function of_bytes(s) {
  var count = [0];
  return from(function () {
              var c = count[0];
              if (c < s.length) {
                ++ count[0];
                return /* Some */[s[c]];
              }
              else {
                return /* None */0;
              }
            });
}

function of_channel(ic) {
  return /* record */[
          0,
          /* Sbuffio */{
            0: /* record */[
              ic,
              new Array(4096),
              0,
              0
            ],
            length: 1,
            tag: 4
          }
        ];
}

function iapp(i, s) {
  return /* record */[
          0,
          /* Sapp */{
            0: i[1],
            1: s[1],
            length: 2,
            tag: 1
          }
        ];
}

function icons(i, s) {
  return /* record */[
          0,
          /* Scons */{
            0: i,
            1: s[1],
            length: 2,
            tag: 0
          }
        ];
}

function ising(i) {
  return /* record */[
          0,
          /* Scons */{
            0: i,
            1: /* Sempty */0,
            length: 2,
            tag: 0
          }
        ];
}

function lapp(f, s) {
  return /* record */[
          0,
          /* Slazy */{
            0: {
              0: function () {
                return /* Sapp */{
                        0: Caml_curry.app1(f, /* () */0)[1],
                        1: s[1],
                        length: 2,
                        tag: 1
                      };
              },
              length: 1,
              tag: 246
            },
            length: 1,
            tag: 2
          }
        ];
}

function lcons(f, s) {
  return /* record */[
          0,
          /* Slazy */{
            0: {
              0: function () {
                return /* Scons */{
                        0: Caml_curry.app1(f, /* () */0),
                        1: s[1],
                        length: 2,
                        tag: 0
                      };
              },
              length: 1,
              tag: 246
            },
            length: 1,
            tag: 2
          }
        ];
}

function lsing(f) {
  return /* record */[
          0,
          /* Slazy */{
            0: {
              0: function () {
                return /* Scons */{
                        0: Caml_curry.app1(f, /* () */0),
                        1: /* Sempty */0,
                        length: 2,
                        tag: 0
                      };
              },
              length: 1,
              tag: 246
            },
            length: 1,
            tag: 2
          }
        ];
}

function slazy(f) {
  return /* record */[
          0,
          /* Slazy */{
            0: {
              0: function () {
                return Caml_curry.app1(f, /* () */0)[1];
              },
              length: 1,
              tag: 246
            },
            length: 1,
            tag: 2
          }
        ];
}

function dump(f, s) {
  Pervasives.print_string("{count = ");
  Pervasives.print_int(s[0]);
  Pervasives.print_string("; data = ");
  dump_data(f, s[1]);
  Pervasives.print_string("}");
  return Pervasives.print_newline(/* () */0);
}

function dump_data(f, param) {
  if (typeof param === "number") {
    return Pervasives.print_string("Sempty");
  }
  else {
    switch (param.tag | 0) {
      case 0 : 
          Pervasives.print_string("Scons (");
          Caml_curry.app1(f, param[0]);
          Pervasives.print_string(", ");
          dump_data(f, param[1]);
          return Pervasives.print_string(")");
      case 1 : 
          Pervasives.print_string("Sapp (");
          dump_data(f, param[0]);
          Pervasives.print_string(", ");
          dump_data(f, param[1]);
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
  return prim[0];
}

var sempty = /* record */[
  0,
  /* Sempty */0
];

exports.Failure    = Failure;
exports.$$Error    = $$Error;
exports.from       = from;
exports.of_list    = of_list;
exports.of_string  = of_string;
exports.of_bytes   = of_bytes;
exports.of_channel = of_channel;
exports.iter       = iter;
exports.next       = next;
exports.empty      = empty;
exports.peek       = peek;
exports.junk       = junk;
exports.count      = count;
exports.npeek      = npeek;
exports.iapp       = iapp;
exports.icons      = icons;
exports.ising      = ising;
exports.lapp       = lapp;
exports.lcons      = lcons;
exports.lsing      = lsing;
exports.sempty     = sempty;
exports.slazy      = slazy;
exports.dump       = dump;
/* No side effect */
