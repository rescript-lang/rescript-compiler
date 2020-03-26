'use strict';

var List = require("./list.js");
var Block = require("./block.js");
var Curry = require("./curry.js");
var Caml_obj = require("./caml_obj.js");
var Caml_bytes = require("./caml_bytes.js");
var Pervasives = require("./pervasives.js");
var Caml_option = require("./caml_option.js");
var Caml_string = require("./caml_string.js");
var Caml_exceptions = require("./caml_exceptions.js");
var CamlinternalLazy = require("./camlinternalLazy.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

var Failure = Caml_exceptions.create("Stream.Failure");

var $$Error = Caml_exceptions.create("Stream.Error");

function count(param) {
  if (param !== void 0) {
    return param.count;
  } else {
    return 0;
  }
}

function data(param) {
  if (param !== void 0) {
    return param.data;
  } else {
    return /* Sempty */0;
  }
}

function fill_buff(b) {
  b.len = Pervasives.input(b.ic, b.buff, 0, b.buff.length);
  b.ind = 0;
  
}

function get_data(count, _d) {
  while(true) {
    var d = _d;
    if (typeof d === "number") {
      return d;
    }
    switch (d.tag | 0) {
      case /* Scons */0 :
          return d;
      case /* Sapp */1 :
          var d2 = d[1];
          var match = get_data(count, d[0]);
          if (typeof match === "number") {
            _d = d2;
            continue ;
          }
          if (!match.tag) {
            return /* Scons */Block.__(0, [
                      match[0],
                      /* Sapp */Block.__(1, [
                          match[1],
                          d2
                        ])
                    ]);
          }
          throw [
                Caml_builtin_exceptions.assert_failure,
                /* tuple */[
                  "stream.ml",
                  53,
                  12
                ]
              ];
      case /* Slazy */2 :
          _d = CamlinternalLazy.force(d[0]);
          continue ;
      case /* Sgen */3 :
          var g = d[0];
          var match$1 = g.curr;
          if (match$1 !== void 0) {
            var match$2 = Caml_option.valFromOption(match$1);
            if (match$2 !== void 0) {
              g.curr = void 0;
              return /* Scons */Block.__(0, [
                        Caml_option.valFromOption(match$2),
                        d
                      ]);
            } else {
              return /* Sempty */0;
            }
          }
          var match$3 = Curry._1(g.func, count);
          if (match$3 !== void 0) {
            return /* Scons */Block.__(0, [
                      Caml_option.valFromOption(match$3),
                      d
                    ]);
          } else {
            g.curr = Caml_option.some(void 0);
            return /* Sempty */0;
          }
      case /* Sbuffio */4 :
          var b = d[0];
          if (b.ind >= b.len) {
            fill_buff(b);
          }
          if (b.len === 0) {
            return /* Sempty */0;
          }
          var r = b.buff[b.ind];
          b.ind = b.ind + 1 | 0;
          return /* Scons */Block.__(0, [
                    r,
                    d
                  ]);
      
    }
  };
}

function peek_data(s) {
  while(true) {
    var match = s.data;
    if (typeof match === "number") {
      return ;
    }
    switch (match.tag | 0) {
      case /* Scons */0 :
          return Caml_option.some(match[0]);
      case /* Sapp */1 :
          var d = get_data(s.count, s.data);
          if (typeof d === "number") {
            return ;
          }
          if (d.tag) {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "stream.ml",
                    82,
                    12
                  ]
                ];
          }
          s.data = d;
          return Caml_option.some(d[0]);
      case /* Slazy */2 :
          s.data = CamlinternalLazy.force(match[0]);
          continue ;
      case /* Sgen */3 :
          var g = match[0];
          var match$1 = g.curr;
          if (match$1 !== void 0) {
            return Caml_option.valFromOption(match$1);
          }
          var x = Curry._1(g.func, s.count);
          g.curr = Caml_option.some(x);
          return x;
      case /* Sbuffio */4 :
          var b = match[0];
          if (b.ind >= b.len) {
            fill_buff(b);
          }
          if (b.len === 0) {
            s.data = /* Sempty */0;
            return ;
          } else {
            return b.buff[b.ind];
          }
      
    }
  };
}

function peek(param) {
  if (param !== void 0) {
    return peek_data(param);
  }
  
}

function junk_data(s) {
  while(true) {
    var match = s.data;
    if (typeof match !== "number") {
      switch (match.tag | 0) {
        case /* Scons */0 :
            s.count = s.count + 1 | 0;
            s.data = match[1];
            return ;
        case /* Sgen */3 :
            var g = match[0];
            var match$1 = g.curr;
            if (match$1 !== void 0) {
              s.count = s.count + 1 | 0;
              g.curr = void 0;
              return ;
            }
            break;
        case /* Sbuffio */4 :
            var b = match[0];
            s.count = s.count + 1 | 0;
            b.ind = b.ind + 1 | 0;
            return ;
        default:
          
      }
    }
    var match$2 = peek_data(s);
    if (match$2 === void 0) {
      return ;
    }
    continue ;
  };
}

function junk(param) {
  if (param !== void 0) {
    return junk_data(param);
  }
  
}

function nget_data(n, s) {
  if (n <= 0) {
    return /* tuple */[
            /* [] */0,
            s.data,
            0
          ];
  }
  var match = peek_data(s);
  if (match === void 0) {
    return /* tuple */[
            /* [] */0,
            s.data,
            0
          ];
  }
  var a = Caml_option.valFromOption(match);
  junk_data(s);
  var match$1 = nget_data(n - 1 | 0, s);
  return /* tuple */[
          /* :: */[
            a,
            match$1[0]
          ],
          /* Scons */Block.__(0, [
              a,
              match$1[1]
            ]),
          match$1[2] + 1 | 0
        ];
}

function npeek(n, param) {
  if (param !== void 0) {
    var match = nget_data(n, param);
    param.count = param.count - match[2] | 0;
    param.data = match[1];
    return match[0];
  } else {
    return /* [] */0;
  }
}

function next(s) {
  var match = peek(s);
  if (match !== void 0) {
    junk(s);
    return Caml_option.valFromOption(match);
  }
  throw Failure;
}

function empty(s) {
  var match = peek(s);
  if (match === void 0) {
    return ;
  }
  throw Failure;
}

function iter(f, strm) {
  var _param;
  while(true) {
    var match = peek(strm);
    if (match === void 0) {
      return ;
    }
    junk(strm);
    Curry._1(f, Caml_option.valFromOption(match));
    _param = void 0;
    continue ;
  };
}

function from(f) {
  return {
          count: 0,
          data: /* Sgen */Block.__(3, [{
                curr: void 0,
                func: f
              }])
        };
}

function of_list(l) {
  return {
          count: 0,
          data: List.fold_right((function (x, l) {
                  return /* Scons */Block.__(0, [
                            x,
                            l
                          ]);
                }), l, /* Sempty */0)
        };
}

function of_string(s) {
  var count = {
    contents: 0
  };
  return from((function (param) {
                var c = count.contents;
                if (c < s.length) {
                  count.contents = count.contents + 1 | 0;
                  return Caml_string.get(s, c);
                }
                
              }));
}

function of_bytes(s) {
  var count = {
    contents: 0
  };
  return from((function (param) {
                var c = count.contents;
                if (c < s.length) {
                  count.contents = count.contents + 1 | 0;
                  return Caml_bytes.get(s, c);
                }
                
              }));
}

function of_channel(ic) {
  return {
          count: 0,
          data: /* Sbuffio */Block.__(4, [{
                ic: ic,
                buff: Caml_bytes.caml_create_bytes(4096),
                len: 0,
                ind: 0
              }])
        };
}

function iapp(i, s) {
  return {
          count: 0,
          data: /* Sapp */Block.__(1, [
              data(i),
              data(s)
            ])
        };
}

function icons(i, s) {
  return {
          count: 0,
          data: /* Scons */Block.__(0, [
              i,
              data(s)
            ])
        };
}

function ising(i) {
  return {
          count: 0,
          data: /* Scons */Block.__(0, [
              i,
              /* Sempty */0
            ])
        };
}

function lapp(f, s) {
  return {
          count: 0,
          data: /* Slazy */Block.__(2, [Caml_obj.caml_lazy_make((function (param) {
                      return /* Sapp */Block.__(1, [
                                data(Curry._1(f, void 0)),
                                data(s)
                              ]);
                    }))])
        };
}

function lcons(f, s) {
  return {
          count: 0,
          data: /* Slazy */Block.__(2, [Caml_obj.caml_lazy_make((function (param) {
                      return /* Scons */Block.__(0, [
                                Curry._1(f, void 0),
                                data(s)
                              ]);
                    }))])
        };
}

function lsing(f) {
  return {
          count: 0,
          data: /* Slazy */Block.__(2, [Caml_obj.caml_lazy_make((function (param) {
                      return /* Scons */Block.__(0, [
                                Curry._1(f, void 0),
                                /* Sempty */0
                              ]);
                    }))])
        };
}

function slazy(f) {
  return {
          count: 0,
          data: /* Slazy */Block.__(2, [Caml_obj.caml_lazy_make((function (param) {
                      return data(Curry._1(f, void 0));
                    }))])
        };
}

function dump_data(f, param) {
  if (typeof param === "number") {
    return Pervasives.print_string("Sempty");
  }
  switch (param.tag | 0) {
    case /* Scons */0 :
        Pervasives.print_string("Scons (");
        Curry._1(f, param[0]);
        Pervasives.print_string(", ");
        dump_data(f, param[1]);
        return Pervasives.print_string(")");
    case /* Sapp */1 :
        Pervasives.print_string("Sapp (");
        dump_data(f, param[0]);
        Pervasives.print_string(", ");
        dump_data(f, param[1]);
        return Pervasives.print_string(")");
    case /* Slazy */2 :
        return Pervasives.print_string("Slazy");
    case /* Sgen */3 :
        return Pervasives.print_string("Sgen");
    case /* Sbuffio */4 :
        return Pervasives.print_string("Sbuffio");
    
  }
}

function dump(f, s) {
  Pervasives.print_string("{count = ");
  Pervasives.print_int(count(s));
  Pervasives.print_string("; data = ");
  dump_data(f, data(s));
  Pervasives.print_string("}");
  return Pervasives.print_newline(void 0);
}

var sempty;

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
