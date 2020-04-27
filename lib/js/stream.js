'use strict';

var List = require("./list.js");
var Block = require("./block.js");
var Curry = require("./curry.js");
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
  if (param !== undefined) {
    return param.count;
  } else {
    return 0;
  }
}

function data(param) {
  if (param !== undefined) {
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
          throw {
                CamlExt: Caml_builtin_exceptions.assert_failure,
                _1: /* tuple */[
                  "stream.ml",
                  53,
                  12
                ]
              };
      case /* Slazy */2 :
          _d = CamlinternalLazy.force(d[0]);
          continue ;
      case /* Sgen */3 :
          var g = d[0];
          var match$1 = g.curr;
          if (match$1 !== undefined) {
            var a = Caml_option.valFromOption(match$1);
            if (a !== undefined) {
              g.curr = undefined;
              return /* Scons */Block.__(0, [
                        Caml_option.valFromOption(a),
                        d
                      ]);
            } else {
              return /* Sempty */0;
            }
          }
          var a$1 = Curry._1(g.func, count);
          if (a$1 !== undefined) {
            return /* Scons */Block.__(0, [
                      Caml_option.valFromOption(a$1),
                      d
                    ]);
          } else {
            g.curr = Caml_option.some(undefined);
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
    var f = s.data;
    if (typeof f === "number") {
      return ;
    }
    switch (f.tag | 0) {
      case /* Scons */0 :
          return Caml_option.some(f[0]);
      case /* Sapp */1 :
          var d = get_data(s.count, s.data);
          if (typeof d === "number") {
            return ;
          }
          if (d.tag) {
            throw {
                  CamlExt: Caml_builtin_exceptions.assert_failure,
                  _1: /* tuple */[
                    "stream.ml",
                    82,
                    12
                  ]
                };
          }
          s.data = d;
          return Caml_option.some(d[0]);
      case /* Slazy */2 :
          s.data = CamlinternalLazy.force(f[0]);
          continue ;
      case /* Sgen */3 :
          var g = f[0];
          var a = g.curr;
          if (a !== undefined) {
            return Caml_option.valFromOption(a);
          }
          var x = Curry._1(g.func, s.count);
          g.curr = Caml_option.some(x);
          return x;
      case /* Sbuffio */4 :
          var b = f[0];
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

function peek(s) {
  if (s !== undefined) {
    return peek_data(s);
  }
  
}

function junk_data(s) {
  while(true) {
    var g = s.data;
    if (typeof g !== "number") {
      switch (g.tag | 0) {
        case /* Scons */0 :
            s.count = s.count + 1 | 0;
            s.data = g[1];
            return ;
        case /* Sgen */3 :
            var g$1 = g[0];
            var match = g$1.curr;
            if (match !== undefined) {
              s.count = s.count + 1 | 0;
              g$1.curr = undefined;
              return ;
            }
            break;
        case /* Sbuffio */4 :
            var b = g[0];
            s.count = s.count + 1 | 0;
            b.ind = b.ind + 1 | 0;
            return ;
        default:
          
      }
    }
    var match$1 = peek_data(s);
    if (match$1 === undefined) {
      return ;
    }
    continue ;
  };
}

function junk(data) {
  if (data !== undefined) {
    return junk_data(data);
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
  var a = peek_data(s);
  if (a === undefined) {
    return /* tuple */[
            /* [] */0,
            s.data,
            0
          ];
  }
  var a$1 = Caml_option.valFromOption(a);
  junk_data(s);
  var match = nget_data(n - 1 | 0, s);
  return /* tuple */[
          /* :: */[
            a$1,
            match[0]
          ],
          /* Scons */Block.__(0, [
              a$1,
              match[1]
            ]),
          match[2] + 1 | 0
        ];
}

function npeek(n, d) {
  if (d !== undefined) {
    var match = nget_data(n, d);
    d.count = d.count - match[2] | 0;
    d.data = match[1];
    return match[0];
  } else {
    return /* [] */0;
  }
}

function next(s) {
  var a = peek(s);
  if (a !== undefined) {
    junk(s);
    return Caml_option.valFromOption(a);
  }
  throw {
        CamlExt: Failure
      };
}

function empty(s) {
  var match = peek(s);
  if (match === undefined) {
    return ;
  }
  throw {
        CamlExt: Failure
      };
}

function iter(f, strm) {
  var _param;
  while(true) {
    var a = peek(strm);
    if (a === undefined) {
      return ;
    }
    junk(strm);
    Curry._1(f, Caml_option.valFromOption(a));
    _param = undefined;
    continue ;
  };
}

function from(f) {
  return {
          count: 0,
          data: /* Sgen */Block.__(3, [{
                curr: undefined,
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
          data: /* Slazy */Block.__(2, [{
                tag: 246,
                value: (function () {
                    return /* Sapp */Block.__(1, [
                              data(Curry._1(f, undefined)),
                              data(s)
                            ]);
                  })
              }])
        };
}

function lcons(f, s) {
  return {
          count: 0,
          data: /* Slazy */Block.__(2, [{
                tag: 246,
                value: (function () {
                    return /* Scons */Block.__(0, [
                              Curry._1(f, undefined),
                              data(s)
                            ]);
                  })
              }])
        };
}

function lsing(f) {
  return {
          count: 0,
          data: /* Slazy */Block.__(2, [{
                tag: 246,
                value: (function () {
                    return /* Scons */Block.__(0, [
                              Curry._1(f, undefined),
                              /* Sempty */0
                            ]);
                  })
              }])
        };
}

function slazy(f) {
  return {
          count: 0,
          data: /* Slazy */Block.__(2, [{
                tag: 246,
                value: (function () {
                    return data(Curry._1(f, undefined));
                  })
              }])
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
  return Pervasives.print_newline(undefined);
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
