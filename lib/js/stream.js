'use strict';

var List = require("./list.js");
var Curry = require("./curry.js");
var Caml_bytes = require("./caml_bytes.js");
var Pervasives = require("./pervasives.js");
var Caml_option = require("./caml_option.js");
var Caml_string = require("./caml_string.js");
var Caml_exceptions = require("./caml_exceptions.js");
var CamlinternalLazy = require("./camlinternalLazy.js");

var Failure = /* @__PURE__ */Caml_exceptions.create("Stream.Failure");

var $$Error = /* @__PURE__ */Caml_exceptions.create("Stream.Error");

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
    switch (d.TAG | 0) {
      case /* Scons */0 :
          return d;
      case /* Sapp */1 :
          var d2 = d._1;
          var match = get_data(count, d._0);
          if (typeof match === "number") {
            _d = d2;
            continue ;
          }
          if (match.TAG === /* Scons */0) {
            return {
                    TAG: /* Scons */0,
                    _0: match._0,
                    _1: {
                      TAG: /* Sapp */1,
                      _0: match._1,
                      _1: d2
                    }
                  };
          }
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "stream.ml",
                  53,
                  12
                ],
                Error: new Error()
              };
      case /* Slazy */2 :
          _d = CamlinternalLazy.force(d._0);
          continue ;
      case /* Sgen */3 :
          var g = d._0;
          var match$1 = g.curr;
          if (match$1 !== undefined) {
            var a = Caml_option.valFromOption(match$1);
            if (a !== undefined) {
              g.curr = undefined;
              return {
                      TAG: /* Scons */0,
                      _0: Caml_option.valFromOption(a),
                      _1: d
                    };
            } else {
              return /* Sempty */0;
            }
          }
          var a$1 = Curry._1(g.func, count);
          if (a$1 !== undefined) {
            return {
                    TAG: /* Scons */0,
                    _0: Caml_option.valFromOption(a$1),
                    _1: d
                  };
          } else {
            g.curr = Caml_option.some(undefined);
            return /* Sempty */0;
          }
      case /* Sbuffio */4 :
          var b = d._0;
          if (b.ind >= b.len) {
            fill_buff(b);
          }
          if (b.len === 0) {
            return /* Sempty */0;
          }
          var r = b.buff[b.ind];
          b.ind = b.ind + 1 | 0;
          return {
                  TAG: /* Scons */0,
                  _0: r,
                  _1: d
                };
      
    }
  };
}

function peek_data(s) {
  while(true) {
    var f = s.data;
    if (typeof f === "number") {
      return ;
    }
    switch (f.TAG | 0) {
      case /* Scons */0 :
          return Caml_option.some(f._0);
      case /* Sapp */1 :
          var d = get_data(s.count, s.data);
          if (typeof d === "number") {
            return ;
          }
          if (d.TAG === /* Scons */0) {
            s.data = d;
            return Caml_option.some(d._0);
          }
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "stream.ml",
                  82,
                  12
                ],
                Error: new Error()
              };
      case /* Slazy */2 :
          s.data = CamlinternalLazy.force(f._0);
          continue ;
      case /* Sgen */3 :
          var g = f._0;
          var a = g.curr;
          if (a !== undefined) {
            return Caml_option.valFromOption(a);
          }
          var x = Curry._1(g.func, s.count);
          g.curr = Caml_option.some(x);
          return x;
      case /* Sbuffio */4 :
          var b = f._0;
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
      switch (g.TAG | 0) {
        case /* Scons */0 :
            s.count = s.count + 1 | 0;
            s.data = g._1;
            return ;
        case /* Sgen */3 :
            var g$1 = g._0;
            var match = g$1.curr;
            if (match !== undefined) {
              s.count = s.count + 1 | 0;
              g$1.curr = undefined;
              return ;
            }
            break;
        case /* Sbuffio */4 :
            var b = g._0;
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
    return [
            /* [] */0,
            s.data,
            0
          ];
  }
  var a = peek_data(s);
  if (a === undefined) {
    return [
            /* [] */0,
            s.data,
            0
          ];
  }
  var a$1 = Caml_option.valFromOption(a);
  junk_data(s);
  var match = nget_data(n - 1 | 0, s);
  return [
          {
            hd: a$1,
            tl: match[0]
          },
          {
            TAG: /* Scons */0,
            _0: a$1,
            _1: match[1]
          },
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
        RE_EXN_ID: Failure,
        Error: new Error()
      };
}

function empty(s) {
  var match = peek(s);
  if (match === undefined) {
    return ;
  }
  throw {
        RE_EXN_ID: Failure,
        Error: new Error()
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
          data: {
            TAG: /* Sgen */3,
            _0: {
              curr: undefined,
              func: f
            }
          }
        };
}

function of_list(l) {
  return {
          count: 0,
          data: List.fold_right((function (x, l) {
                  return {
                          TAG: /* Scons */0,
                          _0: x,
                          _1: l
                        };
                }), l, /* Sempty */0)
        };
}

function of_string(s) {
  var count = {
    contents: 0
  };
  return from(function (param) {
              var c = count.contents;
              if (c < s.length) {
                count.contents = count.contents + 1 | 0;
                return Caml_string.get(s, c);
              }
              
            });
}

function of_bytes(s) {
  var count = {
    contents: 0
  };
  return from(function (param) {
              var c = count.contents;
              if (c < s.length) {
                count.contents = count.contents + 1 | 0;
                return Caml_bytes.get(s, c);
              }
              
            });
}

function of_channel(ic) {
  return {
          count: 0,
          data: {
            TAG: /* Sbuffio */4,
            _0: {
              ic: ic,
              buff: Caml_bytes.caml_create_bytes(4096),
              len: 0,
              ind: 0
            }
          }
        };
}

function iapp(i, s) {
  return {
          count: 0,
          data: {
            TAG: /* Sapp */1,
            _0: data(i),
            _1: data(s)
          }
        };
}

function icons(i, s) {
  return {
          count: 0,
          data: {
            TAG: /* Scons */0,
            _0: i,
            _1: data(s)
          }
        };
}

function ising(i) {
  return {
          count: 0,
          data: {
            TAG: /* Scons */0,
            _0: i,
            _1: /* Sempty */0
          }
        };
}

function lapp(f, s) {
  return {
          count: 0,
          data: {
            TAG: /* Slazy */2,
            _0: {
              LAZY_DONE: false,
              VAL: (function () {
                  return {
                          TAG: /* Sapp */1,
                          _0: data(Curry._1(f, undefined)),
                          _1: data(s)
                        };
                })
            }
          }
        };
}

function lcons(f, s) {
  return {
          count: 0,
          data: {
            TAG: /* Slazy */2,
            _0: {
              LAZY_DONE: false,
              VAL: (function () {
                  return {
                          TAG: /* Scons */0,
                          _0: Curry._1(f, undefined),
                          _1: data(s)
                        };
                })
            }
          }
        };
}

function lsing(f) {
  return {
          count: 0,
          data: {
            TAG: /* Slazy */2,
            _0: {
              LAZY_DONE: false,
              VAL: (function () {
                  return {
                          TAG: /* Scons */0,
                          _0: Curry._1(f, undefined),
                          _1: /* Sempty */0
                        };
                })
            }
          }
        };
}

function slazy(f) {
  return {
          count: 0,
          data: {
            TAG: /* Slazy */2,
            _0: {
              LAZY_DONE: false,
              VAL: (function () {
                  return data(Curry._1(f, undefined));
                })
            }
          }
        };
}

function dump_data(f, param) {
  if (typeof param === "number") {
    return Pervasives.print_string("Sempty");
  }
  switch (param.TAG | 0) {
    case /* Scons */0 :
        Pervasives.print_string("Scons (");
        Curry._1(f, param._0);
        Pervasives.print_string(", ");
        dump_data(f, param._1);
        return Pervasives.print_string(")");
    case /* Sapp */1 :
        Pervasives.print_string("Sapp (");
        dump_data(f, param._0);
        Pervasives.print_string(", ");
        dump_data(f, param._1);
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
