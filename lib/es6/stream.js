

import * as List from "./list.js";
import * as Block from "./block.js";
import * as Curry from "./curry.js";
import * as Caml_obj from "./caml_obj.js";
import * as Caml_bytes from "./caml_bytes.js";
import * as Pervasives from "./pervasives.js";
import * as Caml_option from "./caml_option.js";
import * as Caml_string from "./caml_string.js";
import * as Caml_exceptions from "./caml_exceptions.js";
import * as CamlinternalLazy from "./camlinternalLazy.js";
import * as Caml_builtin_exceptions from "./caml_builtin_exceptions.js";

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
  return /* () */0;
}

function get_data(count, _d) {
  while(true) {
    var d = _d;
    if (typeof d === "number") {
      return d;
    } else {
      switch (d.tag | 0) {
        case /* Scons */0 :
            return d;
        case /* Sapp */1 :
            var d2 = d[1];
            var match = get_data(count, d[0]);
            if (typeof match === "number") {
              _d = d2;
              continue ;
            } else if (match.tag) {
              throw [
                    Caml_builtin_exceptions.assert_failure,
                    /* tuple */[
                      "stream.ml",
                      53,
                      12
                    ]
                  ];
            } else {
              return /* Scons */Block.__(0, [
                        match[0],
                        /* Sapp */Block.__(1, [
                            match[1],
                            d2
                          ])
                      ]);
            }
        case /* Slazy */2 :
            _d = CamlinternalLazy.force(d[0]);
            continue ;
        case /* Sgen */3 :
            var g = d[0];
            var match$1 = g.curr;
            if (match$1 !== undefined) {
              var match$2 = Caml_option.valFromOption(match$1);
              if (match$2 !== undefined) {
                g.curr = undefined;
                return /* Scons */Block.__(0, [
                          Caml_option.valFromOption(match$2),
                          d
                        ]);
              } else {
                return /* Sempty */0;
              }
            } else {
              var match$3 = Curry._1(g.func, count);
              if (match$3 !== undefined) {
                return /* Scons */Block.__(0, [
                          Caml_option.valFromOption(match$3),
                          d
                        ]);
              } else {
                g.curr = Caml_option.some(undefined);
                return /* Sempty */0;
              }
            }
        case /* Sbuffio */4 :
            var b = d[0];
            if (b.ind >= b.len) {
              fill_buff(b);
            }
            if (b.len === 0) {
              return /* Sempty */0;
            } else {
              var r = b.buff[b.ind];
              b.ind = b.ind + 1 | 0;
              return /* Scons */Block.__(0, [
                        r,
                        d
                      ]);
            }
        
      }
    }
  };
}

function peek_data(s) {
  while(true) {
    var match = s.data;
    if (typeof match === "number") {
      return ;
    } else {
      switch (match.tag | 0) {
        case /* Scons */0 :
            return Caml_option.some(match[0]);
        case /* Sapp */1 :
            var d = get_data(s.count, s.data);
            if (typeof d === "number") {
              return ;
            } else if (d.tag) {
              throw [
                    Caml_builtin_exceptions.assert_failure,
                    /* tuple */[
                      "stream.ml",
                      82,
                      12
                    ]
                  ];
            } else {
              s.data = d;
              return Caml_option.some(d[0]);
            }
        case /* Slazy */2 :
            s.data = CamlinternalLazy.force(match[0]);
            continue ;
        case /* Sgen */3 :
            var g = match[0];
            var match$1 = g.curr;
            if (match$1 !== undefined) {
              return Caml_option.valFromOption(match$1);
            } else {
              var x = Curry._1(g.func, s.count);
              g.curr = Caml_option.some(x);
              return x;
            }
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
    }
  };
}

function peek(param) {
  if (param !== undefined) {
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
            return /* () */0;
        case /* Sgen */3 :
            var g = match[0];
            var match$1 = g.curr;
            if (match$1 !== undefined) {
              s.count = s.count + 1 | 0;
              g.curr = undefined;
              return /* () */0;
            }
            break;
        case /* Sbuffio */4 :
            var b = match[0];
            s.count = s.count + 1 | 0;
            b.ind = b.ind + 1 | 0;
            return /* () */0;
        default:
          
      }
    }
    var match$2 = peek_data(s);
    if (match$2 !== undefined) {
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function junk(param) {
  if (param !== undefined) {
    return junk_data(param);
  } else {
    return /* () */0;
  }
}

function nget_data(n, s) {
  if (n <= 0) {
    return /* tuple */[
            /* [] */0,
            s.data,
            0
          ];
  } else {
    var match = peek_data(s);
    if (match !== undefined) {
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
    } else {
      return /* tuple */[
              /* [] */0,
              s.data,
              0
            ];
    }
  }
}

function npeek(n, param) {
  if (param !== undefined) {
    var n$1 = n;
    var s = param;
    var match = nget_data(n$1, s);
    s.count = s.count - match[2] | 0;
    s.data = match[1];
    return match[0];
  } else {
    return /* [] */0;
  }
}

function next(s) {
  var match = peek(s);
  if (match !== undefined) {
    junk(s);
    return Caml_option.valFromOption(match);
  } else {
    throw Failure;
  }
}

function empty(s) {
  var match = peek(s);
  if (match !== undefined) {
    throw Failure;
  } else {
    return /* () */0;
  }
}

function iter(f, strm) {
  var _param = /* () */0;
  while(true) {
    var match = peek(strm);
    if (match !== undefined) {
      junk(strm);
      Curry._1(f, Caml_option.valFromOption(match));
      _param = /* () */0;
      continue ;
    } else {
      return /* () */0;
    }
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
          data: /* Slazy */Block.__(2, [Caml_obj.caml_lazy_make((function (param) {
                      return /* Sapp */Block.__(1, [
                                data(Curry._1(f, /* () */0)),
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
                                Curry._1(f, /* () */0),
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
                                Curry._1(f, /* () */0),
                                /* Sempty */0
                              ]);
                    }))])
        };
}

function slazy(f) {
  return {
          count: 0,
          data: /* Slazy */Block.__(2, [Caml_obj.caml_lazy_make((function (param) {
                      return data(Curry._1(f, /* () */0));
                    }))])
        };
}

function dump_data(f, param) {
  if (typeof param === "number") {
    return Pervasives.print_string("Sempty");
  } else {
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
}

function dump(f, s) {
  Pervasives.print_string("{count = ");
  Pervasives.print_int(count(s));
  Pervasives.print_string("; data = ");
  dump_data(f, data(s));
  Pervasives.print_string("}");
  return Pervasives.print_newline(/* () */0);
}

var sempty = undefined;

export {
  Failure ,
  $$Error ,
  from ,
  of_list ,
  of_string ,
  of_bytes ,
  of_channel ,
  iter ,
  next ,
  empty ,
  peek ,
  junk ,
  count ,
  npeek ,
  iapp ,
  icons ,
  ising ,
  lapp ,
  lcons ,
  lsing ,
  sempty ,
  slazy ,
  dump ,
  
}
/* No side effect */
