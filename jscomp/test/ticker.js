// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_exceptions = require("../runtime/caml_exceptions");
var Pervasives      = require("../stdlib/pervasives");
var Caml_format     = require("../runtime/caml_format");
var Printf          = require("../stdlib/printf");
var Caml_primitive  = require("../runtime/caml_primitive");
var $$String        = require("../stdlib/string");
var List            = require("../stdlib/list");

function split(delim, s) {
  var len = s.length;
  if (len !== 0) {
    var _l = /* [] */0;
    var _i = len;
    while(true) {
      var i = _i;
      var l = _l;
      if (i !== 0) {
        var exit = 0;
        var i$prime;
        try {
          i$prime = $$String.rindex_from(s, i - 1, delim);
          exit = 1;
        }
        catch (exn){
          if (exn === Caml_exceptions.Not_found) {
            return [
                    /* :: */0,
                    $$String.sub(s, 0, i),
                    l
                  ];
          }
          else {
            throw exn;
          }
        }
        if (exit === 1) {
          var l_001 = $$String.sub(s, i$prime + 1, i - i$prime - 1);
          var l$1 = [
            /* :: */0,
            l_001,
            l
          ];
          var l$2 = i$prime ? l$1 : [
              /* :: */0,
              "",
              l$1
            ];
          _i = i$prime;
          _l = l$2;
        }
        
      }
      else {
        return l;
      }
    };
  }
  else {
    return /* [] */0;
  }
}

function string_of_float_option(param) {
  if (param) {
    return Pervasives.string_of_float(param[1]);
  }
  else {
    return "nan";
  }
}

var Util = [
  0,
  split,
  string_of_float_option
];

function string_of_rank(param) {
  if (typeof param === "number") {
    if (param !== 0) {
      return "Visited";
    }
    else {
      return "Uninitialized";
    }
  }
  else {
    return Printf.sprintf([
                  /* Format */0,
                  [
                    /* String_literal */11,
                    "Ranked(",
                    [
                      /* Int */4,
                      /* Int_i */3,
                      /* No_padding */0,
                      /* No_precision */0,
                      [
                        /* Char_literal */12,
                        /* ")" */41,
                        /* End_of_format */0
                      ]
                    ]
                  ],
                  "Ranked(%i)"
                ])(param[1]);
  }
}

function find_ticker_by_name(all_tickers, ticker) {
  return List.find(function (param) {
              return +(param[3] === ticker);
            }, all_tickers);
}

function print_all_composite(all_tickers) {
  return List.iter(function (param) {
              var value = param[1];
              var ticker_name = param[3];
              if (param[4]) {
                if (value) {
                  console.log(ticker_name);
                  return /* () */0;
                }
                else {
                  console.log(ticker_name);
                  return /* () */0;
                }
              }
              else {
                return /* () */0;
              }
            }, all_tickers);
}

function height(param) {
  if (param) {
    return param[5];
  }
  else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return [
          /* Node */0,
          l,
          x,
          d,
          r,
          hl >= hr ? hl + 1 : hr + 1
        ];
}

function singleton(x, d) {
  return [
          /* Node */0,
          /* Empty */0,
          x,
          d,
          /* Empty */0,
          1
        ];
}

function bal(l, x, d, r) {
  var hl = l ? l[5] : 0;
  var hr = r ? r[5] : 0;
  if (hl > hr + 2) {
    if (l) {
      var lr = l[4];
      var ld = l[3];
      var lv = l[2];
      var ll = l[1];
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      }
      else if (lr) {
        return create(create(ll, lv, ld, lr[1]), lr[2], lr[3], create(lr[4], x, d, r));
      }
      else {
        return Pervasives.invalid_arg("Map.bal");
      }
    }
    else {
      return Pervasives.invalid_arg("Map.bal");
    }
  }
  else if (hr > hl + 2) {
    if (r) {
      var rr = r[4];
      var rd = r[3];
      var rv = r[2];
      var rl = r[1];
      if (height(rr) >= height(rl)) {
        return create(create(l, x, d, rl), rv, rd, rr);
      }
      else if (rl) {
        return create(create(l, x, d, rl[1]), rl[2], rl[3], create(rl[4], rv, rd, rr));
      }
      else {
        return Pervasives.invalid_arg("Map.bal");
      }
    }
    else {
      return Pervasives.invalid_arg("Map.bal");
    }
  }
  else {
    return [
            /* Node */0,
            l,
            x,
            d,
            r,
            hl >= hr ? hl + 1 : hr + 1
          ];
  }
}

function is_empty(param) {
  if (param) {
    return /* false */0;
  }
  else {
    return /* true */1;
  }
}

function add(x, data, param) {
  if (param) {
    var r = param[4];
    var d = param[3];
    var v = param[2];
    var l = param[1];
    var c = Caml_primitive.caml_compare(x, v);
    if (c) {
      if (c < 0) {
        return bal(add(x, data, l), v, d, r);
      }
      else {
        return bal(l, v, d, add(x, data, r));
      }
    }
    else {
      return [
              /* Node */0,
              l,
              x,
              data,
              r,
              param[5]
            ];
    }
  }
  else {
    return [
            /* Node */0,
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
      var c = Caml_primitive.caml_compare(x, param[2]);
      if (c) {
        _param = c < 0 ? param[1] : param[4];
      }
      else {
        return param[3];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_primitive.caml_compare(x, param[2]);
      if (c) {
        _param = c < 0 ? param[1] : param[4];
      }
      else {
        return /* true */1;
      }
    }
    else {
      return /* false */0;
    }
  };
}

function min_binding(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param[1];
      if (l) {
        _param = l;
      }
      else {
        return [
                /* tuple */0,
                param[2],
                param[3]
              ];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function max_binding(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var r = param[4];
      if (r) {
        _param = r;
      }
      else {
        return [
                /* tuple */0,
                param[2],
                param[3]
              ];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function remove_min_binding(param) {
  if (param) {
    var l = param[1];
    if (l) {
      return bal(remove_min_binding(l), param[2], param[3], param[4]);
    }
    else {
      return param[4];
    }
  }
  else {
    return Pervasives.invalid_arg("Map.remove_min_elt");
  }
}

function remove(x, param) {
  if (param) {
    var r = param[4];
    var d = param[3];
    var v = param[2];
    var l = param[1];
    var c = Caml_primitive.caml_compare(x, v);
    if (c) {
      if (c < 0) {
        return bal(remove(x, l), v, d, r);
      }
      else {
        return bal(l, v, d, remove(x, r));
      }
    }
    else {
      var t1 = l;
      var t2 = r;
      if (t1) {
        if (t2) {
          var match = min_binding(t2);
          return bal(t1, match[1], match[2], remove_min_binding(t2));
        }
        else {
          return t1;
        }
      }
      else {
        return t2;
      }
    }
  }
  else {
    return /* Empty */0;
  }
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      iter(f, param[1]);
      f(param[2], param[3]);
      _param = param[4];
    }
    else {
      return /* () */0;
    }
  };
}

function map(f, param) {
  if (param) {
    var l$prime = map(f, param[1]);
    var d$prime = f(param[3]);
    var r$prime = map(f, param[4]);
    return [
            /* Node */0,
            l$prime,
            param[2],
            d$prime,
            r$prime,
            param[5]
          ];
  }
  else {
    return /* Empty */0;
  }
}

function mapi(f, param) {
  if (param) {
    var v = param[2];
    var l$prime = mapi(f, param[1]);
    var d$prime = f(v, param[3]);
    var r$prime = mapi(f, param[4]);
    return [
            /* Node */0,
            l$prime,
            v,
            d$prime,
            r$prime,
            param[5]
          ];
  }
  else {
    return /* Empty */0;
  }
}

function fold(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (m) {
      _accu = f(m[2], m[3], fold(f, m[1], accu));
      _m = m[4];
    }
    else {
      return accu;
    }
  };
}

function for_all(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (p(param[2], param[3])) {
        if (for_all(p, param[1])) {
          _param = param[4];
        }
        else {
          return /* false */0;
        }
      }
      else {
        return /* false */0;
      }
    }
    else {
      return /* true */1;
    }
  };
}

function exists(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (p(param[2], param[3])) {
        return /* true */1;
      }
      else if (exists(p, param[1])) {
        return /* true */1;
      }
      else {
        _param = param[4];
      }
    }
    else {
      return /* false */0;
    }
  };
}

function add_min_binding(k, v, param) {
  if (param) {
    return bal(add_min_binding(k, v, param[1]), param[2], param[3], param[4]);
  }
  else {
    return singleton(k, v);
  }
}

function add_max_binding(k, v, param) {
  if (param) {
    return bal(param[1], param[2], param[3], add_max_binding(k, v, param[4]));
  }
  else {
    return singleton(k, v);
  }
}

function join(l, v, d, r) {
  if (l) {
    if (r) {
      var rh = r[5];
      var lh = l[5];
      if (lh > rh + 2) {
        return bal(l[1], l[2], l[3], join(l[4], v, d, r));
      }
      else if (rh > lh + 2) {
        return bal(join(l, v, d, r[1]), r[2], r[3], r[4]);
      }
      else {
        return create(l, v, d, r);
      }
    }
    else {
      return add_max_binding(v, d, l);
    }
  }
  else {
    return add_min_binding(v, d, r);
  }
}

function concat(t1, t2) {
  if (t1) {
    if (t2) {
      var match = min_binding(t2);
      return join(t1, match[1], match[2], remove_min_binding(t2));
    }
    else {
      return t1;
    }
  }
  else {
    return t2;
  }
}

function concat_or_join(t1, v, d, t2) {
  if (d) {
    return join(t1, v, d[1], t2);
  }
  else {
    return concat(t1, t2);
  }
}

function split$1(x, param) {
  if (param) {
    var r = param[4];
    var d = param[3];
    var v = param[2];
    var l = param[1];
    var c = Caml_primitive.caml_compare(x, v);
    if (c) {
      if (c < 0) {
        var match = split$1(x, l);
        return [
                /* tuple */0,
                match[1],
                match[2],
                join(match[3], v, d, r)
              ];
      }
      else {
        var match$1 = split$1(x, r);
        return [
                /* tuple */0,
                join(l, v, d, match$1[1]),
                match$1[2],
                match$1[3]
              ];
      }
    }
    else {
      return [
              /* tuple */0,
              l,
              [
                /* Some */0,
                d
              ],
              r
            ];
    }
  }
  else {
    return [
            /* tuple */0,
            /* Empty */0,
            /* None */0,
            /* Empty */0
          ];
  }
}

function merge(f, s1, s2) {
  var exit = 0;
  if (s1) {
    var v1 = s1[2];
    if (s1[5] >= height(s2)) {
      var match = split$1(v1, s2);
      return concat_or_join(merge(f, s1[1], match[1]), v1, f(v1, [
                      /* Some */0,
                      s1[3]
                    ], match[2]), merge(f, s1[4], match[3]));
    }
    else {
      exit = 1;
    }
  }
  else if (s2) {
    exit = 1;
  }
  else {
    return /* Empty */0;
  }
  if (exit === 1) {
    if (s2) {
      var v2 = s2[2];
      var match$1 = split$1(v2, s1);
      return concat_or_join(merge(f, match$1[1], s2[1]), v2, f(v2, match$1[2], [
                      /* Some */0,
                      s2[3]
                    ]), merge(f, match$1[3], s2[4]));
    }
    else {
      throw [
            0,
            Caml_exceptions.Assert_failure,
            [
              0,
              "map.ml",
              270,
              10
            ]
          ];
    }
  }
  
}

function filter(p, param) {
  if (param) {
    var d = param[3];
    var v = param[2];
    var l$prime = filter(p, param[1]);
    var pvd = p(v, d);
    var r$prime = filter(p, param[4]);
    if (pvd) {
      return join(l$prime, v, d, r$prime);
    }
    else {
      return concat(l$prime, r$prime);
    }
  }
  else {
    return /* Empty */0;
  }
}

function partition(p, param) {
  if (param) {
    var d = param[3];
    var v = param[2];
    var match = partition(p, param[1]);
    var lf = match[2];
    var lt = match[1];
    var pvd = p(v, d);
    var match$1 = partition(p, param[4]);
    var rf = match$1[2];
    var rt = match$1[1];
    if (pvd) {
      return [
              /* tuple */0,
              join(lt, v, d, rt),
              concat(lf, rf)
            ];
    }
    else {
      return [
              /* tuple */0,
              concat(lt, rt),
              join(lf, v, d, rf)
            ];
    }
  }
  else {
    return [
            /* tuple */0,
            /* Empty */0,
            /* Empty */0
          ];
  }
}

function cons_enum(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (m) {
      _e = [
        /* More */0,
        m[2],
        m[3],
        m[4],
        e
      ];
      _m = m[1];
    }
    else {
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
        var c = Caml_primitive.caml_compare(e1[1], e2[1]);
        if (c !== 0) {
          return c;
        }
        else {
          var c$1 = cmp(e1[2], e2[2]);
          if (c$1 !== 0) {
            return c$1;
          }
          else {
            _e2 = cons_enum(e2[3], e2[4]);
            _e1 = cons_enum(e1[3], e1[4]);
          }
        }
      }
      else {
        return 1;
      }
    }
    else if (e2) {
      return -1;
    }
    else {
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
        if (Caml_primitive.caml_compare(e1[1], e2[1])) {
          return /* false */0;
        }
        else if (cmp(e1[2], e2[2])) {
          _e2 = cons_enum(e2[3], e2[4]);
          _e1 = cons_enum(e1[3], e1[4]);
        }
        else {
          return /* false */0;
        }
      }
      else {
        return /* false */0;
      }
    }
    else if (e2) {
      return /* false */0;
    }
    else {
      return /* true */1;
    }
  };
}

function cardinal(param) {
  if (param) {
    return cardinal(param[1]) + 1 + cardinal(param[4]);
  }
  else {
    return 0;
  }
}

function bindings_aux(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[1];
      _accu = [
        /* :: */0,
        [
          /* tuple */0,
          param[2],
          param[3]
        ],
        bindings_aux(accu, param[4])
      ];
    }
    else {
      return accu;
    }
  };
}

function bindings(s) {
  return bindings_aux(/* [] */0, s);
}

var Ticker_map = [
  0,
  /* Empty */0,
  is_empty,
  mem,
  add,
  singleton,
  remove,
  merge,
  compare,
  equal,
  iter,
  fold,
  for_all,
  exists,
  filter,
  partition,
  cardinal,
  bindings,
  min_binding,
  max_binding,
  min_binding,
  split$1,
  find,
  map,
  mapi
];

function compute_update_sequences(all_tickers) {
  List.fold_left(function (counter, ticker) {
        var loop = function (counter, ticker) {
          var rank = ticker[2];
          if (typeof rank === "number") {
            if (rank !== 0) {
              return counter;
            }
            else {
              ticker[2] = /* Visited */1;
              var match = ticker[4];
              if (match) {
                var match$1 = match[1];
                var counter$1 = loop(counter, match$1[3]);
                var counter$2 = loop(counter$1, match$1[2]);
                var counter$3 = counter$2 + 1;
                ticker[2] = [
                  /* Ranked */0,
                  counter$3
                ];
                return counter$3;
              }
              else {
                var counter$4 = counter + 1;
                ticker[2] = [
                  /* Ranked */0,
                  counter$4
                ];
                return counter$4;
              }
            }
          }
          else {
            return counter;
          }
        };
        return loop(counter, ticker);
      }, 0, all_tickers);
  var map = List.fold_left(function (map, ticker) {
        if (ticker[4]) {
          var loop = function (_up, _map, _ticker) {
            while(true) {
              var ticker = _ticker;
              var map = _map;
              var up = _up;
              var type_ = ticker[4];
              var ticker_name = ticker[3];
              if (type_) {
                var match = type_[1];
                var map$1 = loop([
                      /* :: */0,
                      ticker,
                      up
                    ], map, match[3]);
                _ticker = match[2];
                _map = map$1;
                _up = [
                  /* :: */0,
                  ticker,
                  up
                ];
              }
              else {
                var l = find(ticker_name, map);
                return add(ticker_name, Pervasives.$at(up, l), map);
              }
            };
          };
          return loop(/* [] */0, map, ticker);
        }
        else {
          return add(ticker[3], [
                      /* :: */0,
                      ticker,
                      /* [] */0
                    ], map);
        }
      }, /* Empty */0, List.rev(all_tickers));
  return fold(function (k, l, map) {
              var l$1 = List.sort_uniq(function (param, param$1) {
                    var lhs = param[2];
                    var rhs = param$1[2];
                    if (typeof lhs === "number") {
                      return Pervasives.failwith("All nodes should be ranked");
                    }
                    else if (typeof rhs === "number") {
                      return Pervasives.failwith("All nodes should be ranked");
                    }
                    else {
                      return Caml_primitive.caml_int_compare(lhs[1], rhs[1]);
                    }
                  }, l);
              return add(k, l$1, map);
            }, map, map);
}

function process_quote(ticker_map, new_ticker, new_value) {
  var update_sequence = find(new_ticker, ticker_map);
  return List.iter(function (ticker) {
              var match = ticker[4];
              if (match) {
                var match$1 = match[1];
                var match$2 = match$1[3][1];
                var match$3 = match$1[2][1];
                var value;
                if (match$2) {
                  if (match$3) {
                    var y = match$3[1];
                    var x = match$2[1];
                    value = match$1[1] !== 0 ? [
                        /* Some */0,
                        x - y
                      ] : [
                        /* Some */0,
                        x + y
                      ];
                  }
                  else {
                    value = /* None */0;
                  }
                }
                else {
                  value = /* None */0;
                }
                ticker[1] = value;
                return /* () */0;
              }
              else if (ticker[3] === new_ticker) {
                ticker[1] = [
                  /* Some */0,
                  new_value
                ];
                return /* () */0;
              }
              else {
                return Pervasives.failwith("Only single Market ticker should be udpated upon a new quote");
              }
            }, update_sequence);
}

function process_input_line(ticker_map, all_tickers, line) {
  var make_binary_op = function (ticker_name, lhs, rhs, op) {
    var lhs$1 = find_ticker_by_name(all_tickers, lhs);
    var rhs$1 = find_ticker_by_name(all_tickers, rhs);
    return [
            /* record */0,
            /* None */0,
            /* Uninitialized */0,
            ticker_name,
            [
              /* Binary_op */0,
              [
                /* record */0,
                op,
                rhs$1,
                lhs$1
              ]
            ]
          ];
  };
  var tokens = split(/* "|" */124, line);
  if (tokens) {
    switch (tokens[1]) {
      case "Q" : 
          var match = tokens[2];
          if (match) {
            var match$1 = match[2];
            if (match$1) {
              if (match$1[2]) {
                return Pervasives.failwith("Invalid input line");
              }
              else {
                var ticker_map$1 = ticker_map ? ticker_map[1] : compute_update_sequences(all_tickers);
                var value = Caml_format.caml_float_of_string(match$1[1]);
                process_quote(ticker_map$1, match[1], value);
                return [
                        /* tuple */0,
                        all_tickers,
                        [
                          /* Some */0,
                          ticker_map$1
                        ]
                      ];
              }
            }
            else {
              return Pervasives.failwith("Invalid input line");
            }
          }
          else {
            return Pervasives.failwith("Invalid input line");
          }
          break;
      case "R" : 
          var match$2 = tokens[2];
          if (match$2) {
            var match$3 = match$2[2];
            if (match$3) {
              var ticker_name = match$2[1];
              switch (match$3[1]) {
                case "+" : 
                    var match$4 = match$3[2];
                    if (match$4) {
                      var match$5 = match$4[2];
                      if (match$5) {
                        if (match$5[2]) {
                          return Pervasives.failwith("Invalid input line");
                        }
                        else {
                          return [
                                  /* tuple */0,
                                  [
                                    /* :: */0,
                                    make_binary_op(ticker_name, match$4[1], match$5[1], /* PLUS */0),
                                    all_tickers
                                  ],
                                  ticker_map
                                ];
                        }
                      }
                      else {
                        return Pervasives.failwith("Invalid input line");
                      }
                    }
                    else {
                      return Pervasives.failwith("Invalid input line");
                    }
                    break;
                case "-" : 
                    var match$6 = match$3[2];
                    if (match$6) {
                      var match$7 = match$6[2];
                      if (match$7) {
                        if (match$7[2]) {
                          return Pervasives.failwith("Invalid input line");
                        }
                        else {
                          return [
                                  /* tuple */0,
                                  [
                                    /* :: */0,
                                    make_binary_op(ticker_name, match$6[1], match$7[1], /* MINUS */1),
                                    all_tickers
                                  ],
                                  ticker_map
                                ];
                        }
                      }
                      else {
                        return Pervasives.failwith("Invalid input line");
                      }
                    }
                    else {
                      return Pervasives.failwith("Invalid input line");
                    }
                    break;
                case "S" : 
                    if (match$3[2]) {
                      return Pervasives.failwith("Invalid input line");
                    }
                    else {
                      return [
                              /* tuple */0,
                              [
                                /* :: */0,
                                [
                                  /* record */0,
                                  /* None */0,
                                  /* Uninitialized */0,
                                  ticker_name,
                                  /* Market */0
                                ],
                                all_tickers
                              ],
                              ticker_map
                            ];
                    }
                default:
                  return Pervasives.failwith("Invalid input line");
              }
            }
            else {
              return Pervasives.failwith("Invalid input line");
            }
          }
          else {
            return Pervasives.failwith("Invalid input line");
          }
          break;
      default:
        return Pervasives.failwith("Invalid input line");
    }
  }
  else {
    return Pervasives.failwith("Invalid input line");
  }
}

function loop(_lines, _param) {
  while(true) {
    var param = _param;
    var lines = _lines;
    var all_tickers = param[1];
    if (lines) {
      _param = process_input_line(param[2], all_tickers, lines[1]);
      _lines = lines[2];
    }
    else {
      return print_all_composite(all_tickers);
    }
  };
}

var lines = [
  /* :: */0,
  "R|MSFT|S",
  [
    /* :: */0,
    "R|IBM|S",
    [
      /* :: */0,
      "R|FB|S",
      [
        /* :: */0,
        "R|CP1|+|MSFT|IBM",
        [
          /* :: */0,
          "R|CP2|-|FB|IBM",
          [
            /* :: */0,
            "R|CP12|+|CP1|CP2",
            [
              /* :: */0,
              "Q|MSFT|120.",
              [
                /* :: */0,
                "Q|IBM|130.",
                [
                  /* :: */0,
                  "Q|FB|80.",
                  /* [] */0
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
];

exports.Util                     = Util;
exports.string_of_rank           = string_of_rank;
exports.find_ticker_by_name      = find_ticker_by_name;
exports.print_all_composite      = print_all_composite;
exports.Ticker_map               = Ticker_map;
exports.compute_update_sequences = compute_update_sequences;
exports.process_quote            = process_quote;
exports.process_input_line       = process_input_line;
exports.lines                    = lines;
exports.loop                     = loop;
/* No side effect */
