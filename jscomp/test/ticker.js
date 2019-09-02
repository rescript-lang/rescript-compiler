'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");
var $$String = require("../../lib/js/string.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_format = require("../../lib/js/caml_format.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function split(delim, s) {
  var len = s.length;
  if (len !== 0) {
    var _l = "[]";
    var _i = len;
    while(true) {
      var i = _i;
      var l = _l;
      if (i !== 0) {
        var i$prime;
        try {
          i$prime = $$String.rindex_from(s, i - 1 | 0, delim);
        }
        catch (exn){
          if (exn === Caml_builtin_exceptions.not_found) {
            return /* constructor */{
                    tag: "::",
                    Arg0: $$String.sub(s, 0, i),
                    Arg1: l
                  };
          } else {
            throw exn;
          }
        }
        var l$1 = /* constructor */{
          tag: "::",
          Arg0: $$String.sub(s, i$prime + 1 | 0, (i - i$prime | 0) - 1 | 0),
          Arg1: l
        };
        var l$2 = i$prime === 0 ? /* constructor */({
              tag: "::",
              Arg0: "",
              Arg1: l$1
            }) : l$1;
        _i = i$prime;
        _l = l$2;
        continue ;
      } else {
        return l;
      }
    };
  } else {
    return "[]";
  }
}

function string_of_float_option(param) {
  if (param !== undefined) {
    return Pervasives.string_of_float(param);
  } else {
    return "nan";
  }
}

var Util = {
  split: split,
  string_of_float_option: string_of_float_option
};

function string_of_rank(param) {
  if (typeof param === "string") {
    if (param === "Uninitialized") {
      return "Uninitialized";
    } else {
      return "Visited";
    }
  } else {
    return Curry._1(Printf.sprintf(/* constructor */{
                    tag: "Format",
                    Arg0: /* constructor */{
                      tag: "String_literal",
                      Arg0: "Ranked(",
                      Arg1: /* constructor */{
                        tag: "Int",
                        Arg0: "Int_i",
                        Arg1: "No_padding",
                        Arg2: "No_precision",
                        Arg3: /* constructor */{
                          tag: "Char_literal",
                          Arg0: /* ")" */41,
                          Arg1: "End_of_format"
                        }
                      }
                    },
                    Arg1: "Ranked(%i)"
                  }), param.Arg0);
  }
}

function find_ticker_by_name(all_tickers, ticker) {
  return List.find((function (param) {
                return param[/* ticker_name */2] === ticker;
              }), all_tickers);
}

function print_all_composite(all_tickers) {
  return List.iter((function (param) {
                if (param[/* type_ */3] !== "Market") {
                  console.log(param[/* ticker_name */2]);
                  return /* () */0;
                } else {
                  return /* () */0;
                }
              }), all_tickers);
}

function height(param) {
  if (param !== "Empty") {
    return param.Arg4;
  } else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return /* constructor */{
          tag: "Node",
          Arg0: l,
          Arg1: x,
          Arg2: d,
          Arg3: r,
          Arg4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function singleton(x, d) {
  return /* constructor */{
          tag: "Node",
          Arg0: "Empty",
          Arg1: x,
          Arg2: d,
          Arg3: "Empty",
          Arg4: 1
        };
}

function bal(l, x, d, r) {
  var hl = l !== "Empty" ? l.Arg4 : 0;
  var hr = r !== "Empty" ? r.Arg4 : 0;
  if (hl > (hr + 2 | 0)) {
    if (l !== "Empty") {
      var lr = l.Arg3;
      var ld = l.Arg2;
      var lv = l.Arg1;
      var ll = l.Arg0;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      } else if (lr !== "Empty") {
        return create(create(ll, lv, ld, lr.Arg0), lr.Arg1, lr.Arg2, create(lr.Arg3, x, d, r));
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
    if (r !== "Empty") {
      var rr = r.Arg3;
      var rd = r.Arg2;
      var rv = r.Arg1;
      var rl = r.Arg0;
      if (height(rr) >= height(rl)) {
        return create(create(l, x, d, rl), rv, rd, rr);
      } else if (rl !== "Empty") {
        return create(create(l, x, d, rl.Arg0), rl.Arg1, rl.Arg2, create(rl.Arg3, rv, rd, rr));
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
    return /* constructor */{
            tag: "Node",
            Arg0: l,
            Arg1: x,
            Arg2: d,
            Arg3: r,
            Arg4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
}

function is_empty(param) {
  return param === "Empty";
}

function add(x, data, param) {
  if (param !== "Empty") {
    var r = param.Arg3;
    var d = param.Arg2;
    var v = param.Arg1;
    var l = param.Arg0;
    var c = Caml_obj.caml_compare(x, v);
    if (c === 0) {
      return /* constructor */{
              tag: "Node",
              Arg0: l,
              Arg1: x,
              Arg2: data,
              Arg3: r,
              Arg4: param.Arg4
            };
    } else if (c < 0) {
      return bal(add(x, data, l), v, d, r);
    } else {
      return bal(l, v, d, add(x, data, r));
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: "Empty",
            Arg1: x,
            Arg2: data,
            Arg3: "Empty",
            Arg4: 1
          };
  }
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var c = Caml_obj.caml_compare(x, param.Arg1);
      if (c === 0) {
        return param.Arg2;
      } else {
        _param = c < 0 ? param.Arg0 : param.Arg3;
        continue ;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var c = Caml_obj.caml_compare(x, param.Arg1);
      if (c === 0) {
        return true;
      } else {
        _param = c < 0 ? param.Arg0 : param.Arg3;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function min_binding(_param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var l = param.Arg0;
      if (l !== "Empty") {
        _param = l;
        continue ;
      } else {
        return /* tuple */[
                param.Arg1,
                param.Arg2
              ];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function max_binding(_param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var r = param.Arg3;
      if (r !== "Empty") {
        _param = r;
        continue ;
      } else {
        return /* tuple */[
                param.Arg1,
                param.Arg2
              ];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function remove_min_binding(param) {
  if (param !== "Empty") {
    var l = param.Arg0;
    if (l !== "Empty") {
      return bal(remove_min_binding(l), param.Arg1, param.Arg2, param.Arg3);
    } else {
      return param.Arg3;
    }
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Map.remove_min_elt"
        ];
  }
}

function remove(x, param) {
  if (param !== "Empty") {
    var r = param.Arg3;
    var d = param.Arg2;
    var v = param.Arg1;
    var l = param.Arg0;
    var c = Caml_obj.caml_compare(x, v);
    if (c === 0) {
      var t1 = l;
      var t2 = r;
      if (t1 !== "Empty") {
        if (t2 !== "Empty") {
          var match = min_binding(t2);
          return bal(t1, match[0], match[1], remove_min_binding(t2));
        } else {
          return t1;
        }
      } else {
        return t2;
      }
    } else if (c < 0) {
      return bal(remove(x, l), v, d, r);
    } else {
      return bal(l, v, d, remove(x, r));
    }
  } else {
    return "Empty";
  }
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      iter(f, param.Arg0);
      Curry._2(f, param.Arg1, param.Arg2);
      _param = param.Arg3;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function map(f, param) {
  if (param !== "Empty") {
    var l$prime = map(f, param.Arg0);
    var d$prime = Curry._1(f, param.Arg2);
    var r$prime = map(f, param.Arg3);
    return /* constructor */{
            tag: "Node",
            Arg0: l$prime,
            Arg1: param.Arg1,
            Arg2: d$prime,
            Arg3: r$prime,
            Arg4: param.Arg4
          };
  } else {
    return "Empty";
  }
}

function mapi(f, param) {
  if (param !== "Empty") {
    var v = param.Arg1;
    var l$prime = mapi(f, param.Arg0);
    var d$prime = Curry._2(f, v, param.Arg2);
    var r$prime = mapi(f, param.Arg3);
    return /* constructor */{
            tag: "Node",
            Arg0: l$prime,
            Arg1: v,
            Arg2: d$prime,
            Arg3: r$prime,
            Arg4: param.Arg4
          };
  } else {
    return "Empty";
  }
}

function fold(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (m !== "Empty") {
      _accu = Curry._3(f, m.Arg1, m.Arg2, fold(f, m.Arg0, accu));
      _m = m.Arg3;
      continue ;
    } else {
      return accu;
    }
  };
}

function for_all(p, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      if (Curry._2(p, param.Arg1, param.Arg2) && for_all(p, param.Arg0)) {
        _param = param.Arg3;
        continue ;
      } else {
        return false;
      }
    } else {
      return true;
    }
  };
}

function exists(p, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      if (Curry._2(p, param.Arg1, param.Arg2) || exists(p, param.Arg0)) {
        return true;
      } else {
        _param = param.Arg3;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function add_min_binding(k, v, param) {
  if (param !== "Empty") {
    return bal(add_min_binding(k, v, param.Arg0), param.Arg1, param.Arg2, param.Arg3);
  } else {
    return singleton(k, v);
  }
}

function add_max_binding(k, v, param) {
  if (param !== "Empty") {
    return bal(param.Arg0, param.Arg1, param.Arg2, add_max_binding(k, v, param.Arg3));
  } else {
    return singleton(k, v);
  }
}

function join(l, v, d, r) {
  if (l !== "Empty") {
    if (r !== "Empty") {
      var rh = r.Arg4;
      var lh = l.Arg4;
      if (lh > (rh + 2 | 0)) {
        return bal(l.Arg0, l.Arg1, l.Arg2, join(l.Arg3, v, d, r));
      } else if (rh > (lh + 2 | 0)) {
        return bal(join(l, v, d, r.Arg0), r.Arg1, r.Arg2, r.Arg3);
      } else {
        return create(l, v, d, r);
      }
    } else {
      return add_max_binding(v, d, l);
    }
  } else {
    return add_min_binding(v, d, r);
  }
}

function concat(t1, t2) {
  if (t1 !== "Empty") {
    if (t2 !== "Empty") {
      var match = min_binding(t2);
      return join(t1, match[0], match[1], remove_min_binding(t2));
    } else {
      return t1;
    }
  } else {
    return t2;
  }
}

function concat_or_join(t1, v, d, t2) {
  if (d !== undefined) {
    return join(t1, v, Caml_option.valFromOption(d), t2);
  } else {
    return concat(t1, t2);
  }
}

function split$1(x, param) {
  if (param !== "Empty") {
    var r = param.Arg3;
    var d = param.Arg2;
    var v = param.Arg1;
    var l = param.Arg0;
    var c = Caml_obj.caml_compare(x, v);
    if (c === 0) {
      return /* tuple */[
              l,
              Caml_option.some(d),
              r
            ];
    } else if (c < 0) {
      var match = split$1(x, l);
      return /* tuple */[
              match[0],
              match[1],
              join(match[2], v, d, r)
            ];
    } else {
      var match$1 = split$1(x, r);
      return /* tuple */[
              join(l, v, d, match$1[0]),
              match$1[1],
              match$1[2]
            ];
    }
  } else {
    return /* tuple */[
            "Empty",
            undefined,
            "Empty"
          ];
  }
}

function merge(f, s1, s2) {
  if (s1 !== "Empty") {
    var v1 = s1.Arg1;
    if (s1.Arg4 >= height(s2)) {
      var match = split$1(v1, s2);
      return concat_or_join(merge(f, s1.Arg0, match[0]), v1, Curry._3(f, v1, Caml_option.some(s1.Arg2), match[1]), merge(f, s1.Arg3, match[2]));
    }
    
  } else if (s2 === "Empty") {
    return "Empty";
  }
  if (s2 !== "Empty") {
    var v2 = s2.Arg1;
    var match$1 = split$1(v2, s1);
    return concat_or_join(merge(f, match$1[0], s2.Arg0), v2, Curry._3(f, v2, match$1[1], Caml_option.some(s2.Arg2)), merge(f, match$1[2], s2.Arg3));
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "map.ml",
            270,
            10
          ]
        ];
  }
}

function filter(p, param) {
  if (param !== "Empty") {
    var d = param.Arg2;
    var v = param.Arg1;
    var l$prime = filter(p, param.Arg0);
    var pvd = Curry._2(p, v, d);
    var r$prime = filter(p, param.Arg3);
    if (pvd) {
      return join(l$prime, v, d, r$prime);
    } else {
      return concat(l$prime, r$prime);
    }
  } else {
    return "Empty";
  }
}

function partition(p, param) {
  if (param !== "Empty") {
    var d = param.Arg2;
    var v = param.Arg1;
    var match = partition(p, param.Arg0);
    var lf = match[1];
    var lt = match[0];
    var pvd = Curry._2(p, v, d);
    var match$1 = partition(p, param.Arg3);
    var rf = match$1[1];
    var rt = match$1[0];
    if (pvd) {
      return /* tuple */[
              join(lt, v, d, rt),
              concat(lf, rf)
            ];
    } else {
      return /* tuple */[
              concat(lt, rt),
              join(lf, v, d, rf)
            ];
    }
  } else {
    return /* tuple */[
            "Empty",
            "Empty"
          ];
  }
}

function cons_enum(_m, _e) {
  while(true) {
    var e = _e;
    var m = _m;
    if (m !== "Empty") {
      _e = /* constructor */{
        tag: "More",
        Arg0: m.Arg1,
        Arg1: m.Arg2,
        Arg2: m.Arg3,
        Arg3: e
      };
      _m = m.Arg0;
      continue ;
    } else {
      return e;
    }
  };
}

function compare(cmp, m1, m2) {
  var _e1 = cons_enum(m1, "End");
  var _e2 = cons_enum(m2, "End");
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1 !== "End") {
      if (e2 !== "End") {
        var c = Caml_obj.caml_compare(e1.Arg0, e2.Arg0);
        if (c !== 0) {
          return c;
        } else {
          var c$1 = Curry._2(cmp, e1.Arg1, e2.Arg1);
          if (c$1 !== 0) {
            return c$1;
          } else {
            _e2 = cons_enum(e2.Arg2, e2.Arg3);
            _e1 = cons_enum(e1.Arg2, e1.Arg3);
            continue ;
          }
        }
      } else {
        return 1;
      }
    } else if (e2 !== "End") {
      return -1;
    } else {
      return 0;
    }
  };
}

function equal(cmp, m1, m2) {
  var _e1 = cons_enum(m1, "End");
  var _e2 = cons_enum(m2, "End");
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1 !== "End") {
      if (e2 !== "End" && Caml_obj.caml_equal(e1.Arg0, e2.Arg0) && Curry._2(cmp, e1.Arg1, e2.Arg1)) {
        _e2 = cons_enum(e2.Arg2, e2.Arg3);
        _e1 = cons_enum(e1.Arg2, e1.Arg3);
        continue ;
      } else {
        return false;
      }
    } else {
      return e2 === "End";
    }
  };
}

function cardinal(param) {
  if (param !== "Empty") {
    return (cardinal(param.Arg0) + 1 | 0) + cardinal(param.Arg3) | 0;
  } else {
    return 0;
  }
}

function bindings_aux(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param !== "Empty") {
      _param = param.Arg0;
      _accu = /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          param.Arg1,
          param.Arg2
        ],
        Arg1: bindings_aux(accu, param.Arg3)
      };
      continue ;
    } else {
      return accu;
    }
  };
}

function bindings(s) {
  return bindings_aux("[]", s);
}

var Ticker_map = {
  empty: "Empty",
  is_empty: is_empty,
  mem: mem,
  add: add,
  singleton: singleton,
  remove: remove,
  merge: merge,
  compare: compare,
  equal: equal,
  iter: iter,
  fold: fold,
  for_all: for_all,
  exists: exists,
  filter: filter,
  partition: partition,
  cardinal: cardinal,
  bindings: bindings,
  min_binding: min_binding,
  max_binding: max_binding,
  choose: min_binding,
  split: split$1,
  find: find,
  map: map,
  mapi: mapi
};

function compute_update_sequences(all_tickers) {
  List.fold_left((function (counter, ticker) {
          var loop = function (counter, ticker) {
            var rank = ticker[/* rank */1];
            if (typeof rank === "string" && rank === "Uninitialized") {
              ticker[/* rank */1] = "Visited";
              var match = ticker[/* type_ */3];
              if (match !== "Market") {
                var match$1 = match.Arg0;
                var counter$1 = loop(counter, match$1[/* lhs */2]);
                var counter$2 = loop(counter$1, match$1[/* rhs */1]);
                var counter$3 = counter$2 + 1 | 0;
                ticker[/* rank */1] = /* constructor */{
                  tag: "Ranked",
                  Arg0: counter$3
                };
                return counter$3;
              } else {
                var counter$4 = counter + 1 | 0;
                ticker[/* rank */1] = /* constructor */{
                  tag: "Ranked",
                  Arg0: counter$4
                };
                return counter$4;
              }
            } else {
              return counter;
            }
          };
          return loop(counter, ticker);
        }), 0, all_tickers);
  var map = List.fold_left((function (map, ticker) {
          if (ticker[/* type_ */3] !== "Market") {
            var loop = function (_up, _map, _ticker) {
              while(true) {
                var ticker = _ticker;
                var map = _map;
                var up = _up;
                var type_ = ticker[/* type_ */3];
                var ticker_name = ticker[/* ticker_name */2];
                if (type_ !== "Market") {
                  var match = type_.Arg0;
                  var map$1 = loop(/* constructor */{
                        tag: "::",
                        Arg0: ticker,
                        Arg1: up
                      }, map, match[/* lhs */2]);
                  _ticker = match[/* rhs */1];
                  _map = map$1;
                  _up = /* constructor */{
                    tag: "::",
                    Arg0: ticker,
                    Arg1: up
                  };
                  continue ;
                } else {
                  var l = find(ticker_name, map);
                  return add(ticker_name, Pervasives.$at(up, l), map);
                }
              };
            };
            return loop("[]", map, ticker);
          } else {
            return add(ticker[/* ticker_name */2], /* constructor */{
                        tag: "::",
                        Arg0: ticker,
                        Arg1: "[]"
                      }, map);
          }
        }), "Empty", List.rev(all_tickers));
  return fold((function (k, l, map) {
                var l$1 = List.sort_uniq((function (lhs, rhs) {
                        var match = lhs[/* rank */1];
                        if (typeof match === "string") {
                          if (match === "Uninitialized") {
                            throw [
                                  Caml_builtin_exceptions.failure,
                                  "All nodes should be ranked"
                                ];
                          } else {
                            throw [
                                  Caml_builtin_exceptions.failure,
                                  "All nodes should be ranked"
                                ];
                          }
                        } else {
                          var match$1 = rhs[/* rank */1];
                          if (typeof match$1 === "string") {
                            if (match$1 === "Uninitialized") {
                              throw [
                                    Caml_builtin_exceptions.failure,
                                    "All nodes should be ranked"
                                  ];
                            } else {
                              throw [
                                    Caml_builtin_exceptions.failure,
                                    "All nodes should be ranked"
                                  ];
                            }
                          } else {
                            return Caml_primitive.caml_int_compare(match.Arg0, match$1.Arg0);
                          }
                        }
                      }), l);
                return add(k, l$1, map);
              }), map, map);
}

function process_quote(ticker_map, new_ticker, new_value) {
  var update_sequence = find(new_ticker, ticker_map);
  return List.iter((function (ticker) {
                var match = ticker[/* type_ */3];
                if (match !== "Market") {
                  var match$1 = match.Arg0;
                  var match$2 = match$1[/* lhs */2][/* value */0];
                  var match$3 = match$1[/* rhs */1][/* value */0];
                  var value;
                  if (match$2 !== undefined && match$3 !== undefined) {
                    var y = match$3;
                    var x = match$2;
                    value = match$1[/* op */0] !== "PLUS" ? x - y : x + y;
                  } else {
                    value = undefined;
                  }
                  ticker[/* value */0] = value;
                  return /* () */0;
                } else if (ticker[/* ticker_name */2] === new_ticker) {
                  ticker[/* value */0] = new_value;
                  return /* () */0;
                } else {
                  throw [
                        Caml_builtin_exceptions.failure,
                        "Only single Market ticker should be udpated upon a new quote"
                      ];
                }
              }), update_sequence);
}

function process_input_line(ticker_map, all_tickers, line) {
  var make_binary_op = function (ticker_name, lhs, rhs, op) {
    var lhs$1 = find_ticker_by_name(all_tickers, lhs);
    var rhs$1 = find_ticker_by_name(all_tickers, rhs);
    return /* record */[
            /* value */undefined,
            /* rank */"Uninitialized",
            /* ticker_name */ticker_name,
            /* type_ : constructor */{
              tag: "Binary_op",
              Arg0: /* record */[
                /* op */op,
                /* rhs */rhs$1,
                /* lhs */lhs$1
              ]
            }
          ];
  };
  var tokens = split(/* "|" */124, line);
  if (tokens !== "[]") {
    switch (tokens.Arg0) {
      case "Q" :
          var match = tokens.Arg1;
          if (match !== "[]") {
            var match$1 = match.Arg1;
            if (match$1 !== "[]") {
              if (match$1.Arg1 !== "[]") {
                throw [
                      Caml_builtin_exceptions.failure,
                      "Invalid input line"
                    ];
              }
              var ticker_map$1 = ticker_map !== undefined ? Caml_option.valFromOption(ticker_map) : compute_update_sequences(all_tickers);
              var value = Caml_format.caml_float_of_string(match$1.Arg0);
              process_quote(ticker_map$1, match.Arg0, value);
              return /* tuple */[
                      all_tickers,
                      Caml_option.some(ticker_map$1)
                    ];
            } else {
              throw [
                    Caml_builtin_exceptions.failure,
                    "Invalid input line"
                  ];
            }
          } else {
            throw [
                  Caml_builtin_exceptions.failure,
                  "Invalid input line"
                ];
          }
      case "R" :
          var match$2 = tokens.Arg1;
          if (match$2 !== "[]") {
            var match$3 = match$2.Arg1;
            if (match$3 !== "[]") {
              var ticker_name = match$2.Arg0;
              switch (match$3.Arg0) {
                case "+" :
                    var match$4 = match$3.Arg1;
                    if (match$4 !== "[]") {
                      var match$5 = match$4.Arg1;
                      if (match$5 !== "[]") {
                        if (match$5.Arg1 !== "[]") {
                          throw [
                                Caml_builtin_exceptions.failure,
                                "Invalid input line"
                              ];
                        }
                        return /* tuple */[
                                /* constructor */{
                                  tag: "::",
                                  Arg0: make_binary_op(ticker_name, match$4.Arg0, match$5.Arg0, "PLUS"),
                                  Arg1: all_tickers
                                },
                                ticker_map
                              ];
                      } else {
                        throw [
                              Caml_builtin_exceptions.failure,
                              "Invalid input line"
                            ];
                      }
                    } else {
                      throw [
                            Caml_builtin_exceptions.failure,
                            "Invalid input line"
                          ];
                    }
                case "-" :
                    var match$6 = match$3.Arg1;
                    if (match$6 !== "[]") {
                      var match$7 = match$6.Arg1;
                      if (match$7 !== "[]") {
                        if (match$7.Arg1 !== "[]") {
                          throw [
                                Caml_builtin_exceptions.failure,
                                "Invalid input line"
                              ];
                        }
                        return /* tuple */[
                                /* constructor */{
                                  tag: "::",
                                  Arg0: make_binary_op(ticker_name, match$6.Arg0, match$7.Arg0, "MINUS"),
                                  Arg1: all_tickers
                                },
                                ticker_map
                              ];
                      } else {
                        throw [
                              Caml_builtin_exceptions.failure,
                              "Invalid input line"
                            ];
                      }
                    } else {
                      throw [
                            Caml_builtin_exceptions.failure,
                            "Invalid input line"
                          ];
                    }
                case "S" :
                    if (match$3.Arg1 !== "[]") {
                      throw [
                            Caml_builtin_exceptions.failure,
                            "Invalid input line"
                          ];
                    }
                    return /* tuple */[
                            /* constructor */{
                              tag: "::",
                              Arg0: /* record */[
                                /* value */undefined,
                                /* rank */"Uninitialized",
                                /* ticker_name */ticker_name,
                                /* type_ */"Market"
                              ],
                              Arg1: all_tickers
                            },
                            ticker_map
                          ];
                default:
                  throw [
                        Caml_builtin_exceptions.failure,
                        "Invalid input line"
                      ];
              }
            } else {
              throw [
                    Caml_builtin_exceptions.failure,
                    "Invalid input line"
                  ];
            }
          } else {
            throw [
                  Caml_builtin_exceptions.failure,
                  "Invalid input line"
                ];
          }
      default:
        throw [
              Caml_builtin_exceptions.failure,
              "Invalid input line"
            ];
    }
  } else {
    throw [
          Caml_builtin_exceptions.failure,
          "Invalid input line"
        ];
  }
}

function loop(_lines, _param) {
  while(true) {
    var param = _param;
    var lines = _lines;
    var all_tickers = param[0];
    if (lines !== "[]") {
      _param = process_input_line(param[1], all_tickers, lines.Arg0);
      _lines = lines.Arg1;
      continue ;
    } else {
      return print_all_composite(all_tickers);
    }
  };
}

var lines = /* constructor */{
  tag: "::",
  Arg0: "R|MSFT|S",
  Arg1: /* constructor */{
    tag: "::",
    Arg0: "R|IBM|S",
    Arg1: /* constructor */{
      tag: "::",
      Arg0: "R|FB|S",
      Arg1: /* constructor */{
        tag: "::",
        Arg0: "R|CP1|+|MSFT|IBM",
        Arg1: /* constructor */{
          tag: "::",
          Arg0: "R|CP2|-|FB|IBM",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "R|CP12|+|CP1|CP2",
            Arg1: /* constructor */{
              tag: "::",
              Arg0: "Q|MSFT|120.",
              Arg1: /* constructor */{
                tag: "::",
                Arg0: "Q|IBM|130.",
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: "Q|FB|80.",
                  Arg1: "[]"
                }
              }
            }
          }
        }
      }
    }
  }
};

exports.Util = Util;
exports.string_of_rank = string_of_rank;
exports.find_ticker_by_name = find_ticker_by_name;
exports.print_all_composite = print_all_composite;
exports.Ticker_map = Ticker_map;
exports.compute_update_sequences = compute_update_sequences;
exports.process_quote = process_quote;
exports.process_input_line = process_input_line;
exports.lines = lines;
exports.loop = loop;
/* No side effect */
