'use strict';

var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
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
    var _l = /* [] */0;
    var _i = len;
    while(true) {
      var i = _i;
      var l = _l;
      if (i !== 0) {
        var exit = 0;
        var i$prime;
        try {
          i$prime = $$String.rindex_from(s, i - 1 | 0, delim);
          exit = 1;
        }
        catch (exn){
          if (exn === Caml_builtin_exceptions.not_found) {
            return /* :: */[
                    $$String.sub(s, 0, i),
                    l
                  ];
          } else {
            throw exn;
          }
        }
        if (exit === 1) {
          var l_000 = $$String.sub(s, i$prime + 1 | 0, (i - i$prime | 0) - 1 | 0);
          var l$1 = /* :: */[
            l_000,
            l
          ];
          var l$2 = i$prime === 0 ? /* :: */[
              "",
              l$1
            ] : l$1;
          _i = i$prime;
          _l = l$2;
          continue ;
        }
        
      } else {
        return l;
      }
    };
  } else {
    return /* [] */0;
  }
}

function string_of_float_option(param) {
  if (param !== undefined) {
    return Pervasives.string_of_float(param);
  } else {
    return "nan";
  }
}

var Util = /* module */[
  /* split */split,
  /* string_of_float_option */string_of_float_option
];

function string_of_rank(param) {
  if (typeof param === "number") {
    if (param !== 0) {
      return "Visited";
    } else {
      return "Uninitialized";
    }
  } else {
    return Curry._1(Printf.sprintf(/* Format */[
                    /* String_literal */Block.__(11, [
                        "Ranked(",
                        /* Int */Block.__(4, [
                            /* Int_i */3,
                            /* No_padding */0,
                            /* No_precision */0,
                            /* Char_literal */Block.__(12, [
                                /* ")" */41,
                                /* End_of_format */0
                              ])
                          ])
                      ]),
                    "Ranked(%i)"
                  ]), param[0]);
  }
}

function find_ticker_by_name(all_tickers, ticker) {
  return List.find((function (param) {
                return param[/* ticker_name */2] === ticker;
              }), all_tickers);
}

function print_all_composite(all_tickers) {
  return List.iter((function (param) {
                if (param[/* type_ */3]) {
                  console.log(param[/* ticker_name */2]);
                  return /* () */0;
                } else {
                  return /* () */0;
                }
              }), all_tickers);
}

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

function singleton(x, d) {
  return /* Node */[
          /* l : Empty */0,
          /* v */x,
          /* d */d,
          /* r : Empty */0,
          /* h */1
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

function is_empty(param) {
  if (param) {
    return false;
  } else {
    return true;
  }
}

function add(x, data, m) {
  if (m) {
    var r = m[/* r */3];
    var d = m[/* d */2];
    var v = m[/* v */1];
    var l = m[/* l */0];
    var c = Caml_obj.caml_compare(x, v);
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

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_obj.caml_compare(x, param[/* v */1]);
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

function find_first(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[/* v */1];
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _d0 = param[/* d */2];
        var f$1 = f;
        var _param$1 = param[/* l */0];
        while(true) {
          var param$1 = _param$1;
          var d0 = _d0;
          var v0 = _v0;
          if (param$1) {
            var v$1 = param$1[/* v */1];
            if (Curry._1(f$1, v$1)) {
              _param$1 = param$1[/* l */0];
              _d0 = param$1[/* d */2];
              _v0 = v$1;
              continue ;
            } else {
              _param$1 = param$1[/* r */3];
              continue ;
            }
          } else {
            return /* tuple */[
                    v0,
                    d0
                  ];
          }
        };
      } else {
        _param = param[/* r */3];
        continue ;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function find_first_opt(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[/* v */1];
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _d0 = param[/* d */2];
        var f$1 = f;
        var _param$1 = param[/* l */0];
        while(true) {
          var param$1 = _param$1;
          var d0 = _d0;
          var v0 = _v0;
          if (param$1) {
            var v$1 = param$1[/* v */1];
            if (Curry._1(f$1, v$1)) {
              _param$1 = param$1[/* l */0];
              _d0 = param$1[/* d */2];
              _v0 = v$1;
              continue ;
            } else {
              _param$1 = param$1[/* r */3];
              continue ;
            }
          } else {
            return /* tuple */[
                    v0,
                    d0
                  ];
          }
        };
      } else {
        _param = param[/* r */3];
        continue ;
      }
    } else {
      return undefined;
    }
  };
}

function find_last(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[/* v */1];
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _d0 = param[/* d */2];
        var f$1 = f;
        var _param$1 = param[/* r */3];
        while(true) {
          var param$1 = _param$1;
          var d0 = _d0;
          var v0 = _v0;
          if (param$1) {
            var v$1 = param$1[/* v */1];
            if (Curry._1(f$1, v$1)) {
              _param$1 = param$1[/* r */3];
              _d0 = param$1[/* d */2];
              _v0 = v$1;
              continue ;
            } else {
              _param$1 = param$1[/* l */0];
              continue ;
            }
          } else {
            return /* tuple */[
                    v0,
                    d0
                  ];
          }
        };
      } else {
        _param = param[/* l */0];
        continue ;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function find_last_opt(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[/* v */1];
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _d0 = param[/* d */2];
        var f$1 = f;
        var _param$1 = param[/* r */3];
        while(true) {
          var param$1 = _param$1;
          var d0 = _d0;
          var v0 = _v0;
          if (param$1) {
            var v$1 = param$1[/* v */1];
            if (Curry._1(f$1, v$1)) {
              _param$1 = param$1[/* r */3];
              _d0 = param$1[/* d */2];
              _v0 = v$1;
              continue ;
            } else {
              _param$1 = param$1[/* l */0];
              continue ;
            }
          } else {
            return /* tuple */[
                    v0,
                    d0
                  ];
          }
        };
      } else {
        _param = param[/* l */0];
        continue ;
      }
    } else {
      return undefined;
    }
  };
}

function find_opt(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_obj.caml_compare(x, param[/* v */1]);
      if (c === 0) {
        return Caml_option.some(param[/* d */2]);
      } else {
        _param = c < 0 ? param[/* l */0] : param[/* r */3];
        continue ;
      }
    } else {
      return undefined;
    }
  };
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_obj.caml_compare(x, param[/* v */1]);
      if (c === 0) {
        return true;
      } else {
        _param = c < 0 ? param[/* l */0] : param[/* r */3];
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
    if (param) {
      var l = param[/* l */0];
      if (l) {
        _param = l;
        continue ;
      } else {
        return /* tuple */[
                param[/* v */1],
                param[/* d */2]
              ];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function min_binding_opt(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param[/* l */0];
      if (l) {
        _param = l;
        continue ;
      } else {
        return /* tuple */[
                param[/* v */1],
                param[/* d */2]
              ];
      }
    } else {
      return undefined;
    }
  };
}

function max_binding(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var r = param[/* r */3];
      if (r) {
        _param = r;
        continue ;
      } else {
        return /* tuple */[
                param[/* v */1],
                param[/* d */2]
              ];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function max_binding_opt(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var r = param[/* r */3];
      if (r) {
        _param = r;
        continue ;
      } else {
        return /* tuple */[
                param[/* v */1],
                param[/* d */2]
              ];
      }
    } else {
      return undefined;
    }
  };
}

function remove_min_binding(param) {
  if (param) {
    var l = param[/* l */0];
    if (l) {
      return bal(remove_min_binding(l), param[/* v */1], param[/* d */2], param[/* r */3]);
    } else {
      return param[/* r */3];
    }
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Map.remove_min_elt"
        ];
  }
}

function merge(t1, t2) {
  if (t1) {
    if (t2) {
      var match = min_binding(t2);
      return bal(t1, match[0], match[1], remove_min_binding(t2));
    } else {
      return t1;
    }
  } else {
    return t2;
  }
}

function remove(x, m) {
  if (m) {
    var r = m[/* r */3];
    var d = m[/* d */2];
    var v = m[/* v */1];
    var l = m[/* l */0];
    var c = Caml_obj.caml_compare(x, v);
    if (c === 0) {
      return merge(l, r);
    } else if (c < 0) {
      var ll = remove(x, l);
      if (l === ll) {
        return m;
      } else {
        return bal(ll, v, d, r);
      }
    } else {
      var rr = remove(x, r);
      if (r === rr) {
        return m;
      } else {
        return bal(l, v, d, rr);
      }
    }
  } else {
    return /* Empty */0;
  }
}

function update(x, f, m) {
  if (m) {
    var r = m[/* r */3];
    var d = m[/* d */2];
    var v = m[/* v */1];
    var l = m[/* l */0];
    var c = Caml_obj.caml_compare(x, v);
    if (c === 0) {
      var match = Curry._1(f, Caml_option.some(d));
      if (match !== undefined) {
        var data = Caml_option.valFromOption(match);
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
      } else {
        return merge(l, r);
      }
    } else if (c < 0) {
      var ll = update(x, f, l);
      if (l === ll) {
        return m;
      } else {
        return bal(ll, v, d, r);
      }
    } else {
      var rr = update(x, f, r);
      if (r === rr) {
        return m;
      } else {
        return bal(l, v, d, rr);
      }
    }
  } else {
    var match$1 = Curry._1(f, undefined);
    if (match$1 !== undefined) {
      return /* Node */[
              /* l : Empty */0,
              /* v */x,
              /* d */Caml_option.valFromOption(match$1),
              /* r : Empty */0,
              /* h */1
            ];
    } else {
      return /* Empty */0;
    }
  }
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      iter(f, param[/* l */0]);
      Curry._2(f, param[/* v */1], param[/* d */2]);
      _param = param[/* r */3];
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function map(f, param) {
  if (param) {
    var l$prime = map(f, param[/* l */0]);
    var d$prime = Curry._1(f, param[/* d */2]);
    var r$prime = map(f, param[/* r */3]);
    return /* Node */[
            /* l */l$prime,
            /* v */param[/* v */1],
            /* d */d$prime,
            /* r */r$prime,
            /* h */param[/* h */4]
          ];
  } else {
    return /* Empty */0;
  }
}

function mapi(f, param) {
  if (param) {
    var v = param[/* v */1];
    var l$prime = mapi(f, param[/* l */0]);
    var d$prime = Curry._2(f, v, param[/* d */2]);
    var r$prime = mapi(f, param[/* r */3]);
    return /* Node */[
            /* l */l$prime,
            /* v */v,
            /* d */d$prime,
            /* r */r$prime,
            /* h */param[/* h */4]
          ];
  } else {
    return /* Empty */0;
  }
}

function fold(f, _m, _accu) {
  while(true) {
    var accu = _accu;
    var m = _m;
    if (m) {
      _accu = Curry._3(f, m[/* v */1], m[/* d */2], fold(f, m[/* l */0], accu));
      _m = m[/* r */3];
      continue ;
    } else {
      return accu;
    }
  };
}

function for_all(p, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (Curry._2(p, param[/* v */1], param[/* d */2]) && for_all(p, param[/* l */0])) {
        _param = param[/* r */3];
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
    if (param) {
      if (Curry._2(p, param[/* v */1], param[/* d */2]) || exists(p, param[/* l */0])) {
        return true;
      } else {
        _param = param[/* r */3];
        continue ;
      }
    } else {
      return false;
    }
  };
}

function add_min_binding(k, x, param) {
  if (param) {
    return bal(add_min_binding(k, x, param[/* l */0]), param[/* v */1], param[/* d */2], param[/* r */3]);
  } else {
    return singleton(k, x);
  }
}

function add_max_binding(k, x, param) {
  if (param) {
    return bal(param[/* l */0], param[/* v */1], param[/* d */2], add_max_binding(k, x, param[/* r */3]));
  } else {
    return singleton(k, x);
  }
}

function join(l, v, d, r) {
  if (l) {
    if (r) {
      var rh = r[/* h */4];
      var lh = l[/* h */4];
      if (lh > (rh + 2 | 0)) {
        return bal(l[/* l */0], l[/* v */1], l[/* d */2], join(l[/* r */3], v, d, r));
      } else if (rh > (lh + 2 | 0)) {
        return bal(join(l, v, d, r[/* l */0]), r[/* v */1], r[/* d */2], r[/* r */3]);
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
  if (t1) {
    if (t2) {
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
  if (param) {
    var r = param[/* r */3];
    var d = param[/* d */2];
    var v = param[/* v */1];
    var l = param[/* l */0];
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
            /* Empty */0,
            undefined,
            /* Empty */0
          ];
  }
}

function merge$1(f, s1, s2) {
  var exit = 0;
  if (s1) {
    var v1 = s1[/* v */1];
    if (s1[/* h */4] >= height(s2)) {
      var match = split$1(v1, s2);
      return concat_or_join(merge$1(f, s1[/* l */0], match[0]), v1, Curry._3(f, v1, Caml_option.some(s1[/* d */2]), match[1]), merge$1(f, s1[/* r */3], match[2]));
    } else {
      exit = 1;
    }
  } else if (s2) {
    exit = 1;
  } else {
    return /* Empty */0;
  }
  if (exit === 1) {
    if (s2) {
      var v2 = s2[/* v */1];
      var match$1 = split$1(v2, s1);
      return concat_or_join(merge$1(f, match$1[0], s2[/* l */0]), v2, Curry._3(f, v2, match$1[1], Caml_option.some(s2[/* d */2])), merge$1(f, match$1[2], s2[/* r */3]));
    } else {
      throw [
            Caml_builtin_exceptions.assert_failure,
            /* tuple */[
              "map.ml",
              393,
              10
            ]
          ];
    }
  }
  
}

function union(f, s1, s2) {
  if (s1) {
    if (s2) {
      var d2 = s2[/* d */2];
      var v2 = s2[/* v */1];
      var d1 = s1[/* d */2];
      var v1 = s1[/* v */1];
      if (s1[/* h */4] >= s2[/* h */4]) {
        var match = split$1(v1, s2);
        var d2$1 = match[1];
        var l = union(f, s1[/* l */0], match[0]);
        var r = union(f, s1[/* r */3], match[2]);
        if (d2$1 !== undefined) {
          return concat_or_join(l, v1, Curry._3(f, v1, d1, Caml_option.valFromOption(d2$1)), r);
        } else {
          return join(l, v1, d1, r);
        }
      } else {
        var match$1 = split$1(v2, s1);
        var d1$1 = match$1[1];
        var l$1 = union(f, match$1[0], s2[/* l */0]);
        var r$1 = union(f, match$1[2], s2[/* r */3]);
        if (d1$1 !== undefined) {
          return concat_or_join(l$1, v2, Curry._3(f, v2, Caml_option.valFromOption(d1$1), d2), r$1);
        } else {
          return join(l$1, v2, d2, r$1);
        }
      }
    } else {
      return s1;
    }
  } else {
    return s2;
  }
}

function filter(p, m) {
  if (m) {
    var r = m[/* r */3];
    var d = m[/* d */2];
    var v = m[/* v */1];
    var l = m[/* l */0];
    var l$prime = filter(p, l);
    var pvd = Curry._2(p, v, d);
    var r$prime = filter(p, r);
    if (pvd) {
      if (l === l$prime && r === r$prime) {
        return m;
      } else {
        return join(l$prime, v, d, r$prime);
      }
    } else {
      return concat(l$prime, r$prime);
    }
  } else {
    return /* Empty */0;
  }
}

function partition(p, param) {
  if (param) {
    var d = param[/* d */2];
    var v = param[/* v */1];
    var match = partition(p, param[/* l */0]);
    var lf = match[1];
    var lt = match[0];
    var pvd = Curry._2(p, v, d);
    var match$1 = partition(p, param[/* r */3]);
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
        var c = Caml_obj.caml_compare(e1[0], e2[0]);
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
      if (e2 && Caml_obj.caml_equal(e1[0], e2[0]) && Curry._2(cmp, e1[1], e2[1])) {
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

function bindings_aux(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[/* l */0];
      _accu = /* :: */[
        /* tuple */[
          param[/* v */1],
          param[/* d */2]
        ],
        bindings_aux(accu, param[/* r */3])
      ];
      continue ;
    } else {
      return accu;
    }
  };
}

function bindings(s) {
  return bindings_aux(/* [] */0, s);
}

var Ticker_map = [
  /* Empty */0,
  is_empty,
  mem,
  add,
  update,
  singleton,
  remove,
  merge$1,
  union,
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
  min_binding_opt,
  max_binding,
  max_binding_opt,
  min_binding,
  min_binding_opt,
  split$1,
  find,
  find_opt,
  find_first,
  find_first_opt,
  find_last,
  find_last_opt,
  map,
  mapi
];

function compute_update_sequences(all_tickers) {
  List.fold_left((function (counter, ticker) {
          var loop = function (counter, ticker) {
            var rank = ticker[/* rank */1];
            if (typeof rank === "number" && rank === 0) {
              ticker[/* rank */1] = /* Visited */1;
              var match = ticker[/* type_ */3];
              if (match) {
                var match$1 = match[0];
                var counter$1 = loop(counter, match$1[/* lhs */2]);
                var counter$2 = loop(counter$1, match$1[/* rhs */1]);
                var counter$3 = counter$2 + 1 | 0;
                ticker[/* rank */1] = /* Ranked */[counter$3];
                return counter$3;
              } else {
                var counter$4 = counter + 1 | 0;
                ticker[/* rank */1] = /* Ranked */[counter$4];
                return counter$4;
              }
            } else {
              return counter;
            }
          };
          return loop(counter, ticker);
        }), 0, all_tickers);
  var map = List.fold_left((function (map, ticker) {
          if (ticker[/* type_ */3]) {
            var loop = function (_up, _map, _ticker) {
              while(true) {
                var ticker = _ticker;
                var map = _map;
                var up = _up;
                var type_ = ticker[/* type_ */3];
                var ticker_name = ticker[/* ticker_name */2];
                if (type_) {
                  var match = type_[0];
                  var map$1 = loop(/* :: */[
                        ticker,
                        up
                      ], map, match[/* lhs */2]);
                  _ticker = match[/* rhs */1];
                  _map = map$1;
                  _up = /* :: */[
                    ticker,
                    up
                  ];
                  continue ;
                } else {
                  var l = find(ticker_name, map);
                  return add(ticker_name, Pervasives.$at(up, l), map);
                }
              };
            };
            return loop(/* [] */0, map, ticker);
          } else {
            return add(ticker[/* ticker_name */2], /* :: */[
                        ticker,
                        /* [] */0
                      ], map);
          }
        }), /* Empty */0, List.rev(all_tickers));
  return fold((function (k, l, map) {
                var l$1 = List.sort_uniq((function (lhs, rhs) {
                        var match = lhs[/* rank */1];
                        if (typeof match === "number") {
                          throw [
                                Caml_builtin_exceptions.failure,
                                "All nodes should be ranked"
                              ];
                        }
                        var match$1 = rhs[/* rank */1];
                        if (typeof match$1 === "number") {
                          throw [
                                Caml_builtin_exceptions.failure,
                                "All nodes should be ranked"
                              ];
                        }
                        return Caml_primitive.caml_int_compare(match[0], match$1[0]);
                      }), l);
                return add(k, l$1, map);
              }), map, map);
}

function process_quote(ticker_map, new_ticker, new_value) {
  var update_sequence = find(new_ticker, ticker_map);
  return List.iter((function (ticker) {
                var match = ticker[/* type_ */3];
                if (match) {
                  var match$1 = match[0];
                  var match$2 = match$1[/* lhs */2][/* value */0];
                  var match$3 = match$1[/* rhs */1][/* value */0];
                  var value;
                  if (match$2 !== undefined && match$3 !== undefined) {
                    var y = match$3;
                    var x = match$2;
                    value = match$1[/* op */0] ? x - y : x + y;
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
            /* rank : Uninitialized */0,
            /* ticker_name */ticker_name,
            /* type_ : Binary_op */[/* record */[
                /* op */op,
                /* rhs */rhs$1,
                /* lhs */lhs$1
              ]]
          ];
  };
  var tokens = split(/* "|" */124, line);
  if (tokens) {
    switch (tokens[0]) {
      case "Q" : 
          var match = tokens[1];
          if (match) {
            var match$1 = match[1];
            if (match$1) {
              if (match$1[1]) {
                throw [
                      Caml_builtin_exceptions.failure,
                      "Invalid input line"
                    ];
              }
              var ticker_map$1 = ticker_map !== undefined ? Caml_option.valFromOption(ticker_map) : compute_update_sequences(all_tickers);
              var value = Caml_format.caml_float_of_string(match$1[0]);
              process_quote(ticker_map$1, match[0], value);
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
          var match$2 = tokens[1];
          if (match$2) {
            var match$3 = match$2[1];
            if (match$3) {
              var ticker_name = match$2[0];
              switch (match$3[0]) {
                case "+" : 
                    var match$4 = match$3[1];
                    if (match$4) {
                      var match$5 = match$4[1];
                      if (match$5) {
                        if (match$5[1]) {
                          throw [
                                Caml_builtin_exceptions.failure,
                                "Invalid input line"
                              ];
                        }
                        return /* tuple */[
                                /* :: */[
                                  make_binary_op(ticker_name, match$4[0], match$5[0], /* PLUS */0),
                                  all_tickers
                                ],
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
                    var match$6 = match$3[1];
                    if (match$6) {
                      var match$7 = match$6[1];
                      if (match$7) {
                        if (match$7[1]) {
                          throw [
                                Caml_builtin_exceptions.failure,
                                "Invalid input line"
                              ];
                        }
                        return /* tuple */[
                                /* :: */[
                                  make_binary_op(ticker_name, match$6[0], match$7[0], /* MINUS */1),
                                  all_tickers
                                ],
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
                    if (match$3[1]) {
                      throw [
                            Caml_builtin_exceptions.failure,
                            "Invalid input line"
                          ];
                    }
                    return /* tuple */[
                            /* :: */[
                              /* record */[
                                /* value */undefined,
                                /* rank : Uninitialized */0,
                                /* ticker_name */ticker_name,
                                /* type_ : Market */0
                              ],
                              all_tickers
                            ],
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
    if (lines) {
      _param = process_input_line(param[1], all_tickers, lines[0]);
      _lines = lines[1];
      continue ;
    } else {
      return print_all_composite(all_tickers);
    }
  };
}

var lines = /* :: */[
  "R|MSFT|S",
  /* :: */[
    "R|IBM|S",
    /* :: */[
      "R|FB|S",
      /* :: */[
        "R|CP1|+|MSFT|IBM",
        /* :: */[
          "R|CP2|-|FB|IBM",
          /* :: */[
            "R|CP12|+|CP1|CP2",
            /* :: */[
              "Q|MSFT|120.",
              /* :: */[
                "Q|IBM|130.",
                /* :: */[
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
