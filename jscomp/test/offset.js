'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var $$String = require("../../lib/js/string.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function height(param) {
  if (param) {
    return param[3];
  } else {
    return 0;
  }
}

function create(l, v, r) {
  var hl = l ? l[3] : 0;
  var hr = r ? r[3] : 0;
  return /* Node */[
          l,
          v,
          r,
          hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function bal(l, v, r) {
  var hl = l ? l[3] : 0;
  var hr = r ? r[3] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[2];
      var lv = l[1];
      var ll = l[0];
      if (height(ll) >= height(lr)) {
        return create(ll, lv, create(lr, v, r));
      } else if (lr) {
        return create(create(ll, lv, lr[0]), lr[1], create(lr[2], v, r));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Set.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Set.bal"
          ];
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r) {
      var rr = r[2];
      var rv = r[1];
      var rl = r[0];
      if (height(rr) >= height(rl)) {
        return create(create(l, v, rl), rv, rr);
      } else if (rl) {
        return create(create(l, v, rl[0]), rl[1], create(rl[2], rv, rr));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Set.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Set.bal"
          ];
    }
  } else {
    return /* Node */[
            l,
            v,
            r,
            hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function add(x, t) {
  if (t) {
    var r = t[2];
    var v = t[1];
    var l = t[0];
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return t;
    } else if (c < 0) {
      return bal(add(x, l), v, r);
    } else {
      return bal(l, v, add(x, r));
    }
  } else {
    return /* Node */[
            /* Empty */0,
            x,
            /* Empty */0,
            1
          ];
  }
}

function singleton(x) {
  return /* Node */[
          /* Empty */0,
          x,
          /* Empty */0,
          1
        ];
}

function add_min_element(v, param) {
  if (param) {
    return bal(add_min_element(v, param[0]), param[1], param[2]);
  } else {
    return singleton(v);
  }
}

function add_max_element(v, param) {
  if (param) {
    return bal(param[0], param[1], add_max_element(v, param[2]));
  } else {
    return singleton(v);
  }
}

function join(l, v, r) {
  if (l) {
    if (r) {
      var rh = r[3];
      var lh = l[3];
      if (lh > (rh + 2 | 0)) {
        return bal(l[0], l[1], join(l[2], v, r));
      } else if (rh > (lh + 2 | 0)) {
        return bal(join(l, v, r[0]), r[1], r[2]);
      } else {
        return create(l, v, r);
      }
    } else {
      return add_max_element(v, l);
    }
  } else {
    return add_min_element(v, r);
  }
}

function min_elt(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param[0];
      if (l) {
        _param = l;
        continue ;
      } else {
        return param[1];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function max_elt(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var r = param[2];
      if (r) {
        _param = r;
        continue ;
      } else {
        return param[1];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function remove_min_elt(param) {
  if (param) {
    var l = param[0];
    if (l) {
      return bal(remove_min_elt(l), param[1], param[2]);
    } else {
      return param[2];
    }
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Set.remove_min_elt"
        ];
  }
}

function concat(t1, t2) {
  if (t1) {
    if (t2) {
      return join(t1, min_elt(t2), remove_min_elt(t2));
    } else {
      return t1;
    }
  } else {
    return t2;
  }
}

function split(x, param) {
  if (param) {
    var r = param[2];
    var v = param[1];
    var l = param[0];
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return /* tuple */[
              l,
              true,
              r
            ];
    } else if (c < 0) {
      var match = split(x, l);
      return /* tuple */[
              match[0],
              match[1],
              join(match[2], v, r)
            ];
    } else {
      var match$1 = split(x, r);
      return /* tuple */[
              join(l, v, match$1[0]),
              match$1[1],
              match$1[2]
            ];
    }
  } else {
    return /* tuple */[
            /* Empty */0,
            false,
            /* Empty */0
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

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_primitive.caml_string_compare(x, param[1]);
      if (c === 0) {
        return true;
      } else {
        _param = c < 0 ? param[0] : param[2];
        continue ;
      }
    } else {
      return false;
    }
  };
}

function remove(x, param) {
  if (param) {
    var r = param[2];
    var v = param[1];
    var l = param[0];
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      var t1 = l;
      var t2 = r;
      if (t1) {
        if (t2) {
          return bal(t1, min_elt(t2), remove_min_elt(t2));
        } else {
          return t1;
        }
      } else {
        return t2;
      }
    } else if (c < 0) {
      return bal(remove(x, l), v, r);
    } else {
      return bal(l, v, remove(x, r));
    }
  } else {
    return /* Empty */0;
  }
}

function union(s1, s2) {
  if (s1) {
    if (s2) {
      var h2 = s2[3];
      var v2 = s2[1];
      var h1 = s1[3];
      var v1 = s1[1];
      if (h1 >= h2) {
        if (h2 === 1) {
          return add(v2, s1);
        } else {
          var match = split(v1, s2);
          return join(union(s1[0], match[0]), v1, union(s1[2], match[2]));
        }
      } else if (h1 === 1) {
        return add(v1, s2);
      } else {
        var match$1 = split(v2, s1);
        return join(union(match$1[0], s2[0]), v2, union(match$1[2], s2[2]));
      }
    } else {
      return s1;
    }
  } else {
    return s2;
  }
}

function inter(s1, s2) {
  if (s1 && s2) {
    var r1 = s1[2];
    var v1 = s1[1];
    var l1 = s1[0];
    var match = split(v1, s2);
    var l2 = match[0];
    if (match[1]) {
      return join(inter(l1, l2), v1, inter(r1, match[2]));
    } else {
      return concat(inter(l1, l2), inter(r1, match[2]));
    }
  } else {
    return /* Empty */0;
  }
}

function diff(s1, s2) {
  if (s1) {
    if (s2) {
      var r1 = s1[2];
      var v1 = s1[1];
      var l1 = s1[0];
      var match = split(v1, s2);
      var l2 = match[0];
      if (match[1]) {
        return concat(diff(l1, l2), diff(r1, match[2]));
      } else {
        return join(diff(l1, l2), v1, diff(r1, match[2]));
      }
    } else {
      return s1;
    }
  } else {
    return /* Empty */0;
  }
}

function cons_enum(_s, _e) {
  while(true) {
    var e = _e;
    var s = _s;
    if (s) {
      _e = /* More */[
        s[1],
        s[2],
        e
      ];
      _s = s[0];
      continue ;
    } else {
      return e;
    }
  };
}

function compare(s1, s2) {
  var _e1 = cons_enum(s1, /* End */0);
  var _e2 = cons_enum(s2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (e1) {
      if (e2) {
        var c = Caml_primitive.caml_string_compare(e1[0], e2[0]);
        if (c !== 0) {
          return c;
        } else {
          _e2 = cons_enum(e2[1], e2[2]);
          _e1 = cons_enum(e1[1], e1[2]);
          continue ;
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

function equal(s1, s2) {
  return compare(s1, s2) === 0;
}

function subset(_s1, _s2) {
  while(true) {
    var s2 = _s2;
    var s1 = _s1;
    if (s1) {
      if (s2) {
        var r2 = s2[2];
        var l2 = s2[0];
        var r1 = s1[2];
        var v1 = s1[1];
        var l1 = s1[0];
        var c = Caml_primitive.caml_string_compare(v1, s2[1]);
        if (c === 0) {
          if (subset(l1, l2)) {
            _s2 = r2;
            _s1 = r1;
            continue ;
          } else {
            return false;
          }
        } else if (c < 0) {
          if (subset(/* Node */[
                  l1,
                  v1,
                  /* Empty */0,
                  0
                ], l2)) {
            _s1 = r1;
            continue ;
          } else {
            return false;
          }
        } else if (subset(/* Node */[
                /* Empty */0,
                v1,
                r1,
                0
              ], r2)) {
          _s1 = l1;
          continue ;
        } else {
          return false;
        }
      } else {
        return false;
      }
    } else {
      return true;
    }
  };
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      iter(f, param[0]);
      Curry._1(f, param[1]);
      _param = param[2];
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function fold(f, _s, _accu) {
  while(true) {
    var accu = _accu;
    var s = _s;
    if (s) {
      _accu = Curry._2(f, s[1], fold(f, s[0], accu));
      _s = s[2];
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
      if (Curry._1(p, param[1]) && for_all(p, param[0])) {
        _param = param[2];
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
      if (Curry._1(p, param[1]) || exists(p, param[0])) {
        return true;
      } else {
        _param = param[2];
        continue ;
      }
    } else {
      return false;
    }
  };
}

function filter(p, param) {
  if (param) {
    var v = param[1];
    var l$prime = filter(p, param[0]);
    var pv = Curry._1(p, v);
    var r$prime = filter(p, param[2]);
    if (pv) {
      return join(l$prime, v, r$prime);
    } else {
      return concat(l$prime, r$prime);
    }
  } else {
    return /* Empty */0;
  }
}

function partition(p, param) {
  if (param) {
    var v = param[1];
    var match = partition(p, param[0]);
    var lf = match[1];
    var lt = match[0];
    var pv = Curry._1(p, v);
    var match$1 = partition(p, param[2]);
    var rf = match$1[1];
    var rt = match$1[0];
    if (pv) {
      return /* tuple */[
              join(lt, v, rt),
              concat(lf, rf)
            ];
    } else {
      return /* tuple */[
              concat(lt, rt),
              join(lf, v, rf)
            ];
    }
  } else {
    return /* tuple */[
            /* Empty */0,
            /* Empty */0
          ];
  }
}

function cardinal(param) {
  if (param) {
    return (cardinal(param[0]) + 1 | 0) + cardinal(param[2]) | 0;
  } else {
    return 0;
  }
}

function elements_aux(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[0];
      _accu = /* :: */[
        param[1],
        elements_aux(accu, param[2])
      ];
      continue ;
    } else {
      return accu;
    }
  };
}

function elements(s) {
  return elements_aux(/* [] */0, s);
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[1];
      var c = Caml_primitive.caml_string_compare(x, v);
      if (c === 0) {
        return v;
      } else {
        _param = c < 0 ? param[0] : param[2];
        continue ;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function of_list(l) {
  if (l) {
    var match = l[1];
    var x0 = l[0];
    if (match) {
      var match$1 = match[1];
      var x1 = match[0];
      if (match$1) {
        var match$2 = match$1[1];
        var x2 = match$1[0];
        if (match$2) {
          var match$3 = match$2[1];
          var x3 = match$2[0];
          if (match$3) {
            if (match$3[1]) {
              var l$1 = List.sort_uniq($$String.compare, l);
              var sub = function (n, l) {
                var exit = 0;
                if (n > 3 || n < 0) {
                  exit = 1;
                } else {
                  switch (n) {
                    case 0 : 
                        return /* tuple */[
                                /* Empty */0,
                                l
                              ];
                    case 1 : 
                        if (l) {
                          return /* tuple */[
                                  /* Node */[
                                    /* Empty */0,
                                    l[0],
                                    /* Empty */0,
                                    1
                                  ],
                                  l[1]
                                ];
                        } else {
                          exit = 1;
                        }
                        break;
                    case 2 : 
                        if (l) {
                          var match = l[1];
                          if (match) {
                            return /* tuple */[
                                    /* Node */[
                                      /* Node */[
                                        /* Empty */0,
                                        l[0],
                                        /* Empty */0,
                                        1
                                      ],
                                      match[0],
                                      /* Empty */0,
                                      2
                                    ],
                                    match[1]
                                  ];
                          } else {
                            exit = 1;
                          }
                        } else {
                          exit = 1;
                        }
                        break;
                    case 3 : 
                        if (l) {
                          var match$1 = l[1];
                          if (match$1) {
                            var match$2 = match$1[1];
                            if (match$2) {
                              return /* tuple */[
                                      /* Node */[
                                        /* Node */[
                                          /* Empty */0,
                                          l[0],
                                          /* Empty */0,
                                          1
                                        ],
                                        match$1[0],
                                        /* Node */[
                                          /* Empty */0,
                                          match$2[0],
                                          /* Empty */0,
                                          1
                                        ],
                                        2
                                      ],
                                      match$2[1]
                                    ];
                            } else {
                              exit = 1;
                            }
                          } else {
                            exit = 1;
                          }
                        } else {
                          exit = 1;
                        }
                        break;
                    
                  }
                }
                if (exit === 1) {
                  var nl = n / 2 | 0;
                  var match$3 = sub(nl, l);
                  var l$1 = match$3[1];
                  if (l$1) {
                    var match$4 = sub((n - nl | 0) - 1 | 0, l$1[1]);
                    return /* tuple */[
                            create(match$3[0], l$1[0], match$4[0]),
                            match$4[1]
                          ];
                  } else {
                    throw [
                          Caml_builtin_exceptions.assert_failure,
                          [
                            "set.ml",
                            372,
                            18
                          ]
                        ];
                  }
                }
                
              };
              return sub(List.length(l$1), l$1)[0];
            } else {
              return add(match$3[0], add(x3, add(x2, add(x1, singleton(x0)))));
            }
          } else {
            return add(x3, add(x2, add(x1, singleton(x0))));
          }
        } else {
          return add(x2, add(x1, singleton(x0)));
        }
      } else {
        return add(x1, singleton(x0));
      }
    } else {
      return singleton(x0);
    }
  } else {
    return /* Empty */0;
  }
}

var $$Set = [
  /* Empty */0,
  is_empty,
  mem,
  add,
  singleton,
  remove,
  union,
  inter,
  diff,
  compare,
  equal,
  subset,
  iter,
  fold,
  for_all,
  exists,
  filter,
  partition,
  cardinal,
  elements,
  min_elt,
  max_elt,
  min_elt,
  split,
  find,
  of_list
];

var M = /* module */[
  /* x */1,
  /* Set */$$Set
];

var x = 1;

exports.M = M;
exports.x = x;
exports.$$Set = $$Set;
/* No side effect */
