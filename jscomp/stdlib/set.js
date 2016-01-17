// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Pervasives = require("./pervasives");
var Caml_exceptions = require("../runtime/caml_exceptions");
var List = require("./list");

function Make(funarg) {
  var height = function (param) {
    return param ? param[4] : 0;
  };
  var create = function (l, v, r) {
    var hl = l ? l[4] : 0;
    var hr = r ? r[4] : 0;
    return [
            /* Node */0,
            l,
            v,
            r,
            hl >= hr ? hl + 1 : hr + 1
          ];
  };
  var bal = function (l, v, r) {
    var hl = l ? l[4] : 0;
    var hr = r ? r[4] : 0;
    if (hl > hr + 2) {
      if (l) {
        var lr = l[3];
        var lv = l[2];
        var ll = l[1];
        return height(ll) >= height(lr) ? create(ll, lv, create(lr, v, r)) : (
                  lr ? create(create(ll, lv, lr[1]), lr[2], create(lr[3], v, r)) : Pervasives.invalid_arg("Set.bal")
                );
      }
      else {
        return Pervasives.invalid_arg("Set.bal");
      }
    }
    else {
      if (hr > hl + 2) {
        if (r) {
          var rr = r[3];
          var rv = r[2];
          var rl = r[1];
          return height(rr) >= height(rl) ? create(create(l, v, rl), rv, rr) : (
                    rl ? create(create(l, v, rl[1]), rl[2], create(rl[3], rv, rr)) : Pervasives.invalid_arg("Set.bal")
                  );
        }
        else {
          return Pervasives.invalid_arg("Set.bal");
        }
      }
      else {
        return [
                /* Node */0,
                l,
                v,
                r,
                hl >= hr ? hl + 1 : hr + 1
              ];
      }
    }
  };
  var add = function (x, t) {
    if (t) {
      var r = t[3];
      var v = t[2];
      var l = t[1];
      var c = funarg[1](x, v);
      return c ? (
                c < 0 ? bal(add(x, l), v, r) : bal(l, v, add(x, r))
              ) : t;
    }
    else {
      return [
              /* Node */0,
              /* Empty */0,
              x,
              /* Empty */0,
              1
            ];
    }
  };
  var singleton = function (x) {
    return [
            /* Node */0,
            /* Empty */0,
            x,
            /* Empty */0,
            1
          ];
  };
  var add_min_element = function (v, param) {
    return param ? bal(add_min_element(v, param[1]), param[2], param[3]) : singleton(v);
  };
  var add_max_element = function (v, param) {
    return param ? bal(param[1], param[2], add_max_element(v, param[3])) : singleton(v);
  };
  var join = function (l, v, r) {
    if (l) {
      if (r) {
        var rh = r[4];
        var lh = l[4];
        return lh > rh + 2 ? bal(l[1], l[2], join(l[3], v, r)) : (
                  rh > lh + 2 ? bal(join(l, v, r[1]), r[2], r[3]) : create(l, v, r)
                );
      }
      else {
        return add_max_element(v, l);
      }
    }
    else {
      return add_min_element(v, r);
    }
  };
  var min_elt = function (_param) {
    while(/* true */1) {
      var param = _param;
      if (param) {
        var l = param[1];
        if (l) {
          _param = l;
        }
        else {
          return param[2];
        }
      }
      else {
        throw Caml_exceptions.Not_found;
      }
    };
  };
  var max_elt = function (_param) {
    while(/* true */1) {
      var param = _param;
      if (param) {
        var r = param[3];
        if (r) {
          _param = r;
        }
        else {
          return param[2];
        }
      }
      else {
        throw Caml_exceptions.Not_found;
      }
    };
  };
  var remove_min_elt = function (param) {
    if (param) {
      var l = param[1];
      return l ? bal(remove_min_elt(l), param[2], param[3]) : param[3];
    }
    else {
      return Pervasives.invalid_arg("Set.remove_min_elt");
    }
  };
  var merge = function (t1, t2) {
    return t1 ? (
              t2 ? bal(t1, min_elt(t2), remove_min_elt(t2)) : t1
            ) : t2;
  };
  var concat = function (t1, t2) {
    return t1 ? (
              t2 ? join(t1, min_elt(t2), remove_min_elt(t2)) : t1
            ) : t2;
  };
  var split = function (x, param) {
    if (param) {
      var r = param[3];
      var v = param[2];
      var l = param[1];
      var c = funarg[1](x, v);
      if (c) {
        if (c < 0) {
          var match = split(x, l);
          return [
                  /* tuple */0,
                  match[1],
                  match[2],
                  join(match[3], v, r)
                ];
        }
        else {
          var match$1 = split(x, r);
          return [
                  /* tuple */0,
                  join(l, v, match$1[1]),
                  match$1[2],
                  match$1[3]
                ];
        }
      }
      else {
        return [
                /* tuple */0,
                l,
                /* true */1,
                r
              ];
      }
    }
    else {
      return [
              /* tuple */0,
              /* Empty */0,
              /* false */0,
              /* Empty */0
            ];
    }
  };
  var empty = /* Empty */0;
  var is_empty = function (param) {
    return param ? /* false */0 : /* true */1;
  };
  var mem = function (x, param) {
    if (param) {
      var c = funarg[1](x, param[2]);
      return +(c === 0 || mem(x, c < 0 ? param[1] : param[3]));
    }
    else {
      return /* false */0;
    }
  };
  var remove = function (x, param) {
    if (param) {
      var r = param[3];
      var v = param[2];
      var l = param[1];
      var c = funarg[1](x, v);
      return c ? (
                c < 0 ? bal(remove(x, l), v, r) : bal(l, v, remove(x, r))
              ) : merge(l, r);
    }
    else {
      return /* Empty */0;
    }
  };
  var union = function (s1, s2) {
    if (s1) {
      if (s2) {
        var h2 = s2[4];
        var v2 = s2[2];
        var h1 = s1[4];
        var v1 = s1[2];
        if (h1 >= h2) {
          if (h2 === 1) {
            return add(v2, s1);
          }
          else {
            var match = split(v1, s2);
            return join(union(s1[1], match[1]), v1, union(s1[3], match[3]));
          }
        }
        else {
          if (h1 === 1) {
            return add(v1, s2);
          }
          else {
            var match$1 = split(v2, s1);
            return join(union(match$1[1], s2[1]), v2, union(match$1[3], s2[3]));
          }
        }
      }
      else {
        return s1;
      }
    }
    else {
      return s2;
    }
  };
  var inter = function (s1, s2) {
    if (s1) {
      if (s2) {
        var r1 = s1[3];
        var v1 = s1[2];
        var l1 = s1[1];
        var match = split(v1, s2);
        var l2 = match[1];
        return match[2] !== 0 ? join(inter(l1, l2), v1, inter(r1, match[3])) : concat(inter(l1, l2), inter(r1, match[3]));
      }
      else {
        return /* Empty */0;
      }
    }
    else {
      return /* Empty */0;
    }
  };
  var diff = function (s1, s2) {
    if (s1) {
      if (s2) {
        var r1 = s1[3];
        var v1 = s1[2];
        var l1 = s1[1];
        var match = split(v1, s2);
        var l2 = match[1];
        return match[2] !== 0 ? concat(diff(l1, l2), diff(r1, match[3])) : join(diff(l1, l2), v1, diff(r1, match[3]));
      }
      else {
        return s1;
      }
    }
    else {
      return /* Empty */0;
    }
  };
  var cons_enum = function (_s, _e) {
    while(/* true */1) {
      var e = _e;
      var s = _s;
      if (s) {
        _e = [
          /* More */0,
          s[2],
          s[3],
          e
        ];
        _s = s[1];
      }
      else {
        return e;
      }
    };
  };
  var compare_aux = function (_e1, _e2) {
    while(/* true */1) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1) {
        if (e2) {
          var c = funarg[1](e1[1], e2[1]);
          if (c !== 0) {
            return c;
          }
          else {
            _e2 = cons_enum(e2[2], e2[3]);
            _e1 = cons_enum(e1[2], e1[3]);
          }
        }
        else {
          return 1;
        }
      }
      else {
        return e2 ? -1 : 0;
      }
    };
  };
  var compare = function (s1, s2) {
    return compare_aux(cons_enum(s1, /* End */0), cons_enum(s2, /* End */0));
  };
  var equal = function (s1, s2) {
    return +(compare(s1, s2) === 0);
  };
  var subset = function (s1, s2) {
    if (s1) {
      if (s2) {
        var r2 = s2[3];
        var l2 = s2[1];
        var r1 = s1[3];
        var v1 = s1[2];
        var l1 = s1[1];
        var c = funarg[1](v1, s2[2]);
        return c ? (
                  c < 0 ? +(subset([
                            /* Node */0,
                            l1,
                            v1,
                            /* Empty */0,
                            0
                          ], l2) && subset(r1, s2)) : +(subset([
                            /* Node */0,
                            /* Empty */0,
                            v1,
                            r1,
                            0
                          ], r2) && subset(l1, s2))
                ) : +(subset(l1, l2) && subset(r1, r2));
      }
      else {
        return /* false */0;
      }
    }
    else {
      return /* true */1;
    }
  };
  var iter = function (f, _param) {
    while(/* true */1) {
      var param = _param;
      if (param) {
        iter(f, param[1]);
        f(param[2]);
        _param = param[3];
      }
      else {
        return /* () */0;
      }
    };
  };
  var fold = function (f, _s, _accu) {
    while(/* true */1) {
      var accu = _accu;
      var s = _s;
      if (s) {
        _accu = f(s[2], fold(f, s[1], accu));
        _s = s[3];
      }
      else {
        return accu;
      }
    };
  };
  var for_all = function (p, param) {
    return param ? +(p(param[2]) && for_all(p, param[1]) && for_all(p, param[3])) : /* true */1;
  };
  var exists = function (p, param) {
    return param ? +(p(param[2]) || exists(p, param[1]) || exists(p, param[3])) : /* false */0;
  };
  var filter = function (p, param) {
    if (param) {
      var v = param[2];
      var l$prime = filter(p, param[1]);
      var pv = p(v);
      var r$prime = filter(p, param[3]);
      return pv ? join(l$prime, v, r$prime) : concat(l$prime, r$prime);
    }
    else {
      return /* Empty */0;
    }
  };
  var partition = function (p, param) {
    if (param) {
      var v = param[2];
      var match = partition(p, param[1]);
      var lf = match[2];
      var lt = match[1];
      var pv = p(v);
      var match$1 = partition(p, param[3]);
      var rf = match$1[2];
      var rt = match$1[1];
      return pv ? [
                /* tuple */0,
                join(lt, v, rt),
                concat(lf, rf)
              ] : [
                /* tuple */0,
                concat(lt, rt),
                join(lf, v, rf)
              ];
    }
    else {
      return [
              /* tuple */0,
              /* Empty */0,
              /* Empty */0
            ];
    }
  };
  var cardinal = function (param) {
    return param ? cardinal(param[1]) + 1 + cardinal(param[3]) : 0;
  };
  var elements_aux = function (_accu, _param) {
    while(/* true */1) {
      var param = _param;
      var accu = _accu;
      if (param) {
        _param = param[1];
        _accu = [
          /* :: */0,
          param[2],
          elements_aux(accu, param[3])
        ];
      }
      else {
        return accu;
      }
    };
  };
  var elements = function (s) {
    return elements_aux(/* [] */0, s);
  };
  var find = function (x, _param) {
    while(/* true */1) {
      var param = _param;
      if (param) {
        var v = param[2];
        var c = funarg[1](x, v);
        if (c) {
          _param = c < 0 ? param[1] : param[3];
        }
        else {
          return v;
        }
      }
      else {
        throw Caml_exceptions.Not_found;
      }
    };
  };
  var of_sorted_list = function (l) {
    var sub = function (n, l) {
      var exit = 0;
      if (3 < (n >>> 0)) {
        exit = 1;
      }
      else {
        switch (n) {
          case 0 : 
              return [
                      /* tuple */0,
                      /* Empty */0,
                      l
                    ];
          case 1 : 
              if (l) {
                return [
                        /* tuple */0,
                        [
                          /* Node */0,
                          /* Empty */0,
                          l[1],
                          /* Empty */0,
                          1
                        ],
                        l[2]
                      ];
              }
              else {
                exit = 1;
              }
              break;
          case 2 : 
              if (l) {
                var match = l[2];
                if (match) {
                  return [
                          /* tuple */0,
                          [
                            /* Node */0,
                            [
                              /* Node */0,
                              /* Empty */0,
                              l[1],
                              /* Empty */0,
                              1
                            ],
                            match[1],
                            /* Empty */0,
                            2
                          ],
                          match[2]
                        ];
                }
                else {
                  exit = 1;
                }
              }
              else {
                exit = 1;
              }
              break;
          case 3 : 
              if (l) {
                var match$1 = l[2];
                if (match$1) {
                  var match$2 = match$1[2];
                  if (match$2) {
                    return [
                            /* tuple */0,
                            [
                              /* Node */0,
                              [
                                /* Node */0,
                                /* Empty */0,
                                l[1],
                                /* Empty */0,
                                1
                              ],
                              match$1[1],
                              [
                                /* Node */0,
                                /* Empty */0,
                                match$2[1],
                                /* Empty */0,
                                1
                              ],
                              2
                            ],
                            match$2[2]
                          ];
                  }
                  else {
                    exit = 1;
                  }
                }
                else {
                  exit = 1;
                }
              }
              else {
                exit = 1;
              }
              break;
          
        }
      }
      if (exit === 1) {
        var nl = n / 2 | 0;
        var match$3 = sub(nl, l);
        var l$1 = match$3[2];
        if (l$1) {
          var match$4 = sub(n - nl - 1, l$1[2]);
          return [
                  /* tuple */0,
                  create(match$3[1], l$1[1], match$4[1]),
                  match$4[2]
                ];
        }
        else {
          throw [
                0,
                Caml_exceptions.Assert_failure,
                [
                  0,
                  "set.ml",
                  372,
                  18
                ]
              ];
        }
      }
      
    };
    return sub(List.length(l), l)[1];
  };
  var of_list = function (l) {
    if (l) {
      var match = l[2];
      var x0 = l[1];
      if (match) {
        var match$1 = match[2];
        var x1 = match[1];
        if (match$1) {
          var match$2 = match$1[2];
          var x2 = match$1[1];
          if (match$2) {
            var match$3 = match$2[2];
            var x3 = match$2[1];
            return match$3 ? (
                      match$3[2] ? of_sorted_list(List.sort_uniq(funarg[1], l)) : add(match$3[1], add(x3, add(x2, add(x1, singleton(x0)))))
                    ) : add(x3, add(x2, add(x1, singleton(x0))));
          }
          else {
            return add(x2, add(x1, singleton(x0)));
          }
        }
        else {
          return add(x1, singleton(x0));
        }
      }
      else {
        return singleton(x0);
      }
    }
    else {
      return empty;
    }
  };
  return [
          0,
          empty,
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
}

exports.Make = Make;
/* No side effect */
