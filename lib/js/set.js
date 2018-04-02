'use strict';

var List = require("./list.js");
var Curry = require("./curry.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function Make(funarg) {
  var height = function (param) {
    if (param) {
      return param[3];
    } else {
      return 0;
    }
  };
  var create = function (l, v, r) {
    var hl = l ? l[3] : 0;
    var hr = r ? r[3] : 0;
    return /* Node */[
            l,
            v,
            r,
            hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  };
  var bal = function (l, v, r) {
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
  };
  var add = function (x, t) {
    if (t) {
      var r = t[2];
      var v = t[1];
      var l = t[0];
      var c = Curry._2(funarg[/* compare */0], x, v);
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
  };
  var singleton = function (x) {
    return /* Node */[
            /* Empty */0,
            x,
            /* Empty */0,
            1
          ];
  };
  var add_min_element = function (v, param) {
    if (param) {
      return bal(add_min_element(v, param[0]), param[1], param[2]);
    } else {
      return singleton(v);
    }
  };
  var add_max_element = function (v, param) {
    if (param) {
      return bal(param[0], param[1], add_max_element(v, param[2]));
    } else {
      return singleton(v);
    }
  };
  var join = function (l, v, r) {
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
  };
  var min_elt = function (_param) {
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
  };
  var max_elt = function (_param) {
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
  };
  var remove_min_elt = function (param) {
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
  };
  var concat = function (t1, t2) {
    if (t1) {
      if (t2) {
        return join(t1, min_elt(t2), remove_min_elt(t2));
      } else {
        return t1;
      }
    } else {
      return t2;
    }
  };
  var split = function (x, param) {
    if (param) {
      var r = param[2];
      var v = param[1];
      var l = param[0];
      var c = Curry._2(funarg[/* compare */0], x, v);
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
  };
  var is_empty = function (param) {
    if (param) {
      return false;
    } else {
      return true;
    }
  };
  var mem = function (x, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var c = Curry._2(funarg[/* compare */0], x, param[1]);
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
  };
  var remove = function (x, param) {
    if (param) {
      var r = param[2];
      var v = param[1];
      var l = param[0];
      var c = Curry._2(funarg[/* compare */0], x, v);
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
  };
  var union = function (s1, s2) {
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
  };
  var inter = function (s1, s2) {
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
  };
  var diff = function (s1, s2) {
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
  };
  var cons_enum = function (_s, _e) {
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
  };
  var compare = function (s1, s2) {
    var _e1 = cons_enum(s1, /* End */0);
    var _e2 = cons_enum(s2, /* End */0);
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1) {
        if (e2) {
          var c = Curry._2(funarg[/* compare */0], e1[0], e2[0]);
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
  };
  var equal = function (s1, s2) {
    return compare(s1, s2) === 0;
  };
  var subset = function (_s1, _s2) {
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
          var c = Curry._2(funarg[/* compare */0], v1, s2[1]);
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
  };
  var iter = function (f, _param) {
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
  };
  var fold = function (f, _s, _accu) {
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
  };
  var for_all = function (p, _param) {
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
  };
  var exists = function (p, _param) {
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
  };
  var filter = function (p, param) {
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
  };
  var partition = function (p, param) {
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
  };
  var cardinal = function (param) {
    if (param) {
      return (cardinal(param[0]) + 1 | 0) + cardinal(param[2]) | 0;
    } else {
      return 0;
    }
  };
  var elements_aux = function (_accu, _param) {
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
  };
  var elements = function (s) {
    return elements_aux(/* [] */0, s);
  };
  var find = function (x, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var v = param[1];
        var c = Curry._2(funarg[/* compare */0], x, v);
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
  };
  var of_list = function (l) {
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
                var l$1 = List.sort_uniq(funarg[/* compare */0], l);
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
  };
  return [
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
}

exports.Make = Make;
/* No side effect */
