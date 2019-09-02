'use strict';

var List = require("./list.js");
var Curry = require("./curry.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function Make(funarg) {
  var height = function (param) {
    if (param !== "Empty") {
      return param.Arg3;
    } else {
      return 0;
    }
  };
  var create = function (l, v, r) {
    var hl = l !== "Empty" ? l.Arg3 : 0;
    var hr = r !== "Empty" ? r.Arg3 : 0;
    return /* constructor */{
            tag: "Node",
            Arg0: l,
            Arg1: v,
            Arg2: r,
            Arg3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  };
  var bal = function (l, v, r) {
    var hl = l !== "Empty" ? l.Arg3 : 0;
    var hr = r !== "Empty" ? r.Arg3 : 0;
    if (hl > (hr + 2 | 0)) {
      if (l !== "Empty") {
        var lr = l.Arg2;
        var lv = l.Arg1;
        var ll = l.Arg0;
        if (height(ll) >= height(lr)) {
          return create(ll, lv, create(lr, v, r));
        } else if (lr !== "Empty") {
          return create(create(ll, lv, lr.Arg0), lr.Arg1, create(lr.Arg2, v, r));
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
      if (r !== "Empty") {
        var rr = r.Arg2;
        var rv = r.Arg1;
        var rl = r.Arg0;
        if (height(rr) >= height(rl)) {
          return create(create(l, v, rl), rv, rr);
        } else if (rl !== "Empty") {
          return create(create(l, v, rl.Arg0), rl.Arg1, create(rl.Arg2, rv, rr));
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
      return /* constructor */{
              tag: "Node",
              Arg0: l,
              Arg1: v,
              Arg2: r,
              Arg3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
            };
    }
  };
  var add = function (x, t) {
    if (t !== "Empty") {
      var r = t.Arg2;
      var v = t.Arg1;
      var l = t.Arg0;
      var c = Curry._2(funarg.compare, x, v);
      if (c === 0) {
        return t;
      } else if (c < 0) {
        return bal(add(x, l), v, r);
      } else {
        return bal(l, v, add(x, r));
      }
    } else {
      return /* constructor */{
              tag: "Node",
              Arg0: "Empty",
              Arg1: x,
              Arg2: "Empty",
              Arg3: 1
            };
    }
  };
  var singleton = function (x) {
    return /* constructor */{
            tag: "Node",
            Arg0: "Empty",
            Arg1: x,
            Arg2: "Empty",
            Arg3: 1
          };
  };
  var add_min_element = function (v, param) {
    if (param !== "Empty") {
      return bal(add_min_element(v, param.Arg0), param.Arg1, param.Arg2);
    } else {
      return singleton(v);
    }
  };
  var add_max_element = function (v, param) {
    if (param !== "Empty") {
      return bal(param.Arg0, param.Arg1, add_max_element(v, param.Arg2));
    } else {
      return singleton(v);
    }
  };
  var join = function (l, v, r) {
    if (l !== "Empty") {
      if (r !== "Empty") {
        var rh = r.Arg3;
        var lh = l.Arg3;
        if (lh > (rh + 2 | 0)) {
          return bal(l.Arg0, l.Arg1, join(l.Arg2, v, r));
        } else if (rh > (lh + 2 | 0)) {
          return bal(join(l, v, r.Arg0), r.Arg1, r.Arg2);
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
      if (param !== "Empty") {
        var l = param.Arg0;
        if (l !== "Empty") {
          _param = l;
          continue ;
        } else {
          return param.Arg1;
        }
      } else {
        throw Caml_builtin_exceptions.not_found;
      }
    };
  };
  var max_elt = function (_param) {
    while(true) {
      var param = _param;
      if (param !== "Empty") {
        var r = param.Arg2;
        if (r !== "Empty") {
          _param = r;
          continue ;
        } else {
          return param.Arg1;
        }
      } else {
        throw Caml_builtin_exceptions.not_found;
      }
    };
  };
  var remove_min_elt = function (param) {
    if (param !== "Empty") {
      var l = param.Arg0;
      if (l !== "Empty") {
        return bal(remove_min_elt(l), param.Arg1, param.Arg2);
      } else {
        return param.Arg2;
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Set.remove_min_elt"
          ];
    }
  };
  var concat = function (t1, t2) {
    if (t1 !== "Empty") {
      if (t2 !== "Empty") {
        return join(t1, min_elt(t2), remove_min_elt(t2));
      } else {
        return t1;
      }
    } else {
      return t2;
    }
  };
  var split = function (x, param) {
    if (param !== "Empty") {
      var r = param.Arg2;
      var v = param.Arg1;
      var l = param.Arg0;
      var c = Curry._2(funarg.compare, x, v);
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
              "Empty",
              false,
              "Empty"
            ];
    }
  };
  var is_empty = function (param) {
    return param === "Empty";
  };
  var mem = function (x, _param) {
    while(true) {
      var param = _param;
      if (param !== "Empty") {
        var c = Curry._2(funarg.compare, x, param.Arg1);
        if (c === 0) {
          return true;
        } else {
          _param = c < 0 ? param.Arg0 : param.Arg2;
          continue ;
        }
      } else {
        return false;
      }
    };
  };
  var remove = function (x, param) {
    if (param !== "Empty") {
      var r = param.Arg2;
      var v = param.Arg1;
      var l = param.Arg0;
      var c = Curry._2(funarg.compare, x, v);
      if (c === 0) {
        var t1 = l;
        var t2 = r;
        if (t1 !== "Empty") {
          if (t2 !== "Empty") {
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
      return "Empty";
    }
  };
  var union = function (s1, s2) {
    if (s1 !== "Empty") {
      if (s2 !== "Empty") {
        var h2 = s2.Arg3;
        var v2 = s2.Arg1;
        var h1 = s1.Arg3;
        var v1 = s1.Arg1;
        if (h1 >= h2) {
          if (h2 === 1) {
            return add(v2, s1);
          } else {
            var match = split(v1, s2);
            return join(union(s1.Arg0, match[0]), v1, union(s1.Arg2, match[2]));
          }
        } else if (h1 === 1) {
          return add(v1, s2);
        } else {
          var match$1 = split(v2, s1);
          return join(union(match$1[0], s2.Arg0), v2, union(match$1[2], s2.Arg2));
        }
      } else {
        return s1;
      }
    } else {
      return s2;
    }
  };
  var inter = function (s1, s2) {
    if (s1 !== "Empty" && s2 !== "Empty") {
      var r1 = s1.Arg2;
      var v1 = s1.Arg1;
      var l1 = s1.Arg0;
      var match = split(v1, s2);
      var l2 = match[0];
      if (match[1]) {
        return join(inter(l1, l2), v1, inter(r1, match[2]));
      } else {
        return concat(inter(l1, l2), inter(r1, match[2]));
      }
    } else {
      return "Empty";
    }
  };
  var diff = function (s1, s2) {
    if (s1 !== "Empty") {
      if (s2 !== "Empty") {
        var r1 = s1.Arg2;
        var v1 = s1.Arg1;
        var l1 = s1.Arg0;
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
      return "Empty";
    }
  };
  var cons_enum = function (_s, _e) {
    while(true) {
      var e = _e;
      var s = _s;
      if (s !== "Empty") {
        _e = /* constructor */{
          tag: "More",
          Arg0: s.Arg1,
          Arg1: s.Arg2,
          Arg2: e
        };
        _s = s.Arg0;
        continue ;
      } else {
        return e;
      }
    };
  };
  var compare = function (s1, s2) {
    var _e1 = cons_enum(s1, "End");
    var _e2 = cons_enum(s2, "End");
    while(true) {
      var e2 = _e2;
      var e1 = _e1;
      if (e1 !== "End") {
        if (e2 !== "End") {
          var c = Curry._2(funarg.compare, e1.Arg0, e2.Arg0);
          if (c !== 0) {
            return c;
          } else {
            _e2 = cons_enum(e2.Arg1, e2.Arg2);
            _e1 = cons_enum(e1.Arg1, e1.Arg2);
            continue ;
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
  };
  var equal = function (s1, s2) {
    return compare(s1, s2) === 0;
  };
  var subset = function (_s1, _s2) {
    while(true) {
      var s2 = _s2;
      var s1 = _s1;
      if (s1 !== "Empty") {
        if (s2 !== "Empty") {
          var r2 = s2.Arg2;
          var l2 = s2.Arg0;
          var r1 = s1.Arg2;
          var v1 = s1.Arg1;
          var l1 = s1.Arg0;
          var c = Curry._2(funarg.compare, v1, s2.Arg1);
          if (c === 0) {
            if (subset(l1, l2)) {
              _s2 = r2;
              _s1 = r1;
              continue ;
            } else {
              return false;
            }
          } else if (c < 0) {
            if (subset(/* constructor */{
                    tag: "Node",
                    Arg0: l1,
                    Arg1: v1,
                    Arg2: "Empty",
                    Arg3: 0
                  }, l2)) {
              _s1 = r1;
              continue ;
            } else {
              return false;
            }
          } else if (subset(/* constructor */{
                  tag: "Node",
                  Arg0: "Empty",
                  Arg1: v1,
                  Arg2: r1,
                  Arg3: 0
                }, r2)) {
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
      if (param !== "Empty") {
        iter(f, param.Arg0);
        Curry._1(f, param.Arg1);
        _param = param.Arg2;
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
      if (s !== "Empty") {
        _accu = Curry._2(f, s.Arg1, fold(f, s.Arg0, accu));
        _s = s.Arg2;
        continue ;
      } else {
        return accu;
      }
    };
  };
  var for_all = function (p, _param) {
    while(true) {
      var param = _param;
      if (param !== "Empty") {
        if (Curry._1(p, param.Arg1) && for_all(p, param.Arg0)) {
          _param = param.Arg2;
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
      if (param !== "Empty") {
        if (Curry._1(p, param.Arg1) || exists(p, param.Arg0)) {
          return true;
        } else {
          _param = param.Arg2;
          continue ;
        }
      } else {
        return false;
      }
    };
  };
  var filter = function (p, param) {
    if (param !== "Empty") {
      var v = param.Arg1;
      var l$prime = filter(p, param.Arg0);
      var pv = Curry._1(p, v);
      var r$prime = filter(p, param.Arg2);
      if (pv) {
        return join(l$prime, v, r$prime);
      } else {
        return concat(l$prime, r$prime);
      }
    } else {
      return "Empty";
    }
  };
  var partition = function (p, param) {
    if (param !== "Empty") {
      var v = param.Arg1;
      var match = partition(p, param.Arg0);
      var lf = match[1];
      var lt = match[0];
      var pv = Curry._1(p, v);
      var match$1 = partition(p, param.Arg2);
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
              "Empty",
              "Empty"
            ];
    }
  };
  var cardinal = function (param) {
    if (param !== "Empty") {
      return (cardinal(param.Arg0) + 1 | 0) + cardinal(param.Arg2) | 0;
    } else {
      return 0;
    }
  };
  var elements_aux = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (param !== "Empty") {
        _param = param.Arg0;
        _accu = /* constructor */{
          tag: "::",
          Arg0: param.Arg1,
          Arg1: elements_aux(accu, param.Arg2)
        };
        continue ;
      } else {
        return accu;
      }
    };
  };
  var elements = function (s) {
    return elements_aux("[]", s);
  };
  var find = function (x, _param) {
    while(true) {
      var param = _param;
      if (param !== "Empty") {
        var v = param.Arg1;
        var c = Curry._2(funarg.compare, x, v);
        if (c === 0) {
          return v;
        } else {
          _param = c < 0 ? param.Arg0 : param.Arg2;
          continue ;
        }
      } else {
        throw Caml_builtin_exceptions.not_found;
      }
    };
  };
  var of_list = function (l) {
    if (l !== "[]") {
      var match = l.Arg1;
      var x0 = l.Arg0;
      if (match !== "[]") {
        var match$1 = match.Arg1;
        var x1 = match.Arg0;
        if (match$1 !== "[]") {
          var match$2 = match$1.Arg1;
          var x2 = match$1.Arg0;
          if (match$2 !== "[]") {
            var match$3 = match$2.Arg1;
            var x3 = match$2.Arg0;
            if (match$3 !== "[]") {
              if (match$3.Arg1 !== "[]") {
                var l$1 = List.sort_uniq(funarg.compare, l);
                var sub = function (n, l) {
                  switch (n) {
                    case 0 :
                        return /* tuple */[
                                "Empty",
                                l
                              ];
                    case 1 :
                        if (l !== "[]") {
                          return /* tuple */[
                                  /* constructor */{
                                    tag: "Node",
                                    Arg0: "Empty",
                                    Arg1: l.Arg0,
                                    Arg2: "Empty",
                                    Arg3: 1
                                  },
                                  l.Arg1
                                ];
                        }
                        break;
                    case 2 :
                        if (l !== "[]") {
                          var match = l.Arg1;
                          if (match !== "[]") {
                            return /* tuple */[
                                    /* constructor */{
                                      tag: "Node",
                                      Arg0: /* constructor */{
                                        tag: "Node",
                                        Arg0: "Empty",
                                        Arg1: l.Arg0,
                                        Arg2: "Empty",
                                        Arg3: 1
                                      },
                                      Arg1: match.Arg0,
                                      Arg2: "Empty",
                                      Arg3: 2
                                    },
                                    match.Arg1
                                  ];
                          }
                          
                        }
                        break;
                    case 3 :
                        if (l !== "[]") {
                          var match$1 = l.Arg1;
                          if (match$1 !== "[]") {
                            var match$2 = match$1.Arg1;
                            if (match$2 !== "[]") {
                              return /* tuple */[
                                      /* constructor */{
                                        tag: "Node",
                                        Arg0: /* constructor */{
                                          tag: "Node",
                                          Arg0: "Empty",
                                          Arg1: l.Arg0,
                                          Arg2: "Empty",
                                          Arg3: 1
                                        },
                                        Arg1: match$1.Arg0,
                                        Arg2: /* constructor */{
                                          tag: "Node",
                                          Arg0: "Empty",
                                          Arg1: match$2.Arg0,
                                          Arg2: "Empty",
                                          Arg3: 1
                                        },
                                        Arg3: 2
                                      },
                                      match$2.Arg1
                                    ];
                            }
                            
                          }
                          
                        }
                        break;
                    default:
                      
                  }
                  var nl = n / 2 | 0;
                  var match$3 = sub(nl, l);
                  var l$1 = match$3[1];
                  if (l$1 !== "[]") {
                    var match$4 = sub((n - nl | 0) - 1 | 0, l$1.Arg1);
                    return /* tuple */[
                            create(match$3[0], l$1.Arg0, match$4[0]),
                            match$4[1]
                          ];
                  } else {
                    throw [
                          Caml_builtin_exceptions.assert_failure,
                          /* tuple */[
                            "set.ml",
                            372,
                            18
                          ]
                        ];
                  }
                };
                return sub(List.length(l$1), l$1)[0];
              } else {
                return add(match$3.Arg0, add(x3, add(x2, add(x1, singleton(x0)))));
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
      return "Empty";
    }
  };
  return {
          empty: "Empty",
          is_empty: is_empty,
          mem: mem,
          add: add,
          singleton: singleton,
          remove: remove,
          union: union,
          inter: inter,
          diff: diff,
          compare: compare,
          equal: equal,
          subset: subset,
          iter: iter,
          fold: fold,
          for_all: for_all,
          exists: exists,
          filter: filter,
          partition: partition,
          cardinal: cardinal,
          elements: elements,
          min_elt: min_elt,
          max_elt: max_elt,
          choose: min_elt,
          split: split,
          find: find,
          of_list: of_list
        };
}

exports.Make = Make;
/* No side effect */
