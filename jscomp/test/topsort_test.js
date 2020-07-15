'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var $$String = require("../../lib/js/string.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var graph = {
  hd: [
    "a",
    "b"
  ],
  tl: {
    hd: [
      "a",
      "c"
    ],
    tl: {
      hd: [
        "a",
        "d"
      ],
      tl: {
        hd: [
          "b",
          "e"
        ],
        tl: {
          hd: [
            "c",
            "f"
          ],
          tl: {
            hd: [
              "d",
              "e"
            ],
            tl: {
              hd: [
                "e",
                "f"
              ],
              tl: {
                hd: [
                  "e",
                  "g"
                ],
                tl: /* [] */0
              }
            }
          }
        }
      }
    }
  }
};

function nexts(x, g) {
  return List.fold_left((function (acc, param) {
                if (param[0] === x) {
                  return {
                          hd: param[1],
                          tl: acc
                        };
                } else {
                  return acc;
                }
              }), /* [] */0, g);
}

function dfs1(_nodes, graph, _visited) {
  while(true) {
    var visited = _visited;
    var nodes = _nodes;
    if (!nodes) {
      return List.rev(visited);
    }
    var xs = nodes.tl;
    var x = nodes.hd;
    if (List.mem(x, visited)) {
      _nodes = xs;
      continue ;
    }
    console.log(x);
    _visited = {
      hd: x,
      tl: visited
    };
    _nodes = Pervasives.$at(nexts(x, graph), xs);
    continue ;
  };
}

if (!Caml_obj.caml_equal(dfs1({
            hd: "a",
            tl: /* [] */0
          }, graph, /* [] */0), {
        hd: "a",
        tl: {
          hd: "d",
          tl: {
            hd: "e",
            tl: {
              hd: "g",
              tl: {
                hd: "f",
                tl: {
                  hd: "c",
                  tl: {
                    hd: "b",
                    tl: /* [] */0
                  }
                }
              }
            }
          }
        }
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "topsort_test.ml",
          29,
          2
        ],
        Error: new Error()
      };
}

Pervasives.print_newline(undefined);

if (!Caml_obj.caml_equal(dfs1({
            hd: "b",
            tl: /* [] */0
          }, {
            hd: [
              "f",
              "d"
            ],
            tl: graph
          }, /* [] */0), {
        hd: "b",
        tl: {
          hd: "e",
          tl: {
            hd: "g",
            tl: {
              hd: "f",
              tl: {
                hd: "d",
                tl: /* [] */0
              }
            }
          }
        }
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "topsort_test.ml",
          32,
          2
        ],
        Error: new Error()
      };
}

function dfs2(nodes, graph, visited) {
  var aux = function (_nodes, graph, _visited) {
    while(true) {
      var visited = _visited;
      var nodes = _nodes;
      if (!nodes) {
        return visited;
      }
      var xs = nodes.tl;
      var x = nodes.hd;
      if (List.mem(x, visited)) {
        _nodes = xs;
        continue ;
      }
      _visited = aux(nexts(x, graph), graph, {
            hd: x,
            tl: visited
          });
      _nodes = xs;
      continue ;
    };
  };
  return List.rev(aux(nodes, graph, visited));
}

if (!Caml_obj.caml_equal(dfs2({
            hd: "a",
            tl: /* [] */0
          }, graph, /* [] */0), {
        hd: "a",
        tl: {
          hd: "d",
          tl: {
            hd: "e",
            tl: {
              hd: "g",
              tl: {
                hd: "f",
                tl: {
                  hd: "c",
                  tl: {
                    hd: "b",
                    tl: /* [] */0
                  }
                }
              }
            }
          }
        }
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "topsort_test.ml",
          47,
          2
        ],
        Error: new Error()
      };
}

if (!Caml_obj.caml_equal(dfs2({
            hd: "b",
            tl: /* [] */0
          }, {
            hd: [
              "f",
              "d"
            ],
            tl: graph
          }, /* [] */0), {
        hd: "b",
        tl: {
          hd: "e",
          tl: {
            hd: "g",
            tl: {
              hd: "f",
              tl: {
                hd: "d",
                tl: /* [] */0
              }
            }
          }
        }
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "topsort_test.ml",
          48,
          2
        ],
        Error: new Error()
      };
}

function dfs3(nodes, graph) {
  var visited = {
    contents: /* [] */0
  };
  var aux = function (node, graph) {
    if (!List.mem(node, visited.contents)) {
      visited.contents = {
        hd: node,
        tl: visited.contents
      };
      return List.iter((function (x) {
                    return aux(x, graph);
                  }), nexts(node, graph));
    }
    
  };
  List.iter((function (node) {
          return aux(node, graph);
        }), nodes);
  return List.rev(visited.contents);
}

if (!Caml_obj.caml_equal(dfs3({
            hd: "a",
            tl: /* [] */0
          }, graph), {
        hd: "a",
        tl: {
          hd: "d",
          tl: {
            hd: "e",
            tl: {
              hd: "g",
              tl: {
                hd: "f",
                tl: {
                  hd: "c",
                  tl: {
                    hd: "b",
                    tl: /* [] */0
                  }
                }
              }
            }
          }
        }
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "topsort_test.ml",
          65,
          2
        ],
        Error: new Error()
      };
}

if (!Caml_obj.caml_equal(dfs3({
            hd: "b",
            tl: /* [] */0
          }, {
            hd: [
              "f",
              "d"
            ],
            tl: graph
          }), {
        hd: "b",
        tl: {
          hd: "e",
          tl: {
            hd: "g",
            tl: {
              hd: "f",
              tl: {
                hd: "d",
                tl: /* [] */0
              }
            }
          }
        }
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "topsort_test.ml",
          66,
          2
        ],
        Error: new Error()
      };
}

var grwork = {
  hd: [
    "wake",
    "shower"
  ],
  tl: {
    hd: [
      "shower",
      "dress"
    ],
    tl: {
      hd: [
        "dress",
        "go"
      ],
      tl: {
        hd: [
          "wake",
          "eat"
        ],
        tl: {
          hd: [
            "eat",
            "washup"
          ],
          tl: {
            hd: [
              "washup",
              "go"
            ],
            tl: /* [] */0
          }
        }
      }
    }
  }
};

function unsafe_topsort(graph) {
  var visited = {
    contents: /* [] */0
  };
  var sort_node = function (node) {
    if (List.mem(node, visited.contents)) {
      return ;
    }
    var nodes = nexts(node, graph);
    List.iter(sort_node, nodes);
    visited.contents = {
      hd: node,
      tl: visited.contents
    };
    
  };
  List.iter((function (param) {
          return sort_node(param[0]);
        }), graph);
  return visited.contents;
}

if (!Caml_obj.caml_equal(unsafe_topsort(grwork), {
        hd: "wake",
        tl: {
          hd: "shower",
          tl: {
            hd: "dress",
            tl: {
              hd: "eat",
              tl: {
                hd: "washup",
                tl: {
                  hd: "go",
                  tl: /* [] */0
                }
              }
            }
          }
        }
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "topsort_test.ml",
          110,
          2
        ],
        Error: new Error()
      };
}

function height(param) {
  if (param) {
    return param.h;
  } else {
    return 0;
  }
}

function create(l, v, r) {
  var hl = l ? l.h : 0;
  var hr = r ? r.h : 0;
  return /* Node */{
          l,
          v,
          r,
          h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal(l, v, r) {
  var hl = l ? l.h : 0;
  var hr = r ? r.h : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l.r;
      var lv = l.v;
      var ll = l.l;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, create(lr, v, r));
      }
      if (lr) {
        return create(create(ll, lv, lr.l), lr.v, create(lr.r, v, r));
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Set.bal",
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Set.bal",
          Error: new Error()
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */{
            l,
            v,
            r,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (r) {
    var rr = r.r;
    var rv = r.v;
    var rl = r.l;
    if (height(rr) >= height(rl)) {
      return create(create(l, v, rl), rv, rr);
    }
    if (rl) {
      return create(create(l, v, rl.l), rl.v, create(rl.r, rv, rr));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Set.bal",
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Set.bal",
        Error: new Error()
      };
}

function add(x, t) {
  if (!t) {
    return /* Node */{
            l: /* Empty */0,
            v: x,
            r: /* Empty */0,
            h: 1
          };
  }
  var r = t.r;
  var v = t.v;
  var l = t.l;
  var c = Caml_primitive.caml_string_compare(x, v);
  if (c === 0) {
    return t;
  }
  if (c < 0) {
    var ll = add(x, l);
    if (l === ll) {
      return t;
    } else {
      return bal(ll, v, r);
    }
  }
  var rr = add(x, r);
  if (r === rr) {
    return t;
  } else {
    return bal(l, v, rr);
  }
}

function singleton(x) {
  return /* Node */{
          l: /* Empty */0,
          v: x,
          r: /* Empty */0,
          h: 1
        };
}

function add_min_element(x, param) {
  if (param) {
    return bal(add_min_element(x, param.l), param.v, param.r);
  } else {
    return singleton(x);
  }
}

function add_max_element(x, param) {
  if (param) {
    return bal(param.l, param.v, add_max_element(x, param.r));
  } else {
    return singleton(x);
  }
}

function join(l, v, r) {
  if (!l) {
    return add_min_element(v, r);
  }
  if (!r) {
    return add_max_element(v, l);
  }
  var rh = r.h;
  var lh = l.h;
  if (lh > (rh + 2 | 0)) {
    return bal(l.l, l.v, join(l.r, v, r));
  } else if (rh > (lh + 2 | 0)) {
    return bal(join(l, v, r.l), r.v, r.r);
  } else {
    return create(l, v, r);
  }
}

function min_elt(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var l = param.l;
      if (!l) {
        return param.v;
      }
      _param = l;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function min_elt_opt(_param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var l = param.l;
    if (!l) {
      return Caml_option.some(param.v);
    }
    _param = l;
    continue ;
  };
}

function max_elt(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var r = param.r;
      if (!r) {
        return param.v;
      }
      _param = r;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function max_elt_opt(_param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var r = param.r;
    if (!r) {
      return Caml_option.some(param.v);
    }
    _param = r;
    continue ;
  };
}

function remove_min_elt(param) {
  if (param) {
    var l = param.l;
    if (l) {
      return bal(remove_min_elt(l), param.v, param.r);
    } else {
      return param.r;
    }
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Set.remove_min_elt",
        Error: new Error()
      };
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
  if (!param) {
    return [
            /* Empty */0,
            false,
            /* Empty */0
          ];
  }
  var r = param.r;
  var v = param.v;
  var l = param.l;
  var c = Caml_primitive.caml_string_compare(x, v);
  if (c === 0) {
    return [
            l,
            true,
            r
          ];
  }
  if (c < 0) {
    var match = split(x, l);
    return [
            match[0],
            match[1],
            join(match[2], v, r)
          ];
  }
  var match$1 = split(x, r);
  return [
          join(l, v, match$1[0]),
          match$1[1],
          match$1[2]
        ];
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
    if (!param) {
      return false;
    }
    var c = Caml_primitive.caml_string_compare(x, param.v);
    if (c === 0) {
      return true;
    }
    _param = c < 0 ? param.l : param.r;
    continue ;
  };
}

function remove(x, t) {
  if (!t) {
    return /* Empty */0;
  }
  var r = t.r;
  var v = t.v;
  var l = t.l;
  var c = Caml_primitive.caml_string_compare(x, v);
  if (c === 0) {
    if (l) {
      if (r) {
        return bal(l, min_elt(r), remove_min_elt(r));
      } else {
        return l;
      }
    } else {
      return r;
    }
  }
  if (c < 0) {
    var ll = remove(x, l);
    if (l === ll) {
      return t;
    } else {
      return bal(ll, v, r);
    }
  }
  var rr = remove(x, r);
  if (r === rr) {
    return t;
  } else {
    return bal(l, v, rr);
  }
}

function union(s1, s2) {
  if (!s1) {
    return s2;
  }
  if (!s2) {
    return s1;
  }
  var h2 = s2.h;
  var v2 = s2.v;
  var h1 = s1.h;
  var v1 = s1.v;
  if (h1 >= h2) {
    if (h2 === 1) {
      return add(v2, s1);
    }
    var match = split(v1, s2);
    return join(union(s1.l, match[0]), v1, union(s1.r, match[2]));
  }
  if (h1 === 1) {
    return add(v1, s2);
  }
  var match$1 = split(v2, s1);
  return join(union(match$1[0], s2.l), v2, union(match$1[2], s2.r));
}

function inter(s1, s2) {
  if (!s1) {
    return /* Empty */0;
  }
  if (!s2) {
    return /* Empty */0;
  }
  var r1 = s1.r;
  var v1 = s1.v;
  var l1 = s1.l;
  var match = split(v1, s2);
  var l2 = match[0];
  if (match[1]) {
    return join(inter(l1, l2), v1, inter(r1, match[2]));
  } else {
    return concat(inter(l1, l2), inter(r1, match[2]));
  }
}

function diff(s1, s2) {
  if (!s1) {
    return /* Empty */0;
  }
  if (!s2) {
    return s1;
  }
  var r1 = s1.r;
  var v1 = s1.v;
  var l1 = s1.l;
  var match = split(v1, s2);
  var l2 = match[0];
  if (match[1]) {
    return concat(diff(l1, l2), diff(r1, match[2]));
  } else {
    return join(diff(l1, l2), v1, diff(r1, match[2]));
  }
}

function cons_enum(_s, _e) {
  while(true) {
    var e = _e;
    var s = _s;
    if (!s) {
      return e;
    }
    _e = /* More */{
      _0: s.v,
      _1: s.r,
      _2: e
    };
    _s = s.l;
    continue ;
  };
}

function compare(s1, s2) {
  var _e1 = cons_enum(s1, /* End */0);
  var _e2 = cons_enum(s2, /* End */0);
  while(true) {
    var e2 = _e2;
    var e1 = _e1;
    if (!e1) {
      if (e2) {
        return -1;
      } else {
        return 0;
      }
    }
    if (!e2) {
      return 1;
    }
    var c = Caml_primitive.caml_string_compare(e1._0, e2._0);
    if (c !== 0) {
      return c;
    }
    _e2 = cons_enum(e2._1, e2._2);
    _e1 = cons_enum(e1._1, e1._2);
    continue ;
  };
}

function equal(s1, s2) {
  return compare(s1, s2) === 0;
}

function subset(_s1, _s2) {
  while(true) {
    var s2 = _s2;
    var s1 = _s1;
    if (!s1) {
      return true;
    }
    if (!s2) {
      return false;
    }
    var r2 = s2.r;
    var l2 = s2.l;
    var r1 = s1.r;
    var v1 = s1.v;
    var l1 = s1.l;
    var c = Caml_primitive.caml_string_compare(v1, s2.v);
    if (c === 0) {
      if (!subset(l1, l2)) {
        return false;
      }
      _s2 = r2;
      _s1 = r1;
      continue ;
    }
    if (c < 0) {
      if (!subset(/* Node */{
              l: l1,
              v: v1,
              r: /* Empty */0,
              h: 0
            }, l2)) {
        return false;
      }
      _s1 = r1;
      continue ;
    }
    if (!subset(/* Node */{
            l: /* Empty */0,
            v: v1,
            r: r1,
            h: 0
          }, r2)) {
      return false;
    }
    _s1 = l1;
    continue ;
  };
}

function iter(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    iter(f, param.l);
    Curry._1(f, param.v);
    _param = param.r;
    continue ;
  };
}

function fold(f, _s, _accu) {
  while(true) {
    var accu = _accu;
    var s = _s;
    if (!s) {
      return accu;
    }
    _accu = Curry._2(f, s.v, fold(f, s.l, accu));
    _s = s.r;
    continue ;
  };
}

function for_all(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return true;
    }
    if (!Curry._1(p, param.v)) {
      return false;
    }
    if (!for_all(p, param.l)) {
      return false;
    }
    _param = param.r;
    continue ;
  };
}

function exists(p, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    if (Curry._1(p, param.v)) {
      return true;
    }
    if (exists(p, param.l)) {
      return true;
    }
    _param = param.r;
    continue ;
  };
}

function filter(p, t) {
  if (!t) {
    return /* Empty */0;
  }
  var r = t.r;
  var v = t.v;
  var l = t.l;
  var l$prime = filter(p, l);
  var pv = Curry._1(p, v);
  var r$prime = filter(p, r);
  if (pv) {
    if (l === l$prime && r === r$prime) {
      return t;
    } else {
      return join(l$prime, v, r$prime);
    }
  } else {
    return concat(l$prime, r$prime);
  }
}

function partition(p, param) {
  if (!param) {
    return [
            /* Empty */0,
            /* Empty */0
          ];
  }
  var v = param.v;
  var match = partition(p, param.l);
  var lf = match[1];
  var lt = match[0];
  var pv = Curry._1(p, v);
  var match$1 = partition(p, param.r);
  var rf = match$1[1];
  var rt = match$1[0];
  if (pv) {
    return [
            join(lt, v, rt),
            concat(lf, rf)
          ];
  } else {
    return [
            concat(lt, rt),
            join(lf, v, rf)
          ];
  }
}

function cardinal(param) {
  if (param) {
    return (cardinal(param.l) + 1 | 0) + cardinal(param.r) | 0;
  } else {
    return 0;
  }
}

function elements_aux(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (!param) {
      return accu;
    }
    _param = param.l;
    _accu = {
      hd: param.v,
      tl: elements_aux(accu, param.r)
    };
    continue ;
  };
}

function elements(s) {
  return elements_aux(/* [] */0, s);
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param.v;
      var c = Caml_primitive.caml_string_compare(x, v);
      if (c === 0) {
        return v;
      }
      _param = c < 0 ? param.l : param.r;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function find_first(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param.v;
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _param$1 = param.l;
        while(true) {
          var param$1 = _param$1;
          var v0 = _v0;
          if (!param$1) {
            return v0;
          }
          var v$1 = param$1.v;
          if (Curry._1(f, v$1)) {
            _param$1 = param$1.l;
            _v0 = v$1;
            continue ;
          }
          _param$1 = param$1.r;
          continue ;
        };
      }
      _param = param.r;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function find_first_opt(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var v = param.v;
    if (Curry._1(f, v)) {
      var _v0 = v;
      var _param$1 = param.l;
      while(true) {
        var param$1 = _param$1;
        var v0 = _v0;
        if (!param$1) {
          return Caml_option.some(v0);
        }
        var v$1 = param$1.v;
        if (Curry._1(f, v$1)) {
          _param$1 = param$1.l;
          _v0 = v$1;
          continue ;
        }
        _param$1 = param$1.r;
        continue ;
      };
    }
    _param = param.r;
    continue ;
  };
}

function find_last(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param.v;
      if (Curry._1(f, v)) {
        var _v0 = v;
        var _param$1 = param.r;
        while(true) {
          var param$1 = _param$1;
          var v0 = _v0;
          if (!param$1) {
            return v0;
          }
          var v$1 = param$1.v;
          if (Curry._1(f, v$1)) {
            _param$1 = param$1.r;
            _v0 = v$1;
            continue ;
          }
          _param$1 = param$1.l;
          continue ;
        };
      }
      _param = param.l;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function find_last_opt(f, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var v = param.v;
    if (Curry._1(f, v)) {
      var _v0 = v;
      var _param$1 = param.r;
      while(true) {
        var param$1 = _param$1;
        var v0 = _v0;
        if (!param$1) {
          return Caml_option.some(v0);
        }
        var v$1 = param$1.v;
        if (Curry._1(f, v$1)) {
          _param$1 = param$1.r;
          _v0 = v$1;
          continue ;
        }
        _param$1 = param$1.l;
        continue ;
      };
    }
    _param = param.l;
    continue ;
  };
}

function find_opt(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    var v = param.v;
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return Caml_option.some(v);
    }
    _param = c < 0 ? param.l : param.r;
    continue ;
  };
}

function map(f, t) {
  if (!t) {
    return /* Empty */0;
  }
  var r = t.r;
  var v = t.v;
  var l = t.l;
  var l$prime = map(f, l);
  var v$prime = Curry._1(f, v);
  var r$prime = map(f, r);
  if (l === l$prime && v === v$prime && r === r$prime) {
    return t;
  } else if ((l$prime === /* Empty */0 || Caml_primitive.caml_string_compare(max_elt(l$prime), v$prime) < 0) && (r$prime === /* Empty */0 || Caml_primitive.caml_string_compare(v$prime, min_elt(r$prime)) < 0)) {
    return join(l$prime, v$prime, r$prime);
  } else {
    return union(l$prime, add(v$prime, r$prime));
  }
}

function of_list(l) {
  if (!l) {
    return /* Empty */0;
  }
  var match = l.tl;
  var x0 = l.hd;
  if (!match) {
    return singleton(x0);
  }
  var match$1 = match.tl;
  var x1 = match.hd;
  if (!match$1) {
    return add(x1, singleton(x0));
  }
  var match$2 = match$1.tl;
  var x2 = match$1.hd;
  if (!match$2) {
    return add(x2, add(x1, singleton(x0)));
  }
  var match$3 = match$2.tl;
  var x3 = match$2.hd;
  if (match$3) {
    if (match$3.tl) {
      var l$1 = List.sort_uniq($$String.compare, l);
      var sub = function (n, l) {
        switch (n) {
          case 0 :
              return [
                      /* Empty */0,
                      l
                    ];
          case 1 :
              if (l) {
                return [
                        /* Node */{
                          l: /* Empty */0,
                          v: l.hd,
                          r: /* Empty */0,
                          h: 1
                        },
                        l.tl
                      ];
              }
              break;
          case 2 :
              if (l) {
                var match = l.tl;
                if (match) {
                  return [
                          /* Node */{
                            l: /* Node */{
                              l: /* Empty */0,
                              v: l.hd,
                              r: /* Empty */0,
                              h: 1
                            },
                            v: match.hd,
                            r: /* Empty */0,
                            h: 2
                          },
                          match.tl
                        ];
                }
                
              }
              break;
          case 3 :
              if (l) {
                var match$1 = l.tl;
                if (match$1) {
                  var match$2 = match$1.tl;
                  if (match$2) {
                    return [
                            /* Node */{
                              l: /* Node */{
                                l: /* Empty */0,
                                v: l.hd,
                                r: /* Empty */0,
                                h: 1
                              },
                              v: match$1.hd,
                              r: /* Node */{
                                l: /* Empty */0,
                                v: match$2.hd,
                                r: /* Empty */0,
                                h: 1
                              },
                              h: 2
                            },
                            match$2.tl
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
        if (l$1) {
          var match$4 = sub((n - nl | 0) - 1 | 0, l$1.tl);
          return [
                  create(match$3[0], l$1.hd, match$4[0]),
                  match$4[1]
                ];
        }
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "set.ml",
                510,
                18
              ],
              Error: new Error()
            };
      };
      return sub(List.length(l$1), l$1)[0];
    } else {
      return add(match$3.hd, add(x3, add(x2, add(x1, singleton(x0)))));
    }
  } else {
    return add(x3, add(x2, add(x1, singleton(x0))));
  }
}

var String_set = {
  empty: /* Empty */0,
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
  map,
  fold,
  for_all,
  exists,
  filter,
  partition,
  cardinal,
  elements,
  min_elt,
  min_elt_opt,
  max_elt,
  max_elt_opt,
  choose: min_elt,
  choose_opt: min_elt_opt,
  split,
  find,
  find_opt,
  find_first,
  find_first_opt,
  find_last,
  find_last_opt,
  of_list
};

var Cycle = Caml_exceptions.create("Topsort_test.Cycle");

function pathsort(graph) {
  var visited = {
    contents: /* [] */0
  };
  var empty_path = [
    /* Empty */0,
    /* [] */0
  ];
  var $plus$great = function (node, param) {
    var stack = param[1];
    var set = param[0];
    if (mem(node, set)) {
      throw {
            RE_EXN_ID: Cycle,
            _1: {
              hd: node,
              tl: stack
            },
            Error: new Error()
          };
    }
    return [
            add(node, set),
            {
              hd: node,
              tl: stack
            }
          ];
  };
  var sort_nodes = function (path, nodes) {
    return List.iter((function (node) {
                  return sort_node(path, node);
                }), nodes);
  };
  var sort_node = function (path, node) {
    if (!List.mem(node, visited.contents)) {
      sort_nodes($plus$great(node, path), nexts(node, graph));
      visited.contents = {
        hd: node,
        tl: visited.contents
      };
      return ;
    }
    
  };
  List.iter((function (param) {
          return sort_node(empty_path, param[0]);
        }), graph);
  return visited.contents;
}

if (!Caml_obj.caml_equal(pathsort(grwork), {
        hd: "wake",
        tl: {
          hd: "shower",
          tl: {
            hd: "dress",
            tl: {
              hd: "eat",
              tl: {
                hd: "washup",
                tl: {
                  hd: "go",
                  tl: /* [] */0
                }
              }
            }
          }
        }
      })) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "topsort_test.ml",
          150,
          4
        ],
        Error: new Error()
      };
}

try {
  pathsort({
        hd: [
          "go",
          "eat"
        ],
        tl: grwork
      });
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "topsort_test.ml",
          156,
          8
        ],
        Error: new Error()
      };
}
catch (raw_exn){
  var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
  var exit = 0;
  if (exn.RE_EXN_ID === Cycle) {
    var match = exn._1;
    if (match && match.hd === "go") {
      var match$1 = match.tl;
      if (match$1 && match$1.hd === "washup") {
        var match$2 = match$1.tl;
        if (match$2 && match$2.hd === "eat") {
          var match$3 = match$2.tl;
          if (!(match$3 && match$3.hd === "go" && !match$3.tl)) {
            exit = 1;
          }
          
        } else {
          exit = 1;
        }
      } else {
        exit = 1;
      }
    } else {
      exit = 1;
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "topsort_test.ml",
            159,
            11
          ],
          Error: new Error()
        };
  }
  
}

exports.graph = graph;
exports.nexts = nexts;
exports.dfs1 = dfs1;
exports.dfs2 = dfs2;
exports.dfs3 = dfs3;
exports.grwork = grwork;
exports.unsafe_topsort = unsafe_topsort;
exports.String_set = String_set;
exports.Cycle = Cycle;
exports.pathsort = pathsort;
/*  Not a pure module */
