'use strict';

var Caml_obj                = require("../../lib/js/caml_obj");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");
var Bal_set_common          = require("./bal_set_common");
var Curry                   = require("../../lib/js/curry");
var $$Array                 = require("../../lib/js/array");
var List                    = require("../../lib/js/list");

function split(x, tree) {
  if (tree) {
    var r = tree[2];
    var v = tree[1];
    var l = tree[0];
    var c = Caml_obj.caml_compare(x, v);
    if (c) {
      if (c < 0) {
        var match = split(x, l);
        return /* tuple */[
                match[0],
                match[1],
                Bal_set_common.internal_join(match[2], v, r)
              ];
      }
      else {
        var match$1 = split(x, r);
        return /* tuple */[
                Bal_set_common.internal_join(l, v, match$1[0]),
                match$1[1],
                match$1[2]
              ];
      }
    }
    else {
      return /* tuple */[
              l,
              /* true */1,
              r
            ];
    }
  }
  else {
    return /* tuple */[
            /* Empty */0,
            /* false */0,
            /* Empty */0
          ];
  }
}

function add(x, tree) {
  if (tree) {
    var r = tree[2];
    var v = tree[1];
    var l = tree[0];
    var c = Caml_obj.caml_compare(x, v);
    if (c) {
      if (c < 0) {
        return Bal_set_common.internal_bal(add(x, l), v, r);
      }
      else {
        return Bal_set_common.internal_bal(l, v, add(x, r));
      }
    }
    else {
      return tree;
    }
  }
  else {
    return /* Node */[
            /* Empty */0,
            x,
            /* Empty */0,
            1
          ];
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
        }
        else {
          var match = split(v1, s2);
          return Bal_set_common.internal_join(union(s1[0], match[0]), v1, union(s1[2], match[2]));
        }
      }
      else if (h1 === 1) {
        return add(v1, s2);
      }
      else {
        var match$1 = split(v2, s1);
        return Bal_set_common.internal_join(union(match$1[0], s2[0]), v2, union(match$1[2], s2[2]));
      }
    }
    else {
      return s1;
    }
  }
  else {
    return s2;
  }
}

function inter(s1, s2) {
  if (s1) {
    if (s2) {
      var r1 = s1[2];
      var v1 = s1[1];
      var l1 = s1[0];
      var match = split(v1, s2);
      var l2 = match[0];
      if (match[1] !== 0) {
        return Bal_set_common.internal_join(inter(l1, l2), v1, inter(r1, match[2]));
      }
      else {
        return Bal_set_common.internal_concat(inter(l1, l2), inter(r1, match[2]));
      }
    }
    else {
      return /* Empty */0;
    }
  }
  else {
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
      if (match[1] !== 0) {
        return Bal_set_common.internal_concat(diff(l1, l2), diff(r1, match[2]));
      }
      else {
        return Bal_set_common.internal_join(diff(l1, l2), v1, diff(r1, match[2]));
      }
    }
    else {
      return s1;
    }
  }
  else {
    return /* Empty */0;
  }
}

function mem(x, _tree) {
  while(true) {
    var tree = _tree;
    if (tree) {
      var c = Caml_obj.caml_compare(x, tree[1]);
      if (c) {
        _tree = c < 0 ? tree[0] : tree[2];
        continue ;
        
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

function remove(x, param) {
  if (param) {
    var r = param[2];
    var v = param[1];
    var l = param[0];
    var c = Caml_obj.caml_compare(x, v);
    if (c) {
      if (c < 0) {
        return Bal_set_common.internal_bal(remove(x, l), v, r);
      }
      else {
        return Bal_set_common.internal_bal(l, v, remove(x, r));
      }
    }
    else {
      return Bal_set_common.internal_merge(l, r);
    }
  }
  else {
    return /* Empty */0;
  }
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
        var c = Caml_obj.caml_compare(v1, s2[1]);
        if (c) {
          if (c < 0) {
            if (subset(/* Node */[
                    l1,
                    v1,
                    /* Empty */0,
                    0
                  ], l2)) {
              _s1 = r1;
              continue ;
              
            }
            else {
              return /* false */0;
            }
          }
          else if (subset(/* Node */[
                  /* Empty */0,
                  v1,
                  r1,
                  0
                ], r2)) {
            _s1 = l1;
            continue ;
            
          }
          else {
            return /* false */0;
          }
        }
        else if (subset(l1, l2)) {
          _s2 = r2;
          _s1 = r1;
          continue ;
          
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

function compare(s1, s2) {
  return Bal_set_common.compare(Caml_obj.caml_compare, s1, s2);
}

function equal(s1, s2) {
  return +(compare(s1, s2) === 0);
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var v = param[1];
      var c = Caml_obj.caml_compare(x, v);
      if (c) {
        _param = c < 0 ? param[0] : param[2];
        continue ;
        
      }
      else {
        return v;
      }
    }
    else {
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
              return Bal_set_common.of_sorted_list(List.sort_uniq(Caml_obj.caml_compare, l));
            }
            else {
              return add(match$3[0], add(x3, add(x2, add(x1, Bal_set_common.singleton(x0)))));
            }
          }
          else {
            return add(x3, add(x2, add(x1, Bal_set_common.singleton(x0))));
          }
        }
        else {
          return add(x2, add(x1, Bal_set_common.singleton(x0)));
        }
      }
      else {
        return add(x1, Bal_set_common.singleton(x0));
      }
    }
    else {
      return Bal_set_common.singleton(x0);
    }
  }
  else {
    return /* Empty */0;
  }
}

function of_array(l) {
  return $$Array.fold_left(function (acc, x) {
              return add(x, acc);
            }, /* Empty */0, l);
}

function invariant(t) {
  return Bal_set_common.invariant(Caml_obj.caml_compare, t);
}

function Make(S) {
  var split = function (x, param) {
    if (param) {
      var r = param[2];
      var v = param[1];
      var l = param[0];
      var c = Curry._2(S[/* compare */0], x, v);
      if (c) {
        if (c < 0) {
          var match = split(x, l);
          return /* tuple */[
                  match[0],
                  match[1],
                  Bal_set_common.internal_join(match[2], v, r)
                ];
        }
        else {
          var match$1 = split(x, r);
          return /* tuple */[
                  Bal_set_common.internal_join(l, v, match$1[0]),
                  match$1[1],
                  match$1[2]
                ];
        }
      }
      else {
        return /* tuple */[
                l,
                /* true */1,
                r
              ];
      }
    }
    else {
      return /* tuple */[
              /* Empty */0,
              /* false */0,
              /* Empty */0
            ];
    }
  };
  var add = function (x, t) {
    if (t) {
      var r = t[2];
      var v = t[1];
      var l = t[0];
      var c = Curry._2(S[/* compare */0], x, v);
      if (c) {
        if (c < 0) {
          return Bal_set_common.internal_bal(add(x, l), v, r);
        }
        else {
          return Bal_set_common.internal_bal(l, v, add(x, r));
        }
      }
      else {
        return t;
      }
    }
    else {
      return /* Node */[
              /* Empty */0,
              x,
              /* Empty */0,
              1
            ];
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
          }
          else {
            var match = split(v1, s2);
            return Bal_set_common.internal_join(union(s1[0], match[0]), v1, union(s1[2], match[2]));
          }
        }
        else if (h1 === 1) {
          return add(v1, s2);
        }
        else {
          var match$1 = split(v2, s1);
          return Bal_set_common.internal_join(union(match$1[0], s2[0]), v2, union(match$1[2], s2[2]));
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
        var r1 = s1[2];
        var v1 = s1[1];
        var l1 = s1[0];
        var match = split(v1, s2);
        var l2 = match[0];
        if (match[1] !== 0) {
          return Bal_set_common.internal_join(inter(l1, l2), v1, inter(r1, match[2]));
        }
        else {
          return Bal_set_common.internal_concat(inter(l1, l2), inter(r1, match[2]));
        }
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
        var r1 = s1[2];
        var v1 = s1[1];
        var l1 = s1[0];
        var match = split(v1, s2);
        var l2 = match[0];
        if (match[1] !== 0) {
          return Bal_set_common.internal_concat(diff(l1, l2), diff(r1, match[2]));
        }
        else {
          return Bal_set_common.internal_join(diff(l1, l2), v1, diff(r1, match[2]));
        }
      }
      else {
        return s1;
      }
    }
    else {
      return /* Empty */0;
    }
  };
  var mem = function (x, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var c = Curry._2(S[/* compare */0], x, param[1]);
        if (c) {
          _param = c < 0 ? param[0] : param[2];
          continue ;
          
        }
        else {
          return /* true */1;
        }
      }
      else {
        return /* false */0;
      }
    };
  };
  var remove = function (x, param) {
    if (param) {
      var r = param[2];
      var v = param[1];
      var l = param[0];
      var c = Curry._2(S[/* compare */0], x, v);
      if (c) {
        if (c < 0) {
          return Bal_set_common.internal_bal(remove(x, l), v, r);
        }
        else {
          return Bal_set_common.internal_bal(l, v, remove(x, r));
        }
      }
      else {
        return Bal_set_common.internal_merge(l, r);
      }
    }
    else {
      return /* Empty */0;
    }
  };
  var compare = function (s1, s2) {
    return Bal_set_common.compare(Caml_obj.caml_compare, s1, s2);
  };
  var equal = function (s1, s2) {
    return +(compare(s1, s2) === 0);
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
          var c = Curry._2(S[/* compare */0], v1, s2[1]);
          if (c) {
            if (c < 0) {
              if (subset(/* Node */[
                      l1,
                      v1,
                      /* Empty */0,
                      0
                    ], l2)) {
                _s1 = r1;
                continue ;
                
              }
              else {
                return /* false */0;
              }
            }
            else if (subset(/* Node */[
                    /* Empty */0,
                    v1,
                    r1,
                    0
                  ], r2)) {
              _s1 = l1;
              continue ;
              
            }
            else {
              return /* false */0;
            }
          }
          else if (subset(l1, l2)) {
            _s2 = r2;
            _s1 = r1;
            continue ;
            
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
  };
  var find = function (x, _param) {
    while(true) {
      var param = _param;
      if (param) {
        var v = param[1];
        var c = Curry._2(S[/* compare */0], x, v);
        if (c) {
          _param = c < 0 ? param[0] : param[2];
          continue ;
          
        }
        else {
          return v;
        }
      }
      else {
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
                return Bal_set_common.of_sorted_list(List.sort_uniq(S[/* compare */0], l));
              }
              else {
                return add(match$3[0], add(x3, add(x2, add(x1, Bal_set_common.singleton(x0)))));
              }
            }
            else {
              return add(x3, add(x2, add(x1, Bal_set_common.singleton(x0))));
            }
          }
          else {
            return add(x2, add(x1, Bal_set_common.singleton(x0)));
          }
        }
        else {
          return add(x1, Bal_set_common.singleton(x0));
        }
      }
      else {
        return Bal_set_common.singleton(x0);
      }
    }
    else {
      return /* Empty */0;
    }
  };
  var of_array = function (l) {
    return $$Array.fold_left(function (acc, x) {
                return add(x, acc);
              }, /* Empty */0, l);
  };
  var invariant = function (t) {
    Bal_set_common.check(t);
    return Bal_set_common.is_ordered(S[/* compare */0], t);
  };
  return /* module */[
          /* empty : Empty */0,
          /* is_empty */Bal_set_common.is_empty,
          /* iter */Bal_set_common.iter,
          /* fold */Bal_set_common.fold,
          /* for_all */Bal_set_common.for_all,
          /* exists */Bal_set_common.exists,
          /* singleton */Bal_set_common.singleton,
          /* cardinal */Bal_set_common.cardinal,
          /* elements */Bal_set_common.elements,
          /* min_elt */Bal_set_common.min_elt,
          /* max_elt */Bal_set_common.max_elt,
          /* choose */Bal_set_common.choose,
          /* partition */Bal_set_common.partition,
          /* filter */Bal_set_common.filter,
          /* of_sorted_list */Bal_set_common.of_sorted_list,
          /* of_sorted_array */Bal_set_common.of_sorted_array,
          /* split */split,
          /* add */add,
          /* union */union,
          /* inter */inter,
          /* diff */diff,
          /* mem */mem,
          /* remove */remove,
          /* compare */compare,
          /* equal */equal,
          /* subset */subset,
          /* find */find,
          /* of_list */of_list,
          /* of_array */of_array,
          /* invariant */invariant
        ];
}

exports.split     = split;
exports.add       = add;
exports.union     = union;
exports.inter     = inter;
exports.diff      = diff;
exports.mem       = mem;
exports.remove    = remove;
exports.subset    = subset;
exports.compare   = compare;
exports.equal     = equal;
exports.find      = find;
exports.of_list   = of_list;
exports.of_array  = of_array;
exports.invariant = invariant;
exports.Make      = Make;
/* No side effect */
