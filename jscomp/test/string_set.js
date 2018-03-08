'use strict';

var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var $$String = require("../../lib/js/string.js");
var Set_gen = require("./set_gen.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function split(x, tree) {
  if (tree) {
    var r = tree[2];
    var v = tree[1];
    var l = tree[0];
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return /* tuple */[
              l,
              /* true */1,
              r
            ];
    } else if (c < 0) {
      var match = split(x, l);
      return /* tuple */[
              match[0],
              match[1],
              Set_gen.internal_join(match[2], v, r)
            ];
    } else {
      var match$1 = split(x, r);
      return /* tuple */[
              Set_gen.internal_join(l, v, match$1[0]),
              match$1[1],
              match$1[2]
            ];
    }
  } else {
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
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return tree;
    } else if (c < 0) {
      return Set_gen.internal_bal(add(x, l), v, r);
    } else {
      return Set_gen.internal_bal(l, v, add(x, r));
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
          return Set_gen.internal_join(union(s1[0], match[0]), v1, union(s1[2], match[2]));
        }
      } else if (h1 === 1) {
        return add(v1, s2);
      } else {
        var match$1 = split(v2, s1);
        return Set_gen.internal_join(union(match$1[0], s2[0]), v2, union(match$1[2], s2[2]));
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
    if (match[1] !== 0) {
      return Set_gen.internal_join(inter(l1, l2), v1, inter(r1, match[2]));
    } else {
      return Set_gen.internal_concat(inter(l1, l2), inter(r1, match[2]));
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
      if (match[1] !== 0) {
        return Set_gen.internal_concat(diff(l1, l2), diff(r1, match[2]));
      } else {
        return Set_gen.internal_join(diff(l1, l2), v1, diff(r1, match[2]));
      }
    } else {
      return s1;
    }
  } else {
    return /* Empty */0;
  }
}

function mem(x, _tree) {
  while(true) {
    var tree = _tree;
    if (tree) {
      var c = Caml_primitive.caml_string_compare(x, tree[1]);
      if (c === 0) {
        return /* true */1;
      } else {
        _tree = c < 0 ? tree[0] : tree[2];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function remove(x, tree) {
  if (tree) {
    var r = tree[2];
    var v = tree[1];
    var l = tree[0];
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return Set_gen.internal_merge(l, r);
    } else if (c < 0) {
      return Set_gen.internal_bal(remove(x, l), v, r);
    } else {
      return Set_gen.internal_bal(l, v, remove(x, r));
    }
  } else {
    return /* Empty */0;
  }
}

function compare(s1, s2) {
  return Set_gen.compare($$String.compare, s1, s2);
}

function equal(s1, s2) {
  return +(Set_gen.compare($$String.compare, s1, s2) === 0);
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
            return /* false */0;
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
            return /* false */0;
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
          return /* false */0;
        }
      } else {
        return /* false */0;
      }
    } else {
      return /* true */1;
    }
  };
}

function find(x, _tree) {
  while(true) {
    var tree = _tree;
    if (tree) {
      var v = tree[1];
      var c = Caml_primitive.caml_string_compare(x, v);
      if (c === 0) {
        return v;
      } else {
        _tree = c < 0 ? tree[0] : tree[2];
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
              return Set_gen.of_sorted_list(List.sort_uniq($$String.compare, l));
            } else {
              return add(match$3[0], add(x3, add(x2, add(x1, Set_gen.singleton(x0)))));
            }
          } else {
            return add(x3, add(x2, add(x1, Set_gen.singleton(x0))));
          }
        } else {
          return add(x2, add(x1, Set_gen.singleton(x0)));
        }
      } else {
        return add(x1, Set_gen.singleton(x0));
      }
    } else {
      return Set_gen.singleton(x0);
    }
  } else {
    return /* Empty */0;
  }
}

function of_array(l) {
  return $$Array.fold_left((function (acc, x) {
                return add(x, acc);
              }), /* Empty */0, l);
}

function invariant(t) {
  Set_gen.check(t);
  return Set_gen.is_ordered($$String.compare, t);
}

var compare_elt = $$String.compare;

var empty = /* Empty */0;

var is_empty = Set_gen.is_empty;

var iter = Set_gen.iter;

var fold = Set_gen.fold;

var for_all = Set_gen.for_all;

var exists = Set_gen.exists;

var singleton = Set_gen.singleton;

var cardinal = Set_gen.cardinal;

var elements = Set_gen.elements;

var min_elt = Set_gen.min_elt;

var max_elt = Set_gen.max_elt;

var choose = Set_gen.choose;

var partition = Set_gen.partition;

var filter = Set_gen.filter;

var of_sorted_list = Set_gen.of_sorted_list;

var of_sorted_array = Set_gen.of_sorted_array;

exports.compare_elt = compare_elt;
exports.empty = empty;
exports.is_empty = is_empty;
exports.iter = iter;
exports.fold = fold;
exports.for_all = for_all;
exports.exists = exists;
exports.singleton = singleton;
exports.cardinal = cardinal;
exports.elements = elements;
exports.min_elt = min_elt;
exports.max_elt = max_elt;
exports.choose = choose;
exports.partition = partition;
exports.filter = filter;
exports.of_sorted_list = of_sorted_list;
exports.of_sorted_array = of_sorted_array;
exports.split = split;
exports.add = add;
exports.union = union;
exports.inter = inter;
exports.diff = diff;
exports.mem = mem;
exports.remove = remove;
exports.compare = compare;
exports.equal = equal;
exports.subset = subset;
exports.find = find;
exports.of_list = of_list;
exports.of_array = of_array;
exports.invariant = invariant;
/* No side effect */
