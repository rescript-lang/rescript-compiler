'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");
var Bal_set_common          = require("./bal_set_common");
var $$String                = require("../../lib/js/string");
var Caml_string             = require("../../lib/js/caml_string");
var List                    = require("../../lib/js/list");

function add(x, tree) {
  if (tree) {
    var r = tree[2];
    var v = tree[1];
    var l = tree[0];
    var c = Caml_string.caml_string_compare(x, v);
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

function mem(x, _tree) {
  while(true) {
    var tree = _tree;
    if (tree) {
      var c = Caml_string.caml_string_compare(x, tree[1]);
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
              return Bal_set_common.of_sorted_list(List.sort_uniq($$String.compare, l));
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

function find(x, _tree) {
  while(true) {
    var tree = _tree;
    if (tree) {
      var v = tree[1];
      var c = Caml_string.caml_string_compare(x, v);
      if (c) {
        _tree = c < 0 ? tree[0] : tree[2];
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

var compare_elt = $$String.compare;

var empty = /* Empty */0;

var is_empty = Bal_set_common.is_empty;

var iter = Bal_set_common.iter;

var fold = Bal_set_common.fold;

var for_all = Bal_set_common.for_all;

var exists = Bal_set_common.exists;

var singleton = Bal_set_common.singleton;

var cardinal = Bal_set_common.cardinal;

var elements = Bal_set_common.elements;

var min_elt = Bal_set_common.min_elt;

var max_elt = Bal_set_common.max_elt;

var choose = Bal_set_common.choose;

var partition = Bal_set_common.partition;

var filter = Bal_set_common.filter;

var of_sorted_list = Bal_set_common.of_sorted_list;

var of_sorted_array = Bal_set_common.of_sorted_array;

exports.compare_elt     = compare_elt;
exports.empty           = empty;
exports.is_empty        = is_empty;
exports.iter            = iter;
exports.fold            = fold;
exports.for_all         = for_all;
exports.exists          = exists;
exports.singleton       = singleton;
exports.cardinal        = cardinal;
exports.elements        = elements;
exports.min_elt         = min_elt;
exports.max_elt         = max_elt;
exports.choose          = choose;
exports.partition       = partition;
exports.filter          = filter;
exports.of_sorted_list  = of_sorted_list;
exports.of_sorted_array = of_sorted_array;
exports.add             = add;
exports.mem             = mem;
exports.of_list         = of_list;
exports.find            = find;
/* No side effect */
