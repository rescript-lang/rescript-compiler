'use strict';

var U = require("U");
var VV = require("VV");
var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");

function f(x) {
  return (function (param) {
      return x + param | 0;
    });
}

function f1(x, y) {
  return x + y | 0;
}

function f3(g, x) {
  return Curry._1(g, x);
}

function f2(param) {
  return 3 + param | 0;
}

var g = 7;

function ff(param) {
  return U.test_primit(3, param);
}

var fff = VV.test_primit2(3);

function length_aux(_len, _param) {
  while(true) {
    var param = _param;
    var len = _len;
    if (param) {
      _param = param[1];
      _len = len + 1 | 0;
      continue ;
    } else {
      return len;
    }
  };
}

var length = List.length;

var compare_lengths = List.compare_lengths;

var compare_length_with = List.compare_length_with;

var cons = List.cons;

var hd = List.hd;

var tl = List.tl;

var nth = List.nth;

var nth_opt = List.nth_opt;

var rev = List.rev;

var init = List.init;

var append = List.append;

var rev_append = List.rev_append;

var concat = List.concat;

var flatten = List.flatten;

var iter = List.iter;

var iteri = List.iteri;

var map = List.map;

var mapi = List.mapi;

var rev_map = List.rev_map;

var fold_left = List.fold_left;

var fold_right = List.fold_right;

var iter2 = List.iter2;

var map2 = List.map2;

var rev_map2 = List.rev_map2;

var fold_left2 = List.fold_left2;

var fold_right2 = List.fold_right2;

var for_all = List.for_all;

var exists = List.exists;

var for_all2 = List.for_all2;

var exists2 = List.exists2;

var mem = List.mem;

var memq = List.memq;

var find = List.find;

var find_opt = List.find_opt;

var filter = List.filter;

var find_all = List.find_all;

var partition = List.partition;

var assoc = List.assoc;

var assoc_opt = List.assoc_opt;

var assq = List.assq;

var assq_opt = List.assq_opt;

var mem_assoc = List.mem_assoc;

var mem_assq = List.mem_assq;

var remove_assoc = List.remove_assoc;

var remove_assq = List.remove_assq;

var split = List.split;

var combine = List.combine;

var sort = List.sort;

var stable_sort = List.stable_sort;

var fast_sort = List.fast_sort;

var sort_uniq = List.sort_uniq;

var merge = List.merge;

exports.f = f;
exports.f1 = f1;
exports.f3 = f3;
exports.f2 = f2;
exports.g = g;
exports.ff = ff;
exports.fff = fff;
exports.length_aux = length_aux;
exports.length = length;
exports.compare_lengths = compare_lengths;
exports.compare_length_with = compare_length_with;
exports.cons = cons;
exports.hd = hd;
exports.tl = tl;
exports.nth = nth;
exports.nth_opt = nth_opt;
exports.rev = rev;
exports.init = init;
exports.append = append;
exports.rev_append = rev_append;
exports.concat = concat;
exports.flatten = flatten;
exports.iter = iter;
exports.iteri = iteri;
exports.map = map;
exports.mapi = mapi;
exports.rev_map = rev_map;
exports.fold_left = fold_left;
exports.fold_right = fold_right;
exports.iter2 = iter2;
exports.map2 = map2;
exports.rev_map2 = rev_map2;
exports.fold_left2 = fold_left2;
exports.fold_right2 = fold_right2;
exports.for_all = for_all;
exports.exists = exists;
exports.for_all2 = for_all2;
exports.exists2 = exists2;
exports.mem = mem;
exports.memq = memq;
exports.find = find;
exports.find_opt = find_opt;
exports.filter = filter;
exports.find_all = find_all;
exports.partition = partition;
exports.assoc = assoc;
exports.assoc_opt = assoc_opt;
exports.assq = assq;
exports.assq_opt = assq_opt;
exports.mem_assoc = mem_assoc;
exports.mem_assq = mem_assq;
exports.remove_assoc = remove_assoc;
exports.remove_assq = remove_assq;
exports.split = split;
exports.combine = combine;
exports.sort = sort;
exports.stable_sort = stable_sort;
exports.fast_sort = fast_sort;
exports.sort_uniq = sort_uniq;
exports.merge = merge;
/* fff Not a pure module */
