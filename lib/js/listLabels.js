'use strict';

var List = require("./list.js");

var length = List.length;

var hd = List.hd;

var tl = List.tl;

var nth = List.nth;

var rev = List.rev;

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

var filter = List.filter;

var find_all = List.find_all;

var partition = List.partition;

var assoc = List.assoc;

var assq = List.assq;

var mem_assoc = List.mem_assoc;

var mem_assq = List.mem_assq;

var remove_assoc = List.remove_assoc;

var remove_assq = List.remove_assq;

var split = List.split;

var combine = List.combine;

var sort = List.sort;

var stable_sort = List.stable_sort;

var fast_sort = List.fast_sort;

var merge = List.merge;

exports.length = length;
exports.hd = hd;
exports.tl = tl;
exports.nth = nth;
exports.rev = rev;
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
exports.filter = filter;
exports.find_all = find_all;
exports.partition = partition;
exports.assoc = assoc;
exports.assq = assq;
exports.mem_assoc = mem_assoc;
exports.mem_assq = mem_assq;
exports.remove_assoc = remove_assoc;
exports.remove_assq = remove_assq;
exports.split = split;
exports.combine = combine;
exports.sort = sort;
exports.stable_sort = stable_sort;
exports.fast_sort = fast_sort;
exports.merge = merge;
/* No side effect */
