// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let List = require("../../lib/js/list.js");
let Pervasives = require("../../lib/js/pervasives.js");

let Pervasives$1 = {
  length: List.length,
  compare_lengths: List.compare_lengths,
  compare_length_with: List.compare_length_with,
  cons: List.cons,
  hd: List.hd,
  tl: List.tl,
  nth: List.nth,
  nth_opt: List.nth_opt,
  rev: List.rev,
  init: List.init,
  append: List.append,
  rev_append: List.rev_append,
  concat: List.concat,
  flatten: List.flatten,
  iter: List.iter,
  iteri: List.iteri,
  map: List.map,
  mapi: List.mapi,
  rev_map: List.rev_map,
  fold_left: List.fold_left,
  fold_right: List.fold_right,
  iter2: List.iter2,
  map2: List.map2,
  rev_map2: List.rev_map2,
  fold_left2: List.fold_left2,
  fold_right2: List.fold_right2,
  for_all: List.for_all,
  exists: List.exists,
  for_all2: List.for_all2,
  exists2: List.exists2,
  mem: List.mem,
  memq: List.memq,
  find: List.find,
  find_opt: List.find_opt,
  filter: List.filter,
  find_all: List.find_all,
  partition: List.partition,
  assoc: List.assoc,
  assoc_opt: List.assoc_opt,
  assq: List.assq,
  assq_opt: List.assq_opt,
  mem_assoc: List.mem_assoc,
  mem_assq: List.mem_assq,
  remove_assoc: List.remove_assoc,
  remove_assq: List.remove_assq,
  split: List.split,
  combine: List.combine,
  sort: List.sort,
  stable_sort: List.stable_sort,
  fast_sort: List.fast_sort,
  sort_uniq: List.sort_uniq,
  merge: List.merge,
  invalid_arg: Pervasives.invalid_arg,
  failwith: Pervasives.failwith,
  Exit: Pervasives.Exit,
  abs: Pervasives.abs,
  max_int: Pervasives.max_int,
  min_int: Pervasives.min_int,
  lnot: Pervasives.lnot,
  infinity: Pervasives.infinity,
  neg_infinity: Pervasives.neg_infinity,
  max_float: Pervasives.max_float,
  min_float: Pervasives.min_float,
  epsilon_float: Pervasives.epsilon_float,
  classify_float: Pervasives.classify_float,
  char_of_int: Pervasives.char_of_int,
  $at: Pervasives.$at
};

function a0(prim) {
  return Math.abs(prim);
}

function a1(prim) {
  return Math.acos(prim);
}

function a2(prim) {
  return Math.tan(prim);
}

function a3(prim) {
  return Math.tanh(prim);
}

function a4(prim) {
  return Math.asin(prim);
}

function a5(prim0, prim1) {
  return Math.atan2(prim0, prim1);
}

function a6(prim) {
  return Math.atan(prim);
}

function a7(prim) {
  return Math.ceil(prim);
}

function a8(prim) {
  return Math.cos(prim);
}

function a9(prim) {
  return Math.cosh(prim);
}

function a10(prim) {
  return Math.exp(prim);
}

function a11(prim) {
  return Math.sin(prim);
}

function a12(prim) {
  return Math.sinh(prim);
}

function a13(prim) {
  return Math.sqrt(prim);
}

function a14(prim) {
  return Math.floor(prim);
}

function a15(prim) {
  return Math.log(prim);
}

function a16(prim) {
  return Math.log10(prim);
}

function a17(prim) {
  return Math.log1p(prim);
}

function a18(prim0, prim1) {
  return Math.pow(prim0, prim1);
}

let f = Pervasives.$at;

exports.Pervasives = Pervasives$1;
exports.f = f;
exports.a0 = a0;
exports.a1 = a1;
exports.a2 = a2;
exports.a3 = a3;
exports.a4 = a4;
exports.a5 = a5;
exports.a6 = a6;
exports.a7 = a7;
exports.a8 = a8;
exports.a9 = a9;
exports.a10 = a10;
exports.a11 = a11;
exports.a12 = a12;
exports.a13 = a13;
exports.a14 = a14;
exports.a15 = a15;
exports.a16 = a16;
exports.a17 = a17;
exports.a18 = a18;
/* No side effect */
