

import * as Hashtbl from "./hashtbl.js";

function add(tbl, key, data) {
  Hashtbl.add(tbl, key, data);
}

function replace(tbl, key, data) {
  Hashtbl.replace(tbl, key, data);
}

function iter(f, tbl) {
  Hashtbl.iter(((key, data) => f(key, data)), tbl);
}

function filter_map_inplace(f, tbl) {
  Hashtbl.filter_map_inplace(((key, data) => f(key, data)), tbl);
}

function fold(f, tbl, init) {
  return Hashtbl.fold(((key, data, acc) => f(key, data, acc)), tbl, init);
}

function MakeSeeded(H) {
  let include = Hashtbl.MakeSeeded(H);
  let add = include.add;
  let replace = include.replace;
  let iter = include.iter;
  let filter_map_inplace = include.filter_map_inplace;
  let fold = include.fold;
  let add$1 = (tbl, key, data) => add(tbl, key, data);
  let replace$1 = (tbl, key, data) => replace(tbl, key, data);
  let iter$1 = (f, tbl) => iter(((key, data) => f(key, data)), tbl);
  let filter_map_inplace$1 = (f, tbl) => filter_map_inplace(((key, data) => f(key, data)), tbl);
  let fold$1 = (f, tbl, init) => fold(((key, data, acc) => f(key, data, acc)), tbl, init);
  return {
    create: include.create,
    clear: include.clear,
    reset: include.reset,
    copy: include.copy,
    add: add$1,
    remove: include.remove,
    find: include.find,
    find_opt: include.find_opt,
    find_all: include.find_all,
    replace: replace$1,
    mem: include.mem,
    iter: iter$1,
    filter_map_inplace: filter_map_inplace$1,
    fold: fold$1,
    length: include.length,
    stats: include.stats
  };
}

function Make(H) {
  let hash = (_seed, x) => H.hash(x);
  let H_equal = H.equal;
  let H$1 = {
    equal: H_equal,
    hash: hash
  };
  let include = Hashtbl.MakeSeeded(H$1);
  let create = include.create;
  let add = include.add;
  let replace = include.replace;
  let iter = include.iter;
  let filter_map_inplace = include.filter_map_inplace;
  let fold = include.fold;
  let add$1 = (tbl, key, data) => add(tbl, key, data);
  let replace$1 = (tbl, key, data) => replace(tbl, key, data);
  let iter$1 = (f, tbl) => iter(((key, data) => f(key, data)), tbl);
  let filter_map_inplace$1 = (f, tbl) => filter_map_inplace(((key, data) => f(key, data)), tbl);
  let fold$1 = (f, tbl, init) => fold(((key, data, acc) => f(key, data, acc)), tbl, init);
  let create$1 = sz => create(false, sz);
  return {
    create: create$1,
    clear: include.clear,
    reset: include.reset,
    copy: include.copy,
    add: add$1,
    remove: include.remove,
    find: include.find,
    find_opt: include.find_opt,
    find_all: include.find_all,
    replace: replace$1,
    mem: include.mem,
    iter: iter$1,
    filter_map_inplace: filter_map_inplace$1,
    fold: fold$1,
    length: include.length,
    stats: include.stats
  };
}

let create = Hashtbl.create;

let clear = Hashtbl.clear;

let reset = Hashtbl.reset;

let copy = Hashtbl.copy;

let find = Hashtbl.find;

let find_opt = Hashtbl.find_opt;

let find_all = Hashtbl.find_all;

let mem = Hashtbl.mem;

let remove = Hashtbl.remove;

let length = Hashtbl.length;

let randomize = Hashtbl.randomize;

let is_randomized = Hashtbl.is_randomized;

let stats = Hashtbl.stats;

let hash = Hashtbl.hash;

let seeded_hash = Hashtbl.seeded_hash;

let hash_param = Hashtbl.hash_param;

let seeded_hash_param = Hashtbl.seeded_hash_param;

export {
  create,
  clear,
  reset,
  copy,
  find,
  find_opt,
  find_all,
  mem,
  remove,
  length,
  randomize,
  is_randomized,
  stats,
  hash,
  seeded_hash,
  hash_param,
  seeded_hash_param,
  add,
  replace,
  iter,
  filter_map_inplace,
  fold,
  MakeSeeded,
  Make,
}
/* No side effect */
