

import * as Curry from "./curry.js";
import * as Hashtbl from "./hashtbl.js";

var add = Hashtbl.add;

var replace = Hashtbl.replace;

function iter(f, tbl) {
  Hashtbl.iter(Curry.__2(f), tbl);
}

function filter_map_inplace(f, tbl) {
  Hashtbl.filter_map_inplace(Curry.__2(f), tbl);
}

function fold(f, tbl, init) {
  return Hashtbl.fold(Curry.__3(f), tbl, init);
}

function MakeSeeded(H) {
  var include = Hashtbl.MakeSeeded(H);
  var add = include.add;
  var replace = include.replace;
  var iter = include.iter;
  var filter_map_inplace = include.filter_map_inplace;
  var fold = include.fold;
  var add$1 = Curry.__3(add);
  var replace$1 = Curry.__3(replace);
  var iter$1 = function (f, tbl) {
    Curry._2(iter, Curry.__2(f), tbl);
  };
  var filter_map_inplace$1 = function (f, tbl) {
    Curry._2(filter_map_inplace, Curry.__2(f), tbl);
  };
  var fold$1 = function (f, tbl, init) {
    return Curry._3(fold, Curry.__3(f), tbl, init);
  };
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
  var hash = function (_seed, x) {
    return Curry._1(H.hash, x);
  };
  var H_equal = H.equal;
  var H$1 = {
    equal: H_equal,
    hash: hash
  };
  var include = Hashtbl.MakeSeeded(H$1);
  var create = include.create;
  var add = include.add;
  var replace = include.replace;
  var iter = include.iter;
  var filter_map_inplace = include.filter_map_inplace;
  var fold = include.fold;
  var add$1 = Curry.__3(add);
  var replace$1 = Curry.__3(replace);
  var iter$1 = function (f, tbl) {
    Curry._2(iter, Curry.__2(f), tbl);
  };
  var filter_map_inplace$1 = function (f, tbl) {
    Curry._2(filter_map_inplace, Curry.__2(f), tbl);
  };
  var fold$1 = function (f, tbl, init) {
    return Curry._3(fold, Curry.__3(f), tbl, init);
  };
  var create$1 = function (sz) {
    return Curry._2(create, false, sz);
  };
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

var create = Hashtbl.create;

var clear = Hashtbl.clear;

var reset = Hashtbl.reset;

var copy = Hashtbl.copy;

var find = Hashtbl.find;

var find_opt = Hashtbl.find_opt;

var find_all = Hashtbl.find_all;

var mem = Hashtbl.mem;

var remove = Hashtbl.remove;

var length = Hashtbl.length;

var randomize = Hashtbl.randomize;

var is_randomized = Hashtbl.is_randomized;

var stats = Hashtbl.stats;

var hash = Hashtbl.hash;

var seeded_hash = Hashtbl.seeded_hash;

var hash_param = Hashtbl.hash_param;

var seeded_hash_param = Hashtbl.seeded_hash_param;

export {
  create ,
  clear ,
  reset ,
  copy ,
  find ,
  find_opt ,
  find_all ,
  mem ,
  remove ,
  length ,
  randomize ,
  is_randomized ,
  stats ,
  hash ,
  seeded_hash ,
  hash_param ,
  seeded_hash_param ,
  add ,
  replace ,
  iter ,
  filter_map_inplace ,
  fold ,
  MakeSeeded ,
  Make ,
}
/* No side effect */
