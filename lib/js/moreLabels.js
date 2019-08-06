'use strict';

var $$Map = require("./map.js");
var $$Set = require("./set.js");
var Hashtbl = require("./hashtbl.js");

var Hashtbl$1 = /* Hashtbl */[
  Hashtbl.create,
  Hashtbl.clear,
  Hashtbl.reset,
  Hashtbl.copy,
  Hashtbl.add,
  Hashtbl.find,
  Hashtbl.find_opt,
  Hashtbl.find_all,
  Hashtbl.mem,
  Hashtbl.remove,
  Hashtbl.replace,
  Hashtbl.iter,
  Hashtbl.filter_map_inplace,
  Hashtbl.fold,
  Hashtbl.length,
  Hashtbl.randomize,
  Hashtbl.is_randomized,
  Hashtbl.stats,
  Hashtbl.Make,
  Hashtbl.MakeSeeded,
  Hashtbl.hash,
  Hashtbl.seeded_hash,
  Hashtbl.hash_param,
  Hashtbl.seeded_hash_param
];

var $$Map$1 = /* Map */[$$Map.Make];

var $$Set$1 = /* Set */[$$Set.Make];

exports.Hashtbl = Hashtbl$1;
exports.$$Map = $$Map$1;
exports.$$Set = $$Set$1;
/* No side effect */
