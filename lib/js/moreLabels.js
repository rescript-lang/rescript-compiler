'use strict';

var $$Map = require("./map.js");
var $$Set = require("./set.js");
var Hashtbl = require("./hashtbl.js");

var Hashtbl$1 = /* Hashtbl */[
  /* create */Hashtbl.create,
  /* clear */Hashtbl.clear,
  /* reset */Hashtbl.reset,
  /* copy */Hashtbl.copy,
  /* add */Hashtbl.add,
  /* find */Hashtbl.find,
  /* find_all */Hashtbl.find_all,
  /* mem */Hashtbl.mem,
  /* remove */Hashtbl.remove,
  /* replace */Hashtbl.replace,
  /* iter */Hashtbl.iter,
  /* fold */Hashtbl.fold,
  /* length */Hashtbl.length,
  /* randomize */Hashtbl.randomize,
  /* stats */Hashtbl.stats,
  /* Make */Hashtbl.Make,
  /* MakeSeeded */Hashtbl.MakeSeeded,
  /* hash */Hashtbl.hash,
  /* seeded_hash */Hashtbl.seeded_hash,
  /* hash_param */Hashtbl.hash_param,
  /* seeded_hash_param */Hashtbl.seeded_hash_param
];

var $$Map$1 = /* Map */[/* Make */$$Map.Make];

var $$Set$1 = /* Set */[/* Make */$$Set.Make];

exports.Hashtbl = Hashtbl$1;
exports.$$Map = $$Map$1;
exports.$$Set = $$Set$1;
/* No side effect */
