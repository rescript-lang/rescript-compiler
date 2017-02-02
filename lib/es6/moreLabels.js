'use strict';

import * as $$Map   from "./map";
import * as $$Set   from "./set";
import * as Hashtbl from "./hashtbl";

var Hashtbl$1 = /* Hashtbl */[
  Hashtbl.create,
  Hashtbl.clear,
  Hashtbl.reset,
  Hashtbl.copy,
  Hashtbl.add,
  Hashtbl.find,
  Hashtbl.find_all,
  Hashtbl.mem,
  Hashtbl.remove,
  Hashtbl.replace,
  Hashtbl.iter,
  Hashtbl.fold,
  Hashtbl.length,
  Hashtbl.randomize,
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

export {
  Hashtbl$1 as Hashtbl,
  $$Map$1   as $$Map,
  $$Set$1   as $$Set,
  
}
/* Hashtbl Not a pure module */
