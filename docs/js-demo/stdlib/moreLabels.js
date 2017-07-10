'use strict';
define(["exports", "./map.js", "./set.js", "./hashtbl.js"],
  function(exports, $$Map, $$Set, Hashtbl){
    'use strict';
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
    
    exports.Hashtbl = Hashtbl$1;
    exports.$$Map   = $$Map$1;
    exports.$$Set   = $$Set$1;
    
  })
/* Hashtbl Not a pure module */
