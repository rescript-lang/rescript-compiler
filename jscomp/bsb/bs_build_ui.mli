
type 'a file_group = 
  { dir : string ;
    sources : 'a
  } 

type t = 
  { files : Binary_cache.t file_group list ; 
    intervals :  Ext_file_pp.interval list ;
    globbed_dirs : string list ; 
  }


(** entry is to the 
    [sources] in the schema
*)
val parsing_sources : 
  Bs_json.t array ->
  t 
  
