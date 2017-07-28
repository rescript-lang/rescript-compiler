
val supported_format : string -> bool

(* val package_flag : format:string -> string -> string  *)

(* val package_output : format:string -> string -> string  *)


type package_specs
   (* = String_set.t *)

val default_package_specs : package_specs 

(* val cmd_package_specs : package_specs option ref  *)

val get_list_of_output_js : 
  package_specs -> string -> string list


val package_flag_of_package_specs : 
  package_specs -> string -> string

val get_package_specs_from_array : 
  Ext_json_types.t array -> package_specs  