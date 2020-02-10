

(* TODO: provide native load*)
let browse_load ~unit_name : Env.Persistent_signature.t option=
  match Ext_string_array.find_sorted Builtin_cmi_datasets.module_sets unit_name with
  | Some index ->
    (* Format.fprintf Format.err_formatter "reading %s@." unit_name; *)
    Some {filename = Sys.executable_name ; 
          cmi = 
            Lazy.force Builtin_cmi_datasets.module_sets_cmi.(index)}
  | None -> assert false
