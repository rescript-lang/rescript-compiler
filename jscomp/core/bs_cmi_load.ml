

(* TODO: provide native load*)
let browse_load ~unit_name : Env.Persistent_signature.t option=
  match Ext_string_array.find_sorted_assoc Builtin_cmi_datasets.module_sets_cmi unit_name with
  | Some cmi ->
    (* Format.fprintf Format.err_formatter "reading %s@." unit_name; *)
    Some {filename = Sys.executable_name ; 
          cmi = 
            Lazy.force cmi}
  | None -> assert false
