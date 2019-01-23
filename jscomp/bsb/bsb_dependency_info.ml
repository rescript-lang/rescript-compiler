#if BS_NATIVE then
type path = string

type t = {
  mutable all_external_deps: path list;
  mutable all_ocamlfind_dependencies: string list;
  mutable all_ocaml_dependencies: Depend.StringSet.t;
  mutable all_clibs: path list;
  mutable all_c_linker_flags: string list;
  mutable all_toplevel_ppxes: (Bsb_config_types.entries_t list) String_map.t;
}

let (//) = Ext_path.combine

let check_if_dep ~root_project_dir ~backend (dependency_info : t) flag_exec =
  let components = Ext_string.split_by (fun c -> c = Ext_path.sep_char) flag_exec in
  match components with 
  | [dep_name; entry_name] -> begin 
    match String_map.find_opt dep_name dependency_info.all_toplevel_ppxes with
    | None -> 
      Bsb_exception.no_package_found_for_ppx dep_name entry_name
    | Some l -> begin 
      match List.filter Bsb_config_types.(fun {main_module_name} -> main_module_name = entry_name) l with 
      | [] -> Bsb_exception.ppx_not_found_for_package entry_name dep_name
      | head :: _ -> 
        let nested = begin match backend with
          | Bsb_config_types.Js       -> "js"
          | Bsb_config_types.Native   -> "native"
          | Bsb_config_types.Bytecode -> "bytecode"
        end in 
        let extension = begin match head with
          | {backend = [NativeTarget]} -> ".native"
          | {backend = [BytecodeTarget]} -> ".byte"
          | _ -> assert false
        end in
        root_project_dir // Literals.node_modules // dep_name // Bsb_config.lib_bs // nested // entry_name ^ extension
      end
    end
  | _ -> flag_exec
#end
