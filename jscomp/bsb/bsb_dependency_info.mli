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

(* Checks if a ppx is from a dependency, of the form "MyDep/PpxModuleName". 
   This form is analogous to how ppx-flags works on the top level.
   If yes, it'll change the path to be an absolute path to the PPX.
 *)
val check_if_dep : root_project_dir:string -> backend:Bsb_config_types.compilation_kind_t -> t -> string -> string
#end
