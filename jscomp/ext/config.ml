let version = "4.06.1+BS"
let standard_library =
  let (//) = Filename.concat in   
  Filename.dirname Sys.executable_name // Filename.parent_dir_name //  "lib" // "ocaml"
let standard_library_default = standard_library
let syntax_kind = ref `ml
let bs_only = ref true
let unsafe_empty_array = ref true


and cmi_magic_number = "Caml1999I022"

and ast_impl_magic_number = "Caml1999M022"
and ast_intf_magic_number = "Caml1999N022"
and cmt_magic_number = "Caml1999T022"

let load_path = ref ([] : string list)

let interface_suffix = ref ".mli"


(* This is normally the same as in obj.ml, but we have to define it
   separately because it can differ when we're in the middle of a
   bootstrapping phase. *)



let default_uncurry = ref false

let print_config oc =
  let p name valu = Printf.fprintf oc "%s: %s\n" name valu in
  p "version" version;
  p "standard_library_default" standard_library_default;
  p "standard_library" standard_library;
  (* print the magic number *)

  p "cmi_magic_number" cmi_magic_number;
  p "ast_impl_magic_number" ast_impl_magic_number;
  p "ast_intf_magic_number" ast_intf_magic_number;
  p "cmt_magic_number" cmt_magic_number;
  flush oc;
;;
